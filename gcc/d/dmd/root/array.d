
/**
 * Dynamic array implementation.
 *
 * Copyright:   Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/root/array.d, root/_array.d)
 * Documentation:  https://dlang.org/phobos/dmd_root_array.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/root/array.d
 */

module dmd.root.array;

import core.stdc.stdlib : _compare_fp_t;
import core.stdc.string;

import dmd.root.rmem;
import dmd.root.string;

// `qsort` is only `nothrow` since 2.081.0
private extern(C) void qsort(scope void* base, size_t nmemb, size_t size, _compare_fp_t compar) nothrow @nogc;


debug
{
    debug = stomp; // flush out dangling pointer problems by stomping on unused memory
}

extern (C++) struct Array(T)
{
    size_t length;

private:
    T[] data;
    enum SMALLARRAYCAP = 1;
    T[SMALLARRAYCAP] smallarray; // inline storage for small arrays

public:
    /*******************
     * Params:
     *  dim = initial length of array
     */
    this(size_t dim) pure nothrow scope
    {
        reserve(dim);
        this.length = dim;
    }

    @disable this(this);

    ~this() pure nothrow
    {
        debug (stomp) memset(data.ptr, 0xFF, data.length);
        if (data.ptr != &smallarray[0])
            mem.xfree(data.ptr);
    }
    ///returns elements comma separated in []
    extern(D) const(char)[] toString() const
    {
        static const(char)[] toStringImpl(alias toStringFunc, Array)(Array* a, bool quoted = false)
        {
            const(char)[][] buf = (cast(const(char)[]*)mem.xcalloc((char[]).sizeof, a.length))[0 .. a.length];
            size_t len = 2; // [ and ]
            const seplen = quoted ? 3 : 1; // ',' or null terminator and optionally '"'
            if (a.length == 0)
                len += 1; // null terminator
            else
            {
                foreach (u; 0 .. a.length)
                {
                    static if (is(typeof(a.data[u] is null)))
                    {
                        if (a.data[u] is null)
                            buf[u] = "null";
                        else
                            buf[u] = toStringFunc(a.data[u]);
                    }
                    else
                    {
                        buf[u] = toStringFunc(a.data[u]);
                    }

                    len += buf[u].length + seplen;
                }
            }
            char[] str = (cast(char*)mem.xmalloc_noscan(len))[0..len];

            str[0] = '[';
            char* p = str.ptr + 1;
            foreach (u; 0 .. a.length)
            {
                if (u)
                    *p++ = ',';
                if (quoted)
                    *p++ = '"';
                memcpy(p, buf[u].ptr, buf[u].length);
                p += buf[u].length;
                if (quoted)
                    *p++ = '"';
            }
            *p++ = ']';
            *p = 0;
            assert(p - str.ptr == str.length - 1); // null terminator
            mem.xfree(buf.ptr);
            return str[0 .. $-1];
        }

        static if (is(typeof(T.init.toString())))
        {
            return toStringImpl!(a => a.toString)(&this);
        }
        else static if (is(typeof(T.init.toDString())))
        {
            return toStringImpl!(a => a.toDString)(&this, true);
        }
        else
        {
            assert(0);
        }
    }
    ///ditto
    const(char)* toChars() const
    {
        return toString.ptr;
    }

    ref Array push(T ptr) return pure nothrow
    {
        reserve(1);
        data[length++] = ptr;
        return this;
    }

    extern (D) ref Array pushSlice(T[] a) return pure nothrow
    {
        const oldLength = length;
        setDim(oldLength + a.length);
        memcpy(data.ptr + oldLength, a.ptr, a.length * T.sizeof);
        return this;
    }

    ref Array append(typeof(this)* a) return pure nothrow
    {
        insert(length, a);
        return this;
    }

    void reserve(size_t nentries) pure nothrow
    {
        //printf("Array::reserve: length = %d, data.length = %d, nentries = %d\n", cast(int)length, cast(int)data.length, cast(int)nentries);

        // Cold path
        void enlarge(size_t nentries)
        {
            pragma(inline, false);      // never inline cold path
            if (data.length == 0)
            {
                // Not properly initialized, someone memset it to zero
                if (nentries <= SMALLARRAYCAP)
                {
                    data = SMALLARRAYCAP ? smallarray[] : null;
                }
                else
                {
                    auto p = cast(T*)mem.xmalloc(nentries * T.sizeof);
                    data = p[0 .. nentries];
                }
            }
            else if (data.length == SMALLARRAYCAP)
            {
                const allocdim = length + nentries;
                auto p = cast(T*)mem.xmalloc(allocdim * T.sizeof);
                memcpy(p, smallarray.ptr, length * T.sizeof);
                data = p[0 .. allocdim];
            }
            else
            {
                /* Increase size by 1.5x to avoid excessive memory fragmentation
                 */
                auto increment = length / 2;
                if (nentries > increment)       // if 1.5 is not enough
                    increment = nentries;
                const allocdim = length + increment;
                debug (stomp)
                {
                    // always move using allocate-copy-stomp-free
                    auto p = cast(T*)mem.xmalloc(allocdim * T.sizeof);
                    memcpy(p, data.ptr, length * T.sizeof);
                    memset(data.ptr, 0xFF, data.length * T.sizeof);
                    mem.xfree(data.ptr);
                }
                else
                    auto p = cast(T*)mem.xrealloc(data.ptr, allocdim * T.sizeof);
                data = p[0 .. allocdim];
            }

            debug (stomp)
            {
                if (length < data.length)
                    memset(data.ptr + length, 0xFF, (data.length - length) * T.sizeof);
            }
            else
            {
                if (mem.isGCEnabled)
                    if (length < data.length)
                        memset(data.ptr + length, 0xFF, (data.length - length) * T.sizeof);
            }
        }

        if (data.length - length < nentries)  // false means hot path
            enlarge(nentries);
    }

    void remove(size_t i) pure nothrow @nogc
    {
        if (length - i - 1)
            memmove(data.ptr + i, data.ptr + i + 1, (length - i - 1) * T.sizeof);
        length--;
        debug (stomp) memset(data.ptr + length, 0xFF, T.sizeof);
    }

    void insert(size_t index, typeof(this)* a) pure nothrow
    {
        if (a)
        {
            size_t d = a.length;
            reserve(d);
            if (length != index)
                memmove(data.ptr + index + d, data.ptr + index, (length - index) * T.sizeof);
            memcpy(data.ptr + index, a.data.ptr, d * T.sizeof);
            length += d;
        }
    }

    extern (D) void insert(size_t index, T[] a) pure nothrow
    {
        size_t d = a.length;
        reserve(d);
        if (length != index)
            memmove(data.ptr + index + d, data.ptr + index, (length - index) * T.sizeof);
        memcpy(data.ptr + index, a.ptr, d * T.sizeof);
        length += d;
    }

    void insert(size_t index, T ptr) pure nothrow
    {
        reserve(1);
        memmove(data.ptr + index + 1, data.ptr + index, (length - index) * T.sizeof);
        data[index] = ptr;
        length++;
    }

    void setDim(size_t newdim) pure nothrow
    {
        if (length < newdim)
        {
            reserve(newdim - length);
        }
        length = newdim;
    }

    size_t find(T ptr) const nothrow pure
    {
        foreach (i; 0 .. length)
            if (data[i] is ptr)
                return i;
        return size_t.max;
    }

    bool contains(T ptr) const nothrow pure
    {
        return find(ptr) != size_t.max;
    }

    ref inout(T) opIndex(size_t i) inout nothrow pure
    {
        debug
            // This is called so often the array bounds become expensive
            return data[i];
        else
            return data.ptr[i];
    }

    inout(T)* tdata() inout pure nothrow @nogc @trusted
    {
        return data.ptr;
    }

    Array!T* copy() const pure nothrow
    {
        auto a = new Array!T();
        a.setDim(length);
        memcpy(a.data.ptr, data.ptr, length * T.sizeof);
        return a;
    }

    void shift(T ptr) pure nothrow
    {
        reserve(1);
        memmove(data.ptr + 1, data.ptr, length * T.sizeof);
        data[0] = ptr;
        length++;
    }

    void zero() nothrow pure @nogc
    {
        data[0 .. length] = T.init;
    }

    T pop() nothrow pure @nogc
    {
        debug (stomp)
        {
            assert(length);
            auto result = data[length - 1];
            remove(length - 1);
            return result;
        }
        else
            return data[--length];
    }

    extern (D) inout(T)[] opSlice() inout nothrow pure @nogc
    {
        return data[0 .. length];
    }

    extern (D) inout(T)[] opSlice(size_t a, size_t b) inout nothrow pure @nogc
    {
        assert(a <= b && b <= length);
        return data[a .. b];
    }

    /**
     * Sort the elements of an array
     *
     * This function relies on `qsort`.
     *
     * Params:
     *   pred = Predicate to use. Should be a function of type
     *          `int function(scope const T*  e1, scope const T* e2) nothrow`.
     *          The return value of this function should follow the
     *          usual C rule: `e1 >= e2 ? (e1 > e2) : -1`.
     *          The function can have D linkage.
     *
     * Returns:
     *   A reference to this, for easy chaining.
     */
    extern(D) ref typeof(this) sort (alias pred) () nothrow
    {
        if (this.length < 2)
            return this;
        qsort(this.data.ptr, this.length, T.sizeof, &arraySortWrapper!(T, pred));
        return this;
    }

    /// Ditto, but use `opCmp` by default
    extern(D) ref typeof(this) sort () () nothrow
        if (is(typeof(this.data[0].opCmp(this.data[1])) : int))
    {
        return this.sort!(function (scope const(T)* pe1, scope const(T)* pe2) => pe1.opCmp(*pe2));
    }

    alias opDollar = length;

    deprecated("use `.length` instead")
    extern(D) size_t dim() const @nogc nothrow pure @safe { return length; }
}

unittest
{
    // Test for objects implementing toString()
    static struct S
    {
        int s = -1;
        string toString() const
        {
            return "S";
        }
    }
    auto array = Array!S(4);
    assert(array.toString() == "[S,S,S,S]");
    array.setDim(0);
    assert(array.toString() == "[]");

    // Test for toDString()
    auto strarray = Array!(const(char)*)(2);
    strarray[0] = "hello";
    strarray[1] = "world";
    auto str = strarray.toString();
    assert(str == `["hello","world"]`);
    // Test presence of null terminator.
    assert(str.ptr[str.length] == '\0');

    // Test printing an array of classes, which can be null
    static class C
    {
        override string toString() const
        {
            return "x";
        }
    }
    auto nullarray = Array!C(2);
    nullarray[0] = new C();
    nullarray[1] = null;
    assert(nullarray.toString() == `[x,null]`);
}

unittest
{
    auto array = Array!double(4);
    array.shift(10);
    array.push(20);
    array[2] = 15;
    assert(array[0] == 10);
    assert(array.find(10) == 0);
    assert(array.find(20) == 5);
    assert(!array.contains(99));
    array.remove(1);
    assert(array.length == 5);
    assert(array[1] == 15);
    assert(array.pop() == 20);
    assert(array.length == 4);
    array.insert(1, 30);
    assert(array[1] == 30);
    assert(array[2] == 15);
}

unittest
{
    auto arrayA = Array!int(0);
    int[3] buf = [10, 15, 20];
    arrayA.pushSlice(buf);
    assert(arrayA[] == buf[]);
    auto arrayPtr = arrayA.copy();
    assert(arrayPtr);
    assert((*arrayPtr)[] == arrayA[]);
    assert(arrayPtr.tdata != arrayA.tdata);

    arrayPtr.setDim(0);
    int[2] buf2 = [100, 200];
    arrayPtr.pushSlice(buf2);

    arrayA.append(arrayPtr);
    assert(arrayA[3..$] == buf2[]);
    arrayA.insert(0, arrayPtr);
    assert(arrayA[] == [100, 200, 10, 15, 20, 100, 200]);

    arrayA.zero();
    foreach(e; arrayA)
        assert(e == 0);

    arrayA.setDim(0);
    arrayA.pushSlice([5, 6]);
    arrayA.insert(1, [1, 2]);
    assert(arrayA[] == [5, 1, 2, 6]);
    arrayA.insert(0, [7, 8]);
    arrayA.insert(arrayA.length, [0, 9]);
    assert(arrayA[] == [7, 8, 5, 1, 2, 6, 0, 9]);
}

/**
 * Exposes the given root Array as a standard D array.
 * Params:
 *  array = the array to expose.
 * Returns:
 *  The given array exposed to a standard D array.
 */
@property inout(T)[] peekSlice(T)(inout(Array!T)* array) pure nothrow @nogc
{
    return array ? (*array)[] : null;
}

/**
 * Splits the array at $(D index) and expands it to make room for $(D length)
 * elements by shifting everything past $(D index) to the right.
 * Params:
 *  array = the array to split.
 *  index = the index to split the array from.
 *  length = the number of elements to make room for starting at $(D index).
 */
void split(T)(ref Array!T array, size_t index, size_t length) pure nothrow
{
    if (length > 0)
    {
        auto previousDim = array.length;
        array.setDim(array.length + length);
        for (size_t i = previousDim; i > index;)
        {
            i--;
            array[i + length] = array[i];
        }
    }
}
unittest
{
    auto array = Array!int();
    array.split(0, 0);
    assert([] == array[]);
    array.push(1).push(3);
    array.split(1, 1);
    array[1] = 2;
    assert([1, 2, 3] == array[]);
    array.split(2, 3);
    array[2] = 8;
    array[3] = 20;
    array[4] = 4;
    assert([1, 2, 8, 20, 4, 3] == array[]);
    array.split(0, 0);
    assert([1, 2, 8, 20, 4, 3] == array[]);
    array.split(0, 1);
    array[0] = 123;
    assert([123, 1, 2, 8, 20, 4, 3] == array[]);
    array.split(0, 3);
    array[0] = 123;
    array[1] = 421;
    array[2] = 910;
    assert([123, 421, 910, 123, 1, 2, 8, 20, 4, 3] == (&array).peekSlice());
}

/**
 * Reverse an array in-place.
 * Params:
 *      a = array
 * Returns:
 *      reversed a[]
 */
T[] reverse(T)(T[] a) pure nothrow @nogc @safe
{
    if (a.length > 1)
    {
        const mid = (a.length + 1) >> 1;
        foreach (i; 0 .. mid)
        {
            T e = a[i];
            a[i] = a[$ - 1 - i];
            a[$ - 1 - i] = e;
        }
    }
    return a;
}

unittest
{
    int[] a1 = [];
    assert(reverse(a1) == []);
    int[] a2 = [2];
    assert(reverse(a2) == [2]);
    int[] a3 = [2,3];
    assert(reverse(a3) == [3,2]);
    int[] a4 = [2,3,4];
    assert(reverse(a4) == [4,3,2]);
    int[] a5 = [2,3,4,5];
    assert(reverse(a5) == [5,4,3,2]);
}

unittest
{
    //test toString/toChars.  Identifier is a simple object that has a usable .toString
    import dmd.identifier : Identifier;
    import core.stdc.string : strcmp;

    auto array = Array!Identifier();
    array.push(new Identifier("id1"));
    array.push(new Identifier("id2"));

    string expected = "[id1,id2]";
    assert(array.toString == expected);
    assert(strcmp(array.toChars, expected.ptr) == 0);
}

/// Predicate to wrap a D function passed to `qsort`
private template arraySortWrapper(T, alias fn)
{
    pragma(mangle, "arraySortWrapper_" ~ T.mangleof ~ "_" ~ fn.mangleof)
    extern(C) int arraySortWrapper(scope const void* e1, scope const void* e2)
    {
        return fn(cast(const(T*))e1, cast(const(T*))e2);
    }
}

// Test sorting
unittest
{
    Array!(const(char)*) strings;
    strings.push("World");
    strings.push("Foo");
    strings.push("baguette");
    strings.push("Avocado");
    strings.push("Hello");
    // Newer frontend versions will work with (e1, e2) and infer the type
    strings.sort!(function (scope const char** e1, scope const char** e2) => strcmp(*e1, *e2));
    assert(strings[0] == "Avocado");
    assert(strings[1] == "Foo");
    assert(strings[2] == "Hello");
    assert(strings[3] == "World");
    assert(strings[4] == "baguette");

    /// opCmp automatically supported
    static struct MyStruct
    {
        int a;

        int opCmp(const ref MyStruct other) const nothrow
        {
            // Reverse order
            return other.a - this.a;
        }
    }

    Array!MyStruct arr1;
    arr1.push(MyStruct(2));
    arr1.push(MyStruct(4));
    arr1.push(MyStruct(256));
    arr1.push(MyStruct(42));
    arr1.sort();
    assert(arr1[0].a == 256);
    assert(arr1[1].a == 42);
    assert(arr1[2].a == 4);
    assert(arr1[3].a == 2);

    /// But only if user defined
    static struct OtherStruct
    {
        int a;

        static int pred (scope const OtherStruct* pe1, scope const OtherStruct* pe2)
            nothrow @nogc pure @safe
        {
            return pe1.a - pe2.a;
        }
    }

    static assert (!is(typeof(Array!(OtherStruct).init.sort())));
    static assert (!is(typeof(Array!(OtherStruct).init.sort!pred)));
}

/**
 * Iterates the given array and calls the given callable for each element.
 *
 * Use this instead of `foreach` when the array may expand during iteration.
 *
 * Params:
 *  callable = the callable to call for each element
 *  array = the array to iterate
 *
 * See_Also: $(REF foreachDsymbol, dmd, dsymbol)
 */
template each(alias callable, T)
if (is(ReturnType!(typeof((T t) => callable(t))) == void))
{
    void each(ref Array!T array)
    {
        // Do not use foreach, as the size of the array may expand during iteration
        for (size_t i = 0; i < array.length; ++i)
            callable(array[i]);
    }

    void each(Array!T* array)
    {
        if (array)
            each!callable(*array);
    }
}

///
@("iterate over an Array") unittest
{
    static immutable expected = [2, 3, 4, 5];

    Array!int array;

    foreach (e ; expected)
        array.push(e);

    int[] result;
    array.each!((e) {
        result ~= e;
    });

    assert(result == expected);
}

@("iterate over a pointer to an Array") unittest
{
    static immutable expected = [2, 3, 4, 5];

    auto array = new Array!int;

    foreach (e ; expected)
        array.push(e);

    int[] result;
    array.each!((e) {
        result ~= e;
    });

    assert(result == expected);
}

@("iterate while appending to the array being iterated") unittest
{
    static immutable expected = [2, 3, 4, 5];

    Array!int array;

    foreach (e ; expected[0 .. $ - 1])
        array.push(e);

    int[] result;

    array.each!((e) {
        if (e == 2)
            array.push(5);

        result ~= e;
    });

    assert(array[] == expected);
    assert(result == expected);
}

/**
 * Iterates the given array and calls the given callable for each element.
 *
 * If `callable` returns `!= 0`, it will stop the iteration and return that
 * value, otherwise it will return 0.
 *
 * Use this instead of `foreach` when the array may expand during iteration.
 *
 * Params:
 *  callable = the callable to call for each element
 *  array = the array to iterate
 *
 * Returns: the last value returned by `callable`
 * See_Also: $(REF foreachDsymbol, dmd, dsymbol)
 */
template each(alias callable, T)
if (is(ReturnType!(typeof((T t) => callable(t))) == int))
{
    int each(ref Array!T array)
    {
        // Do not use foreach, as the size of the array may expand during iteration
        for (size_t i = 0; i < array.length; ++i)
        {
            if (const result = callable(array[i]))
                return result;
        }

        return 0;
    }

    int each(Array!T* array)
    {
        return array ? each!callable(*array) : 0;
    }
}

///
@("iterate over an Array and stop the iteration") unittest
{
    Array!int array;

    foreach (e ; [2, 3, 4, 5])
        array.push(e);

    int[] result;
    const returnValue = array.each!((e) {
        result ~= e;

        if (e == 3)
            return 8;

        return 0;
    });

    assert(result == [2, 3]);
    assert(returnValue == 8);
}

@("iterate over an Array") unittest
{
    static immutable expected = [2, 3, 4, 5];

    Array!int array;

    foreach (e ; expected)
        array.push(e);

    int[] result;
    const returnValue = array.each!((e) {
        result ~= e;
        return 0;
    });

    assert(result == expected);
    assert(returnValue == 0);
}

@("iterate over a pointer to an Array and stop the iteration") unittest
{
    auto array = new Array!int;

    foreach (e ; [2, 3, 4, 5])
        array.push(e);

    int[] result;
    const returnValue = array.each!((e) {
        result ~= e;

        if (e == 3)
            return 9;

        return 0;
    });

    assert(result == [2, 3]);
    assert(returnValue == 9);
}

@("iterate while appending to the array being iterated and stop the iteration") unittest
{
    Array!int array;

    foreach (e ; [2, 3])
        array.push(e);

    int[] result;

    const returnValue = array.each!((e) {
        if (e == 2)
            array.push(1);

        result ~= e;

        if (e == 1)
            return 7;

        return 0;
    });

    static immutable expected = [2, 3, 1];

    assert(array[] == expected);
    assert(result == expected);
    assert(returnValue == 7);
}

/// Returns: A static array constructed from `array`.
pragma(inline, true) T[n] staticArray(T, size_t n)(auto ref T[n] array)
{
    return array;
}

///
pure nothrow @safe @nogc unittest
{
    enum a = [0, 1].staticArray;
    static assert(is(typeof(a) == int[2]));
    static assert(a == [0, 1]);
}

/// Returns: `true` if the two given ranges are equal
bool equal(Range1, Range2)(Range1 range1, Range2 range2)
{
    template isArray(T)
    {
        static if (is(T U : U[]))
            enum isArray = true;

        else
            enum isArray = false;
    }

    static if (isArray!Range1 && isArray!Range2 && is(typeof(range1 == range2)))
        return range1 == range2;

    else
    {
        static if (hasLength!Range1 && hasLength!Range2 && is(typeof(r1.length == r2.length)))
        {
            if (range1.length != range2.length)
                return false;
        }

        for (; !range1.empty; range1.popFront(), range2.popFront())
        {
            if (range2.empty)
                return false;

            if (range1.front != range2.front)
                return false;
        }

        return range2.empty;
    }
}

///
pure nothrow @nogc @safe unittest
{
    enum a = [ 1, 2, 4, 3 ].staticArray;
    static assert(!equal(a[], a[1..$]));
    static assert(equal(a[], a[]));

    // different types
    enum b = [ 1.0, 2, 4, 3].staticArray;
    static assert(!equal(a[], b[1..$]));
    static assert(equal(a[], b[]));
}

pure nothrow @safe unittest
{
    static assert(equal([1, 2, 3].map!(x => x * 2), [1, 2, 3].map!(x => x * 2)));

    static assert(!equal([1, 2].map!(x => x * 2), [1, 2, 3].map!(x => x * 2)));
}

/**
 * Lazily filters the given range based on the given predicate.
 *
 * Returns: a range containing only elements for which the predicate returns
 *  `true`
 */
auto filter(alias predicate, Range)(Range range)
if (isInputRange!(Unqual!Range) && isPredicateOf!(predicate, ElementType!Range))
{
    return Filter!(predicate, Range)(range);
}

///
pure nothrow @safe @nogc unittest
{
    enum a = [1, 2, 3, 4].staticArray;
    enum result = a[].filter!(e => e > 2);

    enum expected = [3, 4].staticArray;
    static assert(result.equal(expected[]));
}

private struct Filter(alias predicate, Range)
{
    private Range range;
    private bool primed;

    private void prime()
    {
        if (primed)
            return;

        while (!range.empty && !predicate(range.front))
            range.popFront();

        primed = true;
    }

    @property bool empty()
    {
        prime();
        return range.empty;
    }

    @property auto front()
    {
        assert(!range.empty);
        prime();
        return range.front;
    }

    void popFront()
    {
        assert(!range.empty);
        prime();

        do
        {
            range.popFront();
        } while (!range.empty && !predicate(range.front));
    }

    auto opSlice()
    {
        return this;
    }
}

/**
 * Lazily iterates the given range and calls the given callable for each element.
 *
 * Returns: a range containing the result of each call to `callable`
 */
auto map(alias callable, Range)(Range range)
if (isInputRange!(Unqual!Range) && isCallableWith!(callable, ElementType!Range))
{
    return Map!(callable, Range)(range);
}

///
pure nothrow @safe @nogc unittest
{
    enum a = [1, 2, 3, 4].staticArray;
    enum expected = [2, 4, 6, 8].staticArray;

    enum result = a[].map!(e => e * 2);
    static assert(result.equal(expected[]));
}

private struct Map(alias callable, Range)
{
    private Range range;

    @property bool empty()
    {
        return range.empty;
    }

    @property auto front()
    {
        assert(!range.empty);
        return callable(range.front);
    }

    void popFront()
    {
        assert(!range.empty);
        range.popFront();
    }

    static if (hasLength!Range)
    {
        @property auto length()
        {
            return range.length;
        }

        alias opDollar = length;
    }
}

/// Returns: the length of the given range.
auto walkLength(Range)(Range range)
if (isInputRange!Range )
{
    static if (hasLength!Range)
        return range.length;
    else
    {
        size_t result;
        for (; !range.empty; range.popFront())
            ++result;
        return result;
    }
}

///
pure nothrow @safe @nogc unittest
{
    enum a = [1, 2, 3, 4].staticArray;
    static assert(a[].walkLength == 4);

    enum c = a[].filter!(e => e > 2);
    static assert(c.walkLength == 2);
}

/// Evaluates to the element type of `R`.
template ElementType(R)
{
    static if (is(typeof(R.init.front.init) T))
        alias ElementType = T;
    else
        alias ElementType = void;
}

/// Evaluates to `true` if the given type satisfy the input range interface.
enum isInputRange(R) =
    is(typeof(R.init) == R)
    && is(ReturnType!(typeof((R r) => r.empty)) == bool)
    && is(typeof((return ref R r) => r.front))
    && !is(ReturnType!(typeof((R r) => r.front)) == void)
    && is(typeof((R r) => r.popFront));

/// Evaluates to `true` if `func` can be called with a value of `T` and returns
/// a value that is convertible to `bool`.
enum isPredicateOf(alias func, T) = is(typeof((T t) => !func(t)));

/// Evaluates to `true` if `func` be called withl a value of `T`.
enum isCallableWith(alias func, T) =
    __traits(compiles, { auto _ = (T t) => func(t); });

private:

template ReturnType(T)
{
    static if (is(T R == return))
        alias ReturnType = R;
    else
        static assert(false, "argument is not a function");
}

alias Unqual(T) = ReturnType!(typeof((T t) => cast() t));

template hasLength(Range)
{
    static if (is(typeof(((Range* r) => r.length)(null)) Length))
        enum hasLength = is(Length == size_t);
    else
        enum hasLength = false;
}

/// Implements the range interface primitive `front` for built-in arrays.
@property ref inout(T) front(T)(return scope inout(T)[] a) pure nothrow @nogc @safe
{
    assert(a.length, "Attempting to fetch the front of an empty array of " ~ T.stringof);
    return a[0];
}

///
pure nothrow @nogc @safe unittest
{
    enum a = [1, 2, 3].staticArray;
    static assert(a[].front == 1);
}

/// Implements the range interface primitive `empty` for types that obey $(LREF hasLength) property
@property bool empty(T)(auto ref scope T a)
if (is(typeof(a.length) : size_t))
{
    return !a.length;
}

///
pure nothrow @nogc @safe unittest
{
    enum a = [1, 2, 3].staticArray;

    static assert(!a.empty);
    static assert(a[3 .. $].empty);
}

pure nothrow @safe unittest
{
    int[string] b;
    assert(b.empty);
    b["zero"] = 0;
    assert(!b.empty);
}

/// Implements the range interface primitive `popFront` for built-in arrays.
void popFront(T)(/*scope*/ ref inout(T)[] array) pure nothrow @nogc @safe
{                // does not compile with GDC 9 if this is `scope`
    assert(array.length, "Attempting to popFront() past the end of an array of " ~ T.stringof);
    array = array[1 .. $];
}

///
pure nothrow @nogc @safe unittest
{
    auto a = [1, 2, 3].staticArray;
    auto b = a[];
    auto expected = [2, 3].staticArray;

    b.popFront();
    assert(b == expected[]);
}
