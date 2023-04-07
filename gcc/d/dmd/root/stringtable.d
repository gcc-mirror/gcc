/**
 * A specialized associative array with string keys stored in a variable length structure.
 *
 * Copyright: Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * Authors:   Walter Bright, https://www.digitalmars.com
 * License:   $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:    $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/root/stringtable.d, root/_stringtable.d)
 * Documentation:  https://dlang.org/phobos/dmd_root_stringtable.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/root/stringtable.d
 */

module dmd.root.stringtable;

import core.stdc.string;
import dmd.root.rmem, dmd.root.hash;

private enum POOL_BITS = 12;
private enum POOL_SIZE = (1U << POOL_BITS);

/*
Returns the smallest integer power of 2 larger than val.
if val > 2^^63 on 64-bit targets or val > 2^^31 on 32-bit targets it enters an
endless loop because of overflow.
*/
private size_t nextpow2(size_t val) @nogc nothrow pure @safe
{
    size_t res = 1;
    while (res < val)
        res <<= 1;
    return res;
}

unittest
{
    assert(nextpow2(0) == 1);
    assert(nextpow2(0xFFFF) == (1 << 16));
    assert(nextpow2(size_t.max / 2) == size_t.max / 2 + 1);
    // note: nextpow2((1UL << 63) + 1) results in an endless loop
}

private enum loadFactorNumerator = 8;
private enum loadFactorDenominator = 10;        // for a load factor of 0.8

private struct StringEntry
{
    uint hash;
    uint vptr;
}

/********************************
 * StringValue is a variable-length structure. It has neither proper c'tors nor a
 * factory method because the only thing which should be creating these is StringTable.
 * The string characters are stored in memory immediately after the StringValue struct.
 */
struct StringValue(T)
{
    T value; //T is/should typically be a pointer or a slice
    private size_t length;
    /+
    char[length] chars;  // the string characters are stored here
     +/

    char* lstring() @nogc nothrow pure return
    {
        return cast(char*)(&this + 1);
    }

    size_t len() const @nogc nothrow pure @safe
    {
        return length;
    }

    const(char)* toDchars() const @nogc nothrow pure return
    {
        return cast(const(char)*)(&this + 1);
    }

    /// Returns: The content of this entry as a D slice
    const(char)[] toString() const @nogc nothrow pure
    {
        return (cast(inout(char)*)(&this + 1))[0 .. length];
    }
}

struct StringTable(T)
{
private:
    StringEntry[] table;
    ubyte*[] pools;
    size_t nfill;
    size_t count;
    size_t countTrigger;   // amount which will trigger growing the table

public:
    void _init(size_t size = 0) nothrow pure
    {
        size = nextpow2((size * loadFactorDenominator) / loadFactorNumerator);
        if (size < 32)
            size = 32;
        table = (cast(StringEntry*)mem.xcalloc(size, (table[0]).sizeof))[0 .. size];
        countTrigger = (table.length * loadFactorNumerator) / loadFactorDenominator;
        pools = null;
        nfill = 0;
        count = 0;
    }

    void reset(size_t size = 0) nothrow pure
    {
        freeMem();
        _init(size);
    }

    ~this() nothrow pure
    {
        freeMem();
    }

    /**
    Looks up the given string in the string table and returns its associated
    value.

    Params:
     s = the string to look up
     length = the length of $(D_PARAM s)
     str = the string to look up

    Returns: the string's associated value, or `null` if the string doesn't
     exist in the string table
    */
    inout(StringValue!T)* lookup(scope const(char)[] str) inout @nogc nothrow pure
    {
        const(size_t) hash = calcHash(str);
        const(size_t) i = findSlot(hash, str);
        // printf("lookup %.*s %p\n", cast(int)str.length, str.ptr, table[i].value ?: null);
        return getValue(table[i].vptr);
    }

    /// ditto
    inout(StringValue!T)* lookup(scope const(char)* s, size_t length) inout @nogc nothrow pure
    {
        return lookup(s[0 .. length]);
    }

    /**
    Inserts the given string and the given associated value into the string
    table.

    Params:
     s = the string to insert
     length = the length of $(D_PARAM s)
     ptrvalue = the value to associate with the inserted string
     str = the string to insert
     value = the value to associate with the inserted string

    Returns: the newly inserted value, or `null` if the string table already
     contains the string
    */
    StringValue!(T)* insert(scope const(char)[] str, T value) nothrow pure
    {
        const(size_t) hash = calcHash(str);
        size_t i = findSlot(hash, str);
        if (table[i].vptr)
            return null; // already in table
        if (++count > countTrigger)
        {
            grow();
            i = findSlot(hash, str);
        }
        table[i].hash = hash;
        table[i].vptr = allocValue(str, value);
        // printf("insert %.*s %p\n", cast(int)str.length, str.ptr, table[i].value ?: NULL);
        return getValue(table[i].vptr);
    }

    /// ditto
    StringValue!(T)* insert(scope const(char)* s, size_t length, T value) nothrow pure
    {
        return insert(s[0 .. length], value);
    }

    StringValue!(T)* update(scope const(char)[] str) nothrow pure
    {
        const(size_t) hash = calcHash(str);
        size_t i = findSlot(hash, str);
        if (!table[i].vptr)
        {
            if (++count > countTrigger)
            {
                grow();
                i = findSlot(hash, str);
            }
            table[i].hash = hash;
            table[i].vptr = allocValue(str, T.init);
        }
        // printf("update %.*s %p\n", cast(int)str.length, str.ptr, table[i].value ?: NULL);
        return getValue(table[i].vptr);
    }

    StringValue!(T)* update(scope const(char)* s, size_t length) nothrow pure
    {
        return update(s[0 .. length]);
    }

    /********************************
     * Walk the contents of the string table,
     * calling fp for each entry.
     * Params:
     *      fp = function to call. Returns !=0 to stop
     * Returns:
     *      last return value of fp call
     */
    int apply(int function(const(StringValue!T)*) nothrow fp) nothrow
    {
        foreach (const se; table)
        {
            if (!se.vptr)
                continue;
            const sv = getValue(se.vptr);
            int result = (*fp)(sv);
            if (result)
                return result;
        }
        return 0;
    }

    /// ditto
    int opApply(scope int delegate(const(StringValue!T)*) nothrow dg) nothrow
    {
        foreach (const se; table)
        {
            if (!se.vptr)
                continue;
            const sv = getValue(se.vptr);
            int result = dg(sv);
            if (result)
                return result;
        }
        return 0;
    }

private:
    /// Free all memory in use by this StringTable
    void freeMem() nothrow pure
    {
        foreach (pool; pools)
            mem.xfree(pool);
        mem.xfree(table.ptr);
        mem.xfree(pools.ptr);
        table = null;
        pools = null;
    }

    // Note that a copy is made of str
    uint allocValue(scope const(char)[] str, T value) nothrow pure
    {
        const(size_t) nbytes = (StringValue!T).sizeof + str.length + 1;
        if (!pools.length || nfill + nbytes > POOL_SIZE)
        {
            pools = (cast(ubyte**) mem.xrealloc(pools.ptr, (pools.length + 1) * (pools[0]).sizeof))[0 .. pools.length + 1];
            pools[$-1] = cast(ubyte*) mem.xmalloc(nbytes > POOL_SIZE ? nbytes : POOL_SIZE);
            if (mem.isGCEnabled)
                memset(pools[$ - 1], 0xff, POOL_SIZE); // 0xff less likely to produce GC pointer
            nfill = 0;
        }
        StringValue!(T)* sv = cast(StringValue!(T)*)&pools[$ - 1][nfill];
        sv.value = value;
        sv.length = str.length;
        .memcpy(sv.lstring(), str.ptr, str.length);
        sv.lstring()[str.length] = 0;
        const(uint) vptr = cast(uint)(pools.length << POOL_BITS | nfill);
        nfill += nbytes + (-nbytes & 7); // align to 8 bytes
        return vptr;
    }

    inout(StringValue!T)* getValue(uint vptr) inout @nogc nothrow pure
    {
        if (!vptr)
            return null;
        const(size_t) idx = (vptr >> POOL_BITS) - 1;
        const(size_t) off = vptr & POOL_SIZE - 1;
        return cast(inout(StringValue!T)*)&pools[idx][off];
    }

    size_t findSlot(hash_t hash, scope const(char)[] str) const @nogc nothrow pure
    {
        // quadratic probing using triangular numbers
        // https://stackoverflow.com/questions/2348187/moving-from-linear-probing-to-quadratic-probing-hash-collisons/2349774#2349774
        for (size_t i = hash & (table.length - 1), j = 1;; ++j)
        {
            const(StringValue!T)* sv;
            auto vptr = table[i].vptr;
            if (!vptr || table[i].hash == hash && (sv = getValue(vptr)).length == str.length && .memcmp(str.ptr, sv.toDchars(), str.length) == 0)
                return i;
            i = (i + j) & (table.length - 1);
        }
    }

    void grow() nothrow pure
    {
        const odim = table.length;
        auto otab = table;
        const ndim = table.length * 2;
        countTrigger = (ndim * loadFactorNumerator) / loadFactorDenominator;
        table = (cast(StringEntry*)mem.xcalloc_noscan(ndim, (table[0]).sizeof))[0 .. ndim];
        foreach (const se; otab[0 .. odim])
        {
            if (!se.vptr)
                continue;
            const sv = getValue(se.vptr);
            table[findSlot(se.hash, sv.toString())] = se;
        }
        mem.xfree(otab.ptr);
    }
}

nothrow unittest
{
    StringTable!(const(char)*) tab;
    tab._init(10);

    // construct two strings with the same text, but a different pointer
    const(char)[6] fooBuffer = "foofoo";
    const(char)[] foo = fooBuffer[0 .. 3];
    const(char)[] fooAltPtr = fooBuffer[3 .. 6];

    assert(foo.ptr != fooAltPtr.ptr);

    // first insertion returns value
    assert(tab.insert(foo, foo.ptr).value == foo.ptr);

    // subsequent insertion of same string return null
    assert(tab.insert(foo.ptr, foo.length, foo.ptr) == null);
    assert(tab.insert(fooAltPtr, foo.ptr) == null);

    const lookup = tab.lookup("foo");
    assert(lookup.value == foo.ptr);
    assert(lookup.len == 3);
    assert(lookup.toString() == "foo");

    assert(tab.lookup("bar") == null);
    tab.update("bar".ptr, "bar".length);
    assert(tab.lookup("bar").value == null);

    tab.reset(0);
    assert(tab.lookup("foo".ptr, "foo".length) == null);
    //tab.insert("bar");
}

nothrow unittest
{
    StringTable!(void*) tab;
    tab._init(100);

    enum testCount = 2000;

    char[2 * testCount] buf;

    foreach(i; 0 .. testCount)
    {
        buf[i * 2 + 0] = cast(char) (i % 256);
        buf[i * 2 + 1] = cast(char) (i / 256);
        auto toInsert = cast(const(char)[]) buf[i * 2 .. i * 2 + 2];
        tab.insert(toInsert, cast(void*) i);
    }

    foreach(i; 0 .. testCount)
    {
        auto toLookup = cast(const(char)[]) buf[i * 2 .. i * 2 + 2];
        assert(tab.lookup(toLookup).value == cast(void*) i);
    }
}

nothrow unittest
{
    StringTable!(int) tab;
    tab._init(10);
    tab.insert("foo",  4);
    tab.insert("bar",  6);

    static int resultFp = 0;
    int resultDg = 0;
    static bool returnImmediately = false;

    int function(const(StringValue!int)*) nothrow applyFunc = (const(StringValue!int)* s)
    {
        resultFp += s.value;
        return returnImmediately;
    };

    scope int delegate(const(StringValue!int)*) nothrow applyDeleg = (const(StringValue!int)* s)
    {
        resultDg += s.value;
        return returnImmediately;
    };

    tab.apply(applyFunc);
    tab.opApply(applyDeleg);

    assert(resultDg == 10);
    assert(resultFp == 10);

    returnImmediately = true;

    tab.apply(applyFunc);
    tab.opApply(applyDeleg);

    // Order of string table iteration is not specified, either foo or bar could
    // have been visited first.
    assert(resultDg == 14 || resultDg == 16);
    assert(resultFp == 14 || resultFp == 16);
}
