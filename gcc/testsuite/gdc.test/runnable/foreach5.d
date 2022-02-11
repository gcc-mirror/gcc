/*
EXTRA_FILES: imports/test15777a.d imports/test15777b.d
TEST_OUTPUT:
---
int
double
foobar7406(T)
test7406()
int
foobar7406(T)
int
test7406()
---
*/

extern(C) int printf(const char* fmt, ...);

alias AliasSeq(X...) = X;

/***************************************/

void test1()
{
    char[] a;

    int foo()
    {
        printf("foo\n");
        a ~= "foo";
        return 10;
    }

    foreach (i; 0 .. foo())
    {
        printf("%d\n", i);
        a ~= cast(char)('0' + i);
    }
    assert(a == "foo0123456789");

    foreach_reverse (i; 0 .. foo())
    {
        printf("%d\n", i);
        a ~= cast(char)('0' + i);
    }
    assert(a == "foo0123456789foo9876543210");
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=2411

struct S2411
{
    int n;
    string s;
}

void test2411()
{
    S2411 s;
    assert(s.n == 0);
    assert(s.s == "");
    foreach (i, ref e; s.tupleof)
    {
        static if (i == 0)
            e = 10;
        static if (i == 1)
            e = "str";
    }
    assert(s.n == 10);
    assert(s.s == "str");
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=2442

template canForeach(T, E)
{
    enum canForeach = __traits(compiles,
    {
        foreach(a; new T)
        {
            static assert(is(typeof(a) == E));
        }
    });
}

void test2442()
{
    struct S1
    {
        int opApply(int delegate(ref const(int) v) dg) const { return 0; }
        int opApply(int delegate(ref int v) dg)              { return 0; }
    }
          S1 ms1;
    const S1 cs1;
    foreach (x; ms1) { static assert(is(typeof(x) ==       int)); }
    foreach (x; cs1) { static assert(is(typeof(x) == const int)); }

    struct S2
    {
        int opApply(int delegate(ref  int v) dg) { return 0; }
        int opApply(int delegate(ref long v) dg) { return 0; }
    }
    S2 ms2;
    static assert(!__traits(compiles, { foreach (    x; ms2) {} }));    // ambiguous
    static assert( __traits(compiles, { foreach (int x; ms2) {} }));

    struct S3
    {
        int opApply(int delegate(ref int v) dg) const        { return 0; }
        int opApply(int delegate(ref int v) dg) shared const { return 0; }
    }
    immutable S3 ms3;
    static assert(!__traits(compiles, { foreach (int x; ms3) {} }));    // ambiguous

    // from https://github.com/dlang/dmd/pull/120
    static class C
    {
        int opApply(int delegate(ref              int v) dg)              { return 0; }
        int opApply(int delegate(ref        const int v) dg) const        { return 0; }
        int opApply(int delegate(ref    immutable int v) dg) immutable    { return 0; }
        int opApply(int delegate(ref       shared int v) dg) shared       { return 0; }
        int opApply(int delegate(ref shared const int v) dg) shared const { return 0; }
    }
    static class D
    {
        int opApply(int delegate(ref int v) dg) const        { return 0; }
    }
    static class E
    {
        int opApply(int delegate(ref int v) dg) shared const { return 0; }
    }

    static assert( canForeach!(             C  ,              int  ));
    static assert( canForeach!(       const(C) ,        const(int) ));
    static assert( canForeach!(   immutable(C) ,    immutable(int) ));
    static assert( canForeach!(      shared(C) ,       shared(int) ));
    static assert( canForeach!(shared(const(C)), shared(const(int))));

    static assert( canForeach!(             D  , int));
    static assert( canForeach!(       const(D) , int));
    static assert( canForeach!(   immutable(D) , int));
    static assert(!canForeach!(      shared(D) , int));
    static assert(!canForeach!(shared(const(D)), int));

    static assert(!canForeach!(             E  , int));
    static assert(!canForeach!(       const(E) , int));
    static assert( canForeach!(   immutable(E) , int));
    static assert( canForeach!(      shared(E) , int));
    static assert( canForeach!(shared(const(E)), int));
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=2443

struct S2443
{
    int[] arr;
    int opApply(int delegate(size_t i, ref int v) dg)
    {
        int result = 0;
        foreach (i, ref x; arr)
        {
            if ((result = dg(i, x)) != 0)
                break;
        }
        return result;
    }
}

void test2443()
{
    S2443 s;
    foreach (i, ref v; s) {}
    foreach (i,     v; s) {}
    static assert(!__traits(compiles, { foreach (ref i, ref v; s) {} }));
    static assert(!__traits(compiles, { foreach (ref i,     v; s) {} }));
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=3187

class Collection
{
    int opApply(int delegate(ref Object) a)
    {
        return 0;
    }
}

Object testForeach(Collection level1, Collection level2)
{
    foreach (first; level1) {
        foreach (second; level2)
            return second;
    }
    return null;
}

void test3187()
{
    testForeach(new Collection, new Collection);
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=4090

void test4090a()
{
    double[10] arr = 1;
    double tot = 0;

  static assert(!__traits(compiles, {
    foreach (immutable ref x; arr) {}
  }));
    foreach (const ref x; arr)
    {
        static assert(is(typeof(x) == const double));
        tot += x;
    }
    foreach (immutable x; arr)
    {
        static assert(is(typeof(x) == immutable double));
        tot += x;
    }
    assert(tot == 1*10 + 1*10);
}

void test4090b()
{
    int tot = 0;

  static assert(!__traits(compiles, {
    foreach (immutable ref x; 1..11) {}
  }));
    foreach (const ref x; 1..11)
    {
        static assert(is(typeof(x) == const int));
        tot += x;
    }
    foreach (immutable x; 1..11)
    {
        static assert(is(typeof(x) == immutable int));
        tot += x;
    }
    assert(tot == 55 + 55);
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=5605

struct MyRange
{
    int theOnlyOne;

    @property bool empty() const
    {
        return true;
    }

    @property ref int front() return
    {
        return theOnlyOne;
    }

    void popFront()
    {}
}

struct MyCollection
{
    MyRange opSlice() const
    {
        return MyRange();
    }
}

void test5605()
{
    auto coll = MyCollection();

    foreach (i; coll) {            // <-- compilation error
        // ...
    }
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=7004

void func7004(A...)(A args)
{
    foreach (i, e; args){}        // OK
    foreach (uint i, e; args){}   // OK
    foreach (size_t i, e; args){} // NG
}
void test7004()
{
    func7004(1, 3.14);
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=7406

template TypeTuple7406(T...)
{
    alias T TypeTuple7406;
}

template foobar7406(T)
{
    enum foobar = 2;
}

void test7406()
{
    foreach (sym; TypeTuple7406!(int, double))     // OK
        pragma(msg, sym.stringof);

    foreach (sym; TypeTuple7406!(foobar7406))      // OK
        pragma(msg, sym.stringof);

    foreach (sym; TypeTuple7406!(test7406))        // OK
        pragma(msg, sym.stringof);

    foreach (sym; TypeTuple7406!(int, foobar7406)) // Error: type int has no value
        pragma(msg, sym.stringof);

    foreach (sym; TypeTuple7406!(int, test7406))   // Error: type int has no value
        pragma(msg, sym.stringof);
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=6659

void test6659()
{
    static struct Iter
    {
        ~this()
        {
            ++_dtor;
        }

        bool opCmp(ref const Iter rhs) { return _pos == rhs._pos; }
        void opUnary(string op:"++")() { ++_pos; }
        size_t _pos;

        static size_t _dtor;
    }

    foreach (ref iter; Iter(0) .. Iter(10))
    {
        assert(Iter._dtor == 0);
    }
    assert(Iter._dtor == 2);

    Iter._dtor = 0; // reset

    for (auto iter = Iter(0), limit = Iter(10); iter != limit; ++iter)
    {
        assert(Iter._dtor == 0);
    }
    assert(Iter._dtor == 2);
}

void test6659a()
{
    auto value = 0;
    try
    {
        for ({scope(success) { assert(value == 1); value = 2;} }  true; )
        {
            value = 1;
            break;
        }
        assert(value == 2);
    }
    catch (Exception e)
    {
        assert(0);
    }
    assert(value == 2);
}

void test6659b()
{
    auto value = 0;
    try
    {
        for ({scope(failure) value = 1;}  true; )
        {
            throw new Exception("");
        }
        assert(0);
    }
    catch (Exception e)
    {
        assert(e);
    }
    assert(value == 1);
}

void test6659c()
{
    auto value = 0;
    try
    {
        for ({scope(exit) value = 1;}  true; )
        {
            throw new Exception("");
        }
        assert(0);
    }
    catch (Exception e)
    {
        assert(e);
    }
    assert(value == 1);
}

/***************************************/

// https://issues.dlang.org/show_bug.cgi?id=10221

void test10221()
{
    // All of these should work, but most are too slow.  Just check they compile.
    foreach(char i; char.min..char.max+1) {}
    if (0) foreach(wchar i; wchar.min..wchar.max+1) {}
    if (0) foreach(dchar i; dchar.min..dchar.max+1) {}
    foreach(byte i; byte.min..byte.max+1) {}
    foreach(ubyte i; ubyte.min..ubyte.max+1) {}
    if (0) foreach(short i; short.min..short.max+1) {}
    if (0) foreach(ushort i; ushort.min..ushort.max+1) {}
    if (0) foreach(int i; int.min..int.max+1U) {}
    if (0) foreach(uint i; uint.min..uint.max+1L) {}
    if (0) foreach(long i; long.min..long.max+1UL) {}

    foreach_reverse(char i; char.min..char.max+1) { assert(i == typeof(i).max); break; }
    foreach_reverse(wchar i; wchar.min..wchar.max+1) { assert(i == typeof(i).max); break; }
    foreach_reverse(dchar i; dchar.min..dchar.max+1) { assert(i == typeof(i).max); break; }
    foreach_reverse(byte i; byte.min..byte.max+1) { assert(i == typeof(i).max); break; }
    foreach_reverse(ubyte i; ubyte.min..ubyte.max+1) { assert(i == typeof(i).max); break; }
    foreach_reverse(short i; short.min..short.max+1) { assert(i == typeof(i).max); break; }
    foreach_reverse(ushort i; ushort.min..ushort.max+1) { assert(i == typeof(i).max); break; }
    foreach_reverse(int i; int.min..int.max+1U) { assert(i == typeof(i).max); break; }
    foreach_reverse(uint i; uint.min..uint.max+1L) { assert(i == typeof(i).max); break; }
    foreach_reverse(long i; long.min..long.max+1UL) { assert(i == typeof(i).max); break; }
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=7814

struct File7814
{
    ~this(){}
}

struct ByLine7814
{
    File7814 file;

    // foreach interface
    @property bool empty() const    { return true; }
    @property char[] front()        { return null; }
    void popFront(){}
}

void test7814()
{
    int dummy;
    ByLine7814 f;
    foreach (l; f) {
        scope(failure) // 'failure' or 'success' fails, but 'exit' works
            dummy = -1;
        dummy = 0;
    }
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=10049

struct ByLine10049
{
    bool empty() { return true; }
    string front() { return null; }
    void popFront() {}

    ~this() {}  // necessary
}

void test10049()
{
    ByLine10049 r;
    foreach (line; r)
    {
        doNext:
            {}
    }
}

/******************************************/

struct T11955(T...) { T field; alias field this; }

alias X11955 = uint;

struct S11955
{
    enum empty = false;
    T11955!(uint, uint) front;
    void popFront() {}
}

void test11955()
{
    foreach(X11955 i, v; S11955()) {}
}

/******************************************/
// https://issues.dlang.org/show_bug.cgi?id=6652

void test6652()
{
    size_t sum;
    foreach (i; 0 .. 10)
        sum += i++; // 0123456789
    assert(sum == 45);

    sum = 0;
    foreach (ref i; 0 .. 10)
        sum += i++; // 02468
    assert(sum == 20);

    sum = 0;
    foreach_reverse (i; 0 .. 10)
        sum += i--; // 9876543210
    assert(sum == 45);

    sum = 0;
    foreach_reverse (ref i; 0 .. 10)
        sum += i--; // 97531
    assert(sum == 25);

    enum ary = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
    sum = 0;
    foreach (i, v; ary)
    {
        assert(i == v);
        sum += i++; // 0123456789
    }
    assert(sum == 45);

    sum = 0;
    foreach (ref i, v; ary)
    {
        assert(i == v);
        sum += i++; // 02468
    }
    assert(sum == 20);

    sum = 0;
    foreach_reverse (i, v; ary)
    {
        assert(i == v);
        sum += i--; // 9876543210
    }
    assert(sum == 45);

    sum = 0;
    foreach_reverse (ref i, v; ary)
    {
        assert(i == v);
        sum += i--; // 97531
    }
    assert(sum == 25);

    static struct Iter
    {
        ~this()
        {
            ++_dtorCount;
        }

        bool opCmp(ref const Iter rhs)
        {
            return _pos == rhs._pos;
        }

        void opUnary(string op)() if(op == "++" || op == "--")
        {
            mixin(op ~ q{_pos;});
        }

        size_t _pos;
        static size_t _dtorCount;
    }

    Iter._dtorCount = sum = 0;
    foreach (v; Iter(0) .. Iter(10))
        sum += v._pos++; // 0123456789
    assert(sum == 45 && Iter._dtorCount == 12);

    Iter._dtorCount = sum = 0;
    foreach (ref v; Iter(0) .. Iter(10))
        sum += v._pos++; // 02468
    assert(sum == 20 && Iter._dtorCount == 2);

    // additional dtor calls due to unnecessary postdecrements
    Iter._dtorCount = sum = 0;
    foreach_reverse (v; Iter(0) .. Iter(10))
        sum += v._pos--; // 9876543210
    assert(sum == 45 && Iter._dtorCount >= 12);

    Iter._dtorCount = sum = 0;
    foreach_reverse (ref v; Iter(0) .. Iter(10))
        sum += v._pos--; // 97531
    assert(sum == 25 && Iter._dtorCount >= 2);
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=8595

struct OpApply8595
{
    int opApply(int delegate(ref int) dg)
    {
        assert(0);
    }
}

string test8595()
{
    foreach (elem; OpApply8595.init)
    {
        static assert(is(typeof(return) == string));
    }
    assert(0);
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=9068

struct Foo9068
{
    static int[] destroyed;
    int x;
    ~this() { destroyed ~= x; }
}

struct SimpleCounter9068
{
    static int destroyedCount;
    enum int limit = 5;
    int counter;
    ~this() { destroyedCount++; }

    // Range primitives.
    @property bool empty() const { return counter >= limit; }
    @property int front() { return counter; }
    void popFront() { counter++; }
}

void test9068()
{
    //----------------------------------------
    // There was never a bug in this case (no range).
    int sum;
loop_simple:
    foreach (i; [10, 20])
    {
        sum += i;
        break loop_simple;
    }
    assert(sum == 10);

    //----------------------------------------
    // There was a bug with loops over ranges.
    int last = -1;
X:  foreach (i; SimpleCounter9068())
    {
        switch(i)
        {
            case 3:
                break X;
            default:
                last = i;
       }
    }
    assert(last == 2);
    assert(SimpleCounter9068.destroyedCount == 1);

    //----------------------------------------
    // Simpler case: the compiler error had nothing to do with the switch.
    last = -1;
loop_with_range:
    foreach (i; SimpleCounter9068())
    {
        last = i;
        break loop_with_range;
    }
    assert(last == 0);
    assert(SimpleCounter9068.destroyedCount == 2);

    //----------------------------------------
    // Test with destructors: the loop is implicitly wrapped into two
    // try/finally clauses.
loop_with_dtors:
    for (auto x = Foo9068(4), y = Foo9068(5); x.x != 10; ++x.x)
    {
        if (x.x == 8)
            break loop_with_dtors;
    }
    assert(Foo9068.destroyed == [5, 8]);
    Foo9068.destroyed = null;

    //----------------------------------------
    // Same with an unlabelled break.
    for (auto x = Foo9068(4), y = Foo9068(5); x.x != 10; ++x.x)
    {
        if (x.x == 7)
            break;
    }
    assert(Foo9068.destroyed == [5, 7]);
    Foo9068.destroyed = null;
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=11885

struct Foo11885
{
    static int[] destroyed;
    int x;
    ~this() { destroyed ~= x; }
}

struct SimpleCounter11885
{
    static int destroyedCount;
    enum int limit = 5;
    int counter;
    ~this() { destroyedCount++; }

    // Range primitives.
    @property bool empty() const { return counter >= limit; }
    @property int front() { return counter; }
    void popFront() { counter++; }
}

void test11885()
{
    //----------------------------------------
    // There was never a bug in this case (no range).
    int sum;
loop_simple:
    foreach (i; [10, 20])
    {
        sum += i;
        continue loop_simple;
    }
    assert(sum == 30);

    //----------------------------------------
    // There was a bug with loops over ranges.
    int last = -1;
X:  foreach (i; SimpleCounter11885())
    {
        switch(i)
        {
            case 3:
                continue X;
            default:
                last = i;
       }
    }
    assert(last == 4);
    assert(SimpleCounter11885.destroyedCount == 1);

    //----------------------------------------
    // Simpler case: the compiler error had nothing to do with the switch.
    last = -1;
loop_with_range:
    foreach (i; SimpleCounter11885())
    {
        last = i;
        continue loop_with_range;
    }
    assert(last == 4);
    assert(SimpleCounter11885.destroyedCount == 2);

    //----------------------------------------
    // Test with destructors: the loop is implicitly wrapped into two
    // try/finally clauses.
loop_with_dtors:
    for (auto x = Foo11885(4), y = Foo11885(5); x.x != 10; ++x.x)
    {
        if (x.x == 8)
            continue loop_with_dtors;
    }
    assert(Foo11885.destroyed == [5, 10]);
    Foo11885.destroyed = null;

    //----------------------------------------
    // Same with an unlabelled continue.
    for (auto x = Foo11885(4), y = Foo11885(5); x.x != 10; ++x.x)
    {
        if (x.x == 7)
            continue;
    }
    assert(Foo11885.destroyed == [5, 10]);
    Foo11885.destroyed = null;
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=10475

void test10475a()
{
    struct DirIterator
    {
        int _store = 42;
        ~this() { assert(0); }
    }

    DirIterator dirEntries()
    {
        throw new Exception("");
    }

    try
    {
        for (DirIterator c = dirEntries(); true; ) {}
        assert(0);
    }
    catch (Exception e)
    {
        assert(e.next is null);
    }
}

void test10475b()
{
    uint g;
    struct S
    {
        uint flag;
        ~this() { g |= flag; }
    }

    S thrown()
    {
        throw new Exception("");
    }

    g = 0x0;
    try
    {
        for (auto x = S(0x1), y = S(0x2), z = thrown(); true; ) {}
        assert(0);
    }
    catch (Exception e)
    {
        assert(e.next is null);
    }
    assert(g == 0x3);

    g = 0x0;
    try
    {
        for (auto x = S(0x1), y = thrown(), z = S(0x2); true; ) {}
        assert(0);
    }
    catch (Exception e)
    {
        assert(e.next is null);
    }
    assert(g == 0x1);

    g = 0x0;
    try
    {
        for (auto x = thrown(), y = S(0x1), z = S(0x2); true; ) {}
        assert(0);
    }
    catch (Exception e)
    {
        assert(e.next is null);
    }
    assert(g == 0x0);
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=11291

void test11291()
{
    struct Tuple(T...)
    {
        T field;
        alias field this;
    }
    struct zip
    {
        string[] s1, s2;

        bool empty() { return true; }
        auto front() { return Tuple!(string, string)(s1[0], s2[0]); }
        void popFront() {}
    }

    foreach (const a, const b; zip(["foo"], ["bar"]))
    {
        static assert(is(typeof(a) == const string));
        static assert(is(typeof(b) == const string));

        static assert(!__traits(compiles, a = "something"));
        static assert(!__traits(compiles, b = "something"));
    }
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=12103

alias TypeTuple12103(TL...) = TL;

alias Id12103(alias a) = a;

void test12103()
{
    alias fs1 = TypeTuple12103!(() => 0, () => 1);
    foreach (i, f; fs1)
    {
        static assert(f() == i);
        static assert(Id12103!f() == i);
        assert(f() == i);
        assert(Id12103!f() == i);
    }

    alias fs2 = TypeTuple12103!(x=>x+0, y=>y+1);
    foreach (i, f; fs2)
    {
        static assert(f(0) == i);
        static assert(Id12103!f(0) == i);
        assert(f(0) == i);
        assert(Id12103!f(0) == i);
    }
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=12739

struct S12739
{
nothrow:
    int opApply(int delegate(ref int) nothrow dg)
    {
        return 0;
    }
}

void test12739() nothrow
{
    S12739 s;
    foreach (e; s) {}
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=12932

void test12932() @nogc
{
    int sum;
    foreach (e; [1,2,3])
    {
        sum += e;
    }
    assert(sum == 6);
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=13756

void test13756()
{
    printf("test13756()\n");
    int[int] org = [1:2], aa;

    aa = org.dup;
    foreach (v; aa)
    {
        static assert(is(typeof(v) == int));
        v = 20;
    }
    assert(aa == [1:2]);

    aa = org.dup;
    foreach (ref v; aa)
    {
        static assert(is(typeof(v) == int));
        v = 20;
    }
    assert(aa == [1:20]);

    aa = org.dup;
    foreach (k, v; aa)
    {
        static assert(is(typeof(k) == int));
        static assert(is(typeof(v) == int));
        k = 10;
        v = 20;
    }
    assert(aa == [1:2]);

    aa = org.dup;
    foreach (k, ref v; aa)
    {
        static assert(is(typeof(k) == int));
        static assert(is(typeof(v) == int));
        k = 10;
        v = 20;
    }
    assert(aa == [1:20]);

    aa = org.dup;
    foreach (ref k, v; aa)      // NG -> OK
    {
        static assert(is(typeof(k) == const int));
        static assert(is(typeof(v) == int));
        static assert(!__traits(compiles, k = 10));
        v = 20;
    }
    assert(aa == [1:2]);

    aa = org.dup;
    foreach (ref k, ref v; aa)  // NG -> OK
    {
        static assert(is(typeof(k) == const int));
        static assert(is(typeof(v) == int));
        static assert(!__traits(compiles, k = 10));
        v = 20;
    }
    assert(aa == [1:20]);

    foreach (ref const k, v; aa)  // NG -> OK, same with 'ref k'
    {
        static assert(is(typeof(k) == const int));
    }
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=14653

static string result14653;

class RangeClass14653
{
    int a;

    this(T)(T...) { result14653 ~= "c"; }
    ~this()       { result14653 ~= "d"; a = -1; }

    @property bool empty() { result14653 ~= "e"; return a >= 2; }
    @property int front()  { result14653 ~= "f"; assert(a >= 0); return a; }
    void popFront()        { result14653 ~= "p"; ++a; }
}

auto scoped14653(T, A...)(A args)
{
    static struct Scoped(T)
    {
        void[__traits(classInstanceSize, T)] store;
        T payload() { return cast(T)cast(void*)store.ptr; }
        alias payload this;

        ~this()
        {
            //.destroy(payload);
            payload.__dtor();
            (cast(byte[])store)[] = 0;
        }
    }

    Scoped!T result = void;

    //emplace!T(result.store[], args);
    result.store[] = typeid(T).initializer[];
    result.payload.__ctor(args);

    return result;
}

void test14653()
{
    printf("test14653()\n");
    foreach (e; scoped14653!RangeClass14653(1))
    {
        result14653 ~= "b";
    }
    assert(result14653 == "cefbpefbped", result14653);
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=15777

template funA15777()
{
    import imports.test15777a;
    alias funA15777 = fun;
}

template funB15777()
{
    import imports.test15777b;
    alias funB15777 = fun;
}

template funAB15777()
{
    import imports.test15777a;
    import imports.test15777b;
    alias funAB15777 = fun;
}

void foo15777(alias tpl)()
{
    alias seq = AliasSeq!(tpl!());
    // Make alias of 'overload set' in tuple elements
    static assert(seq.length == 1);
    foreach (i, n; seq)
    {
        static assert(__traits(identifier, seq[i]) == "fun");
    }
}

void test15777()
{
    foo15777!funA15777;
    foo15777!funB15777;
    foo15777!funAB15777;
}

/***************************************/
// https://issues.dlang.org/show_bug.cgi?id=17041

auto ref int[2] foo17041(A...)(auto ref A args)
{
    foreach(a; args)
    {
        a = [12, 22];
    }
    foreach(ref a; args)
    {
        a = [31, 41];
        return args[0];
    }
}

void test17041()
{
    int[2] x = [10, 20];
    foreach(a; AliasSeq!(x))
    {
        a = [11, 21];
    }
    assert(x == [10, 20]); // test by value
    foreach(ref a; AliasSeq!(x))
    {
        a = [30, 40];
    }
    assert(x == [30, 40]); // test by ref value

    assert(foo17041(x) == [31, 41]); // test lvalue
    assert(x == [31, 41]);
    assert(foo17041(cast(int[2]) [10, 20]) == [31, 41]); // test rvalue
}

/***************************************/

int main()
{
    test1();
    test2411();
    test2442();
    test2443();
    test3187();
    test4090a();
    test4090b();
    test5605();
    test7004();
    test10221();
    test7406();
    test6659();
    test6659a();
    test6659b();
    test6659c();
    test7814();
    test6652();
    test9068();
    test11885();
    test10475a();
    test10475b();
    test11291();
    test12103();
    test12739();
    test12932();
    test13756();
    test14653();
    test17041();

    printf("Success\n");
    return 0;
}
