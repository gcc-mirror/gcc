// PERMUTE_ARGS:
// EXTRA_FILES: imports/fwdref9514.d imports/fwdref12201a.d

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=6766

class Foo6766
{
    this(int x) { }
    void test(Foo6766 foo = new Foo6766(1)) { }
}

struct Bar6766
{
    this(int x) { }
    void test(Bar6766 bar = Bar6766(1)) { }
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=8609

struct Tuple8609(T)
{
    T arg;
}

// ----

struct Foo8609a
{
    Bar8609a b;
}
struct Bar8609a
{
    int x;
    Tuple8609!(Foo8609a) spam() { return Tuple8609!(Foo8609a)(); }
}

// ----

struct Foo8609b
{
    Bar8609b b;
}
struct Bar8609b
{
    int x;
    Tuple8609!(Foo8609b[1]) spam() { return Tuple8609!(Foo8609b[1])(); }
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=8698

interface IRoot8698a {}
interface IClass8698a : IRoot8698a { }
struct Struct8698a { }
class Class8698a : IClass8698a { alias Struct8698a Value; }
void test8698a(Class8698a.Value) { }
//interface IRoot8698a {}

// ----

//interface IRoot8698b {}
interface IClass8698b : IRoot8698b { }
struct Struct8698b { }
class Class8698b : IClass8698b { alias Struct8698b Value; }
void test8698b(Class8698b.Value) { }
interface IRoot8698b {}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=9514

template TStructHelpers9514a()
{
    void opEquals(Foo9514a)
    {
        auto n = FieldNames9514a!();
    }
}

struct Foo9514a
{
    mixin TStructHelpers9514a!();
}

import imports.fwdref9514 : find9514;  // selective import without aliasing

template FieldNames9514a()
{
    static if (find9514!`true`([1])) enum int FieldNames9514a = 1;
}

// ----

template TStructHelpers9514b()
{
    void opEquals(Foo9514b)
    {
        auto n = FieldNames9514b!();
    }
}

struct Foo9514b
{
    mixin TStructHelpers9514b!();
}

import imports.fwdref9514 : foo9514 = find9514;  // selective import with aliasing

template FieldNames9514b()
{
    static if (foo9514!`true`([1])) enum int FieldNames9514b = 1;
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=10015

struct S10015(T) { alias X = int; }

alias Y10015 = s10015.X;
S10015!int s10015;

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=10101

int front10101(int);

mixin template reflectRange10101()
{
    static if (is(typeof(this.front10101)))
    {
        int x;
    }
}

struct S10101(R)
{
    R r_;

    typeof(r_.front10101) front10101() @property { return r_.front10101; }

    mixin reflectRange10101;
}

void test10101()
{
    S10101!(int) s;
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=11019

class A11019
{
    A11019 View() { return null; }
}

class B11019 : A11019
{
    override D11019 View() { return null; }
}

class D11019 : B11019 {}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=11166

template Tup11166(T...) { alias Tup11166 = T; }

struct S11166a
{
    enum S11166a a = S11166a(0);
    enum S11166a b = S11166a(1);

    this(long value) { }

    long value;

    // only triggered when private and a template instance.
    private alias types = Tup11166!(a, b);
}

struct S11166b
{
    enum S11166b a = S11166b(0);
    enum S11166b b = S11166b(1);

    // not at the last of members
    alias types = Tup11166!(a, b);

    this(long value) { }

    long value;
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=12152

class A12152
{
    alias Y = B12152.X;
}

class B12152 : A12152
{
    alias int X;
}

static assert(is(A12152.Y == int));

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=12201

template T12201()
{
    alias imports.fwdref12201a.FILE* FP;
}

struct S12201a
{
    mixin T12201;
    import imports.fwdref12201a;
}

union U12201
{
    mixin T12201;
    import imports.fwdref12201a;
}

class C12201
{
    mixin T12201;
    import imports.fwdref12201a;
}

interface I12201
{
    mixin T12201;
    import imports.fwdref12201a;
}


template TI12201()
{
    mixin T12201;
    import imports.fwdref12201a;
}
mixin template TM12201()
{
    mixin T12201;
    import imports.fwdref12201a;
}
struct S12201b
{
    alias ti = TI12201!();

    mixin TM12201;
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=12531

struct Node12531(T)
{
    T _val;
}

void test12531()
{
    static struct Foo
    {
        Node12531!Foo* node;
    }
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=12543

class C12543;
static assert(C12543.sizeof == (void*).sizeof);
static assert(C12543.alignof == (void*).sizeof);
static assert(C12543.mangleof == "C10testfwdref6C12543");

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=14010

enum E14010;
static assert(E14010.mangleof == "E10testfwdref6E14010");

struct S14010;
static assert(S14010.mangleof == "S10testfwdref6S14010");

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=12983

alias I12983 = int;
class B12983(T) { alias MyC = C12983!string; }

class C12983(T) : B12983!float
{
    void m() { f12983(0); }
}

alias MyB12983 = B12983!float;

void f12983();
void f12983(I12983);

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=12984

class B12984a { alias MyD = D12984a!int; }
class C12984a : B12984a { }

class D12984a(T) { alias MyE = E12984a!float; }
class E12984a(T) : D12984a!int
{
    void m()
    {
        auto c = new C12984a();
    }
}

static assert(__traits(classInstanceSize, B12984a) == (void*).sizeof * 2);
static assert(__traits(classInstanceSize, C12984a) == (void*).sizeof * 2);

// ----

class B12984b { int b; alias MyD = D12984b!int; }
class C12984b : B12984b { int c; }

class D12984b(T) { int d; alias MyE = E12984b!float; }
class E12984b(T) : D12984b!int
{
    int e;
    void m()
    {
        auto c = new C12984b();
    }
}

static assert(__traits(classInstanceSize, B12984b) == (void*).sizeof * 2 + int.sizeof);
static assert(__traits(classInstanceSize, C12984b) == (void*).sizeof * 2 + int.sizeof * 2);

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=14390

class B14390a { alias MyD = D14390a!int; }
class C14390a : B14390a { void f(int) {} }
class D14390a(T) { alias MyE = E14390a!float; }
class E14390a(T) : D14390a!int { void m() { auto c = new C14390a(); } }

class B14390b { alias MyD = D14390b!int; }
class C14390b : B14390b { static struct S {} }
class D14390b(T) { alias MyE = E14390b!float; }
class E14390b(T) : D14390b!int { void m() { auto c = new C14390b(); } }

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=13860

/*
TEST_OUTPUT:
---
pure nothrow @nogc @safe void()
pure nothrow @nogc @safe void()
---
*/

struct Foo13860(Bar...)
{
    Bar bars;
    auto baz(size_t d)() {}
    pragma(msg, typeof(baz!0));
}

auto bar13860(S, R)(S s, R r)
{
    pragma(msg, typeof(Foo13860!().baz!0));
}

void test13860()
{
    int[] x;
    int[] y;
    x.bar13860(y);
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=14083

class NBase14083
{
    int foo(NA14083 a) { return 1; }
    int foo(NB14083 a) { return 2; }
}
class NA14083 : NBase14083
{
    int v;
    this(int v) { this.v = v; }
}
class NB14083 : NBase14083
{
    override int foo(NA14083 a) { return a.v; }
}

class TBase14083(T)
{
    int foo(TA14083!T a) { return 1; }
    int foo(TB14083!T a) { return 2; }
}
class TA14083(T) : TBase14083!T
{
    T v;
    this(T v) { this.v = v; }
}
class TB14083(T) : TBase14083!T
{
    override int foo(TA14083!T a) { return a.v; }
}

static assert(
{
    NA14083 na = new NA14083(10);
    NB14083 nb = new NB14083();
    assert(na.foo(na) == 1);
    assert(na.foo(nb) == 2);
    assert(nb.foo(na) == 10);

    TA14083!int ta = new TA14083!int(10);
    TB14083!int tb = new TB14083!int();
    assert(ta.foo(ta) == 1);
    assert(ta.foo(tb) == 2);
    assert(tb.foo(ta) == 10);

    return true;
}());

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=14549

string foo14549(T)()
{
    static if (T.tupleof.length >= 0)
        return "";
}

class Frop14549
{
    mixin(foo14549!(typeof(this)));

    static if (__traits(compiles, undefined))
    {
    }
    else
    {
        int bar = 0;
    }

    static if (!__traits(isVirtualMethod, this.bar)) {}
}

// ----
// regression case

template Mix14549()
{
    mixin(code14549!(typeof(this)));
}

template code14549(T)
{
    enum string code14549 =
        q{ static if (!__traits(isVirtualMethod, "boo")) {} };
}

class Bar14549
{
    mixin Mix14549;
    int boo;
}

// ----
// https://issues.dlang.org/show_bug.cgi?id=14609
// regression case

interface Foo14609(T)
{
    static if (is(T == int))
        public int bar();
}
class Frop14609 : Foo14609!int
{
    public int bar() { return 0; }
}

/***************************************************/
// test case 1, comes from Phobos
/*
TEST_OUTPUT:
---
+alias Alias12540
+anySatisfy, T.length == 1
+isStaticArray
+T.stringof in StaticArrayTypeOf
-T.stringof in StaticArrayTypeOf
-isStaticArray
+hasElaborateCpCtor S == struct or else
-hasElaborateCpCtor S == struct or else
-anySatisfy, T.length == 1
-alias Alias12540
---
*/

template anySatisfy15726x(alias F, T...)
{
    //static if (T.length == 1)
    //{
        pragma(msg, "+anySatisfy, T.length == 1");
        enum anySatisfy15726x = F!(T[0]);
        pragma(msg, "-anySatisfy, T.length == 1");
    //}
}

template StaticArrayTypeOf15726x(T)
{
    alias X = T;

    static if (is(X : E[n], E, size_t n))
    {
        //alias StaticArrayTypeOf15726x = X;
    }
    else
    {
        pragma(msg, "+T.stringof in StaticArrayTypeOf");
        // Fixed: T.stringof (T == Class12540) should not invoke
        //        T.size() in ClassDeclaration.search().
        static assert(0, T.stringof~" is not a static array type");
        pragma(msg, "-T.stringof in StaticArrayTypeOf");
    }
}

//enum bool isStaticArray(T) = is(StaticArrayTypeOf15726x!T);
template isStaticArray15726x(T)
{
    pragma(msg, "+isStaticArray");
    enum bool isStaticArray15726x = is(StaticArrayTypeOf15726x!T);
    pragma(msg, "-isStaticArray");
}

template hasElaborateCpCtor15726x(S)
{
    static if (isStaticArray15726x!S && S.length)
    {
        //pragma(msg, "X+");
        enum bool hasElaborateCpCtor15726x =
            hasElaborateCpCtor15726x!(typeof(S.init[0]));
        //pragma(msg, "X-");
    }
    else
    {
        pragma(msg, "+hasElaborateCpCtor S == struct or else");
        static if (is(S == struct))
        {
            enum bool hasElaborateCpCtor15726x = true;
            //enum hasElaborateCpCtor15726x = hasMember!(S, "__postblit")
            //    || anySatisfy15726x!(.hasElaborateCpCtor15726x, FieldTypeTuple!S);
        }
        else
        {
            enum bool hasElaborateCpCtor15726x = false;
        }
        pragma(msg, "-hasElaborateCpCtor S == struct or else");
    }
}

struct VariantN15726x(AllowedTypesParam...)
{
    alias AllowedTypes = AllowedTypesParam;

    static if (!AllowedTypes.length ||
               anySatisfy15726x!(hasElaborateCpCtor15726x, AllowedTypes))
    {
    }
}

template Algebraic15726x(T)
{
    alias Algebraic15726x = VariantN15726x!(T);
}

void test15726x()
{
    static struct DummyScope
    {
        pragma(msg, "+alias Alias12540");
        alias Alias12540 = Algebraic15726x!Class12540;
        pragma(msg, "-alias Alias12540");
        static class Class12540
        {
            Alias12540 entity;
        }
    }
}

/***************************************************/
// test case 2, comes from Phobos

struct RefCounted15726y(T)
{
    struct RefCountedStore
    {
        struct Impl
        {
            T _payload;
        }
        Impl* _store;
    }
    RefCountedStore _refCounted;

    this(this) {}

    ~this()
    {
        _refCounted._store._payload.__xdtor();
    }
}

struct RangeT15726y(A)
{
    A[1] _outer_;
    alias RC = RangeT15726y!(const(A));
}

struct Array15726y(T)
{
    struct Payload
    {
        ~this();
    }

    alias Data = RefCounted15726y!(Payload);
    Data _data;

    alias Range = RangeT15726y!Array15726y;
}

void test15726y()
{
    alias Range = RangeT15726y!(Array15726y!int);
    Range r;
    r = r;  // opAssign
}

/***************************************************/
// https://issues.dlang.org/show_bug.cgi?id=15726

struct RC15726(T)
{
    struct Impl
    {
        T _payload;
    }

    Impl* _store;

    ~this()
    {
        destroy15726a(_store._payload);
    }
}

// ----

struct Con15726a(T)
{
    alias Stmt15726a = .Stmt15726a!T;
}

struct Stmt15726a(T)
{
    alias Con15726a = .Con15726a!T;

    RC15726!Payload data;

    struct Payload
    {
        Con15726a con;
    }
}

Con15726a!int x15726a;

void destroy15726a(T)(ref T obj) @trusted
{
    auto buf = (cast(ubyte*)&obj)[0 .. T.sizeof];
}

// ----

struct Util15726b(C, S) {}

struct Con15726b(T)
{
    alias Util15726b = .Util15726b!(Con15726b!T, Stmt15726b!T);
}

struct Stmt15726b(T)
{
    struct Payload
    {
        Con15726b!T con;
    }

    RC15726!Payload data;
}

Con15726b!int x15726b;
