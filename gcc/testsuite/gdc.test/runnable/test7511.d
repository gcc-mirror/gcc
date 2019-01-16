extern(C) int printf(const char*, ...);

/**********************************/
// 7511

struct S7511(T)
{
    // this is a pure function for T==int
    T foo(T x)
    {
        return 2 * x;
    }
}

void test7511a() pure
{
    S7511!int s;
    s.foo(2); // error -> OK
}

/**********************************/
// certain case - wrapper range

//import std.range;
@property bool empty(T)(in T[] a) { return !a.length; }
@property ref T front(T)(T[] a) { return a[0]; }
void popFront(T)(ref T[] a) { a = a[1 .. $]; }

struct S(T)
{
    int foo()
    {
        auto t = T();
        return t.bar();
    }
}

struct Wrap(R)
{
    R original;
    this(T : R)(T t) { original = t; }
    this(A...)(A args) { original = R(args); }
    @property auto empty() { return original.empty; }
    @property auto front() { return original.front; }
    void popFront() { original.popFront(); }
}

void test7511b() pure @safe
{
    static struct Iota
    {
        size_t curr;
        size_t max;
        @property bool empty() pure @safe { return curr == max; }
        @property size_t front() pure @safe { return curr; }
        void popFront() pure @safe { ++curr; }
    }
    {
        auto a = Iota(0, 3);
        size_t i = 0;
        foreach (e; a) { assert(e == i++); } // OK
    }
    {
        auto a = Wrap!(int[])([0,1,2]);
        size_t i = 0;
        foreach (e; a) { assert(e == i++); } // errors!
    }
    {
        auto a = Wrap!Iota(0, 3);
        size_t i = 0;
        foreach (e; a) { assert(e == i++); } // errors!
    }
}

/**********************************/
// with attribute inheritance

struct X
{
    static int bar() pure nothrow @safe
    {
        return 1;
    }
}

class Class(T)
{
    int foo()
    {   // inferred to pure nothrow @safe
        return T.bar();
    }
}

alias Class!X C;

class D : C
{
    override int foo()
    {   // inherits attributes from Class!X.foo
        return 2;
    }
}

void test7511c() pure nothrow @safe
{
// Disabled for Bigzilla 9952
/+
    assert((new C()).foo() == 1);
    assert((new D()).foo() == 2);
    static assert(typeof(&C.init.foo).stringof == "int delegate() pure nothrow @safe");
    static assert(typeof(&D.init.foo).stringof == "int delegate() pure nothrow @safe");
+/
}

/**********************************/
// curiously recurring template pattern (CRTP)

class BX(T, bool mutual)
{
    int foo()
    {
        static if (mutual)
            return (cast(T)this).foo();
        else
            return 0;
    }
}

class D1 : BX!(D1, true)
{
    alias typeof(super) B;
    int val;
    this(int n) { val = n; }
    override int foo() { return val; }
}
class D2 : BX!(D2, false)
{
    alias typeof(super) B;
    int val;
    this(int n) { val = n; }
    override int foo() { return val; }
}
class D3 : BX!(D3, true)
{
    alias typeof(super) B;
    int val;
    this(int n) { val = n; }
    override int foo() pure nothrow { return val; }
}
class D4 : BX!(D4, false)
{
    alias typeof(super) B;
    int val;
    this(int n) { val = n; }
    override int foo() pure nothrow { return val; }
}

void test7511d()
{
// Disabled for Bigzilla 9952
/+
    // mutual dependent and attribute inference impure, un-@safe, and may throw is default.
    auto d1 = new D1(10);
    static assert(is(typeof(&d1.B.foo) == int function()));
    static assert(is(typeof(&d1.foo) == int delegate()));
    assert(d1.foo() == 10);

    // no mutual dependent.
    auto d2 = new D2(10);
    static assert(is(typeof(&d2.B.foo) == int function() pure nothrow @safe));
    static assert(is(typeof(&d2  .foo) == int delegate() pure nothrow @safe));
    assert(d2.foo() == 10);

    // mutual dependent with explicit attribute specification.
    auto d3 = new D3(10);
    static assert(is(typeof(&d3.B.foo) == int function() pure nothrow));
    static assert(is(typeof(&d3  .foo) == int delegate() pure nothrow));
    assert(d3.foo() == 10);

    // no mutual dependent with explicit attribute specification.
    auto d4 = new D4(10);
    static assert(is(typeof(&d4.B.foo) == int function() pure nothrow @safe));
    static assert(is(typeof(&d4  .foo) == int delegate() pure nothrow @safe));
    assert(d4.foo() == 10);
+/
}

/**********************************/
// 9952

@system void writeln9952(int) {}    // impure throwable

class C9952(T)
{
    T foo()
    {
        return 2;
    }
}

class D9952 : C9952!int
{
    override int foo()
    {
        writeln9952(super.foo());
        return 3;
    }
}

void test9952()
{
    static assert(typeof(&C9952!int.init.foo).stringof == "int delegate()");
    static assert(typeof(&D9952    .init.foo).stringof == "int delegate()");
}

/**********************************/
// 10373

template isMutable10373(T)
{
    enum isMutable10373 = !is(T == const) && !is(T == immutable) && !is(T == inout);
}

struct Test10373a(T, int N = 0)
{
    static if (N == 0) T[ ] mBuffer;
    else               T[N] mBuffer;

    static if (is(T == class))
    {}
    else
    {
        T* at_(size_t n) { return &mBuffer[n]; }

        static if (is(typeof(*at_(0) = T.init)))
        {
            T opIndexAssign(T v, size_t i)
            {
                return (*at_(i) = v);
            }
        }
    }
}
struct Test10373b(T, int N = 0)
{
    static if (is(T == class))
    {}
    else
    {
        T* at_(size_t n) { return &mBuffer[n]; }

        static if (is(typeof(*at_(0) = T.init)))
        {
            T opIndexAssign(T v, size_t i)
            {
                return (*at_(i) = v);
            }
        }
    }

    static if (N == 0) T[ ] mBuffer;
    else               T[N] mBuffer;
}
struct Test10373c(T, int N = 0)
{
    static if (is(T == class))
    {}
    else
    {
        T* at_(size_t n) { return &mBuffer[n]; }

        static if (isMutable10373!T)
        {
            T opIndexAssign(T v, size_t i)
            {
                return (*at_(i) = v);
            }
        }
    }

    static if (N == 0) T[ ] mBuffer;
    else               T[N] mBuffer;
}

void test10373()
{
    static assert(is(Test10373a!(int, 2)));
    static assert(is(Test10373b!(int, 2)));
    static assert(is(Test10373c!(int, 2)));

    Test10373a!(int, 2) testa;  // dmd2.062:OK  dmd2.063:OK
    Test10373b!(int, 2) testb;  // dmd2.062:OK  dmd2.063:NG
    Test10373c!(int, 2) testc;  // dmd2.062:OK  dmd2.063:OK
}

/**********************************/
// 10329

auto foo10329(T)(T arg)
{
    auto bar()
    {
        return arg;
    }
    static assert(is(typeof(&bar) == T delegate() pure nothrow @nogc @safe));
    return bar();
}

auto make10329(T)(T arg)
{
    struct S
    {
        auto front() { return T.init; }
    }
    S s;
    static assert(is(typeof(&s.front) == T delegate() pure nothrow @nogc @safe));
    return s;
}

void test10329() pure nothrow @safe
{
    assert(foo10329(1) == 1);

    auto s = make10329(1);
    assert(s.front() == 0);
}

/**********************************/
// 11896

class Foo11896a(T = int)
{
    static if (!__traits(isVirtualMethod, zoo)) {} else { Undefined x; }

    static void bar() {}
    static void bar(Foo11896a foo) {}

    static void zoo()
    {
        bar(new Foo11896a);
    }
}
Foo11896a!(int) baz11896a;

// ----

Frop11896b!(int) frop11896b;

mixin template baz11896b()
{
    public void bar11896b() {}
}

mixin baz11896b;

class Foo11896b(T)
{
    static if (! __traits(isVirtualMethod, zoo)) {}

    static void zoo()
    {
        bar11896b();
    }
}

class Frop11896b(T) : Foo11896b!T {}

// ----

static bool flag11896c = false;

class Bar11896c {}

class Foo11896c(T = Bar11896c)
{
    static if (! __traits(isVirtualMethod, foo)) {}
    alias Foo11896c!(T) this_type;
    this()
    {
        flag11896c = true;
    }
    static public this_type foo()
    {
        auto c = new this_type();
        return flag11896c ? c : null;
    }
}

void test11896c()
{
    alias Foo11896c!Bar11896c FooBar;
    assert(FooBar.foo() !is null);
}

/**********************************/
// 12392

void f12392(T)() {}
alias fa12392 = f12392;

void test12392() pure nothrow @safe
{
    fa12392!int();
}

/**********************************/

int main()
{
    test7511a();
    test7511b();
    test7511c();
    test7511d();
    test9952();
    test10373();
    test10329();
    test11896c();

    printf("Success\n");
    return 0;
}
