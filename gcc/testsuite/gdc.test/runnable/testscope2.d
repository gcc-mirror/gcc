// RUNNABLE_PHOBOS_TEST
// REQUIRED_ARGS: -dip25

import core.stdc.stdio;

/********************************************/

struct SS
{
    ref ulong foo1(return ref int* delegate() return p) return;
    ref int foo2(return ref int delegate() p);
    ref int foo3(inout ref int* p);
    ref int foo4(return inout ref int* p);
}

pragma(msg, "foo1 ", typeof(&SS.foo1));
pragma(msg, "foo2 ", typeof(&SS.foo2));
pragma(msg, "foo3 ", typeof(&SS.foo3));
pragma(msg, "foo4 ", typeof(&SS.foo4));


void test3()
{
    version (all)
    {
        import std.stdio;
        writeln(SS.foo1.mangleof);
        writeln(SS.foo2.mangleof);
        writeln(SS.foo3.mangleof);
        writeln(SS.foo4.mangleof);
        writeln(typeof(SS.foo1).stringof);
        writeln(typeof(SS.foo2).stringof);
        writeln(typeof(SS.foo3).stringof);
        writeln(typeof(SS.foo4).stringof);
    }

    version (all)
    {
        // Test scope mangling
        assert(SS.foo1.mangleof == "_D10testscope22SS4foo1MFNcNjNkKDFNjZPiZm");
        assert(SS.foo2.mangleof == "_D10testscope22SS4foo2MFNcNkKDFZiZi");
        assert(SS.foo3.mangleof == "_D10testscope22SS4foo3MFNcNkKNgPiZi");
        assert(SS.foo4.mangleof == "_D10testscope22SS4foo4MFNcNkKNgPiZi");

        // Test scope pretty-printing
        assert(typeof(SS.foo1).stringof == "ref return ulong(return ref int* delegate() return p)");
        assert(typeof(SS.foo2).stringof == "ref int(return ref int delegate() p)");
        assert(typeof(SS.foo3).stringof == "ref int(return ref inout(int*) p)");
        assert(typeof(SS.foo4).stringof == "ref int(return ref inout(int*) p)");
    }
}

/********************************************/

ref int foo(return ref int x)
{
    return x;
}

struct S
{
    int x;

    ref S bar() return
    {
        return this;
    }
}

ref T foo2(T)(ref T x)
{
    return x;
}

void test4()
{
    int x;
    foo2(x);
}

/********************************************/

ref int foo(return ref int x, ref int y)
{
    return x;
}

ref int bar()
{
    int x;
    static int y = 7;
    return foo(y, x);
}

void test5()
{
    int x = bar();
    assert(x == 7);
}

/********************************************/

struct S6
{
    int x = 8;

    ref int bar() return
    {
        return x;
    }
}

void test6()
{
    S6 s;
    int b = s.bar();
    assert(b == 8);
}

/********************************************/

class C
{
    int x;
    ref int foo(return ref int x) { return x; }
    ref int bar() return { return x; }
}

class D : C
{
    override ref int foo(ref int x) { static int y; return y; }
    override ref int bar() { static int y; return y; }
}

void test7()
{
}

/********************************************/

struct S8(T)
{
    int x;

    ref int bar() // infer 'return'
    {
        return x;
    }
}

ref int test8a(return ref S8!int s)
{
    return s.bar();
}

void test8()
{
}

/********************************************/

char[] foo9(return out char[4] buf)
{
    return buf[0 .. 1];
}

/********************************************/

struct S10
{
    int x;

    ref inout(int) foo() inout
    {
        return x;
    }
}

/********************************************/

struct RC
{
    this(this) { }
}

struct S11
{
    @disable this(this);

    void remove()
    {
        _ptr[0] = _ptr[1];
    }

    RC* _ptr;
}


void test11()
{
    S11 ary;
}

/********************************************/

int[10] a12;

int* foo12()
{
    foreach (ref x; a12)
        return &x;
    return null;
}

/********************************************/

struct FullCaseEntry
{
    dchar[3] seq;
    ubyte n, size;// n number in batch, size - size of batch
    ubyte entry_len;

    @property auto value() const @trusted pure nothrow @nogc return
    {
        return seq[0..entry_len];
    }
}

/********************************************/

class C12
{
    void member() scope { }
}

/********************************************/

void main()
{
    test3();
    test4();
    test5();
    test6();
    test7();
    test8();
    test11();
    printf("Success\n");
}

