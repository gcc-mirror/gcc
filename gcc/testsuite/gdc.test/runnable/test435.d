import core.stdc.stdio;

class A
{
    immutable size_t f;

    this(T)(T z)
    {
        f = z.sizeof;
    }
}

struct AS
{
    immutable size_t f;

    this(T)(T z)
    {
        f = z.sizeof;
    }
}

void test0()
{
    assert((new A(2.2)).f == double.sizeof);
    assert((new A('g')).f == char.sizeof);
    assert((AS(17)).f == int.sizeof);
    assert((AS(null)).f == typeof(null).sizeof);
}

//------------------------------------------------------------------------------

class C
{
    const int x;
    const int y;

    this(T...)(T z)
    {
        this.tupleof = z;
    }
}

struct CS
{
    const int x;
    const int y;

    this(T...)(T z)
    {
        this.tupleof = z;
    }
}

void test1()
{
    auto c = new C(4, 6);
    assert(c.x == 4);
    assert(c.y == 6);

    auto cs = CS(7, 8);
    assert(cs.x == 7);
    assert(cs.y == 8);
}

//------------------------------------------------------------------------------

// bug 435.
class B
{
    int i;
    this(int k)
    {
        i = k;
    }
}
class D : B
{
    this(A...)(A args)
    {
        super(args);
    }
}

void test2()
{
    auto a = new D(4);
    assert(a.i == 4);
}

//------------------------------------------------------------------------------

// bug 4905
class C2
{
    string x;

    this(T...)(in string msg, T args)
    {
        x = msg;
        foreach (a; args)
            x ~= a;
    }
}

void test3()
{
    auto c2 = new C2("test");
    assert(c2.x == "test");

    auto c3 = new C2("test", " variadic", " constructor");
    assert(c3.x == "test variadic constructor");
}

//------------------------------------------------------------------------------

// bug 4531 test case 2
class MyError : Exception
{
    this(T...)(T msg)
    {
        assert(msg[0] == "Hello, " && msg[1] == 42);
        super("Hello, 42");
    }
}

void test4()
{
    auto err = new MyError("Hello, ", 42);
    assert(err.msg == "Hello, 42");
}

void main()
{
    test0();
    test1();
    test2();
    test3();
    test4();
    printf("Success\n");
}


