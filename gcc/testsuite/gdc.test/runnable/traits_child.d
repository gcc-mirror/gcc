struct A
{
    ulong i;
    void foo(ulong a)
    {
        i = a;
    }

    void foo(string s)
    {
        i = s.length;
    }

    void bar(T)(T a)
    {
        i = a;
    }

    void bar(T : string)(T s)
    {
        i = s.length;
    }
}

alias ai = A.i;
alias afoo = A.foo;
alias abar = A.bar;
alias abar_ulong = A.bar!ulong;
alias abar_string = A.bar!string;

struct B
{
    A a;
}

alias ba = B.a;

template T(alias x)
{
    void set(int n)
    {
        x = n;
    }
}

mixin template M(alias x)
{
    void set(int n)
    {
        x = n;
    }
}

struct C
{
    int i;
    alias t = T!i;
    mixin M!i m;
}

alias ct = C.t;
alias ctset = C.t.set;
alias cm = C.m;
alias cmset = C.m.set;


// adapted from http://thecybershadow.net/d/dconf2017/#/21
struct S { string a, b, c; }

static string printField(alias field)()
{
    S s = { a: "aa", b: "bb", c: "cc" };
    return __traits(child, s, field);
}

void main()
{
    auto f = printField!(S.b)();
    assert(f == "bb");

    A a;
    __traits(child, a, ai) = 3;
    assert(a.i == 3);
    assert(__traits(child, a, ai) == 3);
    __traits(child, a, afoo)(2);
    assert(a.i == 2);
    __traits(child, a, afoo)("hello");
    assert(a.i == 5);
    __traits(child, a, abar)(6);
    assert(a.i == 6);
    __traits(child, a, abar_ulong)(7);
    assert(a.i == 7);
    __traits(child, a, abar_string)("hi");
    assert(a.i == 2);

    __traits(child, a, A.i) = 7;
    assert(a.i == 7);
    __traits(child, a, A.bar)(3);
    assert(a.i == 3);
    __traits(child, a, A.bar!ulong)(4);
    assert(a.i == 4);
    __traits(child, a, __traits(getMember, A, "i")) = 5;
    assert(a.i == 5);
    __traits(child, a, __traits(getOverloads, A, "bar", true)[1])("hi!");
    assert(a.i == 3);

    B b;
    __traits(child, b.a, ai) = 2;
    assert(b.a.i == 2);
    __traits(child, __traits(child, b, ba), ai) = 3;
    assert(b.a.i == 3);

    C c;
    __traits(child, c, ct).set(3);
    assert(c.i == 3);
    __traits(child, c, ctset)(4);
    assert(c.i == 4);
    __traits(child, c, cm).set(5);
    assert(c.i == 5);
    __traits(child, c, cmset)(6);
    assert(c.i == 6);
}
