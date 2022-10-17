// https://issues.dlang.org/show_bug.cgi?id=21719

struct S
{
    auto f()
    {
    } // inferred to be @safe @nogc pure nothrow
}

class C
{
    auto f() // should also infer the same attributes
    {
    }
}

pure @nogc nothrow @safe void test(S s, C c)
{
    s.f;
    c.f;
}
