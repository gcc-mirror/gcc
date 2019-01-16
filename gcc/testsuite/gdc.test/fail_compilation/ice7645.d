/*
TEST_OUTPUT:
---
fail_compilation/ice7645.d(28): Error: need 'this' for 't' of type 'char'
fail_compilation/ice7645.d(31): Error: need 'this' for 'fn' of type 'pure nothrow @nogc @safe void()'
---
*/

class C
{
    class C2()
    {
        char t;
    }
}

struct S
{
    struct S2(T)
    {
        void fn() {}
    }
}

void main()
{
    C c;
    auto v = c.C2!().t;

    S s;
    s.S2!int.fn();
}
