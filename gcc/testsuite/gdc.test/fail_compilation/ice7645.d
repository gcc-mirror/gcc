/*
TEST_OUTPUT:
---
fail_compilation/ice7645.d(28): Error: accessing non-static variable `t` requires an instance of `C2`
fail_compilation/ice7645.d(31): Error: calling non-static function `fn` requires an instance of type `S2`
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
