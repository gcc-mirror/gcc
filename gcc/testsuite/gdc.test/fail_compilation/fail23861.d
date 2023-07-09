// https://issues.dlang.org/show_bug.cgi?id=23861

/*
TEST_OUTPUT:
---
fail_compilation/fail23861.d(24): Error: cannot implicitly convert expression `3` of type `int` to `Foo`
---
*/

Foo global;

struct Foo
{
    ref Foo get()
    {
        return global;
    }
    alias get this;
}

void main()
{
    Foo g;
    g = 3;
}
