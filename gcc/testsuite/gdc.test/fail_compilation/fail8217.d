/*
TEST_OUTPUT:
---
fail_compilation/fail8217.d(22): Error: `this` for `foo` needs to be type `D` not type `fail8217.D.C`
---
*/

class D
{
    int x;
    template bar()
    {
        int foo()
        {
            return x;
        }
    }
    static class C
    {
        int foo()
        {
            return bar!().foo();
        }
    }
}
