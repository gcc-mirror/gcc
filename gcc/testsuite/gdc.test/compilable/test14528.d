// REQUIRED_ARGS: -o-
// PERMTE_ARGS:

import imports.a14528;

class C
{
    protected static void func() {}

    void test()
    {
        foo!func();
    }
}
