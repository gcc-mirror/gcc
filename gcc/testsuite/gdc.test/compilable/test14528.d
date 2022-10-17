// REQUIRED_ARGS: -o-
// EXTRA_FILES: imports/a14528.d
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
