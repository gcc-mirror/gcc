/*
TEST_OUTPUT:
---
fail_compilation/ice15332.d(16): Error: calling non-static function `fun` requires an instance of type `C`
fail_compilation/ice15332.d(17): Error: accessing non-static variable `var` requires an instance of `C`
---
*/

class C
{
    int fun() { return 5; }
    int var;

    void test()
    {
        int a1 = function() { return fun; }();
        int a2 = function() { return var; }();
    }
}
