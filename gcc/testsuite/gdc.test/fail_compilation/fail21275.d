// https://issues.dlang.org/show_bug.cgi?id=21275
// EXTRA_FILES: imports/fail21275a.d

/*
TEST_OUTPUT:
---
fail_compilation/fail21275.d(17): Error: function `imports.fail21275a.Foo.x` of type `ref int() return` is not accessible from module `fail21275`
fail_compilation/fail21275.d(20): Error: function `imports.fail21275a.Bar.x` of type `int(int)` is not accessible from module `fail21275`
---
*/

void main()
{
    import imports.fail21275a;

    auto f = Foo();
    f.x = 3;

    auto b = Bar();
    b.x = 3;
}
