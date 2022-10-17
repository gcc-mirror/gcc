/*
EXTRA_FILES: imports/dip22a.d
TEST_OUTPUT:
---
fail_compilation/dip22a.d(16): Error: no property `bar` for `new Klass` of type `imports.dip22a.Klass`
fail_compilation/dip22a.d(17): Error: no property `bar` for `Struct()` of type `imports.dip22a.Struct`
fail_compilation/dip22a.d(18): Error: undefined identifier `bar` in module `imports.dip22a`
fail_compilation/dip22a.d(19): Error: no property `bar` for `Template!int` of type `void`
fail_compilation/dip22a.d(20): Error: no property `bar` for `12` of type `int`
---
*/
import imports.dip22a;

void test()
{
    new Klass().bar();
    Struct().bar();
    imports.dip22a.bar();
    Template!int.bar();
    12.bar();
}
