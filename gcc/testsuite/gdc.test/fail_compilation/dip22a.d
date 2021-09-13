/*
REQUIRED_ARGS:
TEST_OUTPUT:
---
fail_compilation/dip22a.d(16): Error: no property `bar` for type `imports.dip22a.Klass`, did you mean `imports.dip22a.Klass.bar`?
fail_compilation/dip22a.d(17): Error: no property `bar` for type `Struct`, did you mean `imports.dip22a.Struct.bar`?
fail_compilation/dip22a.d(18): Error: undefined identifier `bar` in module `imports.dip22a`, did you mean function `bar`?
fail_compilation/dip22a.d(19): Error: no property `bar` for type `void`
fail_compilation/dip22a.d(20): Error: no property `bar` for type `int`
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
