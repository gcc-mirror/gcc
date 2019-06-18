/*
EXTRA_FILES: imports/fail347a.d
TEST_OUTPUT:
---
fail_compilation/fail347.d(22): Error: undefined identifier `bbr`, did you mean variable `bar`?
fail_compilation/fail347.d(23): Error: no property `ofo` for type `S`, did you mean `fail347.S.foo`?
fail_compilation/fail347.d(24): Error: undefined identifier `strlenx`, did you mean function `strlen`?
---
*/

//import core.stdc.string;
import imports.fail347a;

struct S
{
    int foo;
}

void main()
{
    S bar;
    bbr.foo = 3;
    bar.ofo = 4;
    auto s = strlenx("hello");
}
