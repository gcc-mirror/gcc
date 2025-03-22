/*
EXTRA_FILES: imports/fail347a.d
TEST_OUTPUT:
---
fail_compilation/fail347.d(26): Error: undefined identifier `bbr`, did you mean variable `bar`?
fail_compilation/fail347.d(27): Error: no property `ofo` for type `S`, did you mean `fail347.S.foo`?
fail_compilation/fail347.d(29): Error: no property `fool` for `sp` of type `fail347.S*`
fail_compilation/fail347.d(20):        did you mean variable `foo`?
fail_compilation/fail347.d(30): Error: undefined identifier `strlenx`, did you mean function `strlen`?
fail_compilation/fail347.d(31): Error: no property `strlenx` for `"hello"` of type `string`
fail_compilation/imports/fail347a.d(3):        did you mean function `strlen`?
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
    auto sp = &bar;
    sp.fool = 5;
    auto s = strlenx("hello");
    auto q = "hello".strlenx();
}
