// EXTRA_SOURCES: imports/b18219.d
// EXTRA_FILES: imports/a18219.d
/*
TEST_OUTPUT:
---
fail_compilation/fail18219.d(17): Error: no property `Foobar` for type `AST`, did you mean `b18219.Foobar`?
fail_compilation/fail18219.d(18): Error: no property `Bar` for type `a18219.AST`
fail_compilation/imports/a18219.d(3):        struct `AST` defined here
fail_compilation/fail18219.d(19): Error: no property `fun` for type `AST`, did you mean `b18219.fun`?
fail_compilation/fail18219.d(20): Error: no property `Foobar` for type `AST`, did you mean `b18219.Foobar`?
---
*/
import imports.a18219;

void main()
{
    AST.Foobar t;
    AST.Bar l;
    AST.fun();
    AST.Foobar.smeth();
}
