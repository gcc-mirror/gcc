/*
REQUIRED_ARGS: -o-
EXTRA_FILES: imports/fail17646.d
TEST_OUTPUT:
---
fail_compilation/imports/fail17646.d(10): Error: found `}` instead of statement
fail_compilation/fail17646.d(11): Error: function `fail17646.runTests!"".runTests` has no `return` statement, but is expected to return a value of type `int`
fail_compilation/fail17646.d(18): Error: template instance `fail17646.runTests!""` error instantiating
---
*/
int runTests(Modules...)()
{
    import imports.fail17646;

    allTestData!Modules;
}

alias fail = runTests!"";
