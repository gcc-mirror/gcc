/*
REQUIRED_ARGS: -o-
PERMUTE_ARGS:
TEST_OUTPUT:
---
fail_compilation/imports/fail17646.d(10): Error: found `}` instead of statement
fail_compilation/imports/fail17646.d(7): Error: function imports.fail17646.allTestData!"".allTestData has no return statement, but is expected to return a value of type const(TestData)[]
fail_compilation/fail17646.d(16): Error: template instance imports.fail17646.allTestData!"" error instantiating
fail_compilation/fail17646.d(19):        instantiated from here: runTests!""
---
*/
int runTests(Modules...)()
{
    import imports.fail17646;

    allTestData!Modules;
}

alias fail = runTests!"";
