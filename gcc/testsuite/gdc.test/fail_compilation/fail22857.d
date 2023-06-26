// https://issues.dlang.org/show_bug.cgi?id=22857
// EXTRA_FILES: imports/import22857.d

/*
TEST_OUTPUT:
---
fail_compilation/imports/import22857.d(4): Error: (expression) expected following `static if`
fail_compilation/imports/import22857.d(4): Error: declaration expected, not `}`
fail_compilation/fail22857.d(17): Error: template instance `unaryFun!()` template `unaryFun` is not defined
---
*/

void isPrettyPropertyName()
{
    import imports.import22857;

    unaryFun!();
}
