// REQUIRED_ARGS: -o-
/*
TEST_OUTPUT:
---
fail_compilation/ice15855.d(28): Error: found `End of File` when expecting `(`
fail_compilation/ice15855.d(28): Error: found `End of File` instead of statement
fail_compilation/ice15855.d(28): Error: expression expected, not `End of File`
fail_compilation/ice15855.d(28): Error: found `End of File` when expecting `;` following `for` condition
fail_compilation/ice15855.d(28): Error: expression expected, not `End of File`
fail_compilation/ice15855.d(28): Error: found `End of File` when expecting `)`
fail_compilation/ice15855.d(28): Error: found `End of File` instead of statement
fail_compilation/ice15855.d(28): Error: matching `}` expected following compound statement, not `End of File`
fail_compilation/ice15855.d(27):        unmatched `{`
fail_compilation/ice15855.d(28): Error: found `End of File` when expecting `]`
fail_compilation/ice15855.d(28): Error: no identifier for declarator `a[()
{
for (__error__
 __error; __error)
{
__error__
}
}
]`
---
*/

a[{for
