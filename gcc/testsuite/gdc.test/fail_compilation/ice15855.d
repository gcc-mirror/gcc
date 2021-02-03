// REQUIRED_ARGS: -o-
/*
TEST_OUTPUT:
---
fail_compilation/ice15855.d(25): Error: found `End of File` when expecting `(`
fail_compilation/ice15855.d(25): Error: found `End of File` instead of statement
fail_compilation/ice15855.d(25): Error: expression expected, not `End of File`
fail_compilation/ice15855.d(25): Error: found `End of File` when expecting `;` following `for` condition
fail_compilation/ice15855.d(25): Error: expression expected, not `End of File`
fail_compilation/ice15855.d(25): Error: found `End of File` when expecting `)`
fail_compilation/ice15855.d(25): Error: found `End of File` instead of statement
fail_compilation/ice15855.d(25): Error: found `End of File` when expecting `}` following compound statement
fail_compilation/ice15855.d(25): Error: found `End of File` when expecting `]`
fail_compilation/ice15855.d(25): Error: no identifier for declarator `a[()
{
for (; 0; 0)
{
}
}
]`
---
*/

a[{for
