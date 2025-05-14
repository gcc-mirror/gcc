/*
TEST_OUTPUT:
---
fail_compilation/e15876_4.d(26): Error: found `)` when expecting `(`
fail_compilation/e15876_4.d(27): Error: found `End of File` when expecting `(`
fail_compilation/e15876_4.d(27): Error: found `End of File` instead of statement
fail_compilation/e15876_4.d(27): Error: expression expected, not `End of File`
fail_compilation/e15876_4.d(27): Error: found `End of File` when expecting `;` following `for` condition
fail_compilation/e15876_4.d(27): Error: expression expected, not `End of File`
fail_compilation/e15876_4.d(27): Error: found `End of File` when expecting `)`
fail_compilation/e15876_4.d(27): Error: found `End of File` instead of statement
fail_compilation/e15876_4.d(27): Error: matching `}` expected following compound statement, not `End of File`
fail_compilation/e15876_4.d(26):        unmatched `{`
fail_compilation/e15876_4.d(27): Error: found `End of File` when expecting `)`
fail_compilation/e15876_4.d(27): Error: variable name expected after type `typeof(()
{
for (__error__
 __error; __error)
{
__error__
}
}
)`, not `End of File`
---
*/
typeof){for
