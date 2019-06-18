/*
TEST_OUTPUT:
---
fail_compilation/e15876_4.d(23): Error: found `)` when expecting `(`
fail_compilation/e15876_4.d(24): Error: found `End of File` when expecting `(`
fail_compilation/e15876_4.d(24): Error: found `End of File` instead of statement
fail_compilation/e15876_4.d(24): Error: expression expected, not `End of File`
fail_compilation/e15876_4.d(24): Error: found `End of File` when expecting `;` following `for` condition
fail_compilation/e15876_4.d(24): Error: expression expected, not `End of File`
fail_compilation/e15876_4.d(24): Error: found `End of File` when expecting `)`
fail_compilation/e15876_4.d(24): Error: found `End of File` instead of statement
fail_compilation/e15876_4.d(24): Error: found `End of File` when expecting `}` following compound statement
fail_compilation/e15876_4.d(24): Error: found `End of File` when expecting `)`
fail_compilation/e15876_4.d(24): Error: no identifier for declarator `typeof(()
{
for (; 0; 0)
{
}
}
)`
---
*/
typeof){for
