/*
TEST_OUTPUT:
---
fail_compilation/e15876_4.d(25): Error: found `)` when expecting `(`
fail_compilation/e15876_4.d(26): Error: found `End of File` when expecting `(`
fail_compilation/e15876_4.d(26): Error: found `End of File` instead of statement
fail_compilation/e15876_4.d(26): Error: expression expected, not `End of File`
fail_compilation/e15876_4.d(26): Error: found `End of File` when expecting `;` following `for` condition
fail_compilation/e15876_4.d(26): Error: expression expected, not `End of File`
fail_compilation/e15876_4.d(26): Error: found `End of File` when expecting `)`
fail_compilation/e15876_4.d(26): Error: found `End of File` instead of statement
fail_compilation/e15876_4.d(26): Error: found `End of File` when expecting `}` following compound statement
fail_compilation/e15876_4.d(26): Error: found `End of File` when expecting `)`
fail_compilation/e15876_4.d(26): Error: no identifier for declarator `typeof(()
{
for (__error__
 0; 0)
{
__error__
}
}
)`
---
*/
typeof){for
