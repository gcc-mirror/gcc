/*
TEST_OUTPUT:
---
fail_compilation/e15876_3.d(25): Error: unexpected `(` in declarator
fail_compilation/e15876_3.d(25): Error: basic type expected, not `=`
fail_compilation/e15876_3.d(26): Error: found `End of File` when expecting `(`
fail_compilation/e15876_3.d(26): Error: found `End of File` instead of statement
fail_compilation/e15876_3.d(26): Error: expression expected, not `End of File`
fail_compilation/e15876_3.d(26): Error: found `End of File` when expecting `;` following `for` condition
fail_compilation/e15876_3.d(26): Error: expression expected, not `End of File`
fail_compilation/e15876_3.d(26): Error: found `End of File` when expecting `)`
fail_compilation/e15876_3.d(26): Error: found `End of File` instead of statement
fail_compilation/e15876_3.d(26): Error: found `End of File` when expecting `}` following compound statement
fail_compilation/e15876_3.d(26): Error: found `End of File` when expecting `)`
fail_compilation/e15876_3.d(26): Error: no identifier for declarator `d(_error_ = ()
{
for (; 0; 0)
{
}
}
)`
fail_compilation/e15876_3.d(26): Error: semicolon expected following function declaration
---
*/
d(={for
