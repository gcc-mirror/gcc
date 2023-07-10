/*
TEST_OUTPUT:
---
fail_compilation/e15876_3.d(28): Error: unexpected `(` in declarator
fail_compilation/e15876_3.d(28): Error: basic type expected, not `=`
fail_compilation/e15876_3.d(29): Error: found `End of File` when expecting `(`
fail_compilation/e15876_3.d(29): Error: found `End of File` instead of statement
fail_compilation/e15876_3.d(29): Error: expression expected, not `End of File`
fail_compilation/e15876_3.d(29): Error: found `End of File` when expecting `;` following `for` condition
fail_compilation/e15876_3.d(29): Error: expression expected, not `End of File`
fail_compilation/e15876_3.d(29): Error: found `End of File` when expecting `)`
fail_compilation/e15876_3.d(29): Error: found `End of File` instead of statement
fail_compilation/e15876_3.d(29): Error: matching `}` expected following compound statement, not `End of File`
fail_compilation/e15876_3.d(28):        unmatched `{`
fail_compilation/e15876_3.d(29): Error: found `End of File` when expecting `)`
fail_compilation/e15876_3.d(29): Error: no identifier for declarator `d(_error_ = ()
{
for (__error__
 0; 0)
{
__error__
}
}
)`
fail_compilation/e15876_3.d(29): Error: semicolon expected following function declaration, not `End of File`
---
*/
d(={for
