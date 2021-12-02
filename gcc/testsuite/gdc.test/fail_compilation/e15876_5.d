/*
TEST_OUTPUT:
---
fail_compilation/e15876_5.d(16): Error: basic type expected, not `End of File`
fail_compilation/e15876_5.d(16): Error: semicolon expected to close `alias` declaration
fail_compilation/e15876_5.d(16): Error: found `End of File` when expecting `}` following compound statement
fail_compilation/e15876_5.d(16): Error: found `End of File` when expecting `]`
fail_compilation/e15876_5.d(16): Error: no identifier for declarator `p[()
{
alias ;
}
]`
---
*/
p[{alias
