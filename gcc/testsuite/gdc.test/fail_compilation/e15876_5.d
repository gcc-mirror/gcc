/*
TEST_OUTPUT:
---
fail_compilation/e15876_5.d(17): Error: basic type expected, not `End of File`
fail_compilation/e15876_5.d(17): Error: semicolon expected to close `alias` declaration, not `End of File`
fail_compilation/e15876_5.d(17): Error: matching `}` expected following compound statement, not `End of File`
fail_compilation/e15876_5.d(16):        unmatched `{`
fail_compilation/e15876_5.d(17): Error: found `End of File` when expecting `]`
fail_compilation/e15876_5.d(17): Error: variable name expected after type `p[()
{
alias ;
}
]`, not `End of File`
---
*/
p[{alias
