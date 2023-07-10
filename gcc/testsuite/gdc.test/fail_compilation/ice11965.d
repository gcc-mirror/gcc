/*
TEST_OUTPUT:
---
fail_compilation/ice11965.d(16): Error: no identifier for declarator `b*`
fail_compilation/ice11965.d(16): Error: matching `}` expected following compound statement, not `End of File`
fail_compilation/ice11965.d(15):        unmatched `{`
fail_compilation/ice11965.d(16): Error: found `End of File` when expecting `]`
fail_compilation/ice11965.d(16): Error: no identifier for declarator `u[()
{
b* A;
}
]`
---
*/
u[{b*A,
