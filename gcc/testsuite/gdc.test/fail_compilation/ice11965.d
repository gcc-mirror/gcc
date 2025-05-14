/*
TEST_OUTPUT:
---
fail_compilation/ice11965.d(16): Error: variable name expected after type `b*`, not `End of File`
fail_compilation/ice11965.d(16): Error: matching `}` expected following compound statement, not `End of File`
fail_compilation/ice11965.d(15):        unmatched `{`
fail_compilation/ice11965.d(16): Error: found `End of File` when expecting `]`
fail_compilation/ice11965.d(16): Error: variable name expected after type `u[()
{
b* A;
}
]`, not `End of File`
---
*/
u[{b*A,
