/*
TEST_OUTPUT:
---
fail_compilation/ice11965.d(15): Error: no identifier for declarator `b*`
fail_compilation/ice11965.d(15): Error: found `End of File` when expecting `}` following compound statement
fail_compilation/ice11965.d(15): Error: found `End of File` when expecting `]`
fail_compilation/ice11965.d(15): Error: no identifier for declarator `u[()
{
b* A;
}
]`
---
*/
u[{b*A,
