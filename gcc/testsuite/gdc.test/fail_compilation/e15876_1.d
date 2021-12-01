/*
TEST_OUTPUT:
---
fail_compilation/e15876_1.d(15): Error: valid scope identifiers are `exit`, `failure`, or `success`, not `x`
fail_compilation/e15876_1.d(16): Error: found `End of File` when expecting `)`
fail_compilation/e15876_1.d(16): Error: found `End of File` instead of statement
fail_compilation/e15876_1.d(16): Error: found `End of File` when expecting `}` following compound statement
fail_compilation/e15876_1.d(16): Error: found `End of File` when expecting `]`
fail_compilation/e15876_1.d(16): Error: no identifier for declarator `o[()
{
scope(exit) }
]`
---
*/
o[{scope(x
