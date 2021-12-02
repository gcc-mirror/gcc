/*
TEST_OUTPUT:
---
fail_compilation/e15876_2.d(15): Error: identifier expected following `template`
fail_compilation/e15876_2.d(15): Error: found `End of File` when expecting `}` following compound statement
fail_compilation/e15876_2.d(15): Error: found `End of File` when expecting `]`
fail_compilation/e15876_2.d(15): Error: no identifier for declarator `o[()
{
;
}
]`
---
*/
o[{template
