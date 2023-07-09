/*
TEST_OUTPUT:
---
fail_compilation/ice11982.d(19): Error: basic type expected, not `scope`
fail_compilation/ice11982.d(19): Error: found `scope` when expecting `;` following statement `new _error_` on line fail_compilation/ice11982.d(19)
fail_compilation/ice11982.d(19): Error: basic type expected, not `}`
fail_compilation/ice11982.d(19): Error: missing `{ ... }` for function literal
fail_compilation/ice11982.d(19): Error: C style cast illegal, use `cast(funk)function _error_()
{
}
`
fail_compilation/ice11982.d(19): Error: found `}` when expecting `;` following statement `cast(funk)function _error_()
{
}
` on line fail_compilation/ice11982.d(19)
fail_compilation/ice11982.d(20): Error: found `End of File` when expecting `}` following compound statement
---
*/
void main() { new scope ( funk ) function }
