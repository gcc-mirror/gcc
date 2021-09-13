/*
TEST_OUTPUT:
---
fail_compilation/ice11982.d(16): Error: basic type expected, not `scope`
fail_compilation/ice11982.d(16): Error: found `scope` when expecting `;` following statement
fail_compilation/ice11982.d(16): Error: basic type expected, not `}`
fail_compilation/ice11982.d(16): Error: missing `{ ... }` for function literal
fail_compilation/ice11982.d(16): Error: C style cast illegal, use `cast(funk)function _error_()
{
}
`
fail_compilation/ice11982.d(16): Error: found `}` when expecting `;` following statement
fail_compilation/ice11982.d(17): Error: found `End of File` when expecting `}` following compound statement
---
*/
void main() { new scope ( funk ) function }
