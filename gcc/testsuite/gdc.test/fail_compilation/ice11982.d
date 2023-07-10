/*
TEST_OUTPUT:
---
fail_compilation/ice11982.d(20): Error: basic type expected, not `scope`
fail_compilation/ice11982.d(20): Error: found `scope` when expecting `;` following statement `new _error_` on line fail_compilation/ice11982.d(20)
fail_compilation/ice11982.d(20): Error: basic type expected, not `}`
fail_compilation/ice11982.d(20): Error: missing `{ ... }` for function literal
fail_compilation/ice11982.d(20): Error: C style cast illegal, use `cast(funk)function _error_()
{
}
`
fail_compilation/ice11982.d(20): Error: found `}` when expecting `;` following statement `cast(funk)function _error_()
{
}
` on line fail_compilation/ice11982.d(20)
fail_compilation/ice11982.d(21): Error: matching `}` expected following compound statement, not `End of File`
fail_compilation/ice11982.d(20):        unmatched `{`
---
*/
void main() { new scope ( funk ) function }
