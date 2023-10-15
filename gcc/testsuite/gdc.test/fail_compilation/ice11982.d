/*
TEST_OUTPUT:
---
fail_compilation/ice11982.d(22): Error: basic type expected, not `scope`
fail_compilation/ice11982.d(22): Error: found `scope` when expecting `;` following expression
fail_compilation/ice11982.d(22):        expression: `new _error_`
fail_compilation/ice11982.d(22): Error: basic type expected, not `}`
fail_compilation/ice11982.d(22): Error: missing `{ ... }` for function literal
fail_compilation/ice11982.d(22): Error: C style cast illegal, use `cast(funk)function _error_()
{
}
`
fail_compilation/ice11982.d(22): Error: found `}` when expecting `;` following expression
fail_compilation/ice11982.d(22):        expression: `cast(funk)function _error_()
{
}
`
fail_compilation/ice11982.d(23): Error: matching `}` expected following compound statement, not `End of File`
fail_compilation/ice11982.d(22):        unmatched `{`
---
*/
void main() { new scope ( funk ) function }
