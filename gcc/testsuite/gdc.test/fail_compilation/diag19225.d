/*
TEST_OUTPUT:
---
fail_compilation/diag19225.d(14): Error: basic type expected, not `else`
fail_compilation/diag19225.d(14):        There's no `static else`, use `else` instead.
fail_compilation/diag19225.d(14): Error: found `else` without a corresponding `if`, `version` or `debug` statement
fail_compilation/diag19225.d(15): Error: unmatched closing brace
---
*/

void main()
{
    static if (true) {}
    static else {}
}
