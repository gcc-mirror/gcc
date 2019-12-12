/*
TEST_OUTPUT:
---
fail_compilation/ice11926.d(11): Error: no identifier for declarator `const(a)`
fail_compilation/ice11926.d(12): Error: no identifier for declarator `const(b)`
---
*/

enum
{
    const a = 1,
    const b = 2
}
