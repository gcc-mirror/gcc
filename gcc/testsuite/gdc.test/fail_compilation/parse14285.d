/*
TEST_OUTPUT:
---
fail_compilation/parse14285.d(10): Error: no identifier for declarator `this`
---
*/

struct S
{
    alias this;
}
