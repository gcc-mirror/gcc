/*
TEST_OUTPUT:
---
fail_compilation/parse14285.d(10): Error: variable name expected after type `this`, not `;`
---
*/

struct S
{
    alias this;
}
