/*
REQUIRED_ARGS: -betterC
TEST_OUTPUT:
---
fail_compilation/fail19911a.d(9): Error: function `fail19911a.fun` D-style variadic functions cannot be used with -betterC
---
*/

void fun(...)
{
}
