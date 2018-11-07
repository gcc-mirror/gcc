/*
TEST_OUTPUT:
---
fail_compilation/fail275.d(10): Error: circular reference to variable 'fail275.C.x'
---
*/
// REQUIRED_ARGS: -d
struct C
{
    const x = C.x;
}
