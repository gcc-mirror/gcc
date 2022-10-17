/*
TEST_OUTPUT:
---
fail_compilation/fail91.d(12): Error: struct `fail91.S` unknown size
---
*/

struct S;

void main()
{
    S* s = new S();
}
