/*
TEST_OUTPUT:
---
fail_compilation/ice10419.d(12): Error: `arr().length` is not an lvalue and cannot be modified
---
*/

int[] arr() { return []; }

void main()
{
    arr().length = 1;
}
