/*
TEST_OUTPUT:
---
fail_compilation/ice10419.d(12): Error: cannot modify expression `arr().length` because it is not an lvalue
---
*/

int[] arr() { return []; }

void main()
{
    arr().length = 1;
}
