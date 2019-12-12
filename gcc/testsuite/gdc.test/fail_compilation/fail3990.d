/*
TEST_OUTPUT:
---
fail_compilation/fail3990.d(12): Error: using * on an array is no longer supported; use *(arr1).ptr instead
fail_compilation/fail3990.d(14): Error: using * on an array is no longer supported; use *(arr2).ptr instead
---
*/

void main()
{
    int[] arr1 = [1, 2, 3];
    assert(*arr1 == 1);
    int[3] arr2 = [1, 2, 3];
    assert(*arr2 == 1);
}
