/*
TEST_OUTPUT:
---
fail_compilation/ice13382.d(18): Error: incompatible types for ((a) == (0)): 'int[]' and 'int'
fail_compilation/ice13382.d(19): Error: incompatible types for ((a) >= (0)): 'int[]' and 'int'
fail_compilation/ice13382.d(20): Error: incompatible types for ((0) == (a)): 'int' and 'int[]'
fail_compilation/ice13382.d(21): Error: incompatible types for ((0) >= (a)): 'int' and 'int[]'
fail_compilation/ice13382.d(22): Error: incompatible types for ((a) is (0)): 'int[]' and 'int'
fail_compilation/ice13382.d(23): Error: incompatible types for ((a) !is (0)): 'int[]' and 'int'
fail_compilation/ice13382.d(24): Error: incompatible types for ((0) is (a)): 'int' and 'int[]'
fail_compilation/ice13382.d(25): Error: incompatible types for ((0) !is (a)): 'int' and 'int[]'
---
*/

void main ()
{
    int[] a;
    if (a == 0) {}
    if (a >= 0) {}
    if (0 == a) {}
    if (0 >= a) {}
    if (a  is 0) {}
    if (a !is 0) {}
    if (0  is a) {}
    if (0 !is a) {}
}
