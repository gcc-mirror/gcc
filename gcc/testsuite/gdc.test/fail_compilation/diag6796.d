/*
TEST_OUTPUT:
---
fail_compilation/diag6796.d(11): Error: cannot implicitly convert expression `0` of type `int` to `int[]`
fail_compilation/diag6796.d(11): Error: cannot implicitly convert expression `1` of type `int` to `int[]`
---
*/

void main()
{
    enum int[][] array = [0, 1];
    array[0] *= 10;
}
