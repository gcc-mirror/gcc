// REQUIRED_ARGS: -m32
/*
TEST_OUTPUT:
---
fail_compilation/fail13434_m32.d(13): Error: cannot implicitly convert expression `()` of type `()` to `uint`
---
*/

alias tuple(A...) = A;
void main()
{
    float[] arr;
    arr[tuple!()] = 0;
}
