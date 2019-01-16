// REQUIRED_ARGS: -m64
/*
TEST_OUTPUT:
---
fail_compilation/fail13434_m64.d(13): Error: cannot implicitly convert expression `()` of type `()` to `ulong`
---
*/

alias tuple(A...) = A;
void main()
{
    float[] arr;
    arr[tuple!()] = 0;
}
