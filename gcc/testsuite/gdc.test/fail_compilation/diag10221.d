/*
TEST_OUTPUT:
---
fail_compilation/diag10221.d(10): Error: cannot implicitly convert expression `256` of type `int` to `ubyte`
---
*/

void main()
{
    foreach(ref ubyte i; 0..256) {}
}
