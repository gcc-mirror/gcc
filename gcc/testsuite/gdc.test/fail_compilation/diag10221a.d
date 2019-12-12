/*
TEST_OUTPUT:
---
fail_compilation/diag10221a.d(10): Error: cannot implicitly convert expression `257` of type `int` to `ubyte`
---
*/

void main()
{
    foreach(ubyte i; 0..257) {}
}
