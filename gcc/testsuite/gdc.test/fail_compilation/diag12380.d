/*
TEST_OUTPUT:
---
fail_compilation/diag12380.d(12): Error: cannot implicitly convert expression `cast(E)0` of type `E` to `void*`
---
*/

enum E { a, b, }

void main()
{
    void* a = E.init;
}
