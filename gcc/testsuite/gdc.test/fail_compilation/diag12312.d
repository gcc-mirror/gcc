/*
TEST_OUTPUT:
---
fail_compilation/diag12312.d(10): Error: variable `diag12312.main.arr` `void[16]` does not have a default initializer
---
*/

void main()
{
    void[16] arr;
}
