/*
TEST_OUTPUT:
---
fail_compilation/diag12312.d(10): Error: variable `diag12312.main.arr` of type `void[16]` does not have a default initializer
fail_compilation/diag12312.d(15): Error: variable `diag12312.bug1176.v` of type `void[1]` does not have a default initializer
---
*/
void main()
{
    void[16] arr;
}

void bug1176()
{
    void[1] v;
}
