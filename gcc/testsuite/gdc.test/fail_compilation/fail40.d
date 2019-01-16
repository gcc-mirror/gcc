/*
TEST_OUTPUT:
---
fail_compilation/fail40.d(11): Error: variable yuiop cannot be read at compile time
---
*/

struct Qwert
{
    int[20] yuiop;
    int* asdfg = yuiop.ptr;
}
