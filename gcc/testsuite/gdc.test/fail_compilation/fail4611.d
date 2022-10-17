/*
TEST_OUTPUT:
---
fail_compilation/fail4611.d(15): Error: `Vec[$n$]` size 4 * $n$ exceeds $?:windows+32=0x1000000|0x7fffffff$ size limit for static array
---
*/

struct Vec
{
    int x;
}

void main()
{
    Vec[ptrdiff_t.max] a;
}
