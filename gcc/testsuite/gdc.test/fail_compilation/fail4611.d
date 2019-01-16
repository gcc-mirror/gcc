/*
---
fail_compilation/fail4611.d(15): Error: Vec[2147483647] size 4 * 2147483647 exceeds 0x7fffffff size limit for static array
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
