/*
TEST_OUTPUT:
---
fail_compilation/fail20448.d(16): Error: returning `p.x` escapes a reference to parameter `p`
fail_compilation/fail20448.d(22): Error: template instance `fail20448.member!"x"` error instantiating
---
*/

struct S
{
    int x, y;
}

ref int member(string mem)(S p)
{
    return p.x;
}

void main()
{
    S p;
    p.member!"x" = 2;
}
