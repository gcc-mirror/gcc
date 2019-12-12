/*
TEST_OUTPUT:
---
fail_compilation/fail14416.d(13): Error: template S(T) does not have property 'sizeof'
---
*/

struct S(T)
{
    int x;
}

enum n = S.sizeof;
