/*
TEST_OUTPUT:
---
fail_compilation/fail155.d(19): Error: overlapping initialization for y
---
*/

struct S
{
    int i;
    union
    {
        int x;
        int y;
    }
    int j;
}

S s = S( 1, 2, 3, 4 );
