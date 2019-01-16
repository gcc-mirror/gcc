/*
TEST_OUTPUT:
---
fail_compilation/diag11132.d(22): Error: overlapping initialization for field a and b
---
*/

struct S
{
    int x;
    union
    {
        int a;
        int b;
    }

    int z;
}

void main()
{
    S s = { 1, 2, 3 };
}
