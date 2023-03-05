/*
TEST_OUTPUT:
---
fail_compilation/diag11132.d(23): Error: overlapping initialization for field `a` and `b`
fail_compilation/diag11132.d(23):        `struct` initializers that contain anonymous unions must initialize only the first member of a `union`. All subsequent non-overlapping fields are default initialized
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
