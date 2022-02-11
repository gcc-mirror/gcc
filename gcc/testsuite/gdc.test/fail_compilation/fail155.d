/*
TEST_OUTPUT:
---
fail_compilation/fail155.d(20): Error: overlapping initialization for `y`
fail_compilation/fail155.d(20):        `struct` initializers that contain anonymous unions must initialize only the first member of a `union`. All subsequent non-overlapping fields are default initialized
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
