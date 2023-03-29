// REQUIRED_ARGS: -d
/*
TEST_OUTPUT:
---
fail_compilation/fail156.d(35): Error: overlapping initialization for field `x` and `y`
fail_compilation/fail156.d(35):        `struct` initializers that contain anonymous unions must initialize only the first member of a `union`. All subsequent non-overlapping fields are default initialized
fail_compilation/fail156.d(42): Error: overlapping initialization for field `x` and `y`
fail_compilation/fail156.d(42):        `struct` initializers that contain anonymous unions must initialize only the first member of a `union`. All subsequent non-overlapping fields are default initialized
---
*/

alias int myint;

struct S
{
    int i;
    union
    {
        int x = 2;
        int y;
    }
    int j = 3;
    myint k = 4;
}

void main()
{
    S s = S( 1, 5 );
    assert(s.i == 1);
    assert(s.x == 5);
    assert(s.y == 5);
    assert(s.j == 3);
    assert(s.k == 4);

    static S t = S( 1, 6, 6 );
    assert(t.i == 1);
    assert(t.x == 6);
    assert(t.y == 6);
    assert(t.j == 3);
    assert(t.k == 4);

    S u = S( 1, 5, 6 );
    assert(u.i == 1);
    assert(u.x == 5);
    assert(u.y == 5);
    assert(u.j == 3);
    assert(u.k == 4);
}
