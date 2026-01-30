// REQUIRED_ARGS: -preview=bitfields
/*
TEST_OUTPUT:
---
fail_compilation/fail21660.d(29): Error: overlapping initialization for field `b` and `c`
fail_compilation/fail21660.d(29):        `struct` initializers that contain anonymous unions must initialize only the first member of a `union`. All subsequent non-overlapping fields are default initialized
fail_compilation/fail21660.d(30): Error: overlapping initialization for field `b` and `c`
fail_compilation/fail21660.d(30):        `struct` initializers that contain anonymous unions must initialize only the first member of a `union`. All subsequent non-overlapping fields are default initialized
fail_compilation/fail21660.d(52): Error: overlapping initialization for field `a` and `b`
fail_compilation/fail21660.d(52): Error: overlapping initialization for field `a` and `d`
fail_compilation/fail21660.d(52): Error: overlapping initialization for field `c` and `d`
fail_compilation/fail21660.d(54): Error: overlapping initialization for field `a` and `d`
---
*/

struct S
{
    uint a : 1;
    union {
        uint b : 2;
        struct {
            uint c : 3;
        }
    }
}

void testS()
{
    S s = S(1, 2, 3);       // b + c overlap
    S t = S(a:1, b:2, c:3); // b + c overlap
    S u = S(a:1, c:3);      // ok
}

union U
{
    union {
        uint a : 5;
        uint b : 4;
    }
    struct {
        uint : 5;
        uint c : 11;
    }
    struct {
        uint : 4;
        uint d : 12;
    }
}

void testU()
{
    U s = U(1, 2, 3, 4); // a + b, a + d, c + d overlap
    U t = U(a:1, c:3);   // ok
    U u = U(a:1, d:2);   // a + d overlap
    U v = U(b:1, c:3);   // ok
    U w = U(b:1, d:2);   // ok
}
