/*
DFLAGS:
REQUIRED_ARGS: -conf= -Ifail_compilation/extra-files/minimal
TEST_OUTPUT:
---
fail_compilation/fail19319b.d(16): Error: `7 ^^ x` requires `std.math` for `^^` operators
fail_compilation/fail19319b.d(17): Error: `x ^^ 7` requires `std.math` for `^^` operators
---
*/

void test19319(int x)
{
    static assert(!__traits(compiles, 7 ^^ x));
    static assert(!__traits(compiles, x ^^= 7));

    int i = 7 ^^ x;
    x ^^= 7;
}
