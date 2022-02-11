// https://issues.dlang.org/show_bug.cgi?id=20779

/*
TEST_OUTPUT:
---
fail_compilation/fail20779.d(12): Error: struct `fail20779.X` cannot have field `x` with same struct type
---
*/

module fail20779;

struct X
{
    X x;

    enum e = __traits(compiles, X.init);
}
