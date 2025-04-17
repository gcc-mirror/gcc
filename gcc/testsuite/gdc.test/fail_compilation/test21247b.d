/*
TEST_OUTPUT:
---
fail_compilation/test21247b.d(10): Error: anonymous union can only be a part of an aggregate, not function `test21247`
---
 */
// https://github.com/dlang/dmd/issues/21247
void test21247()
{
    union {
        uint u = void;
        ubyte[4] b;
    }
}
