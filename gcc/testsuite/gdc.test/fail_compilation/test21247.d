/*
TEST_OUTPUT:
---
fail_compilation/test21247.d(13): Error: anonymous union can only be a part of an aggregate, not function `hang_dmd`
fail_compilation/test21247.d(17): Error: undefined identifier `u`
fail_compilation/test21247.d(18): Error: undefined identifier `b`
fail_compilation/test21247.d(20):        called from here: `hang_dmd(0u)`
---
 */
// https://github.com/dlang/dmd/issues/21247
ubyte[4] hang_dmd(uint a)
{
    union {
        uint u = void;
        ubyte[4] b;
    }
    u = a;
    return b;
}
enum T = hang_dmd(0);
