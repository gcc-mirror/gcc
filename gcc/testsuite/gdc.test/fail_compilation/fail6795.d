// https://issues.dlang.org/show_bug.cgi?id=6795
/*
TEST_OUTPUT:
---
fail_compilation/fail6795.d(19): Error: `[0][0]` is not an lvalue and cannot be modified
fail_compilation/fail6795.d(20): Error: `[0:0][0]` is not an lvalue and cannot be modified
fail_compilation/fail6795.d(22): Error: `[0][0]` is not an lvalue and cannot be modified
fail_compilation/fail6795.d(23): Error: `[0:0][0]` is not an lvalue and cannot be modified
fail_compilation/fail6795.d(25): Error: `[0][0]` is not an lvalue and cannot be modified
fail_compilation/fail6795.d(26): Error: `[0:0][0]` is not an lvalue and cannot be modified
---
*/

void test_wrong_line_num()
{
    enum int[1] sa = [0];
    enum int[int] aa = [0:0];

    sa[0]++;
    --aa[0];

    sa[0] *= 3;
    aa[0] /= 3;

    auto ps = &sa[0];
    auto pa = &aa[0];
}
