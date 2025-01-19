// https://issues.dlang.org/show_bug.cgi?id=6795
/*
TEST_OUTPUT:
---
fail_compilation/fail6795.d(19): Error: cannot modify expression `[0][0]` because it is not an lvalue
fail_compilation/fail6795.d(20): Error: cannot modify expression `[0:0][0]` because it is not an lvalue
fail_compilation/fail6795.d(22): Error: cannot modify expression `[0][0]` because it is not an lvalue
fail_compilation/fail6795.d(23): Error: cannot modify expression `[0:0][0]` because it is not an lvalue
fail_compilation/fail6795.d(25): Error: cannot take address of expression `[0][0]` because it is not an lvalue
fail_compilation/fail6795.d(26): Error: cannot take address of expression `[0:0][0]` because it is not an lvalue
fail_compilation/fail6795.d(30): Error: cannot modify expression `Some["zz"]` because it is not an lvalue
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

    // https://issues.dlang.org/show_bug.cgi?id=24845
    enum Maps : int[string] { Some = ["aa" : 12], Other = ["bb" : 24] }
    Maps.Some["zz"] = 44;
}
