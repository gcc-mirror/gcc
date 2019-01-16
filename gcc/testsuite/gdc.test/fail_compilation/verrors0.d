// REQUIRED_ARGS: -verrors=0

void main()
{
    { T a; }    // 1
    { T a; }    // 2
    { T a; }    // 3
    { T a; }    // 4
    { T a; }    // 5
    { T a; }    // 6
    { T a; }    // 7
    { T a; }    // 8
    { T a; }    // 9
    { T a; }    // 10
    { T a; }    // 11
    { T a; }    // 12
    { T a; }    // 13
    { T a; }    // 14
    { T a; }    // 15
    { T a; }    // 16
    { T a; }    // 17
    { T a; }    // 18
    { T a; }    // 19
    { T a; }    // 20 (default limit)
    { T a; }    // 21
    { T a; }    // 22
    { T a; }    // 23
    { T a; }    // 24
    { T a; }    // 25
}
/*
TEST_OUTPUT:
---
fail_compilation/verrors0.d(5): Error: undefined identifier `T`
fail_compilation/verrors0.d(6): Error: undefined identifier `T`
fail_compilation/verrors0.d(7): Error: undefined identifier `T`
fail_compilation/verrors0.d(8): Error: undefined identifier `T`
fail_compilation/verrors0.d(9): Error: undefined identifier `T`
fail_compilation/verrors0.d(10): Error: undefined identifier `T`
fail_compilation/verrors0.d(11): Error: undefined identifier `T`
fail_compilation/verrors0.d(12): Error: undefined identifier `T`
fail_compilation/verrors0.d(13): Error: undefined identifier `T`
fail_compilation/verrors0.d(14): Error: undefined identifier `T`
fail_compilation/verrors0.d(15): Error: undefined identifier `T`
fail_compilation/verrors0.d(16): Error: undefined identifier `T`
fail_compilation/verrors0.d(17): Error: undefined identifier `T`
fail_compilation/verrors0.d(18): Error: undefined identifier `T`
fail_compilation/verrors0.d(19): Error: undefined identifier `T`
fail_compilation/verrors0.d(20): Error: undefined identifier `T`
fail_compilation/verrors0.d(21): Error: undefined identifier `T`
fail_compilation/verrors0.d(22): Error: undefined identifier `T`
fail_compilation/verrors0.d(23): Error: undefined identifier `T`
fail_compilation/verrors0.d(24): Error: undefined identifier `T`
fail_compilation/verrors0.d(25): Error: undefined identifier `T`
fail_compilation/verrors0.d(26): Error: undefined identifier `T`
fail_compilation/verrors0.d(27): Error: undefined identifier `T`
fail_compilation/verrors0.d(28): Error: undefined identifier `T`
fail_compilation/verrors0.d(29): Error: undefined identifier `T`
---
*/
