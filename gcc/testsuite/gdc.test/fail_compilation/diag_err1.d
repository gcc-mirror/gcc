/*
TEST_OUTPUT:
---
fail_compilation/diag_err1.d(21): Error: undefined identifier `x`
fail_compilation/diag_err1.d(21):        while evaluating `pragma(msg, [1, 2, x].length)`
fail_compilation/diag_err1.d(22): Error: undefined identifier `x`
fail_compilation/diag_err1.d(22): Error: undefined identifier `y`
fail_compilation/diag_err1.d(22):        while evaluating `pragma(msg, (x + y).sizeof)`
fail_compilation/diag_err1.d(23): Error: undefined identifier `x`
fail_compilation/diag_err1.d(23):        while evaluating `pragma(msg, (n += x).sizeof)`
fail_compilation/diag_err1.d(24): Error: incompatible types for `(s) ~ (n)`: `string` and `int`
fail_compilation/diag_err1.d(24):        while evaluating `pragma(msg, (s ~ n).sizeof)`
---
*/

void main()
{
    int n;
    string s;

    pragma(msg, [1,2,x].length);
    pragma(msg, (x + y).sizeof);
    pragma(msg, (n += x).sizeof);
    pragma(msg, (s ~ n).sizeof);
}
