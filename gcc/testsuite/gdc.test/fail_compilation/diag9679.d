/*
TEST_OUTPUT:
---
fail_compilation/diag9679.d(11): Error: variable `diag9679.main.n` - only parameters, functions and `foreach` declarations can be `ref`
fail_compilation/diag9679.d(12): Error: variable `diag9679.main.n` - storage class `auto` has no effect if type is not inferred, did you mean `scope`?
---
*/

void main()
{
    if (ref n = 1) {}
    if (auto int n = 1) {}
}
