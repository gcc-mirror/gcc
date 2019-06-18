/*
TEST_OUTPUT:
---
fail_compilation/diag9357.d(14): Error: cannot implicitly convert expression `1.0` of type `double` to `int`
fail_compilation/diag9357.d(15): Error: cannot implicitly convert expression `10.0` of type `double` to `int`
fail_compilation/diag9357.d(16): Error: cannot implicitly convert expression `11.0` of type `double` to `int`
fail_compilation/diag9357.d(17): Error: cannot implicitly convert expression `99.0` of type `double` to `int`
fail_compilation/diag9357.d(18): Error: cannot implicitly convert expression `1.04858e+06L` of type `real` to `int`
fail_compilation/diag9357.d(19): Error: cannot implicitly convert expression `1.04858e+06L` of type `real` to `int`
---
*/
void main()
{
    { int x = 1.0; }
    { int x = 10.0; }
    { int x = 11.0; }
    { int x = 99.0; }
    { int x = 1048575.0L; }
    { int x = 1048576.0L; }
}
