/*
TEST_OUTPUT:
---
fail_compilation/fail303.d(19): Error: double /= cdouble is undefined. Did you mean double /= cdouble.re ?
fail_compilation/fail303.d(20): Error: ireal *= ireal is an undefined operation
fail_compilation/fail303.d(21): Error: ireal *= creal is undefined. Did you mean ireal *= creal.im ?
fail_compilation/fail303.d(22): Error: ireal %= creal is undefined. Did you mean ireal %= creal.im ?
fail_compilation/fail303.d(23): Error: ireal += real is undefined (result is complex)
fail_compilation/fail303.d(24): Error: ireal -= creal is undefined (result is complex)
fail_compilation/fail303.d(25): Error: double -= idouble is undefined (result is complex)
---
*/


void main()
{
    ireal x = 3.0i;
    double y = 3;
    y /= 2.0 + 6i;
    x *= 7.0i;
    x *= 3.0i + 2;
    x %= (2 + 6.0i);
    x += 2.0;
    x -= 1 + 4i;
    y -= 3.0i;
}
