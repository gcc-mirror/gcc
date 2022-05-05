/*
TEST_OUTPUT:
---
fail_compilation/diag13281.d(20): Error: cannot implicitly convert expression `123` of type `int` to `string`
fail_compilation/diag13281.d(21): Error: cannot implicitly convert expression `123u` of type `uint` to `string`
fail_compilation/diag13281.d(22): Error: cannot implicitly convert expression `123L` of type `long` to `string`
fail_compilation/diag13281.d(23): Error: cannot implicitly convert expression `123LU` of type `ulong` to `string`
fail_compilation/diag13281.d(24): Error: cannot implicitly convert expression `123.4` of type `double` to `int`
fail_compilation/diag13281.d(25): Error: cannot implicitly convert expression `123.4F` of type `float` to `int`
fail_compilation/diag13281.d(26): Error: cannot implicitly convert expression `123.4L` of type `real` to `int`
fail_compilation/diag13281.d(27): Error: cannot implicitly convert expression `123.4i` of type `idouble` to `int`
fail_compilation/diag13281.d(28): Error: cannot implicitly convert expression `123.4Fi` of type `ifloat` to `int`
fail_compilation/diag13281.d(29): Error: cannot implicitly convert expression `123.4Li` of type `ireal` to `int`
fail_compilation/diag13281.d(30): Error: cannot implicitly convert expression `123.4 + 5.6i` of type `cdouble` to `int`
fail_compilation/diag13281.d(31): Error: cannot implicitly convert expression `123.4F + 5.6Fi` of type `cfloat` to `int`
fail_compilation/diag13281.d(32): Error: cannot implicitly convert expression `123.4L + 5.6Li` of type `creal` to `int`
---
*/

string x1 = 123;
string x2 = 123u;
string x3 = 123L;
string x4 = 123uL;
int y1 = 123.4;
int y2 = 123.4f;
int y3 = 123.4L;
int y4 = 123.4i;
int y5 = 123.4fi;
int y6 = 123.4Li;
int y7 = 123.4 +5.6i;
int y8 = 123.4f+5.6fi;
int y9 = 123.4L+5.6Li;
