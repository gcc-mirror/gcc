/*
TEST_OUTPUT:
---
fail_compilation/failinout3748a.d(9): Error: variable `failinout3748a.S3748.err8` only parameters or stack based variables can be `inout`
---
*/
struct S3748
{
    inout(int) err8;
}
