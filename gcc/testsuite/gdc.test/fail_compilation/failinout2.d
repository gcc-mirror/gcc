/*
TEST_OUTPUT:
---
fail_compilation/failinout2.d(8): Error: variable `failinout2.x` - only parameters or stack-based variables can be `inout`
fail_compilation/failinout2.d(12): Error: variable `failinout2.S3748.err8` - only parameters or stack-based variables can be `inout`
---
*/
inout int x;

struct S3748
{
    inout(int) err8;
}
