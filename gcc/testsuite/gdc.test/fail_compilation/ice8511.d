/*
TEST_OUTPUT:
---
fail_compilation/ice8511.d(11): Error: enum ice8511.hypot.SQRTMAX is forward referenced looking for base type
fail_compilation/ice8511.d(12): Error: incompatible types for ((SQRTMAX) / (2)): cannot use '/' with types
---
*/

real hypot()
{
    enum SQRTMAX;
    SQRTMAX/2;
}
