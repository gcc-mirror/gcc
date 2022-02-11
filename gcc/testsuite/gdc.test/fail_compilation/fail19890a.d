/*
TEST_OUTPUT:
---
fail_compilation/fail19890a.d(8): Error: `void[$n$$?:64=LU$]` size 1 * $n$ exceeds $?:windows+32=0x1000000|0x7fffffff$ size limit for static array
---
*/

void[] f = cast(void[-1]) "a";
