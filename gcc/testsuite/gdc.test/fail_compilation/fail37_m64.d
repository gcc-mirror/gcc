// REQUIRED_ARGS: -m64
/*
TEST_OUTPUT:
---
fail_compilation/fail37_m64.d(9): Error: 'cast(float)4LU / cast(float)8LU - cast(float)2147483647' is not of integral type, it is a float
---
*/

ulong[cast(uint)((cast(float)int.sizeof/ulong.sizeof)-int.max>>2)+int.max>>2] hexarray;
