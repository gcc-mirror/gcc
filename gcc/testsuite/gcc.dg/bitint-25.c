// PR c/102989
// { dg-do compile { target bitint } }
// { dg-options "-std=c11 -Wno-c11-c23-compat -pedantic-errors" }

_BitInt(63) a;
signed _BitInt(15) b;
unsigned _BitInt(31) c;
int d = 21wb;
long long e = 60594869054uwb;
__extension__ _BitInt(63) f;
__extension__ _BitInt(15) g;
__extension__ unsigned _BitInt(31) h;
int i = __extension__ 21wb;
long long j = __extension__ 60594869054uwb;
#if 0wb == 0
#endif
#if 0uwb == 0
#endif
