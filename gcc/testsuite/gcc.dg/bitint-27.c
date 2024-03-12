// PR c/102989
// { dg-do compile { target bitint } }
// { dg-options "-std=c11 -pedantic-errors" }

_BitInt(63) a;					/* { dg-error "ISO C does not support '_BitInt\\\(63\\\)' before C2X" } */
signed _BitInt(15) b;				/* { dg-error "ISO C does not support 'signed _BitInt\\\(15\\\)' before C2X" } */
unsigned _BitInt(31) c;				/* { dg-error "ISO C does not support 'unsigned _BitInt\\\(31\\\)' before C2X" } */
int d = 21wb;					/* { dg-error "ISO C does not support literal 'wb' suffixes before C2X" } */
long long e = 60594869054uwb;			/* { dg-error "ISO C does not support literal 'wb' suffixes before C2X" } */
__extension__ _BitInt(63) f;
__extension__ _BitInt(15) g;
__extension__ unsigned _BitInt(31) h;
int i = __extension__ 21wb;
long long j = __extension__ 60594869054uwb;
#if 0wb == 0					/* { dg-error "ISO C does not support literal 'wb' suffixes before C2X" } */
#endif
#if 0uwb == 0					/* { dg-error "ISO C does not support literal 'wb' suffixes before C2X" } */
#endif
