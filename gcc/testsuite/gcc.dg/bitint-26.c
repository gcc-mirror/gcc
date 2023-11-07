// PR c/102989
// { dg-do compile { target bitint } }
// { dg-options "-std=c11 -pedantic" }

_BitInt(63) a;					/* { dg-warning "ISO C does not support '_BitInt\\\(63\\\)' before C23" } */
signed _BitInt(15) b;				/* { dg-warning "ISO C does not support 'signed _BitInt\\\(15\\\)' before C23" } */
unsigned _BitInt(31) c;				/* { dg-warning "ISO C does not support 'unsigned _BitInt\\\(31\\\)' before C23" } */
int d = 21wb;					/* { dg-warning "ISO C does not support literal 'wb' suffixes before C23" } */
long long e = 60594869054uwb;			/* { dg-warning "ISO C does not support literal 'wb' suffixes before C23" } */
__extension__ _BitInt(63) f;
__extension__ _BitInt(15) g;
__extension__ unsigned _BitInt(31) h;
int i = __extension__ 21wb;
long long j = __extension__ 60594869054uwb;
#if 0wb == 0					/* { dg-warning "ISO C does not support literal 'wb' suffixes before C23" } */
#endif
#if 0uwb == 0					/* { dg-warning "ISO C does not support literal 'wb' suffixes before C23" } */
#endif
