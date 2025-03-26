/* { dg-do compile  } */
/* { dg-options "-O2 -Wno-shift-count-overflow" } */
/* { dg-additional-options "-mno-sse -mno-mmx" { target ia32 } } */
/* { dg-final { scan-assembler-times {(?n)shrd[ql]?[\t ]*\$2} 4 } } */
/* { dg-final { scan-assembler-times {(?n)shld[ql]?[\t ]*\$2} 2 } } */

typedef unsigned long long u64;
typedef unsigned int   u32;
typedef unsigned short u16;

long long  a, b;
int   c, d;
short e, f;
const int n = 2;

void test64r () { b = ((u64)b >> n) | (a << (64 - n)); }
void test32r () { d = ((u32)d >> n) | (c << (32 - n)); }

unsigned long long ua, ub;
unsigned int   uc, ud;
unsigned short ue, uf;

void testu64l () { ub = (ub << n) | (ua >> (64 - n)); }
void testu64r () { ub = (ub >> n) | (ua << (64 - n)); }
void testu32l () { ud = (ud << n) | (uc >> (32 - n)); }
void testu32r () { ud = (ud >> n) | (uc << (32 - n)); }
