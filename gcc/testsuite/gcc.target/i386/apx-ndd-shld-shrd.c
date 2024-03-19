/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -Wno-shift-count-overflow -m64 -mapxf" } */
/* { dg-final { scan-assembler-times {(?n)shld[ql]?[\t ]*\$2} 4 } } */
/* { dg-final { scan-assembler-times {(?n)shrd[ql]?[\t ]*\$2} 4 } } */

typedef unsigned long  u64;
typedef unsigned int   u32;

long  a;
int   c;
const char n = 2;

long test64r (long e) { long t = ((u64)a >> n) | (e << (64 - n)); return t;}
long test64l (u64 e) { long t = (a << n) | (e >> (64 - n)); return t;}
int test32r (int f) { int t = ((u32)c >> n) | (f << (32 - n)); return t; }
int test32l (u32 f) { int t = (c << n) | (f >> (32 - n)); return t; }

u64 ua;
u32 uc;

u64 testu64l (u64 ue) { u64 ut = (ua << n) | (ue >> (64 - n)); return ut; }
u64 testu64r (u64 ue) { u64 ut = (ua >> n) | (ue << (64 - n)); return ut; }
u32 testu32l (u32 uf) { u32 ut = (uc << n) | (uf >> (32 - n)); return ut; }
u32 testu32r (u32 uf) { u32 ut = (uc >> n) | (uf << (32 - n)); return ut; }
