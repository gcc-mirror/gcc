/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -msse4.1 -mno-avx" } */

typedef long long v2di __attribute__ ((__vector_size__ (16)));

long long m,n;

v2di fx0(long long x) { return (v2di){ x, 0 }; }
v2di f0x(long long x) { return (v2di){ 0, x }; }
v2di fxx(long long x) { return (v2di){ x, x }; }
v2di fxy(long long x, long long y) { return (v2di){ x, y }; }

v2di fm0() { return (v2di){ m, 0 }; }
v2di f0m() { return (v2di){ 0, m }; }
v2di fmm() { return (v2di){ m, m }; }
v2di fmn() { return (v2di){ m, n }; }

/* { dg-final { scan-assembler-times "movd\[ \t\]" 2 } } */
/* { dg-final { scan-assembler-times "pinsrd" 2 } } */
/* { dg-final { scan-assembler-times "pslldq" 2 } } */
/* { dg-final { scan-assembler-times "movddup" 2 } } */
/* { dg-final { scan-assembler-times "movq" 4 } } */
/* { dg-final { scan-assembler-times "movhps" 2 } } */

