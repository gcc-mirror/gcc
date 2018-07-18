/* Check that record-form instructions are used.  */

/* { dg-do compile } */
/* { dg-options "-O2" } */

/* { dg-final { scan-assembler-times {\mrotl[wd]\.} 2 } } */
/* { dg-final { scan-assembler-times {\msl[wd]\.} 2 } } */
/* { dg-final { scan-assembler-times {\msr[wd]\.} 2 } } */
/* { dg-final { scan-assembler-times {\msra[wd]\.} 2 } } */

/* { dg-final { scan-assembler-times {\mrotl[wd]i\.} 2 } } */
/* { dg-final { scan-assembler-times {\msl[wd]i\.} 2 } } */
/* { dg-final { scan-assembler-times {\msr[wd]i\.} 2 } } */
/* Combine converts the arith shift right compares to a (more expensive)
   direct compare.  Luckily not the other shifts.  XFAIL for now.  */
/* { dg-final { scan-assembler-times {\msra[wd]i\.} 2 { xfail *-*-* } } } */

/* There should not be any extends of the shift amount (or anything else).  */
/* { dg-final { scan-assembler-not {\mextsw\M} } } */
/* { dg-final { scan-assembler-not {\mrldicl\M} } } */
/* { dg-final { scan-assembler-not {\mclrldi\M} } } */
/* { dg-final { scan-assembler-not {\mrlwinm\M} } } */


typedef unsigned long u;
typedef long s;
#define M(n) (8 * sizeof(long) - (n))
#define T1 if ((s)x > 0) g();
#define T2 if ((s)x > 0) g(); return x;

void g(void);

void rot1(u x, u n) { x = (x << M(n)) | (x >> n); T1 }
   u rot2(u x, u n) { x = (x << M(n)) | (x >> n); T2 }
void shl1(u x, u n) { x <<= n; T1 }
   u shl2(u x, u n) { x <<= n; T2 }
void shr1(u x, u n) { x >>= n; T1 }
   u shr2(u x, u n) { x >>= n; T2 }
void asr1(s x, u n) { x >>= n; T1 }
   s asr2(s x, u n) { x >>= n; T2 }

void rot1i(u x) { x = (x << M(23)) | (x >> 23); T1 }
   u rot2i(u x) { x = (x << M(23)) | (x >> 23); T2 }
void shl1i(u x) { x <<= 23; T1 }
   u shl2i(u x) { x <<= 23; T2 }
void shr1i(u x) { x >>= 23; T1 }
   u shr2i(u x) { x >>= 23; T2 }
void asr1i(s x) { x >>= 23; T1 }
   s asr2i(s x) { x >>= 23; T2 }
