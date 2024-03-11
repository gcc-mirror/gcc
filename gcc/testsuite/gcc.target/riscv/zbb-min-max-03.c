/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zba_zbb -mabi=lp64d" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

int f(int x) {
 return x >= 0 ? x : 0;
}

unsigned f2(unsigned x, unsigned y) {
  return x > y ? x : y;
}

unsigned f3(unsigned x, unsigned y) {
  return x < y ? x : y;
}

/* { dg-final { scan-assembler-times "max\t" 1 } } */
/* { dg-final { scan-assembler-not "li\t" } } */
/* { dg-final { scan-assembler-times "maxu\t" 1 } } */
/* { dg-final { scan-assembler-times "minu\t" 1 } } */
/* { dg-final { scan-assembler-not {\mzext\.w\M} } } */
/* { dg-final { scan-assembler-not {\msext\.w\M} } } */

