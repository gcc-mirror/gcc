/* { dg-options "-msimd -O2" } */

typedef short V __attribute__((vector_size(4)));
typedef unsigned short VI __attribute__((vector_size(4)));

V a, b, c;
const VI maskl = {0, 0};
const VI maskh = {1, 1};

void foo (void)
{
  b = __builtin_shuffle (a, maskl);
  c = __builtin_shuffle (a, maskh);
}

/* { dg-final { scan-assembler "vpack2hl" } } */
/* { dg-final { scan-assembler "vpack2hm" } } */
