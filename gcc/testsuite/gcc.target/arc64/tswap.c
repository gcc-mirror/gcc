/* { dg-do compile } */
/* { dg-require-effective-target hs6x } */
/* { dg-options "-O2 -msimd -ftree-vectorize" } */

/* Enable this test when HS5x recognizes various vector permutations
   operations.  */

struct{
  unsigned short x1;
  unsigned short x2;
} vara, varb;

void foo (void)
{
  vara.x1 = varb.x2;
  vara.x2 = varb.x1;
}
/* { dg-final { scan-assembler "swap\\s+r\\d+,r\\d+" } } */
