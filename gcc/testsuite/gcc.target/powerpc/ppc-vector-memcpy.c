/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-O -maltivec -mno-vsx" } */
/* { dg-final { scan-assembler "lvx" } } */

void foo(void)
{
  int x[8] __attribute__((aligned(128))) = { 1, 1, 1, 1, 1, 1, 1, 1 };
  bar (x);
}
