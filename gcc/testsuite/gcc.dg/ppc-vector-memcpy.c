/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-options "-O -maltivec" } */
/* { dg-final { scan-assembler "lvx" } } */

void foo(void)
{
  int x[8] __attribute__((aligned(128))) = { 1 };
  bar (x);
}
