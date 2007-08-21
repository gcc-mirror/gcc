/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-require-effective-target powerpc_spe } */
/* { dg-options "-O -mspe=yes" } */
/* { dg-final { scan-assembler "evstdd" } } */

void foo(void)
{
  int x[8] __attribute__((aligned(64))) = { 1, 1, 1, 1, 1, 1, 1, 1 };
  bar (x);
}
