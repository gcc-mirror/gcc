/* { dg-do compile } */
/* { dg-options "-O -mcpu=8540 -mspe -mabi=spe -mfloat-gprs=single" } */
/* { dg-skip-if "not an SPE target" { ! powerpc_spe_nocache } { "*" } { "" } } */
/* { dg-final { scan-assembler "evstdd" } } */

void foo(void)
{
  int x[8] __attribute__((aligned(64))) = { 1, 1, 1, 1, 1, 1, 1, 1 };
  bar (x);
}
