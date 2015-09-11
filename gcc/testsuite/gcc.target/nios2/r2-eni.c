/* { dg-do compile } */
/* { dg-options "-O2 -march=r2" } */
/* { dg-final { scan-assembler "eni" } } */

void
foo (void)
{
  __builtin_eni (0);
  __builtin_eni (1);
}
