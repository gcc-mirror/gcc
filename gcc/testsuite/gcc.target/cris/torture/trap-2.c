/* As trap-1.c but forcing on.  */
/* { dg-do compile } */
/* { dg-options "-mtrap-using-break8" } */
/* { dg-final { scan-assembler "break 8" } } */
/* { dg-final { scan-assembler-not "bsr" } } */
/* { dg-final { scan-assembler-not "jsr" } } */

void do_trap (void)
{
  __builtin_trap ();
}
