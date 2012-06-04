/* As trap-1.c but with CPU version specified, including.  */
/* { dg-do compile } */
/* { dg-skip-if "" { "*-*-*" } { "-march=*" } { "" } } */
/* { dg-options "-march=v3" } */
/* { dg-final { scan-assembler "break 8" } } */
/* { dg-final { scan-assembler-not "bsr" } } */
/* { dg-final { scan-assembler-not "jsr" } } */

void do_trap (void)
{
  __builtin_trap ();
}
