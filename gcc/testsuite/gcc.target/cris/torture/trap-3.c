/* Like trap-1.c and trap-2.c but force calls to abort.  */
/* { dg-do compile } */
/* { dg-options "-mno-trap-using-break8" } */
/* { dg-final { scan-assembler-not "break\[ \t\]" } } */
/* { dg-final { scan-assembler "\[jb\]sr \[_\]\?abort" } } */

void do_trap (void)
{
  __builtin_trap ();
}
