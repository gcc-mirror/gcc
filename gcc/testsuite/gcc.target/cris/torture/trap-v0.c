/* As trap-1.c but with CPU version specified, excluding.  */
/* { dg-do compile } */
/* { dg-skip-if "" { "*-*-*" } { "-march=*" } { "" } } */
/* { dg-options "-march=v0" } */
/* { dg-final { scan-assembler-not "break\[ \t\]" } } */
/* { dg-final { scan-assembler "\[jb\]sr \[_\]\?abort" } } */

void do_trap (void)
{
  __builtin_trap ();
}
