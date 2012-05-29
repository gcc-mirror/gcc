/* Check that "break 8" defaults according to CPU version.  */
/* { dg-do compile } */
/* { dg-skip-if "" { "*-*-*" } { "-march*" } { "" } } */
/* { dg-final { scan-assembler "break 8" { target { ! cris-*-elf } } } } */
/* { dg-final { scan-assembler-not "bsr" { target { ! cris-*-elf } } } } */
/* { dg-final { scan-assembler-not "jsr" { target { ! cris-*-elf } } } } */
/* { dg-final { scan-assembler-not "break\[ \t\]" { target cris-*-elf } } } */
/* { dg-final { scan-assembler "\[jb\]sr \[_\]\?abort" { target cris-*-elf } } } */

void do_trap (void)
{
  __builtin_trap ();
}
