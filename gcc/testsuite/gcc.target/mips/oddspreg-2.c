/* Check that we disable odd-numbered single precision registers.  */
/* { dg-skip-if "needs asm output" { *-*-* } { "-fno-fat-lto-objects" } { "" } } */
/* { dg-options "-mabi=32 -mno-odd-spreg -mhard-float" } */

void
foo ()
{
  register float foo asm ("$f1"); /* { dg-error "isn't suitable for" } */
  asm volatile ("" : "=f" (foo));
}
