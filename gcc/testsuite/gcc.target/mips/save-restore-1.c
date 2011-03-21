/* Check that we can use the save instruction to save varargs.  */
/* { dg-options "(-mips16) isa_rev>=1 -mabi=32 -O2" } */
/* { dg-skip-if "PR target/46610" { mips-sgi-irix6* } } */

#include <stdarg.h>

int bar (int, va_list ap);

MIPS16 int
foo (int n, ...)
{
  va_list ap;
  int i;

  va_start (ap, n);
  i = bar (n, ap);
  va_end (ap);
  return i + 1;
}
/* { dg-final { scan-assembler "\tsave\t\\\$4-\\\$7" } } */
/* { dg-final { scan-assembler "\trestore\t" } } */
