/* Check that we can use the save instruction to save varargs.  */
/* { dg-mips-options "-mips32r2 -mgp32 -mips16 -O2" } */
#include <stdarg.h>

int bar (int, va_list ap);

int
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
