/* { dg-options "-fno-common isa_rev>=6" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" "-Os" } { "" } } */
/* { dg-final { scan-assembler-not "\tmemcpy" } } */

/* Test that memcpy is inline for target hardware
   without swl, swr.  */

#include <string.h>

char c[40] __attribute__ ((aligned(8)));

void
f1 ()
{
  memcpy (c, "1234567890QWERTYUIOPASDFGHJKLZXCVBNM", 32);
}
