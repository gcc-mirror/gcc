/* { dg-options "-fno-common isa_rev<=5 -mabi=32 (REQUIRES_STDLIB)" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" "-Os"} { "" } } */
/* { dg-final { scan-assembler-not "\tmemcpy" } } */
/* { dg-final { scan-assembler-times "swl" 8 } } */
/* { dg-final { scan-assembler-times "swr" 8 } } */

/* Test that inline memcpy for hardware with swl, swr handles subword
   alignment and produces enough swl/swrs for mips32.  */

#include <string.h>

char c[40] __attribute__ ((aligned(2)));

void
f1 ()
{
  memcpy (c, "1234567890QWERTYUIOPASDFGHJKLZXCVBNM", 32);
}
