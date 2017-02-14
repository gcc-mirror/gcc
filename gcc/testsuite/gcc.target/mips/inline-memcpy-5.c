/* { dg-options "-fno-common isa_rev<=5 -mabi=n32 (REQUIRES_STDLIB)" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" "-Os"} { "" } } */
/* { dg-final { scan-assembler-not "\tmemcpy" } } */
/* { dg-final { scan-assembler-times "sdl" 4 } } */
/* { dg-final { scan-assembler-times "sdr" 4 } } */

/* Test that inline memcpy for hardware with sdl, sdr handles subword
   alignment and produces enough sdr/sdls on n32.  */

#include <string.h>

char c[40] __attribute__ ((aligned(2)));

void
f1 ()
{
  memcpy (c, "1234567890QWERTYUIOPASDFGHJKLZXCVBNM", 32);
}
