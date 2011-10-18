/* { dg-do compile } */
/* { dg-require-effective-target arm_unaligned } */
/* { dg-options "-O2" } */

#include <string.h>

void unknown_alignment (char *dest, char *src)
{
  memcpy (dest, src, 15);
}

/* We should see three unaligned word loads and store pairs, one unaligned
   ldrh/strh pair, and an ldrb/strb pair.  Sanity check that.  */

/* { dg-final { scan-assembler-times "@ unaligned" 8 } } */
/* { dg-final { scan-assembler-times "ldrh" 1 } } */
/* { dg-final { scan-assembler-times "strh" 1 } } */
/* { dg-final { scan-assembler-times "ldrb" 1 } } */
/* { dg-final { scan-assembler-times "strb" 1 } } */
