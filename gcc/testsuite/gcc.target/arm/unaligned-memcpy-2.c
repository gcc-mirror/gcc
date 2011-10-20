/* { dg-do compile } */
/* { dg-require-effective-target arm_unaligned } */
/* { dg-options "-O2" } */

#include <string.h>

char dest[16];

void aligned_dest (char *src)
{
  memcpy (dest, src, 15);
}

/* Expect a multi-word store for the main part of the copy, but subword
   loads/stores for the remainder.  */

/* { dg-final { scan-assembler-times "stmia" 1 } } */
/* { dg-final { scan-assembler-times "ldrh" 1 } } */
/* { dg-final { scan-assembler-times "strh" 1 } } */
/* { dg-final { scan-assembler-times "ldrb" 1 } } */
/* { dg-final { scan-assembler-times "strb" 1 } } */
