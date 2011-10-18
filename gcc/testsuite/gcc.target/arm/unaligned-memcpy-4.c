/* { dg-do compile } */
/* { dg-require-effective-target arm_unaligned } */
/* { dg-options "-O2" } */

#include <string.h>

char src[16];
char dest[16];

void aligned_both (void)
{
  memcpy (dest, src, 15);
}

/* We know both src and dest to be aligned: expect multiword loads/stores.  */

/* { dg-final { scan-assembler-times "ldmia" 1 } } */
/* { dg-final { scan-assembler-times "stmia" 1 } } */
