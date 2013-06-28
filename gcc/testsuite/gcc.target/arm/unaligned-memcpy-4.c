/* { dg-do compile } */
/* { dg-require-effective-target arm_unaligned } */
/* { dg-options "-O2" } */

#include <string.h>

char src[16] = { 0 };
char dest[16] = { 0 };

void aligned_both (void)
{
  memcpy (dest, src, 15);
}

/* We know both src and dest to be aligned: expect multiword loads/stores.  */

/* { dg-final { scan-assembler-times "ldmia" 1 { target { ! { arm_prefer_ldrd_strd } } } } } */
/* { dg-final { scan-assembler-times "stmia" 1 { target { ! { arm_prefer_ldrd_strd } } } } } */
/* { dg-final { scan-assembler "ldrd" { target { arm_prefer_ldrd_strd } } } } */
/* { dg-final { scan-assembler-times "ldm" 0 { target { arm_prefer_ldrd_strd } } } } */
/* { dg-final { scan-assembler "strd" { target { arm_prefer_ldrd_strd } } } } */
/* { dg-final { scan-assembler-times "stm" 0 { target { arm_prefer_ldrd_strd } } } } */
