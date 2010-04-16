/* { dg-do compile } */
/* { dg-options "-mthumb -Os" }  */
/* { dg-require-effective-target arm_thumb1_ok } */

int ldrb(unsigned char* p)
{
  if (p[8] <= 0x7F)
    return 2;
  else
    return 5;
}


/* { dg-final { scan-assembler "127" } } */
/* { dg-final { scan-assembler "bhi" } } */
