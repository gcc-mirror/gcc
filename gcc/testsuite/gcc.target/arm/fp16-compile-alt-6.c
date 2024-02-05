/* { dg-do compile } */
/* { dg-require-effective-target arm_fp16_alternative_ok } */
/* { dg-add-options arm_fp16_alternative } */

/* This number is the maximum value representable in the alternative
   encoding.  */
/* 0x7fff = 32767 */
__fp16 xx = 131008.0;

/* { dg-final { scan-assembler "\t.size\txx, 2" } } */
/* { dg-final { scan-assembler "\t.short\t32767" } } */
