/* { dg-do compile } */
/* { dg-require-effective-target arm_fp16_alternative_ok } */
/* { dg-options "-mfp16-format=alternative -pedantic" } */

/* This number overflows the range of the alternative encoding.  Since this
   encoding doesn't have infinities, we should get a pedantic warning,
   and the value should be set to the largest representable value.  */
/* 0x7fff = 32767 */
__fp16 xx = 123456789.0;  /* { dg-warning "overflow" } */

/* { dg-final { scan-assembler "\t.size\txx, 2" } } */
/* { dg-final { scan-assembler "\t.short\t32767" } } */
