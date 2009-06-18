/* { dg-do compile } */
/* { dg-options "-mfp16-format=alternative" } */

/* This number is the maximum value representable in the alternative
   encoding.  */
/* 0x7fff = 32767 */
__fp16 xx = 131008.0;

/* { dg-final { scan-assembler "\t.size\txx, 2" } } */
/* { dg-final { scan-assembler "\t.short\t32767" } } */
