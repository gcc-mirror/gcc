/* { dg-do compile } */
/* { dg-require-effective-target arm_fp16_alternative_ok } */
/* { dg-options "-mfp16-format=alternative" } */

/* Encoding taken from:  http://en.wikipedia.org/wiki/Half_precision */
/* 0xc000 = 49152 */
__fp16 xx = -2.0;

/* { dg-final { scan-assembler "\t.size\txx, 2" } } */
/* { dg-final { scan-assembler "\t.short\t-16384" } } */
