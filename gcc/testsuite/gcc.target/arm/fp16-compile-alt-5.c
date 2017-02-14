/* { dg-do compile } */
/* { dg-require-effective-target arm_fp16_alternative_ok } */
/* { dg-options "-mfp16-format=alternative" } */

/* Encoding taken from:  http://en.wikipedia.org/wiki/Half_precision */
/* 0x3555 = 13653 */
__fp16 xx = (1.0/3.0);

/* { dg-final { scan-assembler "\t.size\txx, 2" } } */
/* { dg-final { scan-assembler "\t.short\t13653" } } */
