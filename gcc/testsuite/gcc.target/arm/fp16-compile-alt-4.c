/* { dg-do compile } */
/* { dg-require-effective-target arm_fp16_alternative_ok } */
/* { dg-add-options arm_fp16_alternative } */

/* Encoding taken from:  http://en.wikipedia.org/wiki/Half_precision */
/* 0x7bff = 31743 */
__fp16 xx = 65504.0;

/* { dg-final { scan-assembler "\t.size\txx, 2" } } */
/* { dg-final { scan-assembler "\t.short\t31743" } } */
