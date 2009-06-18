/* { dg-do compile } */
/* { dg-options "-mfp16-format=ieee" } */

/* Encoding taken from:  http://en.wikipedia.org/wiki/Half_precision */
/* 0x7bff = 31743 */
__fp16 xx = 65504.0;

/* { dg-final { scan-assembler "\t.size\txx, 2" } } */
/* { dg-final { scan-assembler "\t.short\t31743" } } */
