/* { dg-do compile } */
/* { dg-options "-mfp16-format=ieee" } */

/* Encoding taken from:  http://en.wikipedia.org/wiki/Half_precision */
/* 0x3555 = 13653 */
__fp16 xx = (1.0/3.0);

/* { dg-final { scan-assembler "\t.size\txx, 2" } } */
/* { dg-final { scan-assembler "\t.short\t13653" } } */
