/* { dg-do compile } */
/* { dg-options "-mfp16-format=alternative" } */

/* Encoding taken from:  http://en.wikipedia.org/wiki/Half_precision */
/* 0xc000 = 49152 */
__fp16 xx = -2.0;

/* { dg-final { scan-assembler "\t.size\txx, 2" } } */
/* { dg-final { scan-assembler "\t.short\t49152" } } */
