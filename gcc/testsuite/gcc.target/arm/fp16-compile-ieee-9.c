/* { dg-do compile } */
/* { dg-options "-mfp16-format=ieee" } */

/* Encoding taken from:  http://en.wikipedia.org/wiki/Half_precision */
/* This is the minimum denormalized value.  */
/* 0x0001 = 1 */
__fp16 xx = 5.96046E-8;

/* { dg-final { scan-assembler "\t.size\txx, 2" } } */
/* { dg-final { scan-assembler "\t.short\t1" } } */
