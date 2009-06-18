/* { dg-do compile } */
/* { dg-options "-mfp16-format=ieee" } */

__fp16 xx = 0.0;

/* { dg-final { scan-assembler "\t.eabi_attribute 38, 1" } } */
/* { dg-final { scan-assembler "\t.size\txx, 2" } } */
/* { dg-final { scan-assembler "\t.space\t2" } } */
