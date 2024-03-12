/* With the -munaligned-symbols option all external symbols without
   explicite alignment are assumed to be potentially unaligned and
   therefore cannot be accessed with larl.  */

/* { dg-do compile } */
/* { dg-options "-O3 -march=z900 -fno-section-anchors -munaligned-symbols" } */

extern unsigned char extern_unaligned;
extern unsigned char extern_explicitly_aligned __attribute__((aligned(2)));
unsigned char aligned;

unsigned char
foo ()
{
  return extern_unaligned + extern_explicitly_aligned + aligned;
}

/* { dg-final { scan-assembler-times "larl\t%r\[0-9\]*,extern_unaligned\n" 0 } } */
/* { dg-final { scan-assembler-times "larl\t%r\[0-9\]*,extern_explicitly_aligned\n" 1 } } */
/* { dg-final { scan-assembler-times "larl\t%r\[0-9\]*,aligned\n" 1 } } */
