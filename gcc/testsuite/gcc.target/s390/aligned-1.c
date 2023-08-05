/* Even symbols without explicite alignment are assumed to reside on a
   2 byte boundary, as mandated by the IBM Z ELF ABI, and therefore
   can be accessed using the larl instruction.  */

/* { dg-do compile } */
/* { dg-options "-O3 -march=z900 -fno-section-anchors" } */

extern unsigned char extern_implicitly_aligned;
extern unsigned char extern_explicitly_aligned __attribute__((aligned(2)));
unsigned char aligned;

unsigned char
foo ()
{
  return extern_implicitly_aligned + extern_explicitly_aligned + aligned;
}

/* { dg-final { scan-assembler-times "larl\t%r\[0-9\]*,extern_implicitly_aligned\n" 1 } } */
/* { dg-final { scan-assembler-times "larl\t%r\[0-9\]*,extern_explicitly_aligned\n" 1 } } */
/* { dg-final { scan-assembler-times "larl\t%r\[0-9\]*,aligned\n" 1 } } */
