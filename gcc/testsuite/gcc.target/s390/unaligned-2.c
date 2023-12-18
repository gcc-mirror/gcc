/* weak symbols might get overridden in another module by symbols
   which are not aligned on a 2-byte boundary.  Although this violates
   the zABI we try to handle this gracefully by not using larl on
   these symbols if -munaligned-symbols has been specified.  */

/* { dg-do compile } */
/* { dg-options "-O3 -march=z900 -fno-section-anchors -munaligned-symbols" } */
unsigned char __attribute__((weak)) weaksym = 0;

unsigned char
foo ()
{
  return weaksym;
}

/* { dg-final { scan-assembler-times "larl\t%r\[0-9\]*,weaksym\n" 0 } } */
