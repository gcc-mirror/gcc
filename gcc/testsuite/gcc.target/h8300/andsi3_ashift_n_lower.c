/* { dg-do compile }  */
/* { dg-options "-mh -O2 -fomit-frame-pointer" }  */
/* { dg-final { scan-assembler-times "extu" 1 } }  */

unsigned long foo(unsigned long a)
  { return (a << 4) & 0xffff; }

