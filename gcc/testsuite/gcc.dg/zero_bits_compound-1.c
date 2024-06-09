/* Test whether an AND mask or'ed with the know zero bits that equals a mode
   mask is a candidate for zero extendion.  */

/* Note: This test requires that char, int and long have different sizes and the
   target has a way to do 32 -> 64 bit zero extension other than AND.  */

/* { dg-do compile { target i?86-*-* x86_64-*-* s390*-*-* } } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O3 -dP" } */

unsigned long foo (unsigned char c)
{
  unsigned long l;
  unsigned int i;

  i = ((unsigned int)c) << 8;
  i |= ((unsigned int)c) << 20;
  asm volatile ("":::);
  i = i & 0x0ff0ff00;
  asm volatile ("":::);
  l = (unsigned long)i;

  return l;
}

unsigned long bar (unsigned char c)
{
  unsigned long l;
  unsigned int i;

  i = ((unsigned int)c) << 8;
  i |= ((unsigned int)c) << 20;
  asm volatile ("":::);
  i = i & 0x0ffffff0;
  asm volatile ("":::);
  l = (unsigned long)i;

  return l;
}

/* Check that no pattern containing an AND expression was used.  */
/* { dg-final { scan-assembler-not "\\(and:" { target { ! { s390*-*-* } } } } } */
/* { dg-final { scan-assembler-not "\\tng?rk?\\t" { target { s390*-*-* } } } } */
