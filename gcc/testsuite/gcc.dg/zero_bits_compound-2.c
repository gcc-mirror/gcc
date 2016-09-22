/* Test whether an AND mask or'ed with the know zero bits that equals a mode
   mask is a candidate for zero extendion.  */

/* { dg-do compile { target i?86-*-* x86_64-*-* s390*-*-* aarch64*-*-* } } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O3 -dP" } */

unsigned long foo (unsigned char c)
{
  unsigned long l;
  unsigned int i;

  i = ((unsigned int)c) << 8;
  i |= ((unsigned int)c) << 20;
  asm volatile ("":::);
  i = i & 0x0fe0fe00;
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
  i = i & 0x07f007f0;
  asm volatile ("":::);
  l = (unsigned long)i;

  return l;
}

/* Check that an AND expression was used.  */
/* { dg-final { scan-assembler-times "\\(and:" 2 } } */
