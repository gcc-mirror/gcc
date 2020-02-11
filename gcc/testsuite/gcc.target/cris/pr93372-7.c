/* Check that eliminable compare-instructions are eliminated. */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "\tcmp|\ttest" } } */

#ifndef t
#define t int
#endif

t feq(t *a, t *b)
{
  t c = *a;
  *b = c == 0;
  return c;
}

t fne(t *a, t *b)
{
  t c = *a;
  *b = c != 0;
  return c;
}

t fgt(t *a, t *b)
{
  t c = *a;
  *b = c > 0;
  return c;
}

unsigned t fgtu(unsigned t *a, unsigned t *b)
{
  unsigned t c = *a;
  *b = c > 0;
  return c;
}

t flt(t *a, t *b)
{
  t c = *a;
  *b = c < 0;
  return c;
}

#if 0
/* Always false... */
unsigned t fltu(unsigned t *a, unsigned t *b)
{
  unsigned t c = *a;
  *b = c < 0;
  return c;
}
#endif

t fge(t *a, t *b)
{
  t c = *a;
  *b = c >= 0;
  return c;
}

#if 0
/* Always true... */
unsigned t fgeu(unsigned t *a, unsigned t *b)
{
  unsigned t c = *a;
  *b = c > 0;
  return c;
}
#endif

t fle(t *a, t *b)
{
  t c = *a;
  *b = c <= 0;
  return c;
}

/* Same as eq... */
unsigned t fleu(unsigned t *a, unsigned t *b)
{
  unsigned t c = *a;
  *b = c <= 0;
  return c;
}
