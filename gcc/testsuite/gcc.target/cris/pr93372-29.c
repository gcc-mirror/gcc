/* Check that eliminable compare-instructions are eliminated. */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "\tcmp|\ttest" } } */

#ifndef t
#define t int
#endif
#ifndef t2
#define t2 t
#endif
#ifndef op
#define op(x) ~(x)
#endif

extern void foo(t2);

t2 f(t a, t2 *b, t2 *d)
{
  t2 c = op(a);

  *b = c;

  if (c != 0)
    *d = c;

  return c;
}

t2 g(t a, t2 *b, t2 *d)
{
  t2 c = op(a);

  *b = c;

  if (c <= 0)
    *d = c;

  return c;
}
