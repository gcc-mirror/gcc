/* Check that eliminable compare-instructions are eliminated. */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "\tcmp|\ttest|\tor" } } */

#ifndef t
#define t long long
#endif
#ifndef t2
#define t2 t
#endif
#ifndef op
#define op -
#endif

extern void foo(t2);

void g(t a, t b)
{
  t2 c = a op b;

  if (c >= 0)
    foo(c);
}
