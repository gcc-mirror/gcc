/* Check that eliminable compare-instructions are eliminated. */
/* { dg-do compile } */
/* { dg-skip-if "" { "*-*-*" } { "-march=v0" } { "" } } */
/* { dg-options "-O2" { target march_option } } */
/* { dg-options "-O2 -march=v10" { target { ! march_option } } } */
/* { dg-final { scan-assembler-not "\tcmp|\ttest" } } */

#ifndef t
#define t int
#endif
#ifndef t2
#define t2 t
#endif
#ifndef t3
#define t3 t
#endif
#ifndef op
#define op(xx) __builtin_clz(xx)
#endif

extern t3 x;

t2 f(t a, t2 *b, t2 *d)
{
  t2 c = op(a);

  *b = c;

  if (c != 0)
    *d = c;

  return c;
}
