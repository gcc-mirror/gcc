/* PR tree-optimization/64465 */
/* { dg-do compile } */
/* { dg-options "-O2 -fexceptions" } */

extern int foo (int *);
extern int bar (int, int);
static inline __attribute__ ((__always_inline__))
int baz (int o)
{
  if (__builtin_constant_p (o))
    return bar (o, 1);
  return bar (o, 0);
}

void
test (void)
{
  int s;
  foo (&s);
  baz (4);
  baz (s);
}
