/* PR tree-optimization/84982 */
/* { dg-do run } */
/* { dg-options "-O2" } */

#ifndef __cplusplus
#define bool _Bool
#define true 1
#define false 0
#endif

struct S { bool a, b, c, d; };

__attribute__((noipa)) void
bar (bool *x)
{
  if (x[0] || !x[1] || !x[2] || x[3])
    __builtin_abort ();
}

__attribute__((noipa)) void
foo (struct S *x)
{
  bool a[4];
  a[0] = !x->a;
  a[1] = !x->b;
  a[2] = x->c;
  a[3] = !x->d;
  bar (a);
} 

int
main ()
{
  struct S s;
  s.a = true; s.b = false; s.c = true; s.d = true;
  foo (&s);
  return 0;
}
