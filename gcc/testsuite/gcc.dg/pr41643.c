/* PR tree-optimization/41643 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-dce" } */

struct S { int a; };

int
f (struct S *x)
{
  int a = x->a;
  if (a)
    return f (x) + a;
  else
    return f (x);
}
