/* PR tree-optimization/41661 */
/* { dg-do compile } */
/* { dg-options "-fno-early-inlining" } */

int g;

void foo (int x)
{
  g = x;
}

void bar (double d)
{
  foo (d == 1);
}

void baz (int a)
{
  bar (1);
}
