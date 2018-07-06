/* PR tree-optimization/83044 */
/* { dg-do compile } */
/* { dg-options "-Wall -std=gnu89 -O2" } */

struct A { int b[0]; };
struct B { struct A c[0]; };
void bar (int *);

void
foo (void)
{
  struct B d;
  bar (d.c->b);
}
