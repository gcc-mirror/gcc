/* PR target/64513 */
/* { dg-do compile } */
/* { dg-options "-O2 -mstack-arg-probe" } */

struct A {};
struct B { struct A y; };
int foo (struct A);

int
bar (int x)
{
  struct B b;
  int c;
  while (x--)
    c = foo (b.y);
  return c;
}
