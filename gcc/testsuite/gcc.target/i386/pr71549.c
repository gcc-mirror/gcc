/* { dg-do compile } */
/* { dg-options "-O2 -g" } */

struct S1
{
  int f0;
  int f1;
  int f2;
  int:4;
} a, b;

void
fn1 (struct S1 p1)
{
  a = p1;
  int c = p1.f0;
}

int
main ()
{
  fn1 (b);
  return 0;
}
