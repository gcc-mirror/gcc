/* PR middle-end/57393 */
/* { dg-do compile } */
/* { dg-additional-options "-g -ffast-math" } */

extern void bar (double);

struct S { int n; };
  
void
foo (struct S s, double a, int i, int j, int k)
{
  struct S t;
  bar (s.n * a * i * j);
  t.n = s.n * a * i * k;
}
