/* PR target/91102 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

int
foo (long d, long l)
{
  register long e asm ("x1") = d;
  register long f asm("x2") = l;
  asm ("" : : "r" (e), "r" (f));
  return 3;
}

struct T { int i; int j; };
union S { long h; struct T t; };

void
bar (union S b)
{
  while (1)
    {
      union S c = b;
      c.t.j++;
      b.h = foo (b.h, c.h);
    }
}
