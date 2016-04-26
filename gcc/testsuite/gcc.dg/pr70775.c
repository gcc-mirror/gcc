/* { dg-do compile } */
/* { dg-options "-O3" } */

struct S
{
  int f1;
  int f2;
} a;

int b, c, d, e;
short f;

int
fn1 (int p1, unsigned p2)
{
  return p1 + p2;
}

void
fn2 ()
{
  struct S g;
  int h;
  for (; c; c++)
    for (f = -3; f < 3; f = fn1 (f, 8))
      {
        a.f1 = e;
        if (b)
          a = g;
        else
          for (; h; h++)
            d = b;
      }
}
