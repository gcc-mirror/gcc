/* { dg-do compile } */

int a, b, c, d;

int
fn1 (int p1)
{
  return a > 0 ? p1 : p1 >> a;
}

void
fn2 ()
{
  char e;
  for (; c; c++)
    {
      e = fn1 (!d ^ 2);
      b ^= e;
    }
}
