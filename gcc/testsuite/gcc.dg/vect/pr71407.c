/* { dg-do compile } */

int a, c, d;
short b;

void
fn1 ()
{
  int e;
  for (; c; c++)
    {
      for (; a; a++)
        b = (short) a || e;
      e = d;
    }
}
