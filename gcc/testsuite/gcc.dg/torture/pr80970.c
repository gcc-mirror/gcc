/* { dg-do compile } */

int a, b, c, d, e;
void f ()
{
  long g, h;
  if (c)
    e = d;
  g = d & 31;
  h = 1 << g;
  a = e | h;
  b = a;
}
