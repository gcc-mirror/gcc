/* { dg-do compile } */

int a, b, c, d, e, f;

void fn1 ()
{ 
  int g = b;
  a = -~(d || a) << 4 || e;
  b = c || g ^ a;
  if (f < g)
    d = e;
}
