/* { dg-do compile } */

int a;
_Bool *b;
void f()
{
  int t = a;
  for (int e = 0; e < 2048; e++)
    {
      if (!b[e])
	t = 0;
    }
  a = t;
}
