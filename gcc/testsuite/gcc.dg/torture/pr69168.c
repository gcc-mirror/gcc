/* { dg-do compile } */

long a, b, e;
short *c;
int *d;
void fn1()
{
  int i;
  for (; e; e--)
    {
      i = 2;
      for (; i; i--)
	a = b = *d++ / (1 << 9);
      b = b ? 8 : a;
      *c++ = *c++ = b;
    }
}
