/* { dg-do run } */

int printf(const char*, ...);
short a = 65531;
int b = 3, f;
char c, d;
static void fn1(int p1)
{
  short e;
  b = f;
  if (f > p1 && p1)
    L:
	for (e = 0; 0;)
	  ;
  else if (d) b = 0 >= b;
  for (; e <= 3; e++)
    {
      if (b)
	continue;
      b = 3;
      goto L;
    }
}

int main()
{
  for (; c >= 0; c--)
    {
      if (!b)
	{
	  printf("%d\n", 2);
	  continue;
	}
      fn1(a);
    }
  return 0;
}
