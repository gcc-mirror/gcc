/* PR tree-optimization/78482 */
/* { dg-do run } */

short a = 65531;
int b = 3, f;
signed char c, d;
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

__attribute__((noinline, noclone))
int bar (const char *x, int y)
{
  asm volatile ("" : "+g" (x), "+g" (y) : : "memory");
  if (y == 2)
    __builtin_abort ();
  return 0;
}

int main()
{
  for (; c >= 0; c--)
    {
      if (!b)
	{
	  bar("%d\n", 2);
	  continue;
	}
      fn1(a);
    }
  return 0;
}
