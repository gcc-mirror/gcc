/* PR rtl-optimization/118739 */
/* { dg-do run } */
/* { dg-options "-O3 -fno-tree-forwprop -fno-tree-vrp" } */

volatile int a;
int b, c, d = 1, e, f, g;

int h (void)
{
  int i = 1;

 j:
  for (b = 1; b; b--)
    {
      asm ("#");

      g = 0;

      for (; g <= 1; g++)
	{
	  int k = f = 0;

	  for (; f <= 1; f++)
	    k = (1 == i) >= k || ((d = 0) >= a) + k;
	}
    }

  for (; i < 3; i++)
    {
      if (!c)
	return g;

      if (e)
	goto j;

      asm ("#");
    }

  return 0;
}

int main()
{
  h();

  if (d != 1)
    __builtin_abort();

  return 0;
}
