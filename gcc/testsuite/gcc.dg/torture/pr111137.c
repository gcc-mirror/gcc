/* { dg-do run } */

int b[3][8];
short d;
volatile int t = 1;

void __attribute__((noipa))
foo()
{
  int  g = t;
  for (int e = 1; e >= 0; e--)
    {
      d = 1;
      for (; d >= 0; d--)
	{
	  b[0][d * 2 + 1] = 0;
	  b[g - 1 + d][0] ^= 1;
	  b[0][d * 2 + 2] = 0;
	  b[g - 1 + d][1] ^= 1;
	}
    }
}

int
main()
{
  foo ();
  if (b[0][1] != 1)
    __builtin_abort();
}
