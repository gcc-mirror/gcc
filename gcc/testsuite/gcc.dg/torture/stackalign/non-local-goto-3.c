/* { dg-do run } */
/* { dg-require-effective-target nonlocal_goto } */
/* { dg-require-effective-target trampolines } */

extern void abort (void);

int x(int a, int b)
{
  __label__ xlab;

  void y(int b)
    {
       switch (b)
        {
          case 1: goto xlab;
          case 2: goto xlab;
        }
    }

  a = a + 2;
  y (b);

 xlab:
  return a;
}

int main ()
{
  int i, j;

  for (j = 1; j <= 2; ++j)
    for (i = 1; i <= 2; ++i)
      {
	int a = x (j, i);
	if (a != 2 + j)
	  abort ();
      }

  return 0;
}
