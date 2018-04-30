/* PR c++/83553 */
/* { dg-do run } */

int a[3];

int
foo (int n)
{
  switch (n)
    {
    case 0:
      for (n = 7, a[0]++; 0; a[2] = a[1] + 1)
	{
    case 2:
	  a[1] = a[0] + 1;
	}
    }
  return n;
}

int
main ()
{
  if (foo (0) != 7 || a[0] != 1 || a[1] || a[2])
    __builtin_abort ();
  if (foo (2) != 2 || a[0] != 1 || a[1] != 2 || a[2] != 3)
    __builtin_abort ();
  return 0;
}
