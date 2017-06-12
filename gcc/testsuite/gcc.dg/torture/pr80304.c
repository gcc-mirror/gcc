/* { dg-do run } */

int __attribute__((pure,noinline,noclone)) foo (int *p)
{
  return *p * 2;
}

int main()
{
  int k = 0;
  int i;
#pragma GCC ivdep
  for (k = 0; k < 9;)
    {
      i = 0;
      while (1)
	{
	  k += foo (&i);
	  if (k > 7)
	    break;
	  i++;
	}
    }
  if (k != 12)
    __builtin_abort ();
  return 0;
}
