static int __attribute__((noinline))
foo (int k, int n1, int n2, int n3)
{
  int j, res = 0;
  for (j = 0; j < k; j++)
    {
      int i;
      for (i = 0; i < n1; i++)
        res += i;
      for (i = 0; i < n2; i++)
        res += i;
      for (i = 0; i < n3; i++)
        res += i;
    }

  return res;
}

extern void abort ();

int
main (void)
{ 
  int res = foo (4, 50, 50, 50);
  if (res != 14700)
    abort ();

  return 0;
}
