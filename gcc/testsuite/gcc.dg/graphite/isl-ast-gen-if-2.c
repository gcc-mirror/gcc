/* This test case tests reduction, where the pbbs are duplicated.  */

static int __attribute__((noinline))
foo ()
{
  int i, res = 0;

  for (i = 0; i < 50; i++)
    {
      if (i >= 25)
        res += i;
    }

  return res;
}

extern void abort ();

int
main (void)
{ 
  int res = foo ();

  if (res != 925)
    abort ();

  return 0;
}
