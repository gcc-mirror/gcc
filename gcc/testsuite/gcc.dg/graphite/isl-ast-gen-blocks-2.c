int k = 50;
static int __attribute__((noinline))
foo ()
{
  int i, res;
  for (i = 0, res = 0; i < k; i++)
    res += i;

  return res;
}

extern void abort ();

int
main (void)
{ 
  int res = foo ();


  if (res != 1225)
    abort ();

  return 0;
}
