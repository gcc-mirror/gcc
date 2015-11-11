/* We use a global variable 'k' to avoid ipa-cp. */
int k = 50;
static int __attribute__((noinline))
foo ()
{
  int i, res = 0;
  for (i = k/2; i < k; i++)
    res += i;

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
