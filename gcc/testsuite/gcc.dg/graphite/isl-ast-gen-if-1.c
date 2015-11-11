int st = 1;
static void __attribute__((noinline))
foo (int a[], int n)
{
  int i;
  for (i = 0; i < n; i++)
    {
      if (i < 25)
        a[i] = i;
      a[n - i] = 1;
    }
}

static int __attribute__((noinline))
array_sum (int a[])
{
  int i, res = 0;
  for(i = 0; i < 50; i += st)
    res += a[i];
  return res;
}

extern void abort ();

int
main (void)
{
  int a[51]; /* NB This size allows foo's first iteration to write to a[50].  */
  foo (a, 50);
  int res = array_sum (a);
  if (res != 49)
    abort ();
  return 0;
}
