int n = 50;

void
foo (int a[])
{
  int i;
  for (i = 0; i < n; i++)
    a[i] = i;
}

int
array_sum (int a[])
{
  int i;
  int res = 0;
  for(i = 1; i < n; i *= 2)
    res += a[i];
  return res;
}

extern void abort ();

int
main (void)
{
  int a[50];
  foo (a);
  int res = array_sum (a);
  if (res != 63)
    abort ();
  return 0;
}
