static int foo(int *a)
{
  int i, tot;
  for (i = tot = 0; i < 100; i++)
    tot += a[i];
  return tot;
}
