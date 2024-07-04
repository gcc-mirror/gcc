int
foo (int *p)
{
  int a, b;

  a = 123456;
  a += p[0];
  b = p[0];
  if (a == 0)
    return b;
  return 1;
}
