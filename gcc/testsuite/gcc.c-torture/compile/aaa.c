foo (a, b)
{
  int t;
  while (b < 0)
    {
      t = a;
      a = b;
      b = t;
    }
  return a + b;
}
