int inc(int n)
{
  return n + 1;
}


int bar(void)
{
  int n = 100;
  n = inc(n);
  n = inc(n) + 100;
  return n;
}
