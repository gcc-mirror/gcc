foo (a, b)
{
  int c = a & b;
  if ((a & b) == 0)
    return 0;
  return c;
}
