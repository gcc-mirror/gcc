foo (a)
     unsigned a;
{
  unsigned b = 0;

  if ((a & 12345678) > b)
    return 1;
  return 0;
}
