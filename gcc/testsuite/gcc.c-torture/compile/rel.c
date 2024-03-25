int
foo (int *c, int b)
{
  int a;

  a = *c + b;
  c[1] = a;
  return b;
}
