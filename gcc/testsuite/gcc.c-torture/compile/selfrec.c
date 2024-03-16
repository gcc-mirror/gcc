int
foo (int a)
{
  return foo (a - 1) * a;
}
