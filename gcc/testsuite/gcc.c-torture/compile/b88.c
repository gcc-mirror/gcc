int
foo (double d)
{
  d = -d;
  if (d < 0.0)
    return 1;
  return 2;
}

int
main (void)
{
  foo (0.0);
}
