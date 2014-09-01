foo (double d)
{
  d = -d;
  if (d < 0.0)
    return 1;
  return 2;
}

main ()
{
  foo (0.0);
}
