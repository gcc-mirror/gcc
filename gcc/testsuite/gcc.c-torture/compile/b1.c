foo (long long x)
{
  if (x--)
    return 255;
  return 0;
}

main ()
{
  printf ("%d\n", foo (0));
}
