int
foo (long long x)
{
  if (x--)
    return 255;
  return 0;
}

int
main (void)
{
  __builtin_printf ("%d\n", foo (0));
}
