foo (a)
{
  return __builtin_abs (a);
}

main ()
{
  printf ("%d %d\n", foo (0x80000000), foo (12));
}
