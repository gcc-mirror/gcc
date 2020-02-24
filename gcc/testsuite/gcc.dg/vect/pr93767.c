int
main ()
{
  int a[10], b;
  for (b = 6; b >= 3; b--)
    {
      a[b] = 1;
      a[b + 2] = a[3];
    }
  if (a[5] != 1)
    __builtin_abort ();
  return 0;
}
