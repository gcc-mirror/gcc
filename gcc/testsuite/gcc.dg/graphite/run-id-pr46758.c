int
movegt (int y, long long a)
{
  int i;
  int ret = 0;
  for (i = 0; i < y; i++)
    if (a == -1LL << 33)
      ret = -1;
  return ret;
}

int
main ()
{
  if (movegt (1, -1LL << 33) != -1)
    __builtin_abort ();
  return 0;
}
