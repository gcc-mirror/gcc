struct tmp
{
  long long int pad : 12;
  long long int field : 52;
};

struct tmp2
{
  long long int field : 52;
  long long int pad : 12;
};

struct tmp
sub (struct tmp tmp)
{
  tmp.field |= 0x0008765412345678LL;
  return tmp;
}

struct tmp2
sub2 (struct tmp2 tmp2)
{
  tmp2.field |= 0x0008765412345678LL;
  return tmp2;
}

main()
{
  struct tmp tmp = {0x123, (long long)0xFFFFFF000FFF000F};
  struct tmp2 tmp2 = {(long long)0xFFFFFF000FFF000F, 0x123};

  tmp = sub (tmp);
  tmp2 = sub2 (tmp2);

  if (tmp.pad != 0x123 || tmp.field != (long long)0xFFFFFF541FFF567F)
    abort ();
  if (tmp2.pad != 0x123 || tmp2.field != (long long)0xFFFFFF541FFF567F)
    abort ();
  exit (0);
}
