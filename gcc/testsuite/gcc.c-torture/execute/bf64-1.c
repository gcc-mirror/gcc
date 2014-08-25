/* { dg-xfail-if "ABI specifies bitfields cannot exceed 32 bits" { mcore-*-* } "*" "" } */
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
  struct tmp tmp = {0x123, 0xFFF000FFF000FLL};
  struct tmp2 tmp2 = {0xFFF000FFF000FLL, 0x123};

  tmp = sub (tmp);
  tmp2 = sub2 (tmp2);

  if (tmp.pad != 0x123 || tmp.field != 0xFFFFFF541FFF567FLL)
    abort ();
  if (tmp2.pad != 0x123 || tmp2.field != 0xFFFFFF541FFF567FLL)
    abort ();
  exit (0);
}
