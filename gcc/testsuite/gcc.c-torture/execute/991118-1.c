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
  tmp.field ^= 0x0008765412345678LL;
  return tmp;
}

struct tmp2
sub2 (struct tmp2 tmp2)
{
  tmp2.field ^= 0x0008765412345678LL;
  return tmp2;
}

struct tmp tmp = {0x123, 0x123456789ABCDLL};
struct tmp2 tmp2 = {0x123456789ABCDLL, 0x123};

main()
{

  if (sizeof (long long) != 8)
    exit (0);

  tmp = sub (tmp);
  tmp2 = sub2 (tmp2);

  if (tmp.pad != 0x123 || tmp.field != 0xFFF9551175BDFDB5LL)
    abort ();
  if (tmp2.pad != 0x123 || tmp2.field != 0xFFF9551175BDFDB5LL)
    abort ();
  exit (0);
}

