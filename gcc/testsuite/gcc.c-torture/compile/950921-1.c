f ()
{
  union
    {
      signed char c;
      double d;
    } u;

  u.c = 1;
  u.c = 1;
  return u.c;
}
