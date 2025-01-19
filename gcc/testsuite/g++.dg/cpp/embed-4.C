// { dg-do run { target c++11 } }
// { dg-options "" }

constexpr unsigned char a[] = {
#embed __FILE__
};

constexpr unsigned char
foo (int x)
{
  return a[x];
}
constexpr unsigned char b = a[32];
constexpr unsigned char c = foo (42);

#if __cplusplus >= 201402L
constexpr bool
bar ()
{
  unsigned char d[] = {
  #embed __FILE__
  };
  d[42] = ' ';
  d[32] = 'X';
  d[0] = d[1] + 16;
  d[sizeof (d) - 1] = d[42] - ' ';
  for (int i = 0; i < sizeof (d); ++i)
    switch (i)
      {
      case 0:
	if (d[i] != a[1] + 16)
	  return false;
	break;
      case 32:
	if (d[i] != 'X')
	  return false;
	break;
      case 42:
	if (d[i] != ' ')
	  return false;
	break;
      case sizeof (d) - 1:
	if (d[i] != 0)
	  return false;
	break;
      default:
	if (d[i] != a[i])
	  return false;
	break;
      }
  return true;
}

static_assert (bar (), "");
#endif

int
main ()
{
  unsigned char e[] = {
  #embed __FILE__
  };

  if (b != e[32] || c != e[42])
    __builtin_abort ();
}
