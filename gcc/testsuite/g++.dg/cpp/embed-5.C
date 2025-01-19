// { dg-do run { target c++14 } }
// { dg-options "" }

template <typename T>
constexpr T a[] = {
#embed __FILE__
};

template <typename T>
constexpr T
foo (int x)
{
  return a<T>[x];
}
constexpr unsigned char b = a<unsigned char>[32];
constexpr unsigned char c = foo<unsigned char> (42);
constexpr int b2 = a<int>[32];
constexpr int c2 = foo<int> (42);

template <typename T>
constexpr bool
bar ()
{
  T d[] = {
  #embed __FILE__
  };
  d[42] = ' ';
  d[32] = 'X';
  d[0] = d[1] + 16;
  d[sizeof (d) / sizeof (T) - 1] = d[42] - ' ';
  for (int i = 0; i < sizeof (d) / sizeof (T); ++i)
    switch (i)
      {
      case 0:
	if (d[i] != a<T>[1] + 16)
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
      case sizeof (d) / sizeof (T) - 1:
	if (d[i] != 0)
	  return false;
	break;
      default:
	if (d[i] != a<T>[i])
	  return false;
	break;
      }
  return true;
}

static_assert (bar<unsigned char> (), "");
static_assert (bar<int> (), "");

int
main ()
{
  unsigned char e[] = {
  #embed __FILE__
  };

  if (b != e[32] || c != e[42])
    __builtin_abort ();
  if (b2 != b || c2 != c)
    __builtin_abort ();
}
