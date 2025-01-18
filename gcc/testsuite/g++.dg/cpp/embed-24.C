// PR c++/118534
// { dg-do compile { target c++14 } }
// { dg-options "" }

template<typename T>
constexpr bool
foo ()
{
  T x[160] = {
#embed __FILE__ limit (160)
  };
  const int y[160] = {
    42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42,
#embed __FILE__ limit (147) gnu::offset (13)
  };
  unsigned long n = 13;
  for (T *p = x; n; --n, p++)
    *p = 42;
  for (int i = 0; i < 160; ++i)
    if (x[i] != y[i])
      return false;
  return true;
}

int
main ()
{
  static_assert (foo<int> (), "");
  static_assert (foo<unsigned char> (), "");
}
