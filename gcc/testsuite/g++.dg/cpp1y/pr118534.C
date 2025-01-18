// PR c++/118534
// { dg-do compile { target c++14 } }

template<typename T>
constexpr bool
foo ()
{
  T x[160] = {
#define I8 1, 2, 3, 4, 5, 6, 7, 8
#define I64 I8, I8, I8, I8, I8, I8, I8, I8
    I64, I64, I8, I8, I8, I8
  };
  const int y[160] = {
    42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 6, 7, 8,
    I64, I64, I8, I8
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
