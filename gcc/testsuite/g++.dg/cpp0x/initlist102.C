// PR c++/85977, Incorrect handling of array reference size deduction
// { dg-do compile { target c++11 } }

template <int N>
void fn1 (const char (&)[N]) { static_assert (N == 3, "fn1");}

template <int N>
void fn2 (const short (&)[N]) { static_assert (N == 3, "fn2");}

template <int N>
void fn3 (const int (&)[N]) { static_assert (N == 3, "fn2");}

template <int N>
void fn4 (const long (&)[N]) { static_assert (N == 3, "fn4");}

template <int N>
void fn5 (const unsigned char (&)[N]) { static_assert (N == 3, "fn5");}

template <int N>
void fn6 (const unsigned short (&)[N]) { static_assert (N == 3, "fn6");}

template <int N>
void fn7 (const unsigned int (&)[N]) { static_assert (N == 3, "fn7");}

template <int N>
void fn8 (const unsigned int (&)[N]) { static_assert (N == 3, "fn8");}

void
bar ()
{
  fn1 ({1, 2, 3});
  fn2 ({1, 2, 3});
  fn3 ({1, 2, 3});
  fn4 ({1, 2, 3});
  fn5 ({1, 2, 3});
  fn6 ({1, 2, 3});
  fn7 ({1, 2, 3});
  fn8 ({1, 2, 3});
}
