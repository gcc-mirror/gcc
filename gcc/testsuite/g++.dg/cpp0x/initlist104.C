// PR c++/85977, Incorrect handling of array reference size deduction
// { dg-do compile { target c++11 } }

template <typename T, int N>
void fn (const T (&)[N]) { static_assert (N == 3, "fn"); }

void
bar ()
{
  fn ({1, 2, 3});
}
