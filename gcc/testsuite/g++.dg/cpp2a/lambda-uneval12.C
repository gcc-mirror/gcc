// PR c++/94521
// { dg-do compile { target c++20 } }

template <typename T>
void spam(decltype([]{}) *s)
{
  static_assert(__is_same(int, decltype(s))); // { dg-error "static assertion failed" }
}

void foo()
{
  spam<int>(nullptr);
}
