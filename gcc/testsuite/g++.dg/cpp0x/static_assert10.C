// PR c++/60254
// { dg-do compile { target c++11 } }

template<typename T> bool foo(T)
{
  int i;
  static_assert(foo(i), "Error"); // { dg-error "non-constant condition|not usable|non-constexpr" }
  return true;
}
