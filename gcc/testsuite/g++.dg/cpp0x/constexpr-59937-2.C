// PR c++/59937
// { dg-do compile { target c++11 } }

template<typename T> constexpr bool truth(const T&) { return true; }

template<typename T>
void test()
{
  int i[1];
  constexpr bool untrue = !truth(i);
  static_assert(!untrue, "");
}
