// PR c++/82022
// { dg-do compile { target c++17 } }

template <class T>
void f2()
{
  constexpr bool r = []() constexpr { return false; }();
  if constexpr (r);
  if constexpr ([]() constexpr { return false; }());
}

int main()
{
  if constexpr ([]() constexpr { return false; }());
  f2<int>();
}
