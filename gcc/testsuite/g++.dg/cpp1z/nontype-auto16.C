// PR c++/90236
// { dg-do compile { target c++17 } }

struct foo { };

template <const auto &> void fnc() { } 

void
test()
{
  static constexpr foo a;
  fnc<a>();
}
