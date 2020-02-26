// PR c++/86429
// { dg-do compile { target c++14 } }

struct A
{
  int i;
  constexpr int f(const int&) const { return i; }
};

void g()
{
  constexpr A a = { 42 };
  [&](auto x) {
    constexpr auto y = a.f(x);
  }(24);
}
