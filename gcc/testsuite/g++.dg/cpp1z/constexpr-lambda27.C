// PR c++/106369
// { dg-do compile { target c++17 } }

struct A {
  int a[256];
  constexpr int &operator[] (int n) noexcept { return a[n]; }
  constexpr const int &operator[] (int n) const noexcept { return a[n]; }
};
struct B {};
template <typename T>
struct C {
  constexpr T &foo (const char x) noexcept { c = T::d[x]; return static_cast<T &>(*this); }
  int c;
};
struct D : public C<D>, public B
{
  D () noexcept = default;
  static constexpr char e[9] { 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I' };
  static constexpr A d = [] () constexpr {
    A f {};
    for (int i = 0; i < 9; ++i)
      f[e[i]] = 1;
    return f;
  } ();
};
constexpr auto g = D{}.foo ('E');
