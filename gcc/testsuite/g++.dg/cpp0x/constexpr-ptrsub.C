// PR c++/51489
// DR 1313
// { dg-do compile { target c++11 } }

struct array
{
  constexpr array() :x(0) {}
  constexpr int const* begin() const { return &x; }
  int x;
};
constexpr array aa;
constexpr auto b = aa.begin();
static_assert(b-b == 0, "compiles just fine");
static_assert(aa.begin()-aa.begin() == 0, "compiler thinks it's not a constant expression");
