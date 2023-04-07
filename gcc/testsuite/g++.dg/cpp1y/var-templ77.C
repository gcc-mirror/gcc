// PR c++/107939
// { dg-do compile { target c++14 } }

struct Q {
  struct P {
    const Q* p;
  };
  int n;
  constexpr P operator()(int) const { return {this}; }
};

extern const Q q;
template<int>
constexpr auto p = q(0);
static_assert(p<0>.p == &q, "");

constexpr int
fn (int)
{
  return 42;
}

struct Sur {
  using FN = int(int);
  constexpr operator FN*() const { return &fn; }
};

extern const Sur sur;
template<int>
constexpr int aja = sur (0);
static_assert(aja<0> == 42, "");
static_assert(sur(1) == 42, "");
