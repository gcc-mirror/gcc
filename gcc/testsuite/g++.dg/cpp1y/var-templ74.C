// PR c++/107938
// { dg-do compile { target c++14 } }

struct Q {
  int n;
  constexpr const Q* operator()(int) const { return this; }
};

extern const Q q;

template<int>
constexpr const Q* p = q(0);

void
g ()
{
  constexpr const Q* p2 = q(0);
  constexpr auto x = p<0>;
}
