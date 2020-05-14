// PR c++/94483
// { dg-do compile { target c++20 } }

template<int... a> constexpr auto x1
  = [...z = -a] (auto F) { return F(z...); };

template<const int&... a> constexpr auto x2
  = [&...z = a] (auto F) { return F(z...); };

template<int... a> constexpr auto x3
  = [z = -a] (auto F) { return F(z); }; // { dg-error "packs not expanded" }


constexpr auto sum = [] (auto... xs) { return (xs + ... + 0); };
const int y1 = 1, y2 = 2, y3 = 3;

static_assert(x1<1,2,3>(sum) == -6);
static_assert(x2<y1,y2,y3>(sum) == 6);
