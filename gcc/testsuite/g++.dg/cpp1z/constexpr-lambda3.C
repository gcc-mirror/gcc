// { dg-options -std=c++1z }

constexpr auto add = [] (int n, int m) {
  auto L = [=] { return n; };
  auto R = [=] { return m; };
  return [=] { return L() + R(); };
};
static_assert(add(3, 4)() == 7, "");
