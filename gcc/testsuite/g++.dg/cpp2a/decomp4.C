// PR c++/107276
// { dg-do compile { target c++20 } }

auto f(auto x) {
  auto [y] = x; // { dg-error "cannot decompose" }
  return y;
}
int i = f(0);
