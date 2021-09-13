// PR c++/96443
// { dg-do compile { target c++20 } }

template <class T, class U> concept same_as = __is_same(T, U);

auto f(auto x) -> same_as<decltype(x)> auto { return 0; }; // { dg-error "constraints" }
void g(auto x) { same_as<decltype(x)> auto y = 0; } // { dg-error "constraints" }
auto h(auto x) -> same_as<decltype(x.missing)> auto { return 0; } // { dg-error "constraints|missing" }
template <class T, same_as<T> auto N> void i() {}

int main() {
  f(0); // { dg-bogus "" }
  f(true); // { dg-message "required from here" }
  g(0); // { dg-bogus "" }
  g(true); // { dg-message "required from here" }
  h(0); // { dg-message "required from here" }
  i<int, 0>(); // { dg-bogus "" }
  i<int, true>(); // { dg-error "no match|constraints" }
}
