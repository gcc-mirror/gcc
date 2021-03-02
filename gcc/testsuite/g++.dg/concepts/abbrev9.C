// { dg-do compile { target concepts } }

template <class T, class U> concept same_as = __is_same(T, U);

same_as<int> auto f(auto, auto y) {
  return y; // { dg-error "deduced return type" }
}

template <class>
struct A {
  static auto g(auto x, auto y) -> same_as<decltype(x)> auto {
    return y; // { dg-error "deduced return type" }
  }
};

int main() {
  f(0, 0);   // { dg-bogus "" }
  f("", 0);  // { dg-bogus "" }
  f(0, "");  // { dg-message "required from here" }
  f("", ""); // { dg-message "required from here" }

  A<void>::g(0, 0);   // { dg-bogus "" }
  A<void>::g("", 0);  // { dg-message "required from here" }
  A<void>::g(0, "");  // { dg-message "required from here" }
  A<void>::g("", ""); // { dg-bogus "" }
}
