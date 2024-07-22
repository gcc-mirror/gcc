// PR c++/115504
// { dg-do compile { target c++14 } }

void f(int& x, const int& y) {
  [&x]() {
    decltype(auto) a = x;
    using type = decltype(x);
    using type = decltype(a);
    using type = int&; // not 'int'
  };

  [x]() {
    decltype(auto) a = x; // { dg-error "discards qualifiers" }
  };

  [x]() mutable {
    decltype(auto) a = x;
    using type = decltype(x);
    using type = decltype(a);
    using type = int&;
  };
}
