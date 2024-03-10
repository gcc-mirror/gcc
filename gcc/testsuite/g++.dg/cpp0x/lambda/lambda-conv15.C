// PR c++/83258
// PR c++/80488
// { dg-do compile { target c++11 } }

template<void(*)()> struct A { };

int main() {
  constexpr auto fp = +[]{}; // { dg-error "non-'constexpr' function" "" { target c++14_down } }
  A<fp> a1;    // { dg-error "not a valid template argument" "" { target c++14_down } }
  A<[]{}> a2;  // { dg-error "lambda-expression in template-argument|invalid" "" { target c++17_down } }
}
