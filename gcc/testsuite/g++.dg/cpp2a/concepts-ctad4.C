// PR c++/100138
// { dg-do compile { target c++20 } }

template <class T>
struct A {
  A(T, auto... xs) requires (sizeof...(xs) != 0);
};

constexpr bool f(...) { return true; }

template <class T>
struct B {
  B(T, auto... xs) requires (f(xs...)); // { dg-error "constant expression" }
};

template <class T>
struct C {
  C(T, auto x) requires (f(x)); // { dg-error "constant expression" }
};

int main() {
  A x{1, 2}; // { dg-bogus "" }
  B y{1, 2}; // { dg-error "deduction|no match" }
  C z{1, 2}; // { dg-error "deduction|no match" }
}
