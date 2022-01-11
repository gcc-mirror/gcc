// PR c++/103831
// { dg-do compile { target c++11 } }

struct A {
  constexpr int size() { return 42; } // non-static
};

template<class T>
struct B : T {
  static_assert(A::size() == 42, ""); // { dg-error "without object" }

  static int f() {
    static_assert(A::size() == 42, ""); // { dg-error "without object" }
    return A::size(); // { dg-error "without object" }
  }

  int n = A::size();
  static const int m = A::size(); // { dg-error "without object" }
};
