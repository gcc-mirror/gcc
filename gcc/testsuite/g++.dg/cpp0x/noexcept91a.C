// PR c++/122668
// A version of noexcept91.C where the lookup to be deferred isn't nested
// inside a noexcept-expr.
// { dg-do compile { target c++11 } }

template<class T>
struct A {
  friend void f(A& a) noexcept(g(0)) { }
  static constexpr bool g(int) { return true; }
};

template<class T>
struct B {
  struct C {
    friend void f(C& a) noexcept(g(0)) { }
    static constexpr bool g(int) { return false; }
  };
};

int main() {
  A<int> a;
  static_assert(noexcept(f(a)), "");

  B<int>::C c;
  static_assert(!noexcept(f(c)), "");
}
