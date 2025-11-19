// PR c++/122668
// { dg-do compile { target c++11 } }

template<class T>
struct A {
  friend void f1(A& a) noexcept(noexcept(a.g(0))) { }
  friend void f2(A& a) noexcept(noexcept(A::g(0))) { }
  static void g(int) noexcept;
};

int main() {
  A<int> a;
  static_assert(noexcept(f1(a)), "");
  static_assert(noexcept(f2(a)), "");
}
