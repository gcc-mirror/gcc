// PR c++/63194
// { dg-do compile { target c++11 } }

template <int>
class A {
 public:
  A() noexcept(noexcept(0)) = default;
};
class B {
  A<0> m_points;
};
void fn1(A<0>, A<0>) { B(); }
