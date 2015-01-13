// PR c++/64297
// { dg-do compile { target c++11 } }

struct A {
  typedef int X;
  template <int> X m_fn1() const;
};
template <typename> struct is_function {};
is_function<int() const &> i;
struct D {
  template <typename Y, typename = is_function<Y>> D(Y);
} b(&A::m_fn1<0>);
