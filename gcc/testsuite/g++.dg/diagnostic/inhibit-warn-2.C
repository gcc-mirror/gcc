// PR c++/65882
// PR c++/66467
// { dg-do compile }

template <bool>
struct A
{
  typedef int type;
};

struct B
{
  static const int value = 0;
};

template <class>
struct C
{
  typedef int type;
};

template <class>
struct F : B {};

class D
{
  template <class Expr>
  typename A<F<typename C<Expr>::type>::value || B::value>::type
  operator=(Expr); // { dg-message "declared" }
};

void fn1()
{
  D opt;
  opt = 0; // { dg-error "private" }
}
