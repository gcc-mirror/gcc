// PR c++/88754
// { dg-do compile }

struct A
{
  A(int);
  void foo();
};

template<int N> int value() { return N; }

void bar()
{
  A(value<0>()).foo();
  A(value<0>());
  (A(value<0>())).foo();

  A value<0>; // { dg-error "invalid declaration" }
  A value<0>(); // { dg-error "invalid declaration" }
}
