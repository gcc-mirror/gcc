// PR c++/18436

void foo(int);

struct A
{
  static void foo(A);
};

template <typename T> struct B : T
{
  B() { foo(T()); } // { dg-error "cannot convert" }
};

B<A> b;
