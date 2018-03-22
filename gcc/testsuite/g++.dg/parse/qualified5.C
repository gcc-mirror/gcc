// PR c++/84820

struct A {};

template<int> struct B : A
{
  B()
  {
    A(&A::foo);			// { dg-error "foo" }
  }
};

B<0> b;
