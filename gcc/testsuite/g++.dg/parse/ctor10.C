// PR c++/85262
// { dg-options -fpermissive }

struct A {};

template<int> struct B : A
{
  B()
  {
    A::A(A());			// { dg-warning "constructor" }
  }
};

B<0> b;
