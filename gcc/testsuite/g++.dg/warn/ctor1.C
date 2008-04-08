// PR c++/35734
// { dg-options "-W" }

struct A
{
  A();
  template<typename T> A(const T&);
};

struct B : A
{
  B(const B&) {}		// { dg-warning "base class" }
};
