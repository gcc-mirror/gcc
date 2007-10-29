// PR c++/33841
// { dg-do compile }

template<int> struct A
{
  struct {} : 2;	// { dg-error "with non-integral type" }
};

template<int> struct B
{
  int a;
  struct {} : 2;	// { dg-error "with non-integral type" }
  int b;
};

struct C : A<0> {};
struct D : B<0> {};
