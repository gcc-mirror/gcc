// PR c++/51370

struct A
{
  template<typename> void foo() {}
};

template<void (A::*)()> struct B {};

template<int> struct C
{
  B<&A::foo<int int> > b; // { dg-error "declaration|not a member" }
};

C<0> c;
