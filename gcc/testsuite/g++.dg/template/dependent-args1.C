// PR c++/27582
// { dg-do compile }

struct A
{
  template<int> void foo();
};

template<int N, void (A::*)() = &A::foo<N> > struct B {};  // { dg-error "not a member" }

B<int> b; // { dg-error "type/value mismatch|invalid" }
