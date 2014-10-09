// PR c++/47974

typedef double T;

struct A
{
  template<T> void foo(); // { dg-error "type" }
};

template<T N = 0, void (A::*)() = &A::foo<N> > struct B {}; // { dg-error "type|declared" }

B<> b; // { dg-message "non-type" }

// { dg-prune-output "could not convert" }
