// PR c++/27582
// { dg-do compile }

struct A
{
  template<int> void foo();
};

template<int N, void (A::*)() = &A::foo<N> > struct B {};

B<int> b; // { dg-error "type/value mismatch|expected a constant|invalid type" }

// { dg-prune-output "(could not convert|no match)" }
