// PR c++/54858
// { dg-do compile }

template <int> struct A {};
template <typename T, T *> struct B {};
template <typename D> struct C
{
  A<0> c0; B<A<0>, &C::c0> d0;	// { dg-error "could not convert template argument" }
  A<0> c1; B<A<0>, &C::c1> d1;	// { dg-error "could not convert template argument" }
  A<0> c2; B<A<0>, &C::c2> d2;	// { dg-error "could not convert template argument" }
  A<0> c3; B<A<0>, &C::c3> d3;	// { dg-error "could not convert template argument" }
  A<0> c4; B<A<0>, &C::c4> d4;	// { dg-error "could not convert template argument" }
  A<0> c5; B<A<0>, &C::c5> d5;	// { dg-error "could not convert template argument" }
  A<0> c6; B<A<0>, &C::c6> d6;	// { dg-error "could not convert template argument" }
  A<0> c7; B<A<0>, &C::c7> d7;	// { dg-error "could not convert template argument" }
  A<0> c8; B<A<0>, &C::c8> d8;	// { dg-error "could not convert template argument" }
  A<0> c9; B<A<0>, &C::c9> d9;	// { dg-error "could not convert template argument" }
  A<0> ca; B<A<0>, &C::ca> da;	// { dg-error "could not convert template argument" }
  A<0> cb; B<A<0>, &C::cb> db;	// { dg-error "could not convert template argument" }
};
C<int> e;
