// { dg-do compile }
// Contributed by Matty T. <mattyt-bugzilla at tpg dot com dot au>
// PR c++/13813 [DR206]: Check semantic constraints of members of 
//   non-dependent type at instantiation time.


// DR206 explains that this is ill-formed, no diagnostic required. We emit
//  a diagnostic instead.
class E; 
template < class A > class Z { 
  A a;
  E e;   // { dg-error "incomplete type" }
};


// Nested classes are always dependent names.
template < class A > class Y { 
  class F; 
  F e;   // { dg-bogus "" "nested classes are always dependent, see DR108 and DR224" }
};
