// DR 743: A type without linkage shall not be used as the type of a
//     variable or function with linkage, unless
//   o the variable or function has extern "C" linkage (7.5 [dcl.link]), or
//   o the variable or function is not used (3.2 [basic.def.odr]) or is
//   defined in the same translation unit.

// { dg-do compile { target c++11 } }

template <typename T> struct B {
  void g(T){}
  void h(T);			// { dg-error "never defined" }
  friend void i(B, T){}
  static T t1;			// { dg-error "never defined" }
  static T t2;
};

template <typename T> T B<T>::t2 = { };

enum { E1 } e1;			// OK, defined
extern enum { E2 } e2;		// { dg-error "never defined" }
extern "C" enum { E3 } e3;	// OK, extern "C"

void f() {
  struct A { int x; };  // no linkage
  A a = {1};
  B<A> ba;              // declares B<A>::g(A) and B<A>::h(A)
  ba.t1 = a;		// error, B<T>::t never defined
  ba.t2 = a;		// OK
  ba.g(a);              // OK
  ba.h(a);              // error, B<T>::h never defined
  i(ba, a);             // OK
  e1+e2+e3; // { dg-warning "arithmetic between different enumeration types" "" { target c++20 } }
}
