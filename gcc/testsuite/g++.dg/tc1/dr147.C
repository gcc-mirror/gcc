// { dg-do compile }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// DR147: Naming the constructor (PR 11764) 

namespace N1 {

struct A { A(); void f(); };
struct B: public A { B(); };

A::A() { }
B::B() { }

B::A ba;
A::A a; // { dg-error "constructor" "the injected-class-name can never be found through qualified lookup" }

void A::f()
{
  A::A();			// { dg-message "::A" "c++/42415" }
}

void f()
{
  A::A a; // { dg-error "constructor" "constructor" }
}
}

namespace N2 {

// This is nasty: if we allowed the injected-class-name to be looked as a 
//  qualified type, then the following code would be well-formed. Basically
//  the last line is defining the static member (with redundant parenthesis).
// Instead, it should be rejected as a malformed constructor declaration.

template <class T> struct A {
  template <class T2> A(T2);
  static A x;
};
template<> template <> A<char>::A<char>(char);
template<> A<int>::A<int>(A<int>::x);  // { dg-error "" "this is an invalid declaration of the constructor" }

}

// But DR 318 says that in situations where a type is syntactically
// required, lookup finds it.

struct C
{
  C();
  typedef int T;
};
struct C::C c;
C::C::T t;
struct D: C::C
{
  D(): C::C() { }
};

// And if lookup doesn't find the injected-class-name, we aren't naming the
// constructor (c++/44401).

struct E
{
  int E;
};

int E::*p = &E::E;
