// { dg-do compile }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// DR147: Naming the constructor (PR 11764) 

namespace N1 {

struct A { A(); };
struct B: public A { B(); };

A::A() { }
B::B() { }

B::A ba;
A::A a; // { dg-error "" "the injected-class-name can never be found through qualified lookup" { xfail *-*-* } }

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
template<> A<int>::A<int>(A<int>::x);  // { dg-error "" "this is an invalid declaration of the constructor" { xfail *-*-* } }

}
