// { dg-do assemble  }
// PRMS Id: 4826

class A;
template <class T> void f(const T&, const T&);

void g (const A& a, A& b) {
  f (a, b); // { dg-bogus "" } failed unification
}
