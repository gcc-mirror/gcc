// PRMS Id: 4826
// Build don't link:

class A;
template <class T> void f(const T&, const T&);

void g (const A& a, A& b) {
  f (a, b); // gets bogus error - failed unification
}
