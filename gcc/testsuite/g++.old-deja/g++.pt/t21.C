// { dg-do assemble  }

template <class X> class A { public: int a; X x; };
template <class X> X f (A<X> a) { return a.x; }

extern A<double> a_dbl;

double fred () { return f (a_dbl); }
