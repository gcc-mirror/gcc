// Build don't link: 

template <class X> class B;
template <class X> int f (B<X> b) { return 37; }
template <class Y> class B { public: Y y; B() { y = 1; } };

int foo () {
  B<double> bd;
  return f(bd);
}
