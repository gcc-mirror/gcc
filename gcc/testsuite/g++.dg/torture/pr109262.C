// { dg-do compile }
// { dg-additional-options "-fnon-call-exceptions" }

template < typename > struct au;
template < typename b > au< b > operator*(au< b >, au< b > &p2) {
  au< b > ax;
  ax *= p2;
  return p2;
}
template <> struct au< double > {
  double p() { return __real__ az; }
  double q() { return __imag__ az; }
  void operator*=(au &o) {
    _Complex bd = o.p();
    __imag__ bd = o.q();
    az *= bd;
  }
  _Complex az;
};
long bm, m;
au< double > h;
void bn() {
  for (long k; ;) {
    au< double > br;
    for (long j = 0; 0 < bm; ++j)
      au n = br * h;
  }
}
