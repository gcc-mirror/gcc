// { dg-do compile }
// { dg-additional-options "-ftree-vectorize" }

template <typename> struct a;
template <typename b> struct a<b *> {
  typedef long c;
  typedef b &d;
};
template <typename e> class f {
  e ab;
  typedef a<e> ac;

public:
  typename ac::d operator[](typename ac::c o) { return ab[o]; }
};
template <typename> struct au;
template <typename b> au<b> operator+(au<b> o, au<b> p2) {
  au<b> ax = o;
  ax += p2;
  return ax;
}
template <typename b> au<b> operator-(au<b> o, au<b> p2) {
  au<b> ax = o;
  ax -= p2;
  return ax;
}
template <typename b> au<b> operator*(au<b>, au<b> &p2) {
  au<b> ax;
  ax *= p2;
  return ax;
}
template <> struct au<double> {
  double p() { return __real__ az; }
  double q() { return __imag__ az; }
  void operator+=(au o) {
    az += o.p();
    __imag__ az += o.q();
  }
  void operator-=(au o) {
    az -= o.p();
    __imag__ az -= o.q();
  }
  void operator*=(au &o) {
    _Complex bd = o.p();
    __imag__ bd = o.q();
    az *= bd;
  }
  _Complex az;
};
long bm, m;
f<au<double> *> g;
au<double> h, i, l;
void bn() {
  au<double> bq;
  for (long k; m;) {
    au<double> br;
    for (long j = 0; j < bm; ++j) {
      au<double> n = br * h;
      i = l + n;
      g[k] = l - bq;
    }
  }
}
