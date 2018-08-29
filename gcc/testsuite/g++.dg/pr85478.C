/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O3 -march=arch12 -std=c++11 -Wno-return-type" } */

struct a {
  enum { b };
};
struct c {
  template <typename d, typename e> static void f(d g, e h) {
    d i;
    for (; i != g; ++h, ++i)
      *h = *i;
  }
};
template <int, typename d, typename e> void j(d g, e h) { c::f(g, h); }
template <int k, typename d, typename e> void l(d g, e h) { j<k>(g, h); }
template <typename d, typename e> void aa(d g, e h) { l<a::b>(g, h); }
template <typename> class ab;
template <> struct ab<float> {
  _Complex m() { return n; }
  _Complex n;
};
template <> struct ab<long double> {
  ab(ab<float> g) : n(g.m()) {}
  _Complex long double n;
};
template <int ac, typename o> class p {
public:
  template <typename q> p &operator=(const p<ac, q> &);
  o *ad;
};
template <typename o> class r : public p<2, o> {};
template <int ac, typename o>
template <typename q>
p<ac, o> &p<ac, o>::operator=(const p<ac, q> &g) {
  aa(&g.ad[0], &ad[0]);
}
template <typename ae> class s : public r<ae> {
  template <typename t> s &operator=(const s<t> &);
};
template <typename ae>
template <typename t>
s<ae> &s<ae>::operator=(const s<t> &g) {
  p<2, ae>::operator=(g);
}
template s<ab<long double>> &s<ab<long double>>::
operator=(const s<ab<float>> &);
