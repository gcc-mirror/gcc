// PR rtl-optimization/82778
// { dg-do compile }
// { dg-options "-O2" }

template <typename a, int b> struct c {
  typedef a d[b];
  static a e(d f, int g) { return f[g]; }
};
template <typename a, int b> struct B {
  typedef c<a, b> h;
  typename h::d i;
  long j;
  a at() { return h::e(i, j); }
};
int k, m, r, s, t;
char l, n, q;
short o, p, w;
struct C {
  int u;
};
B<C, 4> v;
void x() {
  if (((p > (q ? v.at().u : k)) >> l - 226) + !(n ^ r * m))
    s = ((-(((p > (q ? v.at().u : k)) >> l - 226) + !(n ^ r * m)) < 0) /
             (-(((p > (q ? v.at().u : k)) >> l - 226) + !(n ^ r * m)) ^
              -25 & o) &&
         p) >>
        (0 <= 0
             ? 0 ||
                   (-(((p > (q ? v.at().u : k)) >> l - 226) + !(n ^ r * m)) <
                    0) /
                       (-(((p > (q ? v.at().u : k)) >> l - 226) +
                          !(n ^ r * m)) ^ -25 & o)
             : 0);
  w = (p > (q ? v.at().u : k)) >> l - 226;
  t = !(n ^ r * m);
}
