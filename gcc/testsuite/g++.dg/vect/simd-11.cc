// { dg-do compile }
// { dg-require-effective-target c++11 }
// { dg-additional-options "-Ofast" }

template <typename> class h {
public:
  ~h();
};
template <typename> struct l;
template <typename c> struct l<h<c> > {
  using d = c;
  template <typename e> using f = h<e>;
};
template <typename g> struct o {
  typedef l<g> j;
  typedef typename j::d k;
  template <typename c> struct p { typedef typename j::f<c> m; };
};
template <typename c, typename g> struct F {
  typedef typename o<g>::p<c>::m q;
  struct : q {
  } r;
};
template <typename c, typename g = h<c> > class s : F<c, g> {
public:
  s(long);
  typename o<typename F<c, g>::q>::k operator[](long);
};
template <int> class t {
public:
  int dimension;
  t(const t &);
  void operator+=(t);
  double values[];
};
template <int dim> t<dim>::t(const t &p1) {
  for (int i = 0; i < dim; ++i)
    values[i] = p1.values[i];
}
template <int dim> class u : public t<dim> {
public:
  double m_fn1(const u &) const;
};
template <int dim> double u<dim>::m_fn1(const u &) const {
  double a;
  for (int i = 0; i < dim; ++i) {
    double b = this->values[i];
    a += b;
  }
  return a;
}
int m_fn2(const u<2> &p1, const u<2> &w) {
  int n;
  s<u<2> > c(n);
  s<double> d(n);
  double e = p1.m_fn1(w);
  for (;;) {
    c[0] += p1;
    d = e;
  }
}
