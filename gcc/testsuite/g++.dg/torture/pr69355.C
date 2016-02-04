// PR c++/69355
// { dg-do run }

template <int> struct A;
template <> struct A<1> {};
template <class Obj, int> struct B
{
  template <class T> struct C
  {
    typedef T *iterator;
    C (iterator p1) : m_iter (p1) {}
    void operator, (T p1) { *m_iter = p1; }
    iterator m_iter;
  };
  typedef double *iterator;
  B (Obj &p1, double) : m_object (p1) {}
  C<double> operator, (double);
  Obj &m_object;
};
template <class Obj, int LEN>
typename B<Obj, LEN>::template C<double>
B<Obj, LEN>::operator, (double p1)
{
  iterator a = m_object.data (), b = a + 1;
  *a = 1;
  *b = p1;
  return C<double>(b + 1);
}
class D {};
inline double operator+(const double &p1, D) { return p1; }
template <int> class U;
template <int Sz, int K = 0> struct F
{
  enum { doIt = K < Sz - 1 ? 1 : 0 };
  template <class Dest, class Src, class Assign>
  static void assign (Dest &p1, Src &p2, Assign &p3)
  {
    p3.apply_on (p1 (K), p2 (K));
    F<Sz * doIt, (K + 1) * doIt>::assign (p1, p2, p3);
  }
  template <class Dest, class Src> static double dot (Dest &p1, Src &p2)
  {
    return p1 (K) * p2 (K) + F<Sz * doIt, (K + 1) * doIt>::dot (p1, p2);
  }
};
template <> struct F<0>
{
  template <class Dest, class Src, class Assign>
  static void assign (Dest &, Src &, Assign &) {}
  template <class Dest, class Src> static D dot (Dest &, Src &) { return D (); }
};
template <class E, int Sz> struct G
{
  enum { ops_assign, use_meta };
  G (const E &p1) : m_expr (p1) {}
  double operator()(int p1) const { return m_expr (p1); }
  template <class Dest, class Src, class Assign>
  static void do_assign (A<1>, Dest &p2, Src &p3, Assign &p4)
  {
    F<Sz>::assign (p2, p3, p4);
  }
  template <class Dest, class Assign>
  void assign_to (Dest &p1, const Assign &p2) const
  {
    do_assign (A<1>(), p1, *this, p2);
  }
  E m_expr;
};
struct H
{
  static double apply_on (double p1, long double p2) { return p1 / p2; }
  static void apply_on (double &p1, double p2) { p1 = p2; }
};
template <class E1, class E2> struct I
{
  I (const E1 &p1, const E2 &p2) : m_lhs (p1), m_rhs (p2) {}
  double operator()(int p1) const
  {
    double c = m_lhs (p1);
    return H::apply_on (c, m_rhs (0));
  }
  E1 m_lhs;
  const E2 m_rhs;
};
struct J
{
  J (double p1) : m_data (p1) {}
  long double operator()(int) const { return m_data; }
  long double m_data;
};
template <int Sz> struct K
{
  K (const U<Sz> &p1) : m_data (p1.data ()) {}
  double operator()(int p1) const { return m_data[p1]; }
  const double *m_data;
};
template <int Sz> struct U
{
  U () {}
  U (const U &p1)
  {
    *this = G<ConstReference, Sz>(p1.const_ref ());
  }
  B<U, Sz> operator=(double) { return B<U, Sz>(*this, 0); }
  double *data () { return m_data; }
  const double *data () const { return m_data; }
  double &operator()(int p1) { return m_data[p1]; }
  double operator()(int p1) const { return m_data[p1]; }
  typedef K<Sz> ConstReference;
  ConstReference const_ref () const { return *this; }
  template <class E> void operator=(const G<E, Sz> &p1)
  {
    p1.assign_to (*this, H ());
  }
  double m_data[Sz];
};
template <int Sz>
G<I<K<Sz>, J>, Sz> div (U<Sz> &p1, double p2)
{
  typedef I<K<Sz>, J> expr_type;
  return G<expr_type, Sz>(expr_type (p1.const_ref (), p2));
}
template <int Sz> double norm2 (U<Sz> &p1)
{
  return __builtin_sqrt (F<Sz>::dot (p1, p1));
}
template <int Sz>
G<I<K<Sz>, J>, Sz> operator/(U<Sz> &p1, double p2)
{
  return div (p1, p2);
}
typedef U<3> V;
V foo (V p1)
{
  double e = norm2 (p1);
  V r;
  r = p1 / e;
  return r;
}
int
main ()
{
  V f;
  f = 1, 2, 3;
  V r = foo (f);
  if (__builtin_fabs (r (0) - 0.267261) > 0.01
      || __builtin_fabs (r (1) - 0.534522) > 0.01
      || __builtin_fabs (r (2) - 0.801784) > 0.01)
    __builtin_abort ();
}
