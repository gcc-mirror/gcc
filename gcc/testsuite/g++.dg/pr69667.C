/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -w -std=c++14" } */

/* target/69667, compiler got
   internal compiler error: Max. number of generated reload insns per insn is achieved (90)  */

struct A {
  typedef int evaluation_error_type;
};
template <class T, class> T get_epsilon();
template <class E> __attribute__((__noreturn__)) void throw_exception(E);
template <class Formatter, class Group> char do_format(Formatter, Group &);
int raise_error_e, non_central_beta_p_k;
template <class, class T> void raise_error(T &p1) {
  char msg = do_format(msg, p1);
  throw_exception(raise_error_e);
}
char raise_evaluation_error_function;
template <class T1, class T2, class Policy>
T1 gamma_p_derivative(T1, T2, Policy);
template <class T, class Policy> T ibeta_imp(Policy, bool, T *);
template <class> struct B {};
template <class Dist, class RealType> B<Dist> complement(Dist, RealType);
template <class, class> class C;
template <class T, class Policy> struct D {
  D(C<T, Policy> p1, T, bool) : dist(p1) {}
  void operator()(T p1) { comp ? cdf(complement(dist, p1)) : T(); }
  C<T, Policy> dist;
  bool comp;
};
template <class, class = A> class C {
public:
  C(int, int, int);
};
template <class RealType, class Policy> void quantile(Policy p1, RealType) {
  nc_beta_quantile(p1, false);
}
double quantile_sanity_check___trans_tmp_1, quantile_sanity_check___trans_tmp_3,
    quantile_sanity_check___trans_tmp_5, quantile_sanity_check___trans_tmp_7;
void Trans_NS_detail_raise_evaluation_error(char *, char *, long double &p3,
                                            int) {
  raise_error<int>(p3);
}
template <class T, class Policy>
void raise_evaluation_error(char *, T &p2, Policy) {
  Trans_NS_detail_raise_evaluation_error(
      &raise_evaluation_error_function, "", p2,
      typename Policy::evaluation_error_type());
}
template <class T, class Policy>
T non_central_beta_p(T p1, T p2, T p3, Policy p4, T p5 = 0) {
  T y, errtol = get_epsilon<T, Policy>(), l2,
       pois = gamma_p_derivative(T(), l2, p4), xterm, last_term = 0;
  if (pois)
    return p5;
  T beta = y ? ibeta_imp(p4, false, &xterm) : ibeta_imp(p4, true, &xterm);
  xterm = y - 1;
  T sum = p5;
  if (beta && xterm)
    return p5;
  for (; non_central_beta_p_k;) {
    sum += beta;
    if (errtol && last_term || beta)
      break;
    xterm *=
        p1 + non_central_beta_p_k - 1 / p3 * p1 + p2 + non_central_beta_p_k - 2;
    last_term = beta;
  }
  raise_evaluation_error("", sum, p4);
}
template <class RealType, class Policy>
RealType non_central_beta_cdf(RealType, bool, Policy) {
  RealType b, a, x;
  non_central_beta_p(a, b, x, A());
}
template <class F, class T, class Tol, class Policy>
int bracket_and_solve_root_01(F p1, T, bool, Tol, unsigned, Policy) {
  T guess;
  p1(guess);
}
template <class RealType, class Policy>
void nc_beta_quantile(C<RealType, Policy>, bool p2) {
  RealType p;
  typedef RealType value_type;
  D<value_type, Policy> f(C<value_type, Policy>(0, 0, 0), p, p2);
  void tol();
  long max_iter =
      bracket_and_solve_root_01(f, value_type(), true, tol, max_iter, Policy());
}
template <class RealType, class Policy> RealType cdf(B<C<RealType, Policy>>) {
  RealType l = non_central_beta_cdf(l, true, Policy());
}
template <typename T> void quantile_sanity_check(T) {
  quantile(C<long double>(quantile_sanity_check___trans_tmp_1,
                          quantile_sanity_check___trans_tmp_3,
                          quantile_sanity_check___trans_tmp_5),
           quantile_sanity_check___trans_tmp_7);
}
void test_accuracy() {
  int ncbeta;
  quantile_sanity_check(ncbeta);
}
