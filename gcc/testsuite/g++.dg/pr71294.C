// { dg-do compile { target { powerpc64*-*-* && lp64 } } }
// { dg-require-effective-target powerpc_p8vector_ok } */
// { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } }
// { dg-options "-mcpu=power8 -O3 -fstack-protector -mno-lra" }

// PAR target/71294 failed because RELOAD could not figure how create a V2DI
// vector that auto vectorization created with each element being the same
// stack address, with stack-protector turned on.

class A;
template <typename _Tp, int m, int n> class B {
public:
  _Tp val[m * n];
};
class C {
public:
  C(A);
};
struct D {
  D();
  unsigned long &operator[](int);
  unsigned long *p;
};
class A {
public:
  template <typename _Tp, int m, int n> A(const B<_Tp, m, n> &, bool);
  int rows, cols;
  unsigned char *data;
  unsigned char *datastart;
  unsigned char *dataend;
  unsigned char *datalimit;
  D step;
};
template <typename _Tp, int m, int n>
A::A(const B<_Tp, m, n> &p1, bool)
    : rows(m), cols(n) {
  step[0] = cols * sizeof(_Tp);
  datastart = data = (unsigned char *)p1.val;
  datalimit = dataend = datastart + rows * step[0];
}
class F {
public:
  static void compute(C);
  template <typename _Tp, int m, int n, int nm>
  static void compute(const B<_Tp, m, n> &, B<_Tp, nm, 1> &, B<_Tp, m, nm> &,
                      B<_Tp, n, nm> &);
};
D::D() {}
unsigned long &D::operator[](int p1) { return p[p1]; }
template <typename _Tp, int m, int n, int nm>
void F::compute(const B<_Tp, m, n> &, B<_Tp, nm, 1> &, B<_Tp, m, nm> &,
                B<_Tp, n, nm> &p4) {
  A a(p4, false);
  compute(a);
}
void fn1() {
  B<double, 4, 4> b, c, e;
  B<double, 4, 1> d;
  F::compute(b, d, c, e);
}
