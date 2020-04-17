/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -O3 -fstack-protector-strong" } */

class a {
  double b[2];
public:
  a();
};

class c {
public:
  typedef a d;
  d m_fn1() {
    a e;
    return e;
  }
};
template <typename f> void operator+(f, typename f::d);
void g() {
  c connector;
  for (;;) {
    c cut;
    a h = cut.m_fn1();
    connector + h;
  }
}
