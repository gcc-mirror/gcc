/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O3 -fstack-protector-strong" } */
/* { dg-require-effective-target powerpc_vsx } */

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
