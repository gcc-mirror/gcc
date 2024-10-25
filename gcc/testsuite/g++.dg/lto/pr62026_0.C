// { dg-lto-do link }
// { dg-lto-options {{-flto -O3 -r}} }
class C;
class F {
  virtual C m_fn1();
};
class C {
 public:
  virtual int *m_fn3(int);
};
class G : F, C {
  int offsets;
  int *m_fn3(int);
};
C *a;
int *G::m_fn3(int) {
  if (offsets) return 0;
}

void fn1() {
  for (;;) a->m_fn3(0);
}
