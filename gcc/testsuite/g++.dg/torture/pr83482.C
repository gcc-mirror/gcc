// PR tree-optimization/83482
// { dg-do compile }

int _setjmp (void **);
void *buf[64];

void a();
struct b {
  virtual long c() { return 0L; }
  void m_fn2() { c(); }
} d;

void e() {
  d.m_fn2();
  try {
    a();
    _setjmp(0);
  } catch (...) {
  }
}
