/* PR tree-optimization/16951 */

void dummy_use(const char *p);

__inline void f(const char *const p) {
  const char q;
  dummy_use(p);
  f(&q);
}

void crash() {
  f(0);
}
