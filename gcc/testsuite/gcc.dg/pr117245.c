/* { dg-do compile } */
/* { dg-require-effective-target trampolines } */
/* { dg-options "" } */

void a() {
  int b;
  struct {
    char c[b];
  } bar() {
  }
  struct bar {
    __attribute__((vector_size(4))) char c[b];
  } (*d)();
  struct bar e() { struct bar f; }
  d = e;
  sizeof(d());
}

