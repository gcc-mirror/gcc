/* { dg-do compile } */

int *b;
static void fn1(int *best, int *dmin) {
  int a[64];
  dmin = a;
  __asm__ volatile("" : "+&r"(dmin) : ""(best));
}

__attribute__((always_inline)) static inline void fn2(int *best) { fn1(best, b); }

void fn3(void) {
  int c[1];
  fn2(c);
}
