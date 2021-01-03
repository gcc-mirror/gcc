/* { dg-do compile { target arm*-*-* aarch64*-*-* } } */

int h(void);
struct c d;
struct c {
  int e[1];
};

void f(void) {
  int g;
  for (;; g = h()) {
    int *i = &d.e[g];
    asm("" : "=Q"(*i));
  }
}
