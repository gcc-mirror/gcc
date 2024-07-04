/* { dg-do compile } */
/* { dg-options "-O2"  } */

struct a {
  int b;
};
struct c {
  long d;
  struct a e;
  long f;
};
int g, h, i;
int j() {return 0;}
static void k(struct a l, int p) {
  if (h)
    g = 0;
  for (; g; g = j())
    if (l.b)
      break;
}
static void m(struct c l) {
  k(l.e, l.f);
  for (;; --i)
    ;
}
int main() {
  struct c n = {10, 9};
  m(n);
}
