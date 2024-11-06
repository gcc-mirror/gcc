/* { dg-do compile } */
/* { dg-options "-std=gnu17 -O3"  } */

int a, b, c, d, *e;
void f(int h) {
  if (b) {
    int g;
    while (g++)
      d = *e;
    e++;
  }
}
static void i();
static void j(int *h, int k, int *l) {
  if (c) {
    int *o = h, m;
    f(*l);
    i(m);
    j(o, 1, o);
    for (;;)
      ;
  }
}
void i() {
  int *n = &a;
  while (1)
    j(n, 1, n);
}
int main() {
  j(&a, 0, &a);
  return 0;
}
