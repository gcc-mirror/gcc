/* { dg-do compile } */
/* { dg-options "-O2" } */

extern void f(int p);
int a, b;
char c;
static int d(int e) { return !e || a == 1 ? 0 : a / e; }
static void h(short e) {
  int g = d(e);
  f(g);
}
void i() {
  c = 128;
  h(c);
  b = d(65536);
}
