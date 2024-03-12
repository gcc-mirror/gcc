// { dg-do compile }
// { dg-options "-O2" }

int a, b, c;
static int d(long e, long f) { return f == 0 || e && f == 1 ?: f; }
int g(void) {static int t; return t;}
static void h(long e) {
  b = e - 1;
  a = d(b || d(e, 8), g());
}
int tt;
void i(void) {
  c = (__SIZE_TYPE__)&tt;
  h(c);
}
