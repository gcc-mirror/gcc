/* PR rtl-optimization/102627 */
/* { dg-do run } */
/* { dg-options "-O1" } */

int a, f, l, m, q, c, d, g;
long b, e;
struct g {
  signed h;
  signed i;
  unsigned j;
  unsigned k;
};
unsigned n;
char o;
int *p = &m;
long r(int s) { return s && b ?: b; }
long __attribute__((noipa)) v() {
  l = 0 || r(n & o);
  return q;
}
void w(int, unsigned, struct g x) {
  c ?: a;
  for (; d < 2; d++)
    *p = x.k;
}
struct g __attribute__((noipa)) y() {
  struct g h = {3, 908, 1, 20};
  for (; g; g++)
    ;
  return h;
}
int main() {
  long t;
  struct g u = y();
  t = e << f;
  w(0, t, u);
  v(0, 4, 4, 4);
  if (m != 20)
    __builtin_abort ();
  return 0;
}
