/* { dg-do compile } */
/* { dg-options "-O1 -fipa-sra" } */

typedef struct {
  int a;
} b;
typedef struct {
  double c;
  double a;
} d;
typedef struct {
  d e;
  d f;
} g;
g h;
b i, m;
int j, k, l, n, o;
static b q(d s) {
  int r = s.c ?: 0;
  if (r)
    if (j)
      l = j - 2;
  o = k;
  n = l;
  i = m;
  return m;
}
static void t(g s) {
  {
    d p = s.e;
    int r = p.c ?: 0;
    if (r) {
      l = j - 2;
    }
  }
  b f = q(s.f);
}
void main() { t(h); }
