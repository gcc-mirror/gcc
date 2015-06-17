/* { dg-do compile } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler-not "\\\$f\[0-9\]+" } } */
int a, b, d, e, j, k, n, o;
unsigned c, h, i, l, m, p;
int *f;
int *g;
int fn1(int p1) { return p1 - a; }

int fn2() {
  b = b + 1 - a;
  e = 1 + o + 1518500249;
  d = d + n;
  c = (int)c + g[0];
  b = b + m + 1;
  d = d + p + 1518500249;
  d = d + k - 1;
  c = fn1(c + j + 1518500249);
  e = fn1(e + i + 1);
  d = d + h + 1859775393 - a;
  c = fn1(c + (d ^ 1 ^ b) + g[1] + 1);
  b = fn1(b + m + 3);
  d = fn1(d + l + 1);
  b = b + (c ^ 1) + p + 1;
  e = fn1(e + (b ^ c ^ d) + n + 1);
  d = o;
  b = 0;
  e = e + k + 1859775393;
  f[0] = e;
  return a;
}
