// { dg-require-effective-target lp64 }
// { dg-options "-O2 -fno-guess-branch-probability -fgcse-sm -fno-expensive-optimizations -fno-gcse" }

int printf(const char *, ...);
int a, b, c, d, e, f, g, i, j, m, h;
long k, l, n, o;
int main() {
  int p = e, r = i << a, q = r & b;
  k = 4073709551613;
  l = m = c = -(c >> j);
  d = g ^ h ^ 4073709551613;
  n = q - h;
  o = ~d;
  f = c * 4073709551613 / 409725 ^ r;
  if ((n && m) || (q && j) || a)
    return 0;
  d = o | p;
  if (g)
    printf("0");
  d = p;
  c++;
  return 0;
}
