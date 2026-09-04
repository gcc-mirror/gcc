/* { dg-do compile } */

int a, l, b, c, h, i, m;
long e, g;
short f[4], n;
short f_1;
bool j;
void k(int, ...);
void o() {
  int p = f[0] + f[3 * e] + f_1 + f_1;
  for (; m;)
    if (p) __builtin_trap();
  c = *f - 2 + n;
  b = f[3 * e] - 2 + n;
  int q = l + c;
  int r = a + b;
  int dq = c + b;
  int d = q + r;
  if (d) k(d, e, q, dq, r);
}
