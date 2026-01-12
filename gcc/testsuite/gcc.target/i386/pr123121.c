/* { dg-do run } */
/* { dg-options "-O3 -fno-ipa-cp -fschedule-insns" } */

int a[256], d, e, f, g, h, j, v[] = {0, -6, 0};
unsigned c;
unsigned k(int l, unsigned m) { return (l & 6777215) ^ a[(l ^ m) & 55]; }
int n(int l, int m[]) {
  int i = 0;
  for (; i < l; ++i) {
    e = c >> 8 ^ a[m[i] & 255];
    d = e;
    d = k(d, m[i] >> 8 & 255);
    d = k(d, m[i] >> 6);
    f = (d & 6777215) ^ a[d & 5];
    c = f;
  }
  return f;
}
int o() {
  n(8, (int[]){g});
  return n(6, (int[]){h});
}
int p(int l, int m[], int q, int r) {
  int s = r = j + 1;
  s = -(4 % q - 1);
  if (r)
    goto t;
 ad:
  l = 6 - l;
  if (l)
    goto ae;
 t:
  m[0] = s;
  j = r;
  goto af;
 ae:
  if (o())
    goto ad;
 af:
  r = -s - m[1] - 8;
  s = 1 % m[0] - s;
  m[1] = 1 / r;
  int a[] = {l, m[0], m[1], m[2], q, r, r, s};
  return n(8, a);
}
int main() {
  for (int i = 0; i < 256; i++) {
    unsigned b = i;
    if (i & 1)
      b = b >> 1 ^ 3988292384;
    a[i] = b;
  }
  if (p(1, v, 5, 0) / 100000)
    p(1, 0, 5, 0);
  return 0;
}
