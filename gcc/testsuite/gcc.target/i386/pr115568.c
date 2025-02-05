/* { dg-do run } */
/* { dg-options "-O2 -fno-tree-sink -fno-tree-ter -fschedule-insns" } */

int a, c, d = 1, e, f = 1, h, i, j;
unsigned b = 1, g;
int main() {
  for (; h < 2; h++) {
    int k = ~(b || 0), l = ((~e - j) ^ a % b) % k, m = (b ^ -1) + e;
    unsigned o = ~a % ~1;
    if (f) {
      l = d;
      m = 10;
      i = e;
      d = -(~e + b);
      g = o % m;
      e = -1;
    n:
      a = a % ~i;
      b = ~k;
      if (!g) {
        b = e + o % -1;
        continue;
      }
      if (!l)
        break;
    }
    int q = (~d + g) << ~e, p = (~d - q) & a >> b;
    unsigned s = ~((g & e) + (p | (b ^ (d + k))));
    int r = (e & s) + p, u = d | ~a,
        t = ((~(q + (~a + (s + e)))) & u) | (-g & (c << d ^ p));
    if (t)
      if (!r)
        goto n;
    g = m;
    e = i;
  }
  return 0;
}
