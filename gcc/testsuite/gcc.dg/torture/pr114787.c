/* { dg-do run } */

int a, b, c, d, e = -1, f, g, h, j, k, n, o, p;
int main() {
  int i, l = 2, m;
  for (b = 0; b < 1; b++)
    l = 0;
  for (; a >= 0; a--)
    for (m = 3; m; m--) {
      k = g;
      i = 0;
      for (; i < 1; i++)
        for (; f < 1; f++)
          h = g;
      n = 2 & ((e ^ d) | 1) * j;
      o = ~(e & n);
    q:
      if (c <= e)
        return 0;
      e = o;
    }
  p = l;
  l = 0;
  if (p)
    goto q;
  return 0;
}
