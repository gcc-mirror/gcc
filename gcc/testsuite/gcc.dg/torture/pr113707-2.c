/* { dg-do compile } */

int a, b, c, d, e, f, g, h, j, k, l;
void n() {
  while (c)
    if (1) {
      for (h = 5; h; h--) {
        int m = e % 2;
        d = ~g || h ^ m / -1;
        if (h > 5)
          e = k;
      }
      return;
    }
}
int main() {
  if (a)
    for (int i = 0; i < 2; i++) {
      for (f = 1; f < 6; f++)
        for (c = 7; c >= 0; c--)
          if (l)
            b = 0;
      n();
    }
  return 0;
}
