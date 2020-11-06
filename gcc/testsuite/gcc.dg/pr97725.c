/* { dg-do compile } */
/* { dg-options "-O2" } */

int a;
unsigned b;

int main() {
  if (a) {
    goto L1;
    while (1)
      while (1) {
        long e = -1L, g;
        int f, h, i;
      L1:
        a = f;
      L2:
        g = e;
        f = h || g;
        e = ~(f & b);
        if (i || g < -1L) {
          ~(g || 0);
          break;
        }
        goto L2;
      }
  }
  return 0;
}
