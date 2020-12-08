/* { dg-do compile } */

int a, b, c, d;

void e() {
  int f = b;
  if (a) {
  L1:
    a = 0;
  L2:
    if (a) {
      c = b;
      goto L1;
    }
  }
  if (d)
    goto L2;
}
