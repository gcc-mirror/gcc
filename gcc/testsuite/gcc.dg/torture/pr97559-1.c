/* { dg-do compile } */

int printf (char *, ...);

int a, b, c, d;

void e () {
  int f = a;
  if (b) {
  L1:
    b = 0;
  L2:
    if (c) {
      if (f)
        printf("0");
      goto L1;
    }
  }
  if (d)
    goto L2;
}
