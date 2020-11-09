/* { dg-do compile } */
/* { dg-options "-O2" } */

int a = 1, b, c;

void d() {
  int e = 1;
L1:
  b = e;
L2:
  e = e / a;
  if (!(e || c || e - 1))
    goto L1;
  if (!b)
    goto L2;
}
