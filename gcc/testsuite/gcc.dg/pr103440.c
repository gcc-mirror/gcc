/* { dg-do run } */
/* { dg-options "-Os" } */
/* { dg-timeout 10 } */

int a, b, c, d, e;
int main() {
  int f = 2, g = 1, h = -3;
L1:
  c = b ^ 1;
  if (!f)
    goto L3;
  if (d)
    g = e;
  f = h;
  if (!c)
    goto L1;
L2:
  if (g)
    a = 0;
L3:
  if (d == g)
    goto L2;
  return 0;
}
