/* { dg-do compile } */
/* { dg-options "-g" } */

/* PR rtl-optimization/120059 */

int a[4];
int c, d;
void f(void) {
  for (int e = 0; e < 4; e++)
    a[e] = e | c;
  int b = 0;
  if ((a[0] & 1) && (a[0] & 4))
    b = 2;
  if (a[0] & 16)
    b |= 1;
  d = ~b;
}
