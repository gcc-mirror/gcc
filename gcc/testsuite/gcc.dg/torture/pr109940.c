/* { dg-additional-options "-march=armv9-a" { target aarch64*-*-* } } */

int a;
int *b;
void
c (int *d) { *d = a; }

int
e(int d, int f) {
  if (d <= 1)
    return 1;
  int g = d / 2;
  for (int h = 0; h < g; h++)
    if (f == (__INTPTR_TYPE__)b > b[h])
      c(&b[h]);
  e(g, f);
  e(g, f);
}
