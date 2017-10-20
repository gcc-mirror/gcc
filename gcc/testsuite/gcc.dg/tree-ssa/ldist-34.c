/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-distribution" } */

#define X (3.0)
int b, c;
double a[30000];
int foo () {
  for (int i = 0; i < 100; ++i) {
    for (int j = 0; j < c; ++j)
      if (b)
        a[0] = b;
    a[i * 100] = a[1] = X;
  }
  return 0;
}
