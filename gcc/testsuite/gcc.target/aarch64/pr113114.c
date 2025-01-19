/* { dg-do compile } */
/* { dg-options "-mabi=ilp32 -Wno-deprecated -O -mearly-ldp-fusion -mlate-ldp-fusion" } */
void foo_n(double *a) {
  int i = 1;
  for (; i < (int)foo_n; i++)
    a[i] = a[i - 1] + a[i + 1] * a[i];
}
