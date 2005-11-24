/* { dg-do compile } */
/* { dg-options "-Wstrict-aliasing=2 -O2" } */

double x;

template <typename T>
T *foo(void)
{
  int a[2];
  float *y = (float *)a; /* { dg-bogus "strict-aliasing" } */
  return (T *)&x; /* { dg-bogus "strict-aliasing" } */
}

