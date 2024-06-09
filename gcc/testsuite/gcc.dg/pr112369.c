/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-additional-options "-mavx2" { target avx2 } } */

struct GdkRGBA2 {
  double a[4];
};
struct GdkRGBA3 {
  float a[4];
};
struct GdkRGBA3 f(struct GdkRGBA2 *color) {
  struct GdkRGBA3 t1;
  for(int i = 0; i < 4; i++)
    t1.a[i] = color->a[i];
  struct GdkRGBA3 t2;
  for(int i = 0; i < 4; i++)
  {
    float tmp = t1.a[i];
    if (__builtin_isnan(tmp))
    t2.a[i] = tmp;
  }
  return t2;
}
