/* { dg-do compile } */
/* { dg-additional-options "-frounding-math -fno-math-errno" } */
double f(int c, double a, double b) {
  if (c)
    return __builtin_sqrt(a);
  return __builtin_sqrt(b);
}
