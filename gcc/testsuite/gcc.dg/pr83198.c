/* PR tree-optimization/83198 */
/* { dg-do compile } */
/* { dg-options "-Wall -Wno-format" } */

int
foo (char *d[6], int x)
{
  int r = 0;
  r += __builtin_sprintf (d[0], "%f", x);
  r += __builtin_sprintf (d[1], "%a", x);
  r += __builtin_sprintf (d[2], "%f", "foo");
  r += __builtin_sprintf (d[3], "%a", "bar");
#ifdef __SIZEOF_FLOAT128__
  r += __builtin_sprintf (d[4], "%a", 1.0Q);
  r += __builtin_sprintf (d[5], "%Lf", 1.0Q);
#endif
  return r;
}
