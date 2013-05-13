/* PR rtl-optimization/56992 */
/* { dg-do compile } */
/* { dg-options "-Og -g" } */

inline int
foo (const char *x)
{
  return __builtin_strlen (x);
}

int
bar (const char *x, unsigned int *y)
{
  unsigned int l = foo (x);
  if (l > 15)
    l = 15;
  *y = l;
}
