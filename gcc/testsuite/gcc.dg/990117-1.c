/* { dg-do compile { target i?86-*-* } } */
/* { dg-options "-O2 -march=pentiumpro" } */

extern __inline  double
fabs (double __x)
{
  register double __value;
  __asm __volatile__
    ("fabs"
     : "=t" (__value) : "0" (__x));
  return __value;
}
int
foo ()
{
  int i, j, k;
  double x = 0, y = ((i == j) ? 1 : 0);
  for (i = 0; i < 10; i++)
    ;
  fabs (x - y);
}
