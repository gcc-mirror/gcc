/* PR target/105624 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O1 -march=k8" } */

union U {
  double d;
  unsigned long long int i;
};

double
fabs (double x)
{
  union U u;

  u.d = x;
  u.i &= ~0ULL >> 1;

  return u.d;
}
