/* PR target/101985 */
/* { dg-do run } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2 -mvsx" } */

#include <altivec.h>

int
main (void)
{
  vector double a = {  1,  -4};
  vector double b = { -10,  40};
  vector double c = {  10, -40};
  a = vec_cpsgn (a, b);
  if (! vec_all_eq (a, c))
    __builtin_abort ();
  return 0;
}
