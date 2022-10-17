/* PR target/101985 */
/* { dg-do run } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2 -mvsx" } */

#include <altivec.h>

int
main (void)
{
  vector float a = {  1,  2, - 3, - 4};
  vector float b = {-10, 20, -30,  40};
  vector float c = { 10, 20, -30, -40};
  a = vec_cpsgn (a, b);
  if (! vec_all_eq (a, c))
    __builtin_abort ();
  return 0;
}
