/* PR tree-optimization/57230 */
/* { dg-do run } */
/* { dg-options "-O2" } */

#include "strlenopt.h"

int
main ()
{
  char p[] = "hello world";
  p[0] = (char) (strlen (p) - 1);
  if (strlen (p) != 11)
    abort ();
  return 0;
}
