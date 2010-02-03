/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <errno.h>
#include <stdlib.h>

int main()
{
  void *p;
  errno = 0;
  p = malloc (-1);
  if (errno != 0)
    do_not_optimize_away ();
  return 0;
}

/* { dg-final { scan-assembler "do_not_optimize_away" } } */
