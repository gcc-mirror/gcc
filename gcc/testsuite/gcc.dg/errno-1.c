/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <errno.h>
#include <stdlib.h>

void do_not_optimize_away ();

int main()
{
  void * volatile p;
  errno = 0;
  /* The malloc call below may cause a -Walloc-size-larger-than warning.  */
  p = malloc (-1);
  if (errno != 0)
    do_not_optimize_away ();
  return 0;
}

/* { dg-final { scan-assembler "do_not_optimize_away" } } */
/* { dg-prune-output "exceeds maximum object size" } */
