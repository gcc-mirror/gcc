/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-fre -fdump-tree-dse-details" } */

#include <string.h>

void blahd (double *);

void fubar ()
{
  double d;
  double *x = &d;

  memset (&d, 0 , sizeof d);
  *x = 0.0;
  blahd (x);
}

/* { dg-final { scan-tree-dump-times "Deleted redundant store" 1 "dse1"} } */
