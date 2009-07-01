/* { dg-require-effective-target vect_double } */

#include <stdlib.h>
#include <stdarg.h>
#include "tree-vect.h"

float x[256];

__attribute__ ((noinline))
double *foo(void)
{
 double *z = malloc (sizeof(double) * 256);

 int i;
 for (i=0; i<256; ++i)
   z[i] = x[i] + 1.0f;

 return z;
}


int main()
{
 int i;

 check_vect ();

 for (i = 0; i < 256; i++)
   x[i] = (float) i;

 foo();

 return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" { target vect_intfloat_cvt } } } */
/* { dg-final { scan-tree-dump-times "vector alignment may not be reachable" 1 "vect" { target { ! vector_alignment_reachable } } } } */
/* { dg-final { scan-tree-dump-times "Alignment of access forced using versioning" 1 "vect" { target { ! vector_alignment_reachable } } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
