/* { dg-do compile } */
/* { dg-require-effective-target tls } */

#include <stdlib.h>
float *work;
int size;
float tol;
void build (void);
#pragma omp threadprivate(work,size,tol)
void
a32 (float t, int n)
{
  tol = t;
  size = n;
#pragma omp parallel copyin(tol,size)
  {
    build ();
  }
}
void
build ()
{
  int i;
  work = (float *) malloc (sizeof (float) * size);
  for (i = 0; i < size; ++i)
    work[i] = tol;
}
