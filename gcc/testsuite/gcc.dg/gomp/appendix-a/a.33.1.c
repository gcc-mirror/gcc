/* { dg-do compile } */
/* { dg-require-effective-target tls_native } */

#include <stdio.h>
float x, y;
#pragma omp threadprivate(x, y)
void
init (float a, float b)
{
#pragma omp single copyprivate(a,b,x,y)
  {
    scanf ("%f %f %f %f", &a, &b, &x, &y);
  }
}
