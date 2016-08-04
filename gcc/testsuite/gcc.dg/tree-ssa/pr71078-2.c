/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -fdump-tree-forwprop-details" } */

#include <math.h>

float f1(float x)
{
  float t1 = fabsf (x);
  float t2 = t1 / x; 
  return t2;
}
 
/* { dg-final { scan-tree-dump "__builtin_copysignf" "forwprop1" } } */
