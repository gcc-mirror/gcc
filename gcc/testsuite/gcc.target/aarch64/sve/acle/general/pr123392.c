/* { dg-do compile } */
/* { dg-options "-O3 -g" } */

#include <arm_sve.h>

svint32_t res (int32_t *a)
{
  svint32_t s = { a[0], a[1], a[2], a[3] };
  return s;
}
