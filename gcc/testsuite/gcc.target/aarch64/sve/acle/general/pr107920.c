/* { dg-do compile } */
/* { dg-options "-O1 -fno-tree-ccp -fno-tree-forwprop" } */

#include "arm_sve.h"

svint8_t
test_s8(int8_t *x)
{
  return svld1rq_s8 (svptrue_b8 (), &x[0]);
}
