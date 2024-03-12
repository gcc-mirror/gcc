/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

#include "arm_sve.h"

svuint32_t l()
{
  _Alignas(16) const unsigned int lanes[4] = {0, 0, 0, 0};
  return svld1rq_u32(svptrue_b8(), lanes);
}

/* { dg-final { scan-tree-dump-not "VEC_PERM_EXPR" "optimized" } } */
