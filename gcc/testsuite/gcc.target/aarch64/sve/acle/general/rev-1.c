/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

#include <arm_sve.h>

svint32_t f(svint32_t v)
{
  return svrev_s32 (svrev_s32 (v));
}

/* { dg-final { scan-tree-dump "return v_1\\(D\\)" "optimized" } } */
/* { dg-final { scan-tree-dump-not "VEC_PERM_EXPR" "optimized" } } */
