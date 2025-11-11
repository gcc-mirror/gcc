/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

#include <arm_sve.h>

svbool_t g (svbool_t p, svbool_t q, svbool_t a, svbool_t b,
            svbool_t c, svbool_t d)
{
  return (p == q) ? p : (a == b ? c : d);
}

/* { dg-final { scan-tree-dump-not {VEC_COND_EXPR} "optimized" } } */
/* { dg-final { scan-assembler-times {\teor\tp[0-9]+\.b} 2 } } */
