/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-gimple-details" } */

#include <stdint.h>

#define T uint64_t

T sat_add_u_1 (T x, T y)
{
  return (T)(x + y) >= x ? (x + y) : -1;
}

/* { dg-final { scan-tree-dump-not " if " "gimple" } } */
/* { dg-final { scan-tree-dump-not " else " "gimple" } } */
/* { dg-final { scan-tree-dump-not " goto " "gimple" } } */
