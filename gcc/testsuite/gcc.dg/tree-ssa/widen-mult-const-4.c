/* { dg-do compile } */
/* { dg-require-effective-target int32 } */
/* { dg-require-effective-target longlong64 } */
/* { dg-options "-O2 -ftrapv -fdump-tree-optimized-raw" } */

#include "widen-mult-const-3.c"

/* { dg-final { scan-tree-dump-times {<mult_expr, [^,]*, [^,]*, 30,} 2 "optimized" } } */
