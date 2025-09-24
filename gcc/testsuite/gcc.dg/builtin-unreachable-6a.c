/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

#include "builtin-unreachable-6.c"

/* { dg-final { scan-tree-dump-times "lab:" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-not "__builtin_unreachable" "optimized" } } */
