/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fno-trapping-math -fdump-tree-ifcvt-stats" } */

#include "ifcvt-fix-trunc-1.c"

/* { dg-final { scan-tree-dump-times "Applying if-conversion" 1 "ifcvt" } } */
