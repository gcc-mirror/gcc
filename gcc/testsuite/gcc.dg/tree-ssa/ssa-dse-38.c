/* { dg-options "-O2 -fdump-tree-dse-details -fno-tree-fre" } */


/* This changes the scope of the destination object and exposes
   missed optimizations in DSE.  */
#define SCOPE extern
#include "ssa-dse-37.c"

/* { dg-final { scan-tree-dump-times "Deleted dead call" 2 "dse1" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "Trimming statement " 4 "dse1" { xfail *-*-* } } } */


