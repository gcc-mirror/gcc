/* PR rtl-optimization/127246 */
/* { dg-do compile } */
/* { dg-require-effective-target label_values } */
/* { dg-require-effective-target indirect_jumps } */
/* { dg-options "-O2 -fno-expensive-optimizations -fno-tree-slp-vectorize -fdump-rtl-jump2-details" } */

/* Without -fexpensive-optimizations computed gotos are not duplicated, so
   tail merging the handler tails is what we want.  */

#include "pr127246-1.c"

/* { dg-final { scan-rtl-dump "Cross jumping" "jump2" } } */
