/* PR rtl-optimization/127246 */
/* { dg-do compile } */
/* { dg-require-effective-target label_values } */
/* { dg-require-effective-target indirect_jumps } */
/* { dg-options "-Os -fdump-rtl-jump2-details" } */

/* When optimizing for size the computed goto is not unfactored, so tail
   merging the handler tails is what we want.  */

#include "pr127246-1.c"

/* { dg-final { scan-rtl-dump "Cross jumping" "jump2" } } */
