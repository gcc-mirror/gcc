/* { dg-do compile } */
/* { dg-options "-O3 -mcpu=neoverse-v3 -mautovec-preference=sve-only -msve-vector-bits=scalable --param=aarch64-autovec-mode=VNx16QI -fdump-tree-vect-details" } */

#include "cost_model_21.c"

/* The QI-to-SI conversion has two stages with counts 2 and 4.  On this
   four-wide model, round the first stage up by two issue slots.  The separate
   SI-to-DI conversion has only one stage and needs no padding.  Count the
   two extra slots with the 34 general operations.  */
/* { dg-final { scan-tree-dump "general operations = 36\\n" "vect" } } */
/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" } } */
