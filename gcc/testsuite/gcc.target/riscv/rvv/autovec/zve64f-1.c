/* { dg-do compile } */
/* { dg-options "-march=rv32gc_zve64f -mabi=ilp32d -mrvv-vector-bits=scalable -fdump-tree-vect-details" } */

#include "template-1.h"

/* Same as in zve64d-1.c we don't vectorize foo4 anymore due to an
   unfortunately tight costing decision.  Therefore expect 4 instead of 5
   vectorized loops.  */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 4 "vect" } } */
