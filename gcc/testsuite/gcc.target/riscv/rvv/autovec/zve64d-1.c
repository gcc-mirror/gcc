/* { dg-do compile } */
/* { dg-options "-march=rv32gc_zve64d -mabi=ilp32d -mrvv-vector-bits=scalable -fdump-tree-vect-details" } */

#include "template-1.h"

/* With length-control for VLS modes we don't vectorize foo4 anymore.
   That's due to a very tight costing decision in the small loop.
   Therefore expect 5 instead of 6 vectorized loops.  */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 5 "vect" } } */
