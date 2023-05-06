/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d --param riscv-autovec-preference=scalable -fdump-tree-vect-details" } */

#include "template-1.h"

/* Currently, we don't support SLP auto-vectorization for VLA. But it's
   necessary that we add this testcase here to make sure such unsupported SLP
   auto-vectorization will not cause an ICE. We will enable "vect" checking when
   we support SLP auto-vectorization for VLA in the future.  */

/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 0 "vect" } } */
