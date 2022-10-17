/* { dg-do compile } */
/* { dg-options "-O2 -march=sapphirerapids -DTYPE=_Float16 -fdump-tree-vect" } */
/* { dg-final { scan-tree-dump ".COND_ADD" "vect" } } */
/* { dg-final { scan-tree-dump ".COND_SUB" "vect" } } */
/* { dg-final { scan-tree-dump ".COND_MUL" "vect" } } */
/* { dg-final { scan-tree-dump ".COND_RDIV" "vect" } } */

#include "cond_op_addsubmuldiv_double-1.c"

