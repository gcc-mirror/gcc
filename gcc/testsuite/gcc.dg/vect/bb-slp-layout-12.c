/* { dg-do compile } */
/* { dg-additional-options "-Os -fno-tree-loop-vectorize" } */

#include "bb-slp-layout-11.c"

/* It would be better to keep the original three permutations.  */
/* { dg-final { scan-tree-dump-times "add new stmt: \[^\\n\\r\]* = VEC_PERM_EXPR" 3 "slp1" { target { vect_int && { vect_perm && vect_hw_misalign } } xfail { *-*-* } } } } */
/* { dg-final { scan-tree-dump-not "duplicating permutation node" "slp1" { target { vect_int && { vect_perm && vect_hw_misalign } } xfail { *-*-* } } } } */
