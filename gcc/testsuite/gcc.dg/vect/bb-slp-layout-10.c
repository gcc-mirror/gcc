/* { dg-do compile } */
/* { dg-additional-options "-Os -fno-tree-loop-vectorize" } */

#include "bb-slp-layout-9.c"

/* { dg-final { scan-tree-dump-times "add new stmt: \[^\\n\\r\]* = VEC_PERM_EXPR" 1 "slp1" { target { vect_int && { vect_perm && vect_hw_misalign } } } } } */
