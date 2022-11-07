/* { dg-do compile } */
/* { dg-additional-options "-Os" } */

#include "bb-slp-layout-15.c"

/* { dg-final { scan-tree-dump-times "add new stmt: \[^\\n\\r\]* = VEC_PERM_EXPR" 1 "slp2" { target { vect_var_shift && { vect_perm && vect_hw_misalign } } } } } */
