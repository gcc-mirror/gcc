/* { dg-do compile } */
/* { dg-additional-options "-Os" } */

#include "bb-slp-layout-5.c"

/* { dg-final { scan-tree-dump "absorbing input layouts" "slp2" { target { vect_int && { vect_perm && vect_hw_misalign } } } } } */
