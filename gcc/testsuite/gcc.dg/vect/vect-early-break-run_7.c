/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break_hw } */
/* { dg-require-effective-target vect_int } */

/* { dg-additional-options "-Ofast" } */

#define N 803
#define P 802
#include "vect-early-break-template_2.c"

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */
