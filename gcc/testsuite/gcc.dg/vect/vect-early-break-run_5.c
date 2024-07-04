/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break_hw } */
/* { dg-require-effective-target vect_int } */

/* { dg-additional-options "-Ofast" } */

#define N 800
#define P 799
#include "vect-early-break-template_1.c"

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */
