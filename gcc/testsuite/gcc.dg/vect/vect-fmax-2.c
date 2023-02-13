/* { dg-require-effective-target vect_float } */

#define TYPE double
#define FN __builtin_fmax

#include "vect-fmax-1.c"

/* { dg-final { scan-tree-dump "Detected reduction" "vect" } } */
/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" { target vect_max_reduc } } } */
