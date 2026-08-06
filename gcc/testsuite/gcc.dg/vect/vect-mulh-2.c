/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target int128 } */

#define SIGNEDNESS unsigned

#include "vect-mulh-1.c"

/* { dg-final { scan-tree-dump {\.MULH} "vect" { target vect_mulh_di } } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loop" 1 "vect" { target vect_mulh_di } } } */
