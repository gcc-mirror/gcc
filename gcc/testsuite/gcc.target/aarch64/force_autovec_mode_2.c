/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8-a --param=aarch64-autovec-mode=V16QI -fdump-tree-vect-details" } */

#include "force_autovec_mode.h"

/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 3 "vect" } } */
/* { dg-final { scan-tree-dump-times "Choosing vector mode V16QI" 3 "vect" } } */
