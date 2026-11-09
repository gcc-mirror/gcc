/* { dg-do compile } */
/* { dg-options "-O3 -msve-vector-bits=scalable -mautovec-preference=sve-only --param=aarch64-autovec-mode=VNx16QI -fdump-tree-vect-details" } */

#include "../force_autovec_mode.h"

/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 3 "vect" } } */
/* { dg-final { scan-tree-dump-times "Choosing vector mode VNx16QI" 3 "vect" } } */
