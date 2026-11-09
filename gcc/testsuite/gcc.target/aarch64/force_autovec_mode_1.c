/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8.2-a+sve -msve-vector-bits=scalable -mautovec-preference=sve-only --param=aarch64-autovec-mode=V8QI -fdump-tree-vect-details" } */

/* The forced Advanced SIMD mode takes priority over the SVE preference.  */
#include "force_autovec_mode.h"

/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 3 "vect" } } */
/* { dg-final { scan-tree-dump-times "Choosing vector mode V8QI" 3 "vect" } } */
