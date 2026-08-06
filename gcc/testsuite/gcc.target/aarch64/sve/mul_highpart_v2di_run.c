/* Run the generic high-part multiply correctness test with the vectoriser
   restricted to Advanced SIMD modes, so that the V2DI patterns are the ones
   being exercised.  */
/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize -march=armv8.2-a+sve -mautovec-preference=asimd-only" } */

#include "../../../gcc.dg/torture/mul-highpart-1.c"
