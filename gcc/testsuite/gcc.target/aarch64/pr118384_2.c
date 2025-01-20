/* { dg-do run { target aarch64_sve256_hw } } */
/* { dg-options "-O2 -fopenmp-simd -fno-trapping-math -msve-vector-bits=256 --param aarch64-autovec-preference=sve-only -fstack-protector-strong" } */

#include "pr118384_1.c"
