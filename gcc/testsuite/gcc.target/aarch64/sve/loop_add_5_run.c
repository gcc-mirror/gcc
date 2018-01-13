/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize" } */
/* { dg-options "-O2 -ftree-vectorize -msve-vector-bits=256" { target aarch64_sve256_hw } } */

#include "loop_add_4_run.c"
