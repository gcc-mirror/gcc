/* { dg-do run { target { riscv_vector } } } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv64gcv --param=riscv-autovec-preference=fixed-vlmax" } */

#include "shift-scalar-template.h"
