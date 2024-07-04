/* { dg-do run { target { riscv_v && rv64 } } } */
/* { dg-options "-O3 -march=rv64gcv -mabi=lp64d -fdump-tree-optimized" } */

#include "pr113607.c"
