/* { dg-do run } */
/* { dg-require-effective-target riscv_xxtvarith_ok } */
/* { dg-require-effective-target riscv_v_ok } */
/* { dg-add-options riscv_v riscv_xxtvarith } */
/* { dg-additional-options "-O3 -mrvv-max-lmul=m8 -std=gnu99" } */

#include "shuffle-interleave-evenodd-run.c"
