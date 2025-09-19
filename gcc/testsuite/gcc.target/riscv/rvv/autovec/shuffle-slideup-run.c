/* { dg-do run } */
/* { dg-require-effective-target riscv_v_ok } */
/* { dg-add-options riscv_v } */
/* { dg-additional-options "-O3 -std=gnu99 -mrvv-max-lmul=m8 -Wno-overflow" } */

#include "vls-vlmax/shuffle-slideup-1.c"
#include "shuffle-slide-run.h"
