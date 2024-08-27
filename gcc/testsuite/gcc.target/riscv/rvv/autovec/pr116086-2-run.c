/* { dg-do run } */
/* { dg-require-effective-target riscv_v } */
/* { dg-require-effective-target rvv_zvl256b_ok } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -mrvv-max-lmul=m2" } */

#include "pr116086-2.c"
