/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include "riscv_vector.h"
int a;
long b, c;
void d() { __riscv_vlseg2e32ff_v_i32mf2x2(&a, &c, b); } /* { dg-error "invalid argument to built-in function" } */
