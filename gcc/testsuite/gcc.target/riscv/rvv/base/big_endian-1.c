/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -mbig-endian -O3" } */

#pragma riscv intrinsic "vector"
vfloat32m1_t foo (vfloat32m1_t) {} // { dg-excess-errors "sorry, unimplemented: Current RISC-V GCC cannot support RVV in big-endian mode" }
