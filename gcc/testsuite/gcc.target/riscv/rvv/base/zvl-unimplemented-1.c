/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv_zvl8192b -mabi=lp64d -mrvv-vector-bits=zvl" } */

void foo () {} // { dg-excess-errors "sorry, unimplemented: Current RISC-V GCC does not support VLEN > 4096bit for 'V' Extension" }
