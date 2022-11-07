/* { dg-do compile } */
/* { dg-options "-O3 -march=rv32gc -mabi=ilp32d" } */

#pragma riscv intrinsic "vector" /* { dg-error {#pragma riscv intrinsic' option 'vector' needs 'V' extension enabled} } */
