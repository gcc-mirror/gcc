/* { dg-do compile } */
/* { dg-options "-mriscv-attribute -march=rv32i_zicond -mabi=ilp32" } */

void foo(){}

/* { dg-final { scan-assembler ".attribute arch, \"rv32i2p1_zicond1p0\"" } } */
