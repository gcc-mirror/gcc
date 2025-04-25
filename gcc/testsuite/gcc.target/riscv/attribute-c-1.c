/* { dg-do compile } */
/* { dg-options "-mriscv-attribute -march=rv32i_zca -mabi=ilp32" } */

void foo(){}

/* { dg-final { scan-assembler ".attribute arch, \"rv32i2p1_c2p0_zca1p0\"" } } */
