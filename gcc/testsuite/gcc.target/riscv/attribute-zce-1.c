/* { dg-do compile } */
/* { dg-options "-mriscv-attribute -march=rv32i_zce -mabi=ilp32" } */

void foo(){}

/* { dg-final { scan-assembler ".attribute arch, \"rv32i2p1_zicsr2p0_zca1p0_zcb1p0_zce1p0_zcmp1p0_zcmt1p0\"" } } */
