/* { dg-do compile } */
/* { dg-options "-mriscv-attribute -march=rv32ifd_zca_zcf -mabi=ilp32" } */

void foo(){}

/* { dg-final { scan-assembler ".attribute arch, \"rv32i2p1_f2p2_d2p2_zicsr2p0_zca1p0_zcf1p0\"" } } */
