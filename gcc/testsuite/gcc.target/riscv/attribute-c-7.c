/* { dg-do compile } */
/* { dg-options "-mriscv-attribute -march=rv64ifd_zca -mabi=lp64" } */

void foo(){}

/* { dg-final { scan-assembler ".attribute arch, \"rv64i2p1_f2p2_d2p2_zicsr2p0_zca1p0\"" } } */
