/* { dg-do compile } */
/* { dg-options "-mriscv-attribute -march=rv64i_zca -mabi=lp64" } */

void foo(){}

/* { dg-final { scan-assembler ".attribute arch, \"rv64i2p1_c2p0_zca1p0\"" } } */
