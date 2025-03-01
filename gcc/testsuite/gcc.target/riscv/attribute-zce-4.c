/* { dg-do compile } */
/* { dg-options "-mriscv-attribute -march=rv64if_zce -mabi=lp64" } */

void foo(){}

/* { dg-final { scan-assembler ".attribute arch, \"rv64i2p1_f2p2_zicsr2p0_zca1p0_zcb1p0_zce1p0_zcmp1p0_zcmt1p0\"" } } */
