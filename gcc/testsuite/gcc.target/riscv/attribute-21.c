/* { dg-do compile } */
/* { dg-options "-mriscv-attribute -march=rv64i_zicond -mabi=lp64" } */

void foo(){}

/* { dg-final { scan-assembler ".attribute arch, \"rv64i2p1_zicond1p0\"" } } */
