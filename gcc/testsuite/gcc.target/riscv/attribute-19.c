/* { dg-do compile } */
/* { dg-options "-mriscv-attribute -march=rv64im -mabi=lp64 -misa-spec=2.2" } */
int foo() {}
/* { dg-final { scan-assembler ".attribute arch, \"rv64i2p0_m2p0\"" } } */
