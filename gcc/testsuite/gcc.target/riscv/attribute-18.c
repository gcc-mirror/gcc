/* { dg-do compile } */
/* { dg-options "-mriscv-attribute -march=rv64imafdc -mabi=lp64d -misa-spec=2.2" } */
int foo() {}
/* { dg-final { scan-assembler ".attribute arch, \"rv64i2p0_m2p0_a2p0_f2p0_d2p0_c2p0\"" } } */
