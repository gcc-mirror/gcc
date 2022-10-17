/* { dg-do compile } */
/* { dg-options "-mriscv-attribute -march=rv32gc -mabi=ilp32 -misa-spec=20191213" } */
int foo()
{
}
/* { dg-final { scan-assembler ".attribute arch, \"rv32i2p1_m2p0_a2p1_f2p2_d2p2_c2p0_zicsr2p0" } } */
