/* { dg-do compile } */
/* { dg-options "-O -mriscv-attribute -march=rv32gc -mabi=ilp32 -misa-spec=20190608" } */
int foo()
{
}
/* { dg-final { scan-assembler ".attribute arch, \"rv32i2p1_m2p0_a2p0_f2p2_d2p2_c2p0_zicsr2p0" } } */
