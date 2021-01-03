/* { dg-do compile } */
/* { dg-options "-O -mriscv-attribute -march=rv32gc -mabi=ilp32 -misa-spec=2.2" } */
int foo()
{
}
/* { dg-final { scan-assembler ".attribute arch, \"rv32i2p0_m2p0_a2p0_f2p0_d2p0_c2p0\"" } } */
