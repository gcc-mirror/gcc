/* { dg-do compile } */
/* { dg-options "-O -mriscv-attribute -march=rv32ifd -mabi=ilp32 -misa-spec=2.2" } */
int foo()
{
}
/* { dg-final { scan-assembler ".attribute arch, \"rv32i2p0_f2p0_d2p0\"" } } */
