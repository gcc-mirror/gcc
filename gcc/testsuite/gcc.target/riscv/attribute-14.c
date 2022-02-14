/* { dg-do compile } */
/* { dg-options "-mriscv-attribute -march=rv32if -mabi=ilp32 -misa-spec=20190608" } */
int foo()
{
}
/* { dg-final { scan-assembler ".attribute arch, \"rv32i2p1_f2p2_zicsr2p0\"" } } */
