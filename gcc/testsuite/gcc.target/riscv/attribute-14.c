/* { dg-do compile } */
/* { dg-options "-O -mriscv-attribute -march=rv32if -mabi=ilp32" } */
int foo()
{
}
/* { dg-final { scan-assembler ".attribute arch, \"rv32i2p0_f2p0_zicsr2p0\"" } } */
