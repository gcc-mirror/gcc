/* { dg-do compile } */
/* { dg-options "-mriscv-attribute -march=rv32gc -mabi=ilp32 -misa-spec=20190608" } */
int foo()
{
}
/* { dg-final { scan-assembler ".attribute arch, \"rv32i2p1_m2p0_a2p0_f2p2_d2p2_c2p0_zicsr2p0_zifencei2p0_zmmul1p0_zaamo1p0_zalrsc1p0_zca1p0_zcd1p0_zcf1p0\"" } } */
