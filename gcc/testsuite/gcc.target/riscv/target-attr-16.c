/* { dg-do compile } */
/* { dg-options "-march=rv32gc_zba" { target { rv32 } } } */
/* { dg-options "-march=rv64gc_zba" { target { rv64 } } } */

__attribute__((target("arch=+zba,+zbb")))
void foo1 (void)
{
}

__attribute__((target("arch=+zbb,+zbb")))
void foo2 (void)
{
}

__attribute__((target("arch=+zba")))
__attribute__((target("arch=+zbb")))
void foo (void)
{
}

__attribute__((target("arch=+zbb")))
__attribute__((target("arch=+zbb")))
void bar (void)
{
}

/* { dg-final { scan-assembler-times ".option arch, rv32i2p1_m2p0_a2p1_f2p2_d2p2_c2p0_zicsr2p0_zifencei2p0_zmmul1p0_zaamo1p0_zalrsc1p0_zca1p0_zcd1p0_zcf1p0_zba1p0_zbb1p0" 4 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times ".option arch, rv64i2p1_m2p0_a2p1_f2p2_d2p2_c2p0_zicsr2p0_zifencei2p0_zmmul1p0_zaamo1p0_zalrsc1p0_zca1p0_zcd1p0_zba1p0_zbb1p0" 4 { target { rv64 } } } } */
