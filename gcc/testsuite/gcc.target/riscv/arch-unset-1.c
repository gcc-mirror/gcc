/* { dg-do compile } */
/* { dg-options "-march=rv64i -march=unset -mcpu=sifive-x280 -mabi=lp64 -misa-spec=20191213" } */
int foo()
{
}

/* { dg-final { scan-assembler "\.attribute arch, \"rv64i2p1_m2p0_a2p1_f2p2_d2p2_c2p0_v1p0_zicsr2p0_zmmul1p0_zaamo1p0_zalrsc1p0_zfh1p0_zfhmin1p0_zca1p0_zcd1p0_zba1p0_zbb1p0_zve32f1p0_zve32x1p0_zve64d1p0_zve64f1p0_zve64x1p0_zvfh1p0_zvl128b1p0_zvl256b1p0_zvl32b1p0_zvl512b1p0_zvl64b1p0\"" } } */
