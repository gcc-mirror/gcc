/* { dg-do compile } */
/* { dg-options "-march=rv64g_sha -mabi=lp64d" } */

void foo(){}

/* { dg-final { scan-assembler ".attribute arch, \"rv64i2p1_m2p0_a2p1_f2p2"
"_d2p2_h1p0_zicsr2p0_zifencei2p0_zmmul1p0_zaamo1p0_zalrsc1p0_sha1p0"
"_shcounterenw1p0_shgatpa1p0_shtvala1p0_shvsatpa1p0_shvstvala1p0_shvstvecd1p0"
"_ssstateen1p0\"" } } */