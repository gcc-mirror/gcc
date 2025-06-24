/* { dg-do compile } */
/* { dg-options "-march=rva23s64 -mabi=lp64d" } */

void foo(){}

/* { dg-final { scan-assembler-times ".attribute arch, \"rv64i2p1_m2p0_a2p1_f2p2_d2p2_c2p0"
"_b1p0_v1p0_zic64b1p0_zicbom1p0_zicbop1p0_zicboz1p0_ziccamoa1p0_ziccif1p0_zicclsm1p0"
"_ziccrse1p0_zicntr2p0_zicond1p0_zicsr2p0_zihintntl1p0_zihintpause2p0_zihpm2p0_zimop1p0"
"_za64rs1p0_zaamo1p0_zalrsc1p0_zawrs1p0_zfa1p0_zfhmin1p0_zca1p0_zcb1p0_zcd1p0_zcmop1p0"
"_zba1p0_zbb1p0_zbs1p0_zkt1p0_zvbb1p0_zve32f1p0_zve32x1p0_zve64d1p0_zve64f1p0_zve64x1p0"
"_zvfhmin1p0_zvkb1p0_zvkt1p0_zvl128b1p0_zvl32b1p0_zvl64b1p0_sha1p0_shcounterenw1p0"
"_shgatpa1p0_shtvala1p0_shvsatpa1p0_shvstvala1p0_shvstvecd1p0_ssccptr1p0_sscofpmf1p0"
"_sscounterenw1p0_ssnpm1p0_ssstateen1p0_sstc1p0_sstvala1p0_sstvecd1p0_ssu64xl1p0_supm1p0"
"_svade1p0_svbare1p0_svinval1p0_svnapot1p0_svpbmt1p0\" 1} } */
