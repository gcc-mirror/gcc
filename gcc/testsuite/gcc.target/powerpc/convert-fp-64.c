/* { dg-do compile { target { ! longdouble128 } } } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target dfp } */

#define conv(M,N) mode_##N conv##M##N(mode_##M x) { return x; }

#define mode_sf float
#define mode_df double
#define mode_sd _Decimal32
#define mode_dd _Decimal64
#define mode_td _Decimal128

#define conv1(M) \
	conv(M,sf) conv(M,df) \
	conv(M,sd) conv(M,dd) conv(M,td)
#define conv2 \
	conv1(sf) conv1(df) \
	conv1(sd) conv1(dd) conv1(td)

conv2



/* { dg-final { scan-assembler-times {\mbl\M} 18 { target { ! hard_dfp } } } } */
/* { dg-final { scan-assembler-times {\mbl\M} 13 { target { hard_dfp } } } } */


/* { dg-final { scan-assembler-times {\mbl __dpd_extendsfsd\M} 1 } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_extendsfdd\M} 1 } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_extendsftd\M} 1 } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_truncdfsd\M} 1 } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_extenddfdd\M} 1 } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_extenddftd\M} 1 } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_truncsdsf\M} 1 } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_extendsddf\M} 1 } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_extendsddd2\M} 1 { target { ! dfp } } } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_extendsdtd2\M} 1 { target { ! dfp } } } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_truncddsf\M} 1 } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_truncdddf\M} 1 } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_truncddsd2\M} 1 { target { ! dfp } } } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_extendddtd2\M} 1 { target { ! dfp } } } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_trunctdsf\M} 1 } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_trunctddf\M} 1 } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_trunctdsd2\M} 1 } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_trunctddd2\M} 1 { target { ! dfp } } } } */


/* { dg-final { scan-assembler-times {\mfrsp|xsrsp\M} 1 } } */


/* { dg-final { scan-assembler-times {\mfmr\M} 0 { target { ! hard_dfp } } } } */
/* { dg-final { scan-assembler-times {\mfmr\M} 1 { target { hard_dfp } } } } */



/* { dg-final { scan-assembler-times {\mdrsp\M} 1 { target { hard_dfp } } } } */
/* { dg-final { scan-assembler-times {\mdrdpq\M} 1 { target { hard_dfp } } } } */
/* { dg-final { scan-assembler-times {\mdctdp\M} 2 { target { hard_dfp } } } } */
/* { dg-final { scan-assembler-times {\mdctqpq\M} 2 { target { hard_dfp } } } } */


