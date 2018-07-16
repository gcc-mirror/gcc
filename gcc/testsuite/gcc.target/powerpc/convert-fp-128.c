/* { dg-options "-O2" } */
/* { dg-require-effective-target longdouble128 } */
/* { dg-require-effective-target dfp } */

#define conv(M,N) mode_##N conv##M##N(mode_##M x) { return x; }

#define mode_sf float
#define mode_df double
typedef float __attribute__((mode(IF))) mode_if;
typedef float __attribute__((mode(KF))) mode_kf;
#define mode_sd _Decimal32
#define mode_dd _Decimal64
#define mode_td _Decimal128

#ifdef __FLOAT128_TYPE__
#define conv1(M) \
	conv(M,sf) conv(M,df) conv(M,if) conv(M,kf) \
	conv(M,sd) conv(M,dd) conv(M,td)
#define conv2 \
	conv1(sf) conv1(df) conv1(if) conv1(kf) \
	conv1(sd) conv1(dd) conv1(td)
#else
#define conv1(M) \
	conv(M,sf) conv(M,df) conv(M,if) \
	conv(M,sd) conv(M,dd) conv(M,td)
#define conv2 \
	conv1(sf) conv1(df) conv1(if) \
	conv1(sd) conv1(dd) conv1(td)
#endif

conv2



/* { dg-final { scan-assembler-times {\mbl\M} 24 { target { ! hard_dfp } } } } */
/* { dg-final { scan-assembler-times {\mbl\M} 19 { target { hard_dfp && { ! ppc_float128 } } } } } */
/* { dg-final { scan-assembler-times {\mbl\M} 31 { target { hard_dfp && { ppc_float128 && { ! ppc_float128_insns } } } } } } */
/* { dg-final { scan-assembler-times {\mbl\M} 27 { target { hard_dfp && { ppc_float128 && { ppc_float128_insns } } } } } } */


/* { dg-final { scan-assembler-times {\mbl __extendsfkf2\M} 1 { target { ppc_float128 && { ! ppc_float128_insns } } } } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_extendsfsd\M} 1 } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_extendsfdd\M} 1 } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_extendsftd\M} 1 } } */
/* { dg-final { scan-assembler-times {\mbl __extenddfkf2\M} 1 { target { ppc_float128 && { ! ppc_float128_insns } } } } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_truncdfsd\M} 1 } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_extenddfdd\M} 1 } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_extenddftd\M} 1 } } */
/* { dg-final { scan-assembler-times {\mbl __trunctfkf2\M} 1 { target { ppc_float128 } } } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_trunctfsd\M} 1 } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_trunctfdd\M} 1 } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_extendtftd\M} 1 } } */
/* { dg-final { scan-assembler-times {\mbl __trunckfsf2\M} 1 { target { ppc_float128 && { ! ppc_float128_insns } } } } } */
/* { dg-final { scan-assembler-times {\mbl __trunckfdf2\M} 1 { target { ppc_float128 && { ! ppc_float128_insns } } } } } */
/* { dg-final { scan-assembler-times {\mbl __extendkftf2\M} 1 { target { ppc_float128 } } } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_trunckfsd\M} 1 { target { ppc_float128 } } } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_trunckfdd\M} 1 { target { ppc_float128 } } } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_extendkftd\M} 1 { target { ppc_float128 } } } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_truncsdsf\M} 1 } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_extendsddf\M} 1 } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_extendsdtf\M} 1 } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_extendsdkf\M} 1 { target { ppc_float128 } } } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_extendsddd2\M} 1 { target { ! dfp } } } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_extendsdtd2\M} 1 { target { ! dfp } } } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_truncddsf\M} 1 } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_truncdddf\M} 1 } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_extendddtf\M} 1 } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_extendddkf\M} 1 { target { ppc_float128 } } } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_truncddsd2\M} 1 { target { ! dfp } } } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_extendddtd2\M} 1 { target { ! dfp } } } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_trunctdsf\M} 1 } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_trunctddf\M} 1 } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_trunctdtf\M} 1 } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_trunctdkf\M} 1 { target { ppc_float128 } } } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_trunctdsd2\M} 1 } } */
/* { dg-final { scan-assembler-times {\mbl __dpd_trunctddd2\M} 1 { target { ! dfp } } } } */


/* { dg-final { scan-assembler-times {\mfrsp|xsrsp\M} 2 { target { ! ppc_float128_insns } } } } */
/* { dg-final { scan-assembler-times {\mfrsp|xsrsp\M} 3 { target { ppc_float128_insns } } } } */


/* { dg-final { scan-assembler-times {\mfmr\M} 0 { target { ! hard_dfp } } } } */
/* { dg-final { scan-assembler-times {\mfmr\M} 1 { target { hard_dfp } } } } */

/* { dg-final { scan-assembler-times {\mlfd\M} 2 { target { ! powerpc_vsx } } } } */
/* { dg-final { scan-assembler-times {\mxxlxor\M} 2 { target { powerpc_vsx } } } } */
/* { dg-final { scan-assembler-times {\mxxlor|xscpsgndp\M} 3 { target { ppc_float128_insns } } } } */


/* { dg-final { scan-assembler-times {\mdrsp\M} 1 { target { hard_dfp } } } } */
/* { dg-final { scan-assembler-times {\mdrdpq\M} 1 { target { hard_dfp } } } } */
/* { dg-final { scan-assembler-times {\mdctdp\M} 2 { target { hard_dfp } } } } */
/* { dg-final { scan-assembler-times {\mdctqpq\M} 2 { target { hard_dfp } } } } */


/* { dg-final { scan-assembler-times {\mxscvdpqp\M} 2 { target { ppc_float128_insns } } } } */
/* { dg-final { scan-assembler-times {\mxscvqpdpo\M} 1 { target { ppc_float128_insns } } } } */
/* { dg-final { scan-assembler-times {\mxscvqpdp\M} 1 { target { ppc_float128_insns } } } } */
