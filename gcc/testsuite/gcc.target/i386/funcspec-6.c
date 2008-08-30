/* Test whether all of the 64-bit function specific options are accepted
   without error.  */
/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */

extern void test_abm (void)			__attribute__((__target__("abm")));
extern void test_aes (void)			__attribute__((__target__("aes")));
extern void test_fused_madd (void)		__attribute__((__target__("fused-madd")));
extern void test_mmx (void)			__attribute__((__target__("mmx")));
extern void test_pclmul (void)			__attribute__((__target__("pclmul")));
extern void test_popcnt (void)			__attribute__((__target__("popcnt")));
extern void test_recip (void)			__attribute__((__target__("recip")));
extern void test_sse (void)			__attribute__((__target__("sse")));
extern void test_sse2 (void)			__attribute__((__target__("sse2")));
extern void test_sse3 (void)			__attribute__((__target__("sse3")));
extern void test_sse4 (void)			__attribute__((__target__("sse4")));
extern void test_sse4_1 (void)			__attribute__((__target__("sse4.1")));
extern void test_sse4_2 (void)			__attribute__((__target__("sse4.2")));
extern void test_sse4a (void)			__attribute__((__target__("sse4a")));
extern void test_sse5 (void)			__attribute__((__target__("sse5")));
extern void test_ssse3 (void)			__attribute__((__target__("ssse3")));

extern void test_no_abm (void)			__attribute__((__target__("no-abm")));
extern void test_no_aes (void)			__attribute__((__target__("no-aes")));
extern void test_no_fused_madd (void)		__attribute__((__target__("no-fused-madd")));
extern void test_no_mmx (void)			__attribute__((__target__("no-mmx")));
extern void test_no_pclmul (void)		__attribute__((__target__("no-pclmul")));
extern void test_no_popcnt (void)		__attribute__((__target__("no-popcnt")));
extern void test_no_recip (void)		__attribute__((__target__("no-recip")));
extern void test_no_sse (void)			__attribute__((__target__("no-sse")));
extern void test_no_sse2 (void)			__attribute__((__target__("no-sse2")));
extern void test_no_sse3 (void)			__attribute__((__target__("no-sse3")));
extern void test_no_sse4 (void)			__attribute__((__target__("no-sse4")));
extern void test_no_sse4_1 (void)		__attribute__((__target__("no-sse4.1")));
extern void test_no_sse4_2 (void)		__attribute__((__target__("no-sse4.2")));
extern void test_no_sse4a (void)		__attribute__((__target__("no-sse4a")));
extern void test_no_sse5 (void)			__attribute__((__target__("no-sse5")));
extern void test_no_ssse3 (void)		__attribute__((__target__("no-ssse3")));

extern void test_arch_nocona (void)		__attribute__((__target__("arch=nocona")));
extern void test_arch_core2 (void)		__attribute__((__target__("arch=core2")));
extern void test_arch_k8 (void)			__attribute__((__target__("arch=k8")));
extern void test_arch_k8_sse3 (void)		__attribute__((__target__("arch=k8-sse3")));
extern void test_arch_opteron (void)		__attribute__((__target__("arch=opteron")));
extern void test_arch_opteron_sse3 (void)	__attribute__((__target__("arch=opteron-sse3")));
extern void test_arch_athlon64 (void)		__attribute__((__target__("arch=athlon64")));
extern void test_arch_athlon64_sse3 (void)	__attribute__((__target__("arch=athlon64-sse3")));
extern void test_arch_athlon_fx (void)		__attribute__((__target__("arch=athlon-fx")));
extern void test_arch_amdfam10 (void)		__attribute__((__target__("arch=amdfam10")));
extern void test_arch_barcelona (void)		__attribute__((__target__("arch=barcelona")));
extern void test_arch_foo (void)		__attribute__((__target__("arch=foo"))); /* { dg-error "bad value" } */

extern void test_tune_nocona (void)		__attribute__((__target__("tune=nocona")));
extern void test_tune_core2 (void)		__attribute__((__target__("tune=core2")));
extern void test_tune_k8 (void)			__attribute__((__target__("tune=k8")));
extern void test_tune_k8_sse3 (void)		__attribute__((__target__("tune=k8-sse3")));
extern void test_tune_opteron (void)		__attribute__((__target__("tune=opteron")));
extern void test_tune_opteron_sse3 (void)	__attribute__((__target__("tune=opteron-sse3")));
extern void test_tune_athlon64 (void)		__attribute__((__target__("tune=athlon64")));
extern void test_tune_athlon64_sse3 (void)	__attribute__((__target__("tune=athlon64-sse3")));
extern void test_tune_athlon_fx (void)		__attribute__((__target__("tune=athlon-fx")));
extern void test_tune_amdfam10 (void)		__attribute__((__target__("tune=amdfam10")));
extern void test_tune_barcelona (void)		__attribute__((__target__("tune=barcelona")));
extern void test_tune_generic (void)		__attribute__((__target__("tune=generic")));
extern void test_tune_foo (void)		__attribute__((__target__("tune=foo"))); /* { dg-error "bad value" } */

extern void test_fpmath_sse (void)		__attribute__((__target__("sse2,fpmath=sse")));
extern void test_fpmath_387 (void)		__attribute__((__target__("sse2,fpmath=387")));
extern void test_fpmath_sse_387 (void)		__attribute__((__target__("sse2,fpmath=sse+387")));
extern void test_fpmath_387_sse (void)		__attribute__((__target__("sse2,fpmath=387+sse")));
extern void test_fpmath_both (void)		__attribute__((__target__("sse2,fpmath=both")));
