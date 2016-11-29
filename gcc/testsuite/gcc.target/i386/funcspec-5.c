/* Test whether all of the 32-bit function specific options are accepted
   without error.  */
/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */

#include "funcspec-56.inc"

extern void test_arch_i386 (void)		__attribute__((__target__("arch=i386")));
extern void test_arch_i486 (void)		__attribute__((__target__("arch=i486")));
extern void test_arch_i586 (void)		__attribute__((__target__("arch=i586")));
extern void test_arch_pentium (void)		__attribute__((__target__("arch=pentium")));
extern void test_arch_pentium_mmx (void)	__attribute__((__target__("arch=pentium-mmx")));
extern void test_arch_winchip_c6 (void)		__attribute__((__target__("arch=winchip-c6")));
extern void test_arch_winchip2 (void)		__attribute__((__target__("arch=winchip2")));
extern void test_arch_c3 (void)			__attribute__((__target__("arch=c3")));
extern void test_arch_c3_2 (void)		__attribute__((__target__("arch=c3-2")));
extern void test_arch_i686 (void)		__attribute__((__target__("arch=i686")));
extern void test_arch_pentiumpro (void)		__attribute__((__target__("arch=pentiumpro")));
extern void test_arch_pentium2 (void)		__attribute__((__target__("arch=pentium2")));
extern void test_arch_pentium3 (void)		__attribute__((__target__("arch=pentium3")));
extern void test_arch_pentium3m (void)		__attribute__((__target__("arch=pentium3m")));
extern void test_arch_pentium_m (void)		__attribute__((__target__("arch=pentium-m")));
extern void test_arch_pentium4 (void)		__attribute__((__target__("arch=pentium4")));
extern void test_arch_pentium4m (void)		__attribute__((__target__("arch=pentium4m")));
extern void test_arch_prescott (void)		__attribute__((__target__("arch=prescott")));
extern void test_arch_geode (void)		__attribute__((__target__("arch=geode")));
extern void test_arch_k6 (void)			__attribute__((__target__("arch=k6")));
extern void test_arch_k6_2 (void)		__attribute__((__target__("arch=k6-2")));
extern void test_arch_k6_3 (void)		__attribute__((__target__("arch=k6-3")));
extern void test_arch_athlon (void)		__attribute__((__target__("arch=athlon")));
extern void test_arch_athlon_tbird (void)	__attribute__((__target__("arch=athlon-tbird")));
extern void test_arch_athlon_4 (void)		__attribute__((__target__("arch=athlon-4")));
extern void test_arch_athlon_xp (void)		__attribute__((__target__("arch=athlon-xp")));
extern void test_arch_athlon_mp (void)		__attribute__((__target__("arch=athlon-mp")));
extern void test_arch_foo (void)		__attribute__((__target__("arch=foo"))); /* { dg-error "bad value" } */

extern void test_tune_i386 (void)		__attribute__((__target__("tune=i386")));
extern void test_tune_i486 (void)		__attribute__((__target__("tune=i486")));
extern void test_tune_i586 (void)		__attribute__((__target__("tune=i586")));
extern void test_tune_pentium (void)		__attribute__((__target__("tune=pentium")));
extern void test_tune_pentium_mmx (void)	__attribute__((__target__("tune=pentium-mmx")));
extern void test_tune_winchip_c6 (void)		__attribute__((__target__("tune=winchip-c6")));
extern void test_tune_winchip2 (void)		__attribute__((__target__("tune=winchip2")));
extern void test_tune_c3 (void)			__attribute__((__target__("tune=c3")));
extern void test_tune_c3_2 (void)		__attribute__((__target__("tune=c3-2")));
extern void test_tune_i686 (void)		__attribute__((__target__("tune=i686")));
extern void test_tune_pentiumpro (void)		__attribute__((__target__("tune=pentiumpro")));
extern void test_tune_pentium2 (void)		__attribute__((__target__("tune=pentium2")));
extern void test_tune_pentium3 (void)		__attribute__((__target__("tune=pentium3")));
extern void test_tune_pentium3m (void)		__attribute__((__target__("tune=pentium3m")));
extern void test_tune_pentium_m (void)		__attribute__((__target__("tune=pentium-m")));
extern void test_tune_pentium4 (void)		__attribute__((__target__("tune=pentium4")));
extern void test_tune_pentium4m (void)		__attribute__((__target__("tune=pentium4m")));
extern void test_tune_prescott (void)		__attribute__((__target__("tune=prescott")));
extern void test_tune_geode (void)		__attribute__((__target__("tune=geode")));
extern void test_tune_k6 (void)			__attribute__((__target__("tune=k6")));
extern void test_tune_k6_2 (void)		__attribute__((__target__("tune=k6-2")));
extern void test_tune_k6_3 (void)		__attribute__((__target__("tune=k6-3")));
extern void test_tune_athlon (void)		__attribute__((__target__("tune=athlon")));
extern void test_tune_athlon_tbird (void)	__attribute__((__target__("tune=athlon-tbird")));
extern void test_tune_athlon_4 (void)		__attribute__((__target__("tune=athlon-4")));
extern void test_tune_athlon_xp (void)		__attribute__((__target__("tune=athlon-xp")));
extern void test_tune_athlon_mp (void)		__attribute__((__target__("tune=athlon-mp")));
extern void test_tune_foo (void)		__attribute__((__target__("tune=foo"))); /* { dg-error "bad value" } */
