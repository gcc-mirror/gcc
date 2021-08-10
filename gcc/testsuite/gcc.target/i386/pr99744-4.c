/* { dg-do compile } */
/* { dg-options "-O2 -mbmi -mbmi2 -mcldemote -mclflushopt -mclwb -mclzero -mcrc32 -menqcmd -mfsgsbase -mfxsr -mhreset -mlzcnt -mlwp -mmovdir64b -mmovdiri -mmwaitx -mpconfig -mpku -mpopcnt -mptwrite -mrdpid -mrdrnd -mrdseed -mrtm -msgx -mshstk -mtbm -mtsxldtrk -mxsave -mxsavec -mxsaveopt -mxsaves -mwaitpkg -mwbnoinvd" } */
/* { dg-additional-options "-muintr" { target { ! ia32 } } }  */

/* Test calling GPR intrinsics from functions with general-regs-only
   target attribute.  */

#include <x86gprintrin.h>

#define _CONCAT(x,y) x ## y

#define test_0(func, type)						\
  __attribute__ ((target("general-regs-only")))				\
  type _CONCAT(do_,func) (void)						\
  { return func (); }

#define test_0_i1(func, type, imm)					\
  __attribute__ ((target("general-regs-only")))				\
  type _CONCAT(do_,func) (void)						\
  { return func (imm); }

#define test_1(func, type, op1_type)					\
  __attribute__ ((target("general-regs-only")))				\
  type _CONCAT(do_,func) (op1_type A)					\
  { return func (A); }

#define test_1_i1(func, type, op1_type, imm)				\
  __attribute__ ((target("general-regs-only")))				\
  type _CONCAT(do_,func) (op1_type A)					\
  { return func (A, imm); }

#define test_2(func, type, op1_type, op2_type)				\
  __attribute__ ((target("general-regs-only")))				\
  type _CONCAT(do_,func) (op1_type A, op2_type B)			\
  { return func (A, B); }

#define test_2_i1(func, type, op1_type, op2_type, imm)			\
  __attribute__ ((target("general-regs-only")))				\
  type _CONCAT(do_,func) (op1_type A, op2_type B)			\
  { return func (A, B, imm); }

#define test_3(func, type, op1_type, op2_type, op3_type)		\
  __attribute__ ((target("general-regs-only")))				\
  type _CONCAT(do_,func) (op1_type A, op2_type B, op3_type C)		\
  { return func (A, B, C); }

#define test_4(func, type, op1_type, op2_type, op3_type, op4_type)	\
  __attribute__ ((target("general-regs-only")))				\
  type _CONCAT(do_,func) (op1_type A, op2_type B, op3_type C,		\
			  op4_type D)					\
  { return func (A, B, C, D); }

/* ia32intrin.h  */
test_1 (__bsfd, int, int)
test_1 (__bsrd, int, int)
test_1 (__bswapd, int, int)
test_1 (__popcntd, int, unsigned int)
test_2 (__rolb, unsigned char, unsigned char, int)
test_2 (__rolw, unsigned short, unsigned short, int)
test_2 (__rold, unsigned int, unsigned int, int)
test_2 (__rorb, unsigned char, unsigned char, int)
test_2 (__rorw, unsigned short, unsigned short, int)
test_2 (__rord, unsigned int, unsigned int, int)

#ifndef __iamcu__
/* adxintrin.h */
test_4 (_subborrow_u32, unsigned char, unsigned char, unsigned int,
	unsigned int, unsigned int *)
test_4 (_addcarry_u32, unsigned char, unsigned char, unsigned int,
	unsigned int, unsigned int *)
test_4 (_addcarryx_u32, unsigned char, unsigned char, unsigned int,
	unsigned int, unsigned int *)

/* bmiintrin.h */
test_1 (__tzcnt_u16, unsigned short, unsigned short)
test_2 (__andn_u32, unsigned int, unsigned int, unsigned int)
test_2 (__bextr_u32, unsigned int, unsigned int, unsigned int)
test_3 (_bextr_u32, unsigned int, unsigned int, unsigned int,
	unsigned int)
test_1 (__blsi_u32, unsigned int, unsigned int)
test_1 (_blsi_u32, unsigned int, unsigned int)
test_1 (__blsmsk_u32, unsigned int, unsigned int)
test_1 (_blsmsk_u32, unsigned int, unsigned int)
test_1 (__blsr_u32, unsigned int, unsigned int)
test_1 (_blsr_u32, unsigned int, unsigned int)
test_1 (__tzcnt_u32, unsigned int, unsigned int)
test_1 (_tzcnt_u32, unsigned int, unsigned int)

/* bmi2intrin.h */
test_2 (_bzhi_u32, unsigned int, unsigned int, unsigned int)
test_2 (_pdep_u32, unsigned int, unsigned int, unsigned int)
test_2 (_pext_u32, unsigned int, unsigned int, unsigned int)

/* cetintrin.h */
test_1 (_inc_ssp, void, unsigned int)
test_0 (_saveprevssp, void)
test_1 (_rstorssp, void, void *)
test_2 (_wrssd, void, unsigned int, void *)
test_2 (_wrussd, void, unsigned int, void *)
test_0 (_setssbsy, void)
test_1 (_clrssbsy, void, void *)

/* cldemoteintrin.h */
test_1 (_cldemote, void, void *)

/* clflushoptintrin.h */
test_1 (_mm_clflushopt, void, void *)

/* clwbintrin.h */
test_1 (_mm_clwb, void, void *)

/* clzerointrin.h */
test_1 (_mm_clzero, void, void *)

/* enqcmdintrin.h */
test_2 (_enqcmd, int, void *, const void *)
test_2 (_enqcmds, int, void *, const void *)

/* fxsrintrin.h */
test_1 (_fxsave, void, void *)
test_1 (_fxrstor, void, void *)

/* hresetintrin.h */
test_1 (_hreset, void, unsigned int)

/* ia32intrin.h  */
test_2 (__crc32b, unsigned int, unsigned char, unsigned char)
test_2 (__crc32w, unsigned int, unsigned short, unsigned short)
test_2 (__crc32d, unsigned int, unsigned int, unsigned int)
test_1 (__rdpmc, unsigned long long, int)
test_0 (__rdtsc, unsigned long long)
test_1 (__rdtscp, unsigned long long, unsigned int *)
test_0 (__pause, void)

/* lzcntintrin.h */
test_1 (__lzcnt16, unsigned short, unsigned short)
test_1 (__lzcnt32, unsigned int, unsigned int)
test_1 (_lzcnt_u32, unsigned int, unsigned int)

/* lwpintrin.h */
test_1 (__llwpcb, void, void *)
test_0 (__slwpcb, void *)
test_2_i1 (__lwpval32, void, unsigned int, unsigned int, 1)
test_2_i1 (__lwpins32, unsigned char, unsigned int, unsigned int, 1)

/* movdirintrin.h */
test_2 (_directstoreu_u32, void, void *, unsigned int)
test_2 (_movdir64b, void, void *, const void *)

/* mwaitxintrin.h */
test_3 (_mm_monitorx, void, void const *, unsigned int, unsigned int)
test_3 (_mm_mwaitx, void, unsigned int, unsigned int, unsigned int)

/* pconfigintrin.h */
test_2 (_pconfig_u32, unsigned int, const unsigned int, size_t *)

/* pkuintrin.h */
test_0 (_rdpkru_u32, unsigned int)
test_1 (_wrpkru, void, unsigned int)

/* popcntintrin.h */
test_1 (_mm_popcnt_u32, int, unsigned int)

/* rdseedintrin.h */
test_1 (_rdseed16_step, int, unsigned short *)
test_1 (_rdseed32_step, int, unsigned int *)

/* rtmintrin.h */
test_0 (_xbegin, unsigned int)
test_0 (_xend, void)
test_0_i1 (_xabort, void, 1)

/* sgxintrin.h */
test_2 (_encls_u32, unsigned int, const unsigned int, size_t *)
test_2 (_enclu_u32, unsigned int, const unsigned int, size_t *)
test_2 (_enclv_u32, unsigned int, const unsigned int, size_t *)

/* tbmintrin.h */
test_1_i1 (__bextri_u32, unsigned int, unsigned int, 1)
test_1 (__blcfill_u32, unsigned int, unsigned int)
test_1 (__blci_u32, unsigned int, unsigned int)
test_1 (__blcic_u32, unsigned int, unsigned int)
test_1 (__blcmsk_u32, unsigned int, unsigned int)
test_1 (__blcs_u32, unsigned int, unsigned int)
test_1 (__blsfill_u32, unsigned int, unsigned int)
test_1 (__blsic_u32, unsigned int, unsigned int)
test_1 (__t1mskc_u32, unsigned int, unsigned int)
test_1 (__tzmsk_u32, unsigned int, unsigned int)

/* tsxldtrkintrin.h */
test_0 (_xsusldtrk, void)
test_0 (_xresldtrk, void)

/* x86gprintrin.h */
test_1 (_ptwrite32, void, unsigned int)
test_1 (_rdrand16_step, int, unsigned short *)
test_1 (_rdrand32_step, int, unsigned int *)
test_0 (_wbinvd, void)

/* xtestintrin.h */
test_0 (_xtest, int)

/* xsaveintrin.h */
test_2 (_xsave, void, void *, long long)
test_2 (_xrstor, void, void *, long long)
test_2 (_xsetbv, void, unsigned int, long long)
test_1 (_xgetbv, long long, unsigned int)

/* xsavecintrin.h */
test_2 (_xsavec, void, void *, long long)

/* xsaveoptintrin.h */
test_2 (_xsaveopt, void, void *, long long)

/* xsavesintrin.h */
test_2 (_xsaves, void, void *, long long)
test_2 (_xrstors, void, void *, long long)

/* wbnoinvdintrin.h */
test_0 (_wbnoinvd, void)

#ifdef __x86_64__
/* adxintrin.h */
test_4 (_subborrow_u64, unsigned char, unsigned char,
	unsigned long long, unsigned long long,
	unsigned long long *)
test_4 (_addcarry_u64, unsigned char, unsigned char,
	unsigned long long, unsigned long long,
	unsigned long long *)
test_4 (_addcarryx_u64, unsigned char, unsigned char,
	unsigned long long, unsigned long long,
	unsigned long long *)

/* bmiintrin.h */
test_2 (__andn_u64, unsigned long long, unsigned long long,
	unsigned long long)
test_2 (__bextr_u64, unsigned long long, unsigned long long,
	unsigned long long)
test_3 (_bextr_u64, unsigned long long, unsigned long long,
	unsigned long long, unsigned long long)
test_1 (__blsi_u64, unsigned long long, unsigned long long)
test_1 (_blsi_u64, unsigned long long, unsigned long long)
test_1 (__blsmsk_u64, unsigned long long, unsigned long long)
test_1 (_blsmsk_u64, unsigned long long, unsigned long long)
test_1 (__blsr_u64, unsigned long long, unsigned long long)
test_1 (_blsr_u64, unsigned long long, unsigned long long)
test_1 (__tzcnt_u64, unsigned long long, unsigned long long)
test_1 (_tzcnt_u64, unsigned long long, unsigned long long)

/* bmi2intrin.h */
test_2 (_bzhi_u64, unsigned long long, unsigned long long,
	unsigned long long)
test_2 (_pdep_u64, unsigned long long, unsigned long long,
	unsigned long long)
test_2 (_pext_u64, unsigned long long, unsigned long long,
	unsigned long long)
test_3 (_mulx_u64, unsigned long long, unsigned long long,
	unsigned long long, unsigned long long *)

/* cetintrin.h */
test_0 (_get_ssp, unsigned long long)
test_2 (_wrssq, void, unsigned long long, void *)
test_2 (_wrussq, void, unsigned long long, void *)

/* fxsrintrin.h */
test_1 (_fxsave64, void, void *)
test_1 (_fxrstor64, void, void *)

/* ia32intrin.h  */
test_1 (__bsfq, int, long long)
test_1 (__bsrq, int, long long)
test_1 (__bswapq, long long, long long)
test_2 (__crc32q, unsigned long long, unsigned long long,
	unsigned long long)
test_1 (__popcntq, long long, unsigned long long)
test_2 (__rolq, unsigned long long, unsigned long long, int)
test_2 (__rorq, unsigned long long, unsigned long long, int)
test_0 (__readeflags, unsigned long long)
test_1 (__writeeflags, void, unsigned int)

/* lzcntintrin.h */
test_1 (__lzcnt64, unsigned long long, unsigned long long)
test_1 (_lzcnt_u64, unsigned long long, unsigned long long)

/* lwpintrin.h */
test_2_i1 (__lwpval64, void, unsigned long long, unsigned int, 1)
test_2_i1 (__lwpins64, unsigned char, unsigned long long,
	   unsigned int, 1)

/* movdirintrin.h */
test_2 (_directstoreu_u64, void, void *, unsigned long long)

/* popcntintrin.h */
test_1 (_mm_popcnt_u64, long long, unsigned long long)

/* rdseedintrin.h */
test_1 (_rdseed64_step, int, unsigned long long *)

/* tbmintrin.h */
test_1_i1 (__bextri_u64, unsigned long long, unsigned long long, 1)
test_1 (__blcfill_u64, unsigned long long, unsigned long long)
test_1 (__blci_u64, unsigned long long, unsigned long long)
test_1 (__blcic_u64, unsigned long long, unsigned long long)
test_1 (__blcmsk_u64, unsigned long long, unsigned long long)
test_1 (__blcs_u64, unsigned long long, unsigned long long)
test_1 (__blsfill_u64, unsigned long long, unsigned long long)
test_1 (__blsic_u64, unsigned long long, unsigned long long)
test_1 (__t1mskc_u64, unsigned long long, unsigned long long)
test_1 (__tzmsk_u64, unsigned long long, unsigned long long)

/* uintrintrin.h */
test_0 (_clui, void)
test_1 (_senduipi, void, unsigned long long)
test_0 (_stui, void)
test_0 (_testui, unsigned char)

/* x86gprintrin.h */
test_1 (_ptwrite64, void, unsigned long long)
test_0 (_readfsbase_u32, unsigned int)
test_0 (_readfsbase_u64, unsigned long long)
test_0 (_readgsbase_u32, unsigned int)
test_0 (_readgsbase_u64, unsigned long long)
test_1 (_rdrand64_step, int, unsigned long long *)
test_1 (_writefsbase_u32, void, unsigned int)
test_1 (_writefsbase_u64, void, unsigned long long)
test_1 (_writegsbase_u32, void, unsigned int)
test_1 (_writegsbase_u64, void, unsigned long long)

/* xsaveintrin.h */
test_2 (_xsave64, void, void *, long long)
test_2 (_xrstor64, void, void *, long long)

/* xsavecintrin.h */
test_2 (_xsavec64, void, void *, long long)

/* xsaveoptintrin.h */
test_2 (_xsaveopt64, void, void *, long long)

/* xsavesintrin.h */
test_2 (_xsaves64, void, void *, long long)
test_2 (_xrstors64, void, void *, long long)

/* waitpkgintrin.h */
test_1 (_umonitor, void, void *)
test_2 (_umwait, unsigned char, unsigned int, unsigned long long)
test_2 (_tpause, unsigned char, unsigned int, unsigned long long)

#else /* !__x86_64__ */
/* bmi2intrin.h */
test_3 (_mulx_u32, unsigned int, unsigned int, unsigned int,
	unsigned int *)

/* cetintrin.h */
test_0 (_get_ssp, unsigned int)
#endif /* __x86_64__ */

#endif
