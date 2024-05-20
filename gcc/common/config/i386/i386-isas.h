/* i386 ISA table.
   Copyright (C) 2020-2024 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* These are the target attribute strings for which a dispatcher is
   available, from fold_builtin_cpu.  */
struct _isa_names_table
{
  const char *const name;
  const enum processor_features feature;
  const enum feature_priority priority;
  const char *const option;
};

/* NB: isa_names_table is shared by i386-builtins.cc, driver-i386.cc and
   gcc.target/i386/builtin_target.c.  isa_names_table is a static const
   array in i386-builtins.cc and driver-i386.cc.  But it is a list of
   assert statements in gcc.target/i386/builtin_target.c.  */

#ifndef ISA_NAMES_TABLE_START
# define ISA_NAMES_TABLE_START \
    static const struct _isa_names_table isa_names_table[] = {
#endif

#ifndef ISA_NAMES_TABLE_END
# define ISA_NAMES_TABLE_END };
#endif

#ifndef ISA_NAMES_TABLE_ENTRY
# define ISA_NAMES_TABLE_ENTRY(name, feature, priority, option)  \
    {name, feature, priority, option},
#endif

ISA_NAMES_TABLE_START
  ISA_NAMES_TABLE_ENTRY("cmov", FEATURE_CMOV, P_NONE, NULL)
  ISA_NAMES_TABLE_ENTRY("mmx", FEATURE_MMX, P_MMX, "-mmmx")
  ISA_NAMES_TABLE_ENTRY("popcnt", FEATURE_POPCNT, P_POPCNT, "-mpopcnt")
  ISA_NAMES_TABLE_ENTRY("sse", FEATURE_SSE, P_SSE, "-msse")
  ISA_NAMES_TABLE_ENTRY("sse2", FEATURE_SSE2, P_SSE2, "-msse2")
  ISA_NAMES_TABLE_ENTRY("sse3", FEATURE_SSE3, P_SSE3, "-msse3")
  ISA_NAMES_TABLE_ENTRY("ssse3", FEATURE_SSSE3, P_SSSE3, "-mssse3")
  ISA_NAMES_TABLE_ENTRY("sse4.1", FEATURE_SSE4_1, P_SSE4_1, "-msse4.1")
  ISA_NAMES_TABLE_ENTRY("sse4.2", FEATURE_SSE4_2, P_SSE4_2, "-msse4.2")
  ISA_NAMES_TABLE_ENTRY("avx", FEATURE_AVX, P_AVX, "-mavx")
  ISA_NAMES_TABLE_ENTRY("avx2", FEATURE_AVX2, P_AVX2, "-mavx2")
  ISA_NAMES_TABLE_ENTRY("sse4a", FEATURE_SSE4_A, P_SSE4_A, "-msse4a")
  ISA_NAMES_TABLE_ENTRY("fma4", FEATURE_FMA4, P_FMA4, "-mfma4")
  ISA_NAMES_TABLE_ENTRY("xop", FEATURE_XOP, P_XOP, "-mxop")
  ISA_NAMES_TABLE_ENTRY("fma", FEATURE_FMA, P_FMA, "-mfma")
  ISA_NAMES_TABLE_ENTRY("avx512f", FEATURE_AVX512F, P_AVX512F,
			"-mavx512f")
  ISA_NAMES_TABLE_ENTRY("bmi", FEATURE_BMI, P_BMI, "-mbmi")
  ISA_NAMES_TABLE_ENTRY("bmi2", FEATURE_BMI2, P_BMI2, "-mbmi2")
  ISA_NAMES_TABLE_ENTRY("aes", FEATURE_AES, P_AES, "-maes")
  ISA_NAMES_TABLE_ENTRY("pclmul", FEATURE_PCLMUL, P_PCLMUL, "-mpclmul")
  ISA_NAMES_TABLE_ENTRY("avx512vl", FEATURE_AVX512VL, P_NONE,
			"-mavx512vl")
  ISA_NAMES_TABLE_ENTRY("avx512bw", FEATURE_AVX512BW, P_NONE,
			"-mavx512bw")
  ISA_NAMES_TABLE_ENTRY("avx512dq", FEATURE_AVX512DQ, P_NONE,
			"-mavx512dq")
  ISA_NAMES_TABLE_ENTRY("avx512cd", FEATURE_AVX512CD, P_NONE,
			"-mavx512cd")
  ISA_NAMES_TABLE_ENTRY("avx512er", FEATURE_AVX512ER, P_NONE,
			"-mavx512er")
  ISA_NAMES_TABLE_ENTRY("avx512pf", FEATURE_AVX512PF, P_NONE,
			"-mavx512pf")
  ISA_NAMES_TABLE_ENTRY("avx512vbmi", FEATURE_AVX512VBMI, P_NONE,
			"-mavx512vbmi")
  ISA_NAMES_TABLE_ENTRY("avx512ifma", FEATURE_AVX512IFMA, P_NONE,
			"-mavx512ifma")
  ISA_NAMES_TABLE_ENTRY("avx5124vnniw", FEATURE_AVX5124VNNIW, P_NONE,
			"-mavx5124vnniw")
  ISA_NAMES_TABLE_ENTRY("avx5124fmaps", FEATURE_AVX5124FMAPS, P_NONE,
			"-mavx5124fmaps")
  ISA_NAMES_TABLE_ENTRY("avx512vpopcntdq", FEATURE_AVX512VPOPCNTDQ,
			P_NONE, "-mavx512vpopcntdq")
  ISA_NAMES_TABLE_ENTRY("avx512vbmi2", FEATURE_AVX512VBMI2, P_NONE,
			"-mavx512vbmi2")
  ISA_NAMES_TABLE_ENTRY("gfni", FEATURE_GFNI, P_NONE, "-mgfni")
  ISA_NAMES_TABLE_ENTRY("vpclmulqdq", FEATURE_VPCLMULQDQ, P_NONE,
			"-mvpclmulqdq")
  ISA_NAMES_TABLE_ENTRY("avx512vnni", FEATURE_AVX512VNNI, P_NONE,
			"-mavx512vnni")
  ISA_NAMES_TABLE_ENTRY("avx512bitalg", FEATURE_AVX512BITALG, P_NONE,
			"-mavx512bitalg")
  ISA_NAMES_TABLE_ENTRY("avx512bf16", FEATURE_AVX512BF16, P_NONE,
			"-mavx512bf16")
  ISA_NAMES_TABLE_ENTRY("avx512vp2intersect", FEATURE_AVX512VP2INTERSECT,
			P_NONE, "-mavx512vp2intersect")
  ISA_NAMES_TABLE_ENTRY("3dnow", FEATURE_3DNOW, P_NONE, "-m3dnow")
  ISA_NAMES_TABLE_ENTRY("3dnowp", FEATURE_3DNOWP, P_NONE, NULL)
  ISA_NAMES_TABLE_ENTRY("adx", FEATURE_ADX, P_NONE, "-madx")
  ISA_NAMES_TABLE_ENTRY("abm", FEATURE_ABM, P_NONE, "-mabm")
  ISA_NAMES_TABLE_ENTRY("cldemote", FEATURE_CLDEMOTE, P_NONE,
			"-mcldemote")
  ISA_NAMES_TABLE_ENTRY("clflushopt", FEATURE_CLFLUSHOPT, P_NONE,
			"-mclflushopt")
  ISA_NAMES_TABLE_ENTRY("clwb", FEATURE_CLWB, P_NONE, "-mclwb")
  ISA_NAMES_TABLE_ENTRY("clzero", FEATURE_CLZERO, P_NONE, "-mclzero")
  ISA_NAMES_TABLE_ENTRY("cmpxchg16b", FEATURE_CMPXCHG16B, P_NONE,
			"-mcx16")
  ISA_NAMES_TABLE_ENTRY("cmpxchg8b", FEATURE_CMPXCHG8B, P_NONE, NULL)
  ISA_NAMES_TABLE_ENTRY("enqcmd", FEATURE_ENQCMD, P_NONE, "-menqcmd")
  ISA_NAMES_TABLE_ENTRY("f16c", FEATURE_F16C, P_NONE, "-mf16c")
  ISA_NAMES_TABLE_ENTRY("fsgsbase", FEATURE_FSGSBASE, P_NONE,
			"-mfsgsbase")
  ISA_NAMES_TABLE_ENTRY("fxsave", FEATURE_FXSAVE, P_NONE, "-mfxsr")
  ISA_NAMES_TABLE_ENTRY("hle", FEATURE_HLE, P_NONE, "-mhle")
  ISA_NAMES_TABLE_ENTRY("ibt", FEATURE_IBT, P_NONE, NULL)
  ISA_NAMES_TABLE_ENTRY("lahf_lm", FEATURE_LAHF_LM, P_NONE, "-msahf")
  ISA_NAMES_TABLE_ENTRY("lm", FEATURE_LM, P_NONE, NULL)
  ISA_NAMES_TABLE_ENTRY("lwp", FEATURE_LWP, P_NONE, "-mlwp")
  ISA_NAMES_TABLE_ENTRY("lzcnt", FEATURE_LZCNT, P_NONE, "-mlzcnt")
  ISA_NAMES_TABLE_ENTRY("movbe", FEATURE_MOVBE, P_NONE, "-mmovbe")
  ISA_NAMES_TABLE_ENTRY("movdir64b", FEATURE_MOVDIR64B, P_NONE,
			"-mmovdir64b")
  ISA_NAMES_TABLE_ENTRY("movdiri", FEATURE_MOVDIRI, P_NONE, "-mmovdiri")
  ISA_NAMES_TABLE_ENTRY("mwaitx", FEATURE_MWAITX, P_NONE, "-mmwaitx")
  ISA_NAMES_TABLE_ENTRY("osxsave", FEATURE_OSXSAVE, P_NONE, NULL)
  ISA_NAMES_TABLE_ENTRY("pconfig", FEATURE_PCONFIG, P_NONE, "-mpconfig")
  ISA_NAMES_TABLE_ENTRY("pku", FEATURE_PKU, P_NONE, "-mpku")
  ISA_NAMES_TABLE_ENTRY("prefetchwt1", FEATURE_PREFETCHWT1, P_NONE,
			"-mprefetchwt1")
  ISA_NAMES_TABLE_ENTRY("prfchw", FEATURE_PRFCHW, P_NONE, "-mprfchw")
  ISA_NAMES_TABLE_ENTRY("ptwrite", FEATURE_PTWRITE, P_NONE, "-mptwrite")
  ISA_NAMES_TABLE_ENTRY("rdpid", FEATURE_RDPID, P_NONE, "-mrdpid")
  ISA_NAMES_TABLE_ENTRY("rdrnd", FEATURE_RDRND, P_NONE, "-mrdrnd")
  ISA_NAMES_TABLE_ENTRY("rdseed", FEATURE_RDSEED, P_NONE, "-mrdseed")
  ISA_NAMES_TABLE_ENTRY("rtm", FEATURE_RTM, P_NONE, "-mrtm")
  ISA_NAMES_TABLE_ENTRY("serialize", FEATURE_SERIALIZE, P_NONE,
			"-mserialize")
  ISA_NAMES_TABLE_ENTRY("sgx", FEATURE_SGX, P_NONE, "-msgx")
  ISA_NAMES_TABLE_ENTRY("sha", FEATURE_SHA, P_NONE, "-msha")
  ISA_NAMES_TABLE_ENTRY("shstk", FEATURE_SHSTK, P_NONE, "-mshstk")
  ISA_NAMES_TABLE_ENTRY("tbm", FEATURE_TBM, P_NONE, "-mtbm")
  ISA_NAMES_TABLE_ENTRY("tsxldtrk", FEATURE_TSXLDTRK, P_NONE,
			"-mtsxldtrk")
  ISA_NAMES_TABLE_ENTRY("vaes", FEATURE_VAES, P_NONE, "-mvaes")
  ISA_NAMES_TABLE_ENTRY("waitpkg", FEATURE_WAITPKG, P_NONE, "-mwaitpkg")
  ISA_NAMES_TABLE_ENTRY("wbnoinvd", FEATURE_WBNOINVD, P_NONE,
			"-mwbnoinvd")
  ISA_NAMES_TABLE_ENTRY("xsave", FEATURE_XSAVE, P_NONE, "-mxsave")
  ISA_NAMES_TABLE_ENTRY("xsavec", FEATURE_XSAVEC, P_NONE, "-mxsavec")
  ISA_NAMES_TABLE_ENTRY("xsaveopt", FEATURE_XSAVEOPT, P_NONE,
			"-mxsaveopt")
  ISA_NAMES_TABLE_ENTRY("xsaves", FEATURE_XSAVES, P_NONE, "-mxsaves")
  ISA_NAMES_TABLE_ENTRY("amx-tile", FEATURE_AMX_TILE, P_NONE, "-mamx-tile")
  ISA_NAMES_TABLE_ENTRY("amx-int8", FEATURE_AMX_INT8, P_NONE, "-mamx-int8")
  ISA_NAMES_TABLE_ENTRY("amx-bf16", FEATURE_AMX_BF16, P_NONE, "-mamx-bf16")
  ISA_NAMES_TABLE_ENTRY("uintr", FEATURE_UINTR, P_NONE, "-muintr")
  ISA_NAMES_TABLE_ENTRY("hreset", FEATURE_HRESET, P_NONE, "-mhreset")
  ISA_NAMES_TABLE_ENTRY("kl", FEATURE_KL, P_NONE, "-mkl")
  ISA_NAMES_TABLE_ENTRY("aeskle", FEATURE_AESKLE, P_NONE, NULL)
  ISA_NAMES_TABLE_ENTRY("widekl", FEATURE_WIDEKL, P_NONE, "-mwidekl")
  ISA_NAMES_TABLE_ENTRY("avxvnni", FEATURE_AVXVNNI, P_NONE, "-mavxvnni")
  ISA_NAMES_TABLE_ENTRY("avx512fp16", FEATURE_AVX512FP16, P_NONE, "-mavx512fp16")
  ISA_NAMES_TABLE_ENTRY("x86-64", FEATURE_X86_64_BASELINE, P_X86_64_BASELINE,
			NULL)
  ISA_NAMES_TABLE_ENTRY("x86-64-v2", FEATURE_X86_64_V2, P_X86_64_V2, NULL)
  ISA_NAMES_TABLE_ENTRY("x86-64-v3", FEATURE_X86_64_V3, P_X86_64_V3, NULL)
  ISA_NAMES_TABLE_ENTRY("x86-64-v4", FEATURE_X86_64_V4, P_X86_64_V4, NULL)
  ISA_NAMES_TABLE_ENTRY("avxifma", FEATURE_AVXIFMA, P_NONE, "-mavxifma")
  ISA_NAMES_TABLE_ENTRY("avxvnniint8", FEATURE_AVXVNNIINT8,
			P_NONE, "-mavxvnniint8")
  ISA_NAMES_TABLE_ENTRY("avxneconvert", FEATURE_AVXNECONVERT,
			P_NONE, "-mavxneconvert")
  ISA_NAMES_TABLE_ENTRY("cmpccxadd", FEATURE_CMPCCXADD, P_NONE, "-mcmpccxadd")
  ISA_NAMES_TABLE_ENTRY("amx-fp16", FEATURE_AMX_FP16, P_NONE, "-mamx-fp16")
  ISA_NAMES_TABLE_ENTRY("prefetchi", FEATURE_PREFETCHI, P_NONE, "-mprefetchi")
  ISA_NAMES_TABLE_ENTRY("raoint", FEATURE_RAOINT, P_NONE, "-mraoint")
  ISA_NAMES_TABLE_ENTRY("amx-complex", FEATURE_AMX_COMPLEX,
			P_NONE, "-mamx-complex")
  ISA_NAMES_TABLE_ENTRY("avxvnniint16", FEATURE_AVXVNNIINT16,
			P_NONE, "-mavxvnniint16")
  ISA_NAMES_TABLE_ENTRY("sm3", FEATURE_SM3, P_NONE, "-msm3")
  ISA_NAMES_TABLE_ENTRY("sha512", FEATURE_SHA512, P_NONE, "-msha512")
  ISA_NAMES_TABLE_ENTRY("sm4", FEATURE_SM4, P_NONE, "-msm4")
  ISA_NAMES_TABLE_ENTRY("apxf", FEATURE_APX_F, P_NONE, "-mapxf")
  ISA_NAMES_TABLE_ENTRY("usermsr", FEATURE_USER_MSR, P_NONE, "-musermsr")
  ISA_NAMES_TABLE_ENTRY("avx10.1", FEATURE_AVX10_1_256, P_NONE, "-mavx10.1")
  ISA_NAMES_TABLE_ENTRY("avx10.1-256", FEATURE_AVX10_1_256, P_AVX10_1_256, "-mavx10.1-256")
  ISA_NAMES_TABLE_ENTRY("avx10.1-512", FEATURE_AVX10_1_512, P_AVX10_1_512, "-mavx10.1-512")
ISA_NAMES_TABLE_END
