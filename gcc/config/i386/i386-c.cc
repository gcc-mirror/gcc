/* Subroutines used for macro/preprocessor support on the ia-32.
   Copyright (C) 2008-2022 Free Software Foundation, Inc.

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

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "c-family/c-common.h"
#include "memmodel.h"
#include "tm_p.h"
#include "c-family/c-pragma.h"

static bool ix86_pragma_target_parse (tree, tree);
static void ix86_target_macros_internal
  (HOST_WIDE_INT, HOST_WIDE_INT, enum processor_type, enum processor_type, enum fpmath_unit,
   void (*def_or_undef) (cpp_reader *, const char *));

/* Internal function to either define or undef the appropriate system
   macros.  */
static void
ix86_target_macros_internal (HOST_WIDE_INT isa_flag,
			     HOST_WIDE_INT isa_flag2,
			     enum processor_type arch,
			     enum processor_type tune,
			     enum fpmath_unit fpmath,
			     void (*def_or_undef) (cpp_reader *,
						   const char *))
{
  /* For some of the k6/pentium varients there weren't separate ISA bits to
     identify which tune/arch flag was passed, so figure it out here.  */
  size_t arch_len = strlen (ix86_arch_string);
  size_t tune_len = strlen (ix86_tune_string);
  int last_arch_char = ix86_arch_string[arch_len - 1];
  int last_tune_char = ix86_tune_string[tune_len - 1];

  /* Built-ins based on -march=.  */
  switch (arch)
    {
    case PROCESSOR_I386:
      break;
    case PROCESSOR_I486:
      def_or_undef (parse_in, "__i486");
      def_or_undef (parse_in, "__i486__");
      break;
    case PROCESSOR_LAKEMONT:
      /* Intel MCU is based on Intel Pentium CPU.  */
    case PROCESSOR_PENTIUM:
      def_or_undef (parse_in, "__i586");
      def_or_undef (parse_in, "__i586__");
      def_or_undef (parse_in, "__pentium");
      def_or_undef (parse_in, "__pentium__");
      if (isa_flag & OPTION_MASK_ISA_MMX)
	def_or_undef (parse_in, "__pentium_mmx__");
      break;
    case PROCESSOR_PENTIUMPRO:
      def_or_undef (parse_in, "__i686");
      def_or_undef (parse_in, "__i686__");
      def_or_undef (parse_in, "__pentiumpro");
      def_or_undef (parse_in, "__pentiumpro__");
      break;
    case PROCESSOR_GEODE:
      def_or_undef (parse_in, "__geode");
      def_or_undef (parse_in, "__geode__");
      break;
    case PROCESSOR_K6:
      def_or_undef (parse_in, "__k6");
      def_or_undef (parse_in, "__k6__");
      if (last_arch_char == '2')
	def_or_undef (parse_in, "__k6_2__");
      else if (last_arch_char == '3')
	def_or_undef (parse_in, "__k6_3__");
      else if (isa_flag & OPTION_MASK_ISA_3DNOW)
	def_or_undef (parse_in, "__k6_3__");
      break;
    case PROCESSOR_ATHLON:
      def_or_undef (parse_in, "__athlon");
      def_or_undef (parse_in, "__athlon__");
      if (isa_flag & OPTION_MASK_ISA_SSE)
	def_or_undef (parse_in, "__athlon_sse__");
      break;
    case PROCESSOR_K8:
      def_or_undef (parse_in, "__k8");
      def_or_undef (parse_in, "__k8__");
      break;
    case PROCESSOR_AMDFAM10:
      def_or_undef (parse_in, "__amdfam10");
      def_or_undef (parse_in, "__amdfam10__");
      break;
    case PROCESSOR_BDVER1:
      def_or_undef (parse_in, "__bdver1");
      def_or_undef (parse_in, "__bdver1__");
      break;
    case PROCESSOR_BDVER2:
      def_or_undef (parse_in, "__bdver2");
      def_or_undef (parse_in, "__bdver2__");
      break;
    case PROCESSOR_BDVER3:
      def_or_undef (parse_in, "__bdver3");
      def_or_undef (parse_in, "__bdver3__");
      break;
    case PROCESSOR_BDVER4:
      def_or_undef (parse_in, "__bdver4");
      def_or_undef (parse_in, "__bdver4__");
      break;
    case PROCESSOR_ZNVER1:
      def_or_undef (parse_in, "__znver1");
      def_or_undef (parse_in, "__znver1__");
      break;
    case PROCESSOR_ZNVER2:
      def_or_undef (parse_in, "__znver2");
      def_or_undef (parse_in, "__znver2__");
      break;
    case PROCESSOR_ZNVER3:
      def_or_undef (parse_in, "__znver3");
      def_or_undef (parse_in, "__znver3__");
      break;
    case PROCESSOR_ZNVER4:
      def_or_undef (parse_in, "__znver4");
      def_or_undef (parse_in, "__znver4__");
      break;
    case PROCESSOR_BTVER1:
      def_or_undef (parse_in, "__btver1");
      def_or_undef (parse_in, "__btver1__");
      break;
    case PROCESSOR_BTVER2:
      def_or_undef (parse_in, "__btver2");
      def_or_undef (parse_in, "__btver2__");
      break;
    case PROCESSOR_LUJIAZUI:
      def_or_undef (parse_in, "__lujiazui");
      def_or_undef (parse_in, "__lujiazui__");
      break;
    case PROCESSOR_PENTIUM4:
      def_or_undef (parse_in, "__pentium4");
      def_or_undef (parse_in, "__pentium4__");
      break;
    case PROCESSOR_NOCONA:
      def_or_undef (parse_in, "__nocona");
      def_or_undef (parse_in, "__nocona__");
      break;
    case PROCESSOR_CORE2:
      def_or_undef (parse_in, "__core2");
      def_or_undef (parse_in, "__core2__");
      break;
    case PROCESSOR_NEHALEM:
      def_or_undef (parse_in, "__corei7");
      def_or_undef (parse_in, "__corei7__");
      def_or_undef (parse_in, "__nehalem");
      def_or_undef (parse_in, "__nehalem__");
      break;
    case PROCESSOR_SANDYBRIDGE:
      def_or_undef (parse_in, "__corei7_avx");
      def_or_undef (parse_in, "__corei7_avx__");
      def_or_undef (parse_in, "__sandybridge");
      def_or_undef (parse_in, "__sandybridge__");
      break;
    case PROCESSOR_HASWELL:
      def_or_undef (parse_in, "__core_avx2");
      def_or_undef (parse_in, "__core_avx2__");
      def_or_undef (parse_in, "__haswell");
      def_or_undef (parse_in, "__haswell__");
      break;
    case PROCESSOR_BONNELL:
      def_or_undef (parse_in, "__atom");
      def_or_undef (parse_in, "__atom__");
      def_or_undef (parse_in, "__bonnell");
      def_or_undef (parse_in, "__bonnell__");
      break;
    case PROCESSOR_SILVERMONT:
      def_or_undef (parse_in, "__slm");
      def_or_undef (parse_in, "__slm__");
      def_or_undef (parse_in, "__silvermont");
      def_or_undef (parse_in, "__silvermont__");
      break;
    case PROCESSOR_GOLDMONT:
      def_or_undef (parse_in, "__goldmont");
      def_or_undef (parse_in, "__goldmont__");
      break;
    case PROCESSOR_GOLDMONT_PLUS:
      def_or_undef (parse_in, "__goldmont_plus");
      def_or_undef (parse_in, "__goldmont_plus__");
      break;
    case PROCESSOR_TREMONT:
      def_or_undef (parse_in, "__tremont");
      def_or_undef (parse_in, "__tremont__");
      break;
    case PROCESSOR_SIERRAFOREST:
      def_or_undef (parse_in, "__sierraforest");
      def_or_undef (parse_in, "__sierraforest__");
      break;
    case PROCESSOR_GRANDRIDGE:
      def_or_undef (parse_in, "__grandridge");
      def_or_undef (parse_in, "__grandridge__");
      break;
    case PROCESSOR_KNL:
      def_or_undef (parse_in, "__knl");
      def_or_undef (parse_in, "__knl__");
      break;
    case PROCESSOR_KNM:
      def_or_undef (parse_in, "__knm");
      def_or_undef (parse_in, "__knm__");
      break;
    case PROCESSOR_SKYLAKE:
      def_or_undef (parse_in, "__skylake");
      def_or_undef (parse_in, "__skylake__");
      break;
    case PROCESSOR_SKYLAKE_AVX512:
      def_or_undef (parse_in, "__skylake_avx512");
      def_or_undef (parse_in, "__skylake_avx512__");
      break;
    case PROCESSOR_CANNONLAKE:
      def_or_undef (parse_in, "__cannonlake");
      def_or_undef (parse_in, "__cannonlake__");
      break;
    case PROCESSOR_ICELAKE_CLIENT:
      def_or_undef (parse_in, "__icelake_client");
      def_or_undef (parse_in, "__icelake_client__");
      break;
    case PROCESSOR_ICELAKE_SERVER:
      def_or_undef (parse_in, "__icelake_server");
      def_or_undef (parse_in, "__icelake_server__");
      break;
    case PROCESSOR_CASCADELAKE:
      def_or_undef (parse_in, "__cascadelake");
      def_or_undef (parse_in, "__cascadelake__");
      break;
    case PROCESSOR_TIGERLAKE:
      def_or_undef (parse_in, "__tigerlake");
      def_or_undef (parse_in, "__tigerlake__");
      break;
    case PROCESSOR_COOPERLAKE:
      def_or_undef (parse_in, "__cooperlake");
      def_or_undef (parse_in, "__cooperlake__");
      break;
    case PROCESSOR_SAPPHIRERAPIDS:
      def_or_undef (parse_in, "__sapphirerapids");
      def_or_undef (parse_in, "__sapphirerapids__");
      break;
    case PROCESSOR_GRANITERAPIDS:
      def_or_undef (parse_in, "__graniterapids");
      def_or_undef (parse_in, "__graniterapids__");
      break;
    case PROCESSOR_ALDERLAKE:
      def_or_undef (parse_in, "__alderlake");
      def_or_undef (parse_in, "__alderlake__");
      break;
    case PROCESSOR_ROCKETLAKE:
      def_or_undef (parse_in, "__rocketlake");
      def_or_undef (parse_in, "__rocketlake__");
      break;
    /* use PROCESSOR_max to not set/unset the arch macro.  */
    case PROCESSOR_max:
      break;
    case PROCESSOR_INTEL:
    case PROCESSOR_GENERIC:
      gcc_unreachable ();
    }

  /* Built-ins based on -mtune=.  */
  switch (tune)
    {
    case PROCESSOR_I386:
      def_or_undef (parse_in, "__tune_i386__");
      break;
    case PROCESSOR_I486:
      def_or_undef (parse_in, "__tune_i486__");
      break;
    case PROCESSOR_PENTIUM:
      def_or_undef (parse_in, "__tune_i586__");
      def_or_undef (parse_in, "__tune_pentium__");
      if (last_tune_char == 'x')
	def_or_undef (parse_in, "__tune_pentium_mmx__");
      break;
    case PROCESSOR_PENTIUMPRO:
      def_or_undef (parse_in, "__tune_i686__");
      def_or_undef (parse_in, "__tune_pentiumpro__");
      switch (last_tune_char)
	{
	case '3':
	  def_or_undef (parse_in, "__tune_pentium3__");
	  /* FALLTHRU */
	case '2':
	  def_or_undef (parse_in, "__tune_pentium2__");
	  break;
	}
      break;
    case PROCESSOR_GEODE:
      def_or_undef (parse_in, "__tune_geode__");
      break;
    case PROCESSOR_K6:
      def_or_undef (parse_in, "__tune_k6__");
      if (last_tune_char == '2')
	def_or_undef (parse_in, "__tune_k6_2__");
      else if (last_tune_char == '3')
	def_or_undef (parse_in, "__tune_k6_3__");
      else if (isa_flag & OPTION_MASK_ISA_3DNOW)
	def_or_undef (parse_in, "__tune_k6_3__");
      break;
    case PROCESSOR_ATHLON:
      def_or_undef (parse_in, "__tune_athlon__");
      if (isa_flag & OPTION_MASK_ISA_SSE)
	def_or_undef (parse_in, "__tune_athlon_sse__");
      break;
    case PROCESSOR_K8:
      def_or_undef (parse_in, "__tune_k8__");
      break;
    case PROCESSOR_AMDFAM10:
      def_or_undef (parse_in, "__tune_amdfam10__");
      break;
    case PROCESSOR_BDVER1:
      def_or_undef (parse_in, "__tune_bdver1__");
      break;
    case PROCESSOR_BDVER2:
      def_or_undef (parse_in, "__tune_bdver2__");
      break;
    case PROCESSOR_BDVER3:
      def_or_undef (parse_in, "__tune_bdver3__");
      break;
    case PROCESSOR_BDVER4:
      def_or_undef (parse_in, "__tune_bdver4__");
      break;
    case PROCESSOR_ZNVER1:
      def_or_undef (parse_in, "__tune_znver1__");
      break;
    case PROCESSOR_ZNVER2:
      def_or_undef (parse_in, "__tune_znver2__");
      break;
    case PROCESSOR_ZNVER3:
      def_or_undef (parse_in, "__tune_znver3__");
      break;
    case PROCESSOR_ZNVER4:
      def_or_undef (parse_in, "__tune_znver4__");
      break;
    case PROCESSOR_BTVER1:
      def_or_undef (parse_in, "__tune_btver1__");
      break;
    case PROCESSOR_BTVER2:
      def_or_undef (parse_in, "__tune_btver2__");
       break;
    case PROCESSOR_LUJIAZUI:
      def_or_undef (parse_in, "__tune_lujiazui__");
       break;
    case PROCESSOR_PENTIUM4:
      def_or_undef (parse_in, "__tune_pentium4__");
      break;
    case PROCESSOR_NOCONA:
      def_or_undef (parse_in, "__tune_nocona__");
      break;
    case PROCESSOR_CORE2:
      def_or_undef (parse_in, "__tune_core2__");
      break;
    case PROCESSOR_NEHALEM:
      def_or_undef (parse_in, "__tune_corei7__");
      def_or_undef (parse_in, "__tune_nehalem__");
      break;
    case PROCESSOR_SANDYBRIDGE:
      def_or_undef (parse_in, "__tune_corei7_avx__");
      def_or_undef (parse_in, "__tune_sandybridge__");
      break;
    case PROCESSOR_HASWELL:
      def_or_undef (parse_in, "__tune_core_avx2__");
      def_or_undef (parse_in, "__tune_haswell__");
      break;
    case PROCESSOR_BONNELL:
      def_or_undef (parse_in, "__tune_atom__");
      def_or_undef (parse_in, "__tune_bonnell__");
      break;
    case PROCESSOR_SILVERMONT:
      def_or_undef (parse_in, "__tune_slm__");
      def_or_undef (parse_in, "__tune_silvermont__");
      break;
    case PROCESSOR_GOLDMONT:
      def_or_undef (parse_in, "__tune_goldmont__");
      break;
    case PROCESSOR_GOLDMONT_PLUS:
      def_or_undef (parse_in, "__tune_goldmont_plus__");
      break;
    case PROCESSOR_TREMONT:
      def_or_undef (parse_in, "__tune_tremont__");
      break;
    case PROCESSOR_SIERRAFOREST:
      def_or_undef (parse_in, "__tune_sierraforest__");
      break;
    case PROCESSOR_GRANDRIDGE:
      def_or_undef (parse_in, "__tune_grandridge__");
      break;
    case PROCESSOR_KNL:
      def_or_undef (parse_in, "__tune_knl__");
      break;
    case PROCESSOR_KNM:
      def_or_undef (parse_in, "__tune_knm__");
      break;
    case PROCESSOR_SKYLAKE:
      def_or_undef (parse_in, "__tune_skylake__");
      break;
    case PROCESSOR_SKYLAKE_AVX512:
      def_or_undef (parse_in, "__tune_skylake_avx512__");
      break;
    case PROCESSOR_CANNONLAKE:
      def_or_undef (parse_in, "__tune_cannonlake__");
      break;
    case PROCESSOR_ICELAKE_CLIENT:
      def_or_undef (parse_in, "__tune_icelake_client__");
      break;
    case PROCESSOR_ICELAKE_SERVER:
      def_or_undef (parse_in, "__tune_icelake_server__");
      break;
    case PROCESSOR_LAKEMONT:
      def_or_undef (parse_in, "__tune_lakemont__");
      break;
    case PROCESSOR_CASCADELAKE:
      def_or_undef (parse_in, "__tune_cascadelake__");
      break;
    case PROCESSOR_TIGERLAKE:
      def_or_undef (parse_in, "__tune_tigerlake__");
      break;
    case PROCESSOR_COOPERLAKE:
      def_or_undef (parse_in, "__tune_cooperlake__");
      break;
    case PROCESSOR_SAPPHIRERAPIDS:
      def_or_undef (parse_in, "__tune_sapphirerapids__");
      break;
    case PROCESSOR_ALDERLAKE:
      def_or_undef (parse_in, "__tune_alderlake__");
      break;
    case PROCESSOR_ROCKETLAKE:
      def_or_undef (parse_in, "__tune_rocketlake__");
      break;
    case PROCESSOR_GRANITERAPIDS:
      def_or_undef (parse_in, "__tune_graniterapids__");
      break;
    case PROCESSOR_INTEL:
    case PROCESSOR_GENERIC:
      break;
    /* use PROCESSOR_max to not set/unset the tune macro.  */
    case PROCESSOR_max:
      break;
    }

  switch (ix86_cmodel)
    {
    case CM_SMALL:
    case CM_SMALL_PIC:
      def_or_undef (parse_in, "__code_model_small__");
      break;
    case CM_MEDIUM:
    case CM_MEDIUM_PIC:
      def_or_undef (parse_in, "__code_model_medium__");
      break;
    case CM_LARGE:
    case CM_LARGE_PIC:
      def_or_undef (parse_in, "__code_model_large__");
      break;
    case CM_32:
      def_or_undef (parse_in, "__code_model_32__");
      break;
    case CM_KERNEL:
      def_or_undef (parse_in, "__code_model_kernel__");
      break;
    default:
      ;
    }

  if (isa_flag2 & OPTION_MASK_ISA2_WBNOINVD)
    def_or_undef (parse_in, "__WBNOINVD__");
  if (isa_flag2 & OPTION_MASK_ISA2_AVX512VP2INTERSECT)
    def_or_undef (parse_in, "__AVX512VP2INTERSECT__");
  if (isa_flag & OPTION_MASK_ISA_MMX)
    def_or_undef (parse_in, "__MMX__");
  if (isa_flag & OPTION_MASK_ISA_3DNOW)
    def_or_undef (parse_in, "__3dNOW__");
  if (isa_flag & OPTION_MASK_ISA_3DNOW_A)
    def_or_undef (parse_in, "__3dNOW_A__");
  if (isa_flag & OPTION_MASK_ISA_SSE)
    def_or_undef (parse_in, "__SSE__");
  if (isa_flag & OPTION_MASK_ISA_SSE2)
    def_or_undef (parse_in, "__SSE2__");
  if (isa_flag & OPTION_MASK_ISA_SSE3)
    def_or_undef (parse_in, "__SSE3__");
  if (isa_flag & OPTION_MASK_ISA_SSSE3)
    def_or_undef (parse_in, "__SSSE3__");
  if (isa_flag & OPTION_MASK_ISA_SSE4_1)
    def_or_undef (parse_in, "__SSE4_1__");
  if (isa_flag & OPTION_MASK_ISA_SSE4_2)
    def_or_undef (parse_in, "__SSE4_2__");
  if (isa_flag & OPTION_MASK_ISA_AES)
    def_or_undef (parse_in, "__AES__");
  if (isa_flag & OPTION_MASK_ISA_SHA)
    def_or_undef (parse_in, "__SHA__");
  if (isa_flag & OPTION_MASK_ISA_PCLMUL)
    def_or_undef (parse_in, "__PCLMUL__");
  if (isa_flag & OPTION_MASK_ISA_AVX)
    def_or_undef (parse_in, "__AVX__");
  if (isa_flag & OPTION_MASK_ISA_AVX2)
    def_or_undef (parse_in, "__AVX2__");
  if (isa_flag & OPTION_MASK_ISA_AVX512F)
    def_or_undef (parse_in, "__AVX512F__");
  if (isa_flag & OPTION_MASK_ISA_AVX512ER)
    def_or_undef (parse_in, "__AVX512ER__");
  if (isa_flag & OPTION_MASK_ISA_AVX512CD)
    def_or_undef (parse_in, "__AVX512CD__");
  if (isa_flag & OPTION_MASK_ISA_AVX512PF)
    def_or_undef (parse_in, "__AVX512PF__");
  if (isa_flag & OPTION_MASK_ISA_AVX512DQ)
    def_or_undef (parse_in, "__AVX512DQ__");
  if (isa_flag & OPTION_MASK_ISA_AVX512BW)
    def_or_undef (parse_in, "__AVX512BW__");
  if (isa_flag & OPTION_MASK_ISA_AVX512VL)
    def_or_undef (parse_in, "__AVX512VL__");
  if (isa_flag & OPTION_MASK_ISA_AVX512VBMI)
    def_or_undef (parse_in, "__AVX512VBMI__");
  if (isa_flag & OPTION_MASK_ISA_AVX512IFMA)
    def_or_undef (parse_in, "__AVX512IFMA__");
  if (isa_flag2 & OPTION_MASK_ISA2_AVX5124VNNIW)
    def_or_undef (parse_in, "__AVX5124VNNIW__");
  if (isa_flag & OPTION_MASK_ISA_AVX512VBMI2)
    def_or_undef (parse_in, "__AVX512VBMI2__");
  if (isa_flag & OPTION_MASK_ISA_AVX512VNNI)
    def_or_undef (parse_in, "__AVX512VNNI__");
  if (isa_flag2 & OPTION_MASK_ISA2_PCONFIG)
    def_or_undef (parse_in, "__PCONFIG__");
  if (isa_flag2 & OPTION_MASK_ISA2_SGX)
    def_or_undef (parse_in, "__SGX__");
  if (isa_flag2 & OPTION_MASK_ISA2_AVX5124FMAPS)
    def_or_undef (parse_in, "__AVX5124FMAPS__");
  if (isa_flag & OPTION_MASK_ISA_AVX512BITALG)
    def_or_undef (parse_in, "__AVX512BITALG__");
  if (isa_flag & OPTION_MASK_ISA_AVX512VPOPCNTDQ)
    def_or_undef (parse_in, "__AVX512VPOPCNTDQ__");
  if (isa_flag & OPTION_MASK_ISA_FMA)
    def_or_undef (parse_in, "__FMA__");
  if (isa_flag & OPTION_MASK_ISA_RTM)
    def_or_undef (parse_in, "__RTM__");
  if (isa_flag & OPTION_MASK_ISA_SSE4A)
    def_or_undef (parse_in, "__SSE4A__");
  if (isa_flag & OPTION_MASK_ISA_FMA4)
    def_or_undef (parse_in, "__FMA4__");
  if (isa_flag & OPTION_MASK_ISA_XOP)
    def_or_undef (parse_in, "__XOP__");
  if (isa_flag & OPTION_MASK_ISA_LWP)
    def_or_undef (parse_in, "__LWP__");
  if (isa_flag & OPTION_MASK_ISA_ABM)
    def_or_undef (parse_in, "__ABM__");
  if (isa_flag & OPTION_MASK_ISA_BMI)
    def_or_undef (parse_in, "__BMI__");
  if (isa_flag & OPTION_MASK_ISA_BMI2)
    def_or_undef (parse_in, "__BMI2__");
  if (isa_flag & OPTION_MASK_ISA_LZCNT)
    def_or_undef (parse_in, "__LZCNT__");
  if (isa_flag & OPTION_MASK_ISA_TBM)
    def_or_undef (parse_in, "__TBM__");
  if (isa_flag & OPTION_MASK_ISA_CRC32)
    def_or_undef (parse_in, "__CRC32__");
  if (isa_flag & OPTION_MASK_ISA_POPCNT)
    def_or_undef (parse_in, "__POPCNT__");
  if (isa_flag & OPTION_MASK_ISA_FSGSBASE)
    def_or_undef (parse_in, "__FSGSBASE__");
  if (isa_flag & OPTION_MASK_ISA_RDRND)
    def_or_undef (parse_in, "__RDRND__");
  if (isa_flag & OPTION_MASK_ISA_F16C)
    def_or_undef (parse_in, "__F16C__");
  if (isa_flag & OPTION_MASK_ISA_RDSEED)
    def_or_undef (parse_in, "__RDSEED__");
  if (isa_flag & OPTION_MASK_ISA_PRFCHW)
    def_or_undef (parse_in, "__PRFCHW__");
  if (isa_flag & OPTION_MASK_ISA_ADX)
    def_or_undef (parse_in, "__ADX__");
  if (isa_flag & OPTION_MASK_ISA_FXSR)
    def_or_undef (parse_in, "__FXSR__");
  if (isa_flag & OPTION_MASK_ISA_XSAVE)
    def_or_undef (parse_in, "__XSAVE__");
  if (isa_flag & OPTION_MASK_ISA_XSAVEOPT)
    def_or_undef (parse_in, "__XSAVEOPT__");
  if (isa_flag & OPTION_MASK_ISA_PREFETCHWT1)
    def_or_undef (parse_in, "__PREFETCHWT1__");
  if ((fpmath & FPMATH_SSE) && (isa_flag & OPTION_MASK_ISA_SSE))
    def_or_undef (parse_in, "__SSE_MATH__");
  if ((fpmath & FPMATH_SSE) && (isa_flag & OPTION_MASK_ISA_SSE2))
    def_or_undef (parse_in, "__SSE2_MATH__");
  if (isa_flag & OPTION_MASK_ISA_CLFLUSHOPT)
    def_or_undef (parse_in, "__CLFLUSHOPT__");
  if (isa_flag2 & OPTION_MASK_ISA2_CLZERO)
    def_or_undef (parse_in, "__CLZERO__");
  if (isa_flag & OPTION_MASK_ISA_XSAVEC)
    def_or_undef (parse_in, "__XSAVEC__");
  if (isa_flag & OPTION_MASK_ISA_XSAVES)
    def_or_undef (parse_in, "__XSAVES__");
  if (isa_flag & OPTION_MASK_ISA_CLWB)
    def_or_undef (parse_in, "__CLWB__");
  if (isa_flag2 & OPTION_MASK_ISA2_MWAITX)
    def_or_undef (parse_in, "__MWAITX__");
  if (isa_flag & OPTION_MASK_ISA_PKU)
    def_or_undef (parse_in, "__PKU__");
  if (isa_flag2 & OPTION_MASK_ISA2_RDPID)
    def_or_undef (parse_in, "__RDPID__");
  if (isa_flag & OPTION_MASK_ISA_GFNI)
    def_or_undef (parse_in, "__GFNI__");
  if ((isa_flag & OPTION_MASK_ISA_SHSTK))
    def_or_undef (parse_in, "__SHSTK__");
  if (isa_flag2 & OPTION_MASK_ISA2_VAES)
    def_or_undef (parse_in, "__VAES__");
  if (isa_flag & OPTION_MASK_ISA_VPCLMULQDQ)
    def_or_undef (parse_in, "__VPCLMULQDQ__");
  if (isa_flag & OPTION_MASK_ISA_MOVDIRI)
    def_or_undef (parse_in, "__MOVDIRI__");
  if (isa_flag2 & OPTION_MASK_ISA2_MOVDIR64B)
    def_or_undef (parse_in, "__MOVDIR64B__");
  if (isa_flag2 & OPTION_MASK_ISA2_WAITPKG)
    def_or_undef (parse_in, "__WAITPKG__");
  if (isa_flag2 & OPTION_MASK_ISA2_CLDEMOTE)
    def_or_undef (parse_in, "__CLDEMOTE__");
  if (isa_flag2 & OPTION_MASK_ISA2_SERIALIZE)
    def_or_undef (parse_in, "__SERIALIZE__");
  if (isa_flag2 & OPTION_MASK_ISA2_PTWRITE)
    def_or_undef (parse_in, "__PTWRITE__");
  if (isa_flag2 & OPTION_MASK_ISA2_AVX512BF16)
    def_or_undef (parse_in, "__AVX512BF16__");
  if (isa_flag2 & OPTION_MASK_ISA2_AVX512FP16)
    def_or_undef (parse_in, "__AVX512FP16__");
  if (TARGET_MMX_WITH_SSE)
    def_or_undef (parse_in, "__MMX_WITH_SSE__");
  if (isa_flag2 & OPTION_MASK_ISA2_ENQCMD)
    def_or_undef (parse_in, "__ENQCMD__");
  if (isa_flag2 & OPTION_MASK_ISA2_TSXLDTRK)
    def_or_undef (parse_in, "__TSXLDTRK__");
  if (isa_flag2 & OPTION_MASK_ISA2_AMX_TILE)
    def_or_undef (parse_in, "__AMX_TILE__");
  if (isa_flag2 & OPTION_MASK_ISA2_AMX_INT8)
    def_or_undef (parse_in, "__AMX_INT8__");
  if (isa_flag2 & OPTION_MASK_ISA2_AMX_BF16)
    def_or_undef (parse_in, "__AMX_BF16__");
  if (isa_flag & OPTION_MASK_ISA_SAHF)
    def_or_undef (parse_in, "__LAHF_SAHF__");
  if (isa_flag2 & OPTION_MASK_ISA2_MOVBE)
    def_or_undef (parse_in, "__MOVBE__");
  if (isa_flag2 & OPTION_MASK_ISA2_UINTR)
    def_or_undef (parse_in, "__UINTR__");
  if (isa_flag2 & OPTION_MASK_ISA2_HRESET)
    def_or_undef (parse_in, "__HRESET__");
  if (isa_flag2 & OPTION_MASK_ISA2_KL)
    def_or_undef (parse_in, "__KL__");
  if (isa_flag2 & OPTION_MASK_ISA2_WIDEKL)
    def_or_undef (parse_in, "__WIDEKL__");
  if (isa_flag2 & OPTION_MASK_ISA2_AVXVNNI)
    def_or_undef (parse_in, "__AVXVNNI__");
  if (isa_flag2 & OPTION_MASK_ISA2_AVXIFMA)
    def_or_undef (parse_in, "__AVXIFMA__");
  if (isa_flag2 & OPTION_MASK_ISA2_AVXVNNIINT8)
    def_or_undef (parse_in, "__AVXVNNIINT8__");
  if (isa_flag2 & OPTION_MASK_ISA2_AVXNECONVERT)
    def_or_undef (parse_in, "__AVXNECONVERT__");
  if (isa_flag2 & OPTION_MASK_ISA2_CMPCCXADD)
    def_or_undef (parse_in, "__CMPCCXADD__");
  if (isa_flag2 & OPTION_MASK_ISA2_AMX_FP16)
    def_or_undef (parse_in, "__AMX_FP16__");
  if (isa_flag2 & OPTION_MASK_ISA2_PREFETCHI)
    def_or_undef (parse_in, "__PREFETCHI__");
  if (isa_flag2 & OPTION_MASK_ISA2_RAOINT)
    def_or_undef (parse_in, "__RAOINT__");
  if (TARGET_IAMCU)
    {
      def_or_undef (parse_in, "__iamcu");
      def_or_undef (parse_in, "__iamcu__");
    }
}


/* Hook to validate the current #pragma GCC target and set the state, and
   update the macros based on what was changed.  If ARGS is NULL, then
   POP_TARGET is used to reset the options.  */

static bool
ix86_pragma_target_parse (tree args, tree pop_target)
{
  tree prev_tree
    = build_target_option_node (&global_options, &global_options_set);
  tree cur_tree;
  struct cl_target_option *prev_opt;
  struct cl_target_option *cur_opt;
  HOST_WIDE_INT prev_isa;
  HOST_WIDE_INT cur_isa;
  HOST_WIDE_INT diff_isa;
  HOST_WIDE_INT prev_isa2;
  HOST_WIDE_INT cur_isa2;
  HOST_WIDE_INT diff_isa2;
  enum processor_type prev_arch;
  enum processor_type prev_tune;
  enum processor_type cur_arch;
  enum processor_type cur_tune;

  if (! args)
    {
      cur_tree = (pop_target ? pop_target : target_option_default_node);
      cl_target_option_restore (&global_options, &global_options_set,
				TREE_TARGET_OPTION (cur_tree));
    }
  else
    {
      cur_tree = ix86_valid_target_attribute_tree (NULL_TREE, args,
						   &global_options,
						   &global_options_set, 0);
      if (!cur_tree || cur_tree == error_mark_node)
       {
         cl_target_option_restore (&global_options, &global_options_set,
                                   TREE_TARGET_OPTION (prev_tree));
         return false;
       }
    }

  target_option_current_node = cur_tree;
  ix86_reset_previous_fndecl ();

  /* Figure out the previous/current isa, arch, tune and the differences.  */
  prev_opt  = TREE_TARGET_OPTION (prev_tree);
  cur_opt   = TREE_TARGET_OPTION (cur_tree);
  prev_isa  = prev_opt->x_ix86_isa_flags;
  cur_isa   = cur_opt->x_ix86_isa_flags;
  diff_isa  = (prev_isa ^ cur_isa);
  prev_isa2  = prev_opt->x_ix86_isa_flags2;
  cur_isa2   = cur_opt->x_ix86_isa_flags2;
  diff_isa2  = (prev_isa2 ^ cur_isa2);
  prev_arch = (enum processor_type) prev_opt->arch;
  prev_tune = (enum processor_type) prev_opt->tune;
  cur_arch  = (enum processor_type) cur_opt->arch;
  cur_tune  = (enum processor_type) cur_opt->tune;

  /* If the same processor is used for both previous and current options, don't
     change the macros.  */
  if (cur_arch == prev_arch)
    cur_arch = prev_arch = PROCESSOR_max;

  if (cur_tune == prev_tune)
    cur_tune = prev_tune = PROCESSOR_max;

  /* Undef all of the macros for that are no longer current.  */
  cpp_force_token_locations (parse_in, BUILTINS_LOCATION);
  ix86_target_macros_internal (prev_isa & diff_isa,
			       prev_isa2 & diff_isa2,
			       prev_arch,
			       prev_tune,
			       (enum fpmath_unit) prev_opt->x_ix86_fpmath,
			       cpp_undef);
  cpp_stop_forcing_token_locations (parse_in);

  /* For the definitions, ensure all newly defined macros are considered
     as used for -Wunused-macros.  There is no point warning about the
     compiler predefined macros.  */
  cpp_options *cpp_opts = cpp_get_options (parse_in);
  unsigned char saved_warn_unused_macros = cpp_opts->warn_unused_macros;
  cpp_opts->warn_unused_macros = 0;

  /* Define all of the macros for new options that were just turned on.  */
  cpp_force_token_locations (parse_in, BUILTINS_LOCATION);
  ix86_target_macros_internal (cur_isa & diff_isa,
			       cur_isa2 & diff_isa2,
			       cur_arch,
			       cur_tune,
			       (enum fpmath_unit) cur_opt->x_ix86_fpmath,
			       cpp_define);
  cpp_stop_forcing_token_locations (parse_in);

  cpp_opts->warn_unused_macros = saved_warn_unused_macros;

  return true;
}

/* Function to tell the preprocessor about the defines for the current target.  */

void
ix86_target_macros (void)
{
  /* 32/64-bit won't change with target specific options, so do the assert and
     builtin_define_std calls here.  */
  if (TARGET_64BIT)
    {
      cpp_assert (parse_in, "cpu=x86_64");
      cpp_assert (parse_in, "machine=x86_64");
      cpp_define (parse_in, "__amd64");
      cpp_define (parse_in, "__amd64__");
      cpp_define (parse_in, "__x86_64");
      cpp_define (parse_in, "__x86_64__");
      if (TARGET_X32)
	{
	  cpp_define (parse_in, "_ILP32");
	  cpp_define (parse_in, "__ILP32__");
	}
    }
  else
    {
      cpp_assert (parse_in, "cpu=i386");
      cpp_assert (parse_in, "machine=i386");
      builtin_define_std ("i386");
      cpp_define (parse_in, "_ILP32");
      cpp_define (parse_in, "__ILP32__");
    }

  if (!TARGET_80387)
    cpp_define (parse_in, "_SOFT_FLOAT");

  if (TARGET_LONG_DOUBLE_64)
    cpp_define (parse_in, "__LONG_DOUBLE_64__");

  if (TARGET_LONG_DOUBLE_128)
    cpp_define (parse_in, "__LONG_DOUBLE_128__");

  cpp_define_formatted (parse_in, "__SIZEOF_FLOAT80__=%d",
			GET_MODE_SIZE (XFmode));

  cpp_define (parse_in, "__SIZEOF_FLOAT128__=16");

  cpp_define_formatted (parse_in, "__ATOMIC_HLE_ACQUIRE=%d", IX86_HLE_ACQUIRE);
  cpp_define_formatted (parse_in, "__ATOMIC_HLE_RELEASE=%d", IX86_HLE_RELEASE);

  cpp_define (parse_in, "__GCC_ASM_FLAG_OUTPUTS__");

  ix86_target_macros_internal (ix86_isa_flags,
			       ix86_isa_flags2,
			       ix86_arch,
			       ix86_tune,
			       ix86_fpmath,
			       cpp_define);

  cpp_define (parse_in, "__SEG_FS");
  cpp_define (parse_in, "__SEG_GS");

  if (flag_cf_protection != CF_NONE)
    cpp_define_formatted (parse_in, "__CET__=%d", flag_cf_protection & ~CF_SET);
}


/* Register target pragmas.  We need to add the hook for parsing #pragma GCC
   option here rather than in i386.cc since it will pull in various preprocessor
   functions, and those are not present in languages like fortran without a
   preprocessor.  */

void
ix86_register_pragmas (void)
{
  /* Update pragma hook to allow parsing #pragma GCC target.  */
  targetm.target_option.pragma_parse = ix86_pragma_target_parse;

  c_register_addr_space ("__seg_fs", ADDR_SPACE_SEG_FS);
  c_register_addr_space ("__seg_gs", ADDR_SPACE_SEG_GS);

#ifdef REGISTER_SUBTARGET_PRAGMAS
  REGISTER_SUBTARGET_PRAGMAS ();
#endif
}
