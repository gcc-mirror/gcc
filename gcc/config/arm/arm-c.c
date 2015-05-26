/* Copyright (C) 2007-2015 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3, or (at your option) any later
   version.

   GCC is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tm_p.h"
#include "hash-set.h"
#include "machmode.h"
#include "vec.h"
#include "double-int.h"
#include "input.h"
#include "alias.h"
#include "symtab.h"
#include "wide-int.h"
#include "inchash.h"
#include "tree.h"
#include "c-family/c-common.h"

/* Output C specific EABI object attributes.  These can not be done in
   arm.c because they require information from the C frontend.  */

static void
arm_output_c_attributes (void)
{
  int wchar_size = (int)(TYPE_PRECISION (wchar_type_node) / BITS_PER_UNIT);
  arm_emit_eabi_attribute ("Tag_ABI_PCS_wchar_t", 18, wchar_size);
}


/* Setup so that common code calls arm_output_c_attributes.  */

void
arm_lang_object_attributes_init (void)
{
  arm_lang_output_object_attributes_hook = arm_output_c_attributes;
}

#define builtin_define(TXT) cpp_define (pfile, TXT)
#define builtin_assert(TXT) cpp_assert (pfile, TXT)

void
arm_cpu_cpp_builtins (struct cpp_reader * pfile)
{
  if (TARGET_DSP_MULTIPLY)
    builtin_define ("__ARM_FEATURE_DSP");
  if (TARGET_ARM_QBIT)
    builtin_define ("__ARM_FEATURE_QBIT");
  if (TARGET_ARM_SAT)
    builtin_define ("__ARM_FEATURE_SAT");
  if (TARGET_CRYPTO)
    builtin_define ("__ARM_FEATURE_CRYPTO");
  if (unaligned_access)
    builtin_define ("__ARM_FEATURE_UNALIGNED");
  if (TARGET_CRC32)
    builtin_define ("__ARM_FEATURE_CRC32");
  if (TARGET_32BIT)
    builtin_define ("__ARM_32BIT_STATE");
  if (TARGET_ARM_FEATURE_LDREX)
    builtin_define_with_int_value ("__ARM_FEATURE_LDREX", 
				   TARGET_ARM_FEATURE_LDREX);
  if ((TARGET_ARM_ARCH >= 5 && !TARGET_THUMB)
      || TARGET_ARM_ARCH_ISA_THUMB >=2)
    builtin_define ("__ARM_FEATURE_CLZ");
  if (TARGET_INT_SIMD)
    builtin_define ("__ARM_FEATURE_SIMD32");

  builtin_define_with_int_value ("__ARM_SIZEOF_MINIMAL_ENUM",
				 flag_short_enums ? 1 : 4);
  builtin_define_type_sizeof ("__ARM_SIZEOF_WCHAR_T", wchar_type_node);
  if (TARGET_ARM_ARCH_PROFILE)
    builtin_define_with_int_value ("__ARM_ARCH_PROFILE",
				   TARGET_ARM_ARCH_PROFILE);

  /* Define __arm__ even when in thumb mode, for
     consistency with armcc.  */
  builtin_define ("__arm__");
  if (TARGET_ARM_ARCH)
    builtin_define_with_int_value ("__ARM_ARCH", TARGET_ARM_ARCH);
  if (arm_arch_notm)
    builtin_define ("__ARM_ARCH_ISA_ARM");
  builtin_define ("__APCS_32__");
  if (TARGET_THUMB)
    builtin_define ("__thumb__");
  if (TARGET_THUMB2)
    builtin_define ("__thumb2__");
  if (TARGET_ARM_ARCH_ISA_THUMB)
    builtin_define_with_int_value ("__ARM_ARCH_ISA_THUMB",
				   TARGET_ARM_ARCH_ISA_THUMB);

  if (TARGET_BIG_END)
    {
      builtin_define ("__ARMEB__");
      builtin_define ("__ARM_BIG_ENDIAN");
      if (TARGET_THUMB)
	builtin_define ("__THUMBEB__");
    }
  else
    {
      builtin_define ("__ARMEL__");
      if (TARGET_THUMB)
	builtin_define ("__THUMBEL__");
    }

  if (TARGET_SOFT_FLOAT)
    builtin_define ("__SOFTFP__");

  if (TARGET_VFP)
    builtin_define ("__VFP_FP__");
	
  if (TARGET_ARM_FP)
    builtin_define_with_int_value ("__ARM_FP", TARGET_ARM_FP);
  if (arm_fp16_format == ARM_FP16_FORMAT_IEEE)
    builtin_define ("__ARM_FP16_FORMAT_IEEE");
  if (arm_fp16_format == ARM_FP16_FORMAT_ALTERNATIVE)
    builtin_define ("__ARM_FP16_FORMAT_ALTERNATIVE");
  if (TARGET_FMA)
    builtin_define ("__ARM_FEATURE_FMA");

  if (TARGET_NEON)
    {
      builtin_define ("__ARM_NEON__");
      builtin_define ("__ARM_NEON");
    }
  if (TARGET_NEON_FP)
    builtin_define_with_int_value ("__ARM_NEON_FP", TARGET_NEON_FP);
  
  /* Add a define for interworking. Needed when building libgcc.a.  */
  if (arm_cpp_interwork)
    builtin_define ("__THUMB_INTERWORK__");

  builtin_assert ("cpu=arm");
  builtin_assert ("machine=arm");

  builtin_define (arm_arch_name);
  if (arm_arch_xscale)
    builtin_define ("__XSCALE__");
  if (arm_arch_iwmmxt)
    {
      builtin_define ("__IWMMXT__");
      builtin_define ("__ARM_WMMX");
    }
  if (arm_arch_iwmmxt2)
    builtin_define ("__IWMMXT2__");
  if (TARGET_AAPCS_BASED)
    {
      if (arm_pcs_default == ARM_PCS_AAPCS_VFP)
	builtin_define ("__ARM_PCS_VFP");
      else if (arm_pcs_default == ARM_PCS_AAPCS)
	builtin_define ("__ARM_PCS");
      builtin_define ("__ARM_EABI__");
    }
  if (TARGET_IDIV)
    {
      builtin_define ("__ARM_ARCH_EXT_IDIV__");
      builtin_define ("__ARM_FEATURE_IDIV");
    }
  if (inline_asm_unified)
    builtin_define ("__ARM_ASM_SYNTAX_UNIFIED__");
}
