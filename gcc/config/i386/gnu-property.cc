/* Functions for x86 GNU property.
   Copyright (C) 2017-2025 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "output.h"
#include "linux-common.h"
#include "i386-protos.h"

static void
emit_gnu_property (unsigned int type, unsigned int data)
{
  int p2align = ptr_mode == SImode ? 2 : 3;

  switch_to_section (get_section (".note.gnu.property",
				  SECTION_NOTYPE, NULL));

  ASM_OUTPUT_ALIGN (asm_out_file, p2align);
  /* name length.  */
  fprintf (asm_out_file, ASM_LONG "1f - 0f\n");
  /* data length.  */
  fprintf (asm_out_file, ASM_LONG "4f - 1f\n");
  /* note type: NT_GNU_PROPERTY_TYPE_0.  */
  fprintf (asm_out_file, ASM_LONG "5\n");
  fprintf (asm_out_file, "0:\n");
  /* vendor name: "GNU".  */
  fprintf (asm_out_file, STRING_ASM_OP "\"GNU\"\n");
  fprintf (asm_out_file, "1:\n");
  ASM_OUTPUT_ALIGN (asm_out_file, p2align);
  /* pr_type.  */
  fprintf (asm_out_file, ASM_LONG "0x%x\n", type);
  /* pr_datasz.  */
  fprintf (asm_out_file, ASM_LONG "3f - 2f\n");
  fprintf (asm_out_file, "2:\n");
  fprintf (asm_out_file, ASM_LONG "0x%x\n", data);
  fprintf (asm_out_file, "3:\n");
  ASM_OUTPUT_ALIGN (asm_out_file, p2align);
  fprintf (asm_out_file, "4:\n");
}

void
file_end_indicate_exec_stack_and_gnu_property (void)
{
  file_end_indicate_exec_stack ();

  if (flag_cf_protection == CF_NONE
      && !ix86_needed
      && !ix86_has_no_direct_extern_access)
    return;

  unsigned int feature_1 = 0;

  if (flag_cf_protection & CF_BRANCH)
    /* GNU_PROPERTY_X86_FEATURE_1_IBT.  */
    feature_1 |= 0x1;

  if (flag_cf_protection & CF_RETURN)
    /* GNU_PROPERTY_X86_FEATURE_1_SHSTK.  */
    feature_1 |= 0x2;

  /* Generate GNU_PROPERTY_X86_FEATURE_1_AND.  */
  if (feature_1)
    emit_gnu_property (0xc0000002, feature_1);

  unsigned int isa_1 = 0;
  if (ix86_needed)
    {
      /* GNU_PROPERTY_X86_ISA_1_BASELINE.  */
      if (TARGET_64BIT
	  || TARGET_FXSR
	  || TARGET_80387
	  || TARGET_MMX
	  || TARGET_SSE
	  || TARGET_SSE2)
	isa_1 |= 1 << 0;

      /* GNU_PROPERTY_X86_ISA_1_V2.  */
      if (TARGET_CMPXCHG16B
	  || (TARGET_64BIT && TARGET_SAHF)
	  || TARGET_POPCNT
	  || TARGET_SSE3
	  || TARGET_SSSE3
	  || TARGET_SSE4_1
	  || TARGET_SSE4_2)
	isa_1 |= 1 << 1;

      /* GNU_PROPERTY_X86_ISA_1_V3.  */
      if (TARGET_AVX
	  || TARGET_AVX2
	  || TARGET_F16C
	  || TARGET_FMA
	  || TARGET_LZCNT
	  || TARGET_MOVBE
	  || TARGET_XSAVE)
	isa_1 |= 1 << 2;

      /* GNU_PROPERTY_X86_ISA_1_V4.  */
      if (TARGET_AVX512F
	  || TARGET_AVX512BW
	  || TARGET_AVX512CD
	  || TARGET_AVX512DQ
	  || TARGET_AVX512VL)
	isa_1 |= 1 << 3;
    }

  /* Generate GNU_PROPERTY_X86_ISA_1_NEEDED.  */
  if (isa_1)
    emit_gnu_property (0xc0008002, isa_1);

  if (ix86_has_no_direct_extern_access)
    /* Emite a GNU_PROPERTY_1_NEEDED note with
       GNU_PROPERTY_1_NEEDED_INDIRECT_EXTERN_ACCESS.  */
    emit_gnu_property (0xb0008000, (1U << 0));
}
