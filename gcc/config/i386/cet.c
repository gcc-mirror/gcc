/* Functions for CET/x86.
   Copyright (C) 2017 Free Software Foundation, Inc.

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

void
file_end_indicate_exec_stack_and_cet (void)
{
  file_end_indicate_exec_stack ();

  if (flag_cf_protection == CF_NONE)
    return;

  unsigned int feature_1 = 0;

  if (TARGET_IBT)
    /* GNU_PROPERTY_X86_FEATURE_1_IBT.  */
    feature_1 |= 0x1;

  if (TARGET_SHSTK)
    /* GNU_PROPERTY_X86_FEATURE_1_SHSTK.  */
    feature_1 |= 0x2;

  if (feature_1)
    {
      int p2align = ptr_mode == SImode ? 2 : 3;

      /* Generate GNU_PROPERTY_X86_FEATURE_1_XXX.  */
      switch_to_section (get_section (".note.gnu.property",
				      SECTION_NOTYPE, NULL));

      ASM_OUTPUT_ALIGN (asm_out_file, p2align);
      /* name length.  */
      fprintf (asm_out_file, ASM_LONG " 1f - 0f\n");
      /* data length.  */
      fprintf (asm_out_file, ASM_LONG " 4f - 1f\n");
      /* note type: NT_GNU_PROPERTY_TYPE_0.  */
      fprintf (asm_out_file, ASM_LONG " 5\n");
      ASM_OUTPUT_LABEL (asm_out_file, "0");
      /* vendor name: "GNU".  */
      fprintf (asm_out_file, STRING_ASM_OP " \"GNU\"\n");
      ASM_OUTPUT_LABEL (asm_out_file, "1");
      ASM_OUTPUT_ALIGN (asm_out_file, p2align);
      /* pr_type: GNU_PROPERTY_X86_FEATURE_1_AND.  */
      fprintf (asm_out_file, ASM_LONG " 0xc0000002\n");
      /* pr_datasz.  */\
      fprintf (asm_out_file, ASM_LONG " 3f - 2f\n");
      ASM_OUTPUT_LABEL (asm_out_file, "2");
      /* GNU_PROPERTY_X86_FEATURE_1_XXX.  */
      fprintf (asm_out_file, ASM_LONG " 0x%x\n", feature_1);
      ASM_OUTPUT_LABEL (asm_out_file, "3");
      ASM_OUTPUT_ALIGN (asm_out_file, p2align);
      ASM_OUTPUT_LABEL (asm_out_file, "4");
    }
}
