/* Copyright (C) 2007 Free Software Foundation, Inc.

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
#include "tree.h"
#include "output.h"
#include "c-common.h"


/* Output C specific EABI object attributes.  These can not be done in
   arm.c because they require information from the C frontend.  */

static void arm_output_c_attributes(void)
{
  /* Tag_ABI_PCS_wchar_t.  */
  asm_fprintf (asm_out_file, "\t.eabi_attribute 18, %d\n",
	       (int)(TYPE_PRECISION (wchar_type_node) / BITS_PER_UNIT));
}


/* Setup so that common code calls arm_output_c_attributes.  */

void arm_lang_object_attributes_init(void)
{
  arm_lang_output_object_attributes_hook = arm_output_c_attributes;
}
