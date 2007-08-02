/* Common VxWorks target definitions for GNU compiler.
   Copyright (C) 2007
   Free Software Foundation, Inc.
   Contributed by CodeSourcery, Inc.

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
#include "target.h"
#include "toplev.h"
#include "output.h"
#include "tm.h"

/* Like default_named_section_asm_out_constructor, except that even
   constructors with DEFAULT_INIT_PRIORITY must go in a numbered
   section on VxWorks.  The VxWorks runtime uses a clever trick to get
   the sentinel entry (-1) inserted at the beginning of the .ctors
   segment.  This trick will not work if we ever generate any entries
   in plain .ctors sections; we must always use .ctors.PRIORITY.  */

void
vxworks_asm_out_constructor (rtx symbol, int priority)
{
  section *sec;

  sec = get_cdtor_priority_section (priority,
				    /*constructor_p=*/true);
  assemble_addr_to_section (symbol, sec);
}

/* See comment for vxworks_asm_out_constructor.  */

void
vxworks_asm_out_destructor (rtx symbol, int priority)
{
  section *sec;

  sec = get_cdtor_priority_section (priority,
				    /*constructor_p=*/false);
  assemble_addr_to_section (symbol, sec);
}

/* Do VxWorks-specific parts of OVERRIDE_OPTIONS.  */

void
vxworks_override_options (void)
{
  /* We can use .ctors/.dtors sections only in RTP mode.  */
  targetm.have_ctors_dtors = TARGET_VXWORKS_RTP;

  /* PIC is only supported for RTPs.  */
  if (flag_pic && !TARGET_VXWORKS_RTP)
    error ("PIC is only supported for RTPs");
}
