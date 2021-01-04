/* Subroutines for DJGPP.
   Copyright (C) 2013-2021 Free Software Foundation, Inc.

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

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "output.h"
#include "lto-section-names.h"

void
i386_djgpp_asm_named_section(const char *name, unsigned int flags,
			     tree)
{
  char flagchars[8], *f = flagchars;

  if (flags & SECTION_WRITE)
    *f++ = 'w';
  if (flags & SECTION_CODE)
    *f++ = 'x';

  /* LTO sections need 1-byte alignment to avoid confusing the
     zlib decompression algorithm with trailing zero pad bytes.  */
  if (strncmp (name, LTO_SECTION_NAME_PREFIX,
			strlen (LTO_SECTION_NAME_PREFIX)) == 0)
    *f++ = '0';

  *f++ = '\0';

  fprintf (asm_out_file, "\t.section\t%s,\"%s\"\n", name, flagchars);
}

/* Kludge because of missing COFF support for early LTO debug.  */

static enum debug_info_levels saved_debug_info_level;

void
i386_djgpp_asm_lto_start (void)
{
  saved_debug_info_level = debug_info_level;
  debug_info_level = DINFO_LEVEL_NONE;
}

void
i386_djgpp_asm_lto_end (void)
{
  debug_info_level = saved_debug_info_level;
}
