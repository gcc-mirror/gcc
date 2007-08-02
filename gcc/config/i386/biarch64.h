/* Make configure files to produce biarch compiler defaulting to 64bit mode.
   This file must be included very first, while the OS specific file later
   to overwrite otherwise wrong defaults. 
   Copyright (C) 2001, 2007 Free Software Foundation, Inc.
   Contributed by Bo Thorsen <bo@suse.de>.

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

#define TARGET_64BIT_DEFAULT OPTION_MASK_ISA_64BIT
#define TARGET_BI_ARCH 1
