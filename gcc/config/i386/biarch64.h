/* Make configure files to produce biarch compiler defaulting to 64bit mode.
   This file must be included very first, while the OS specific file later
   to overwrite otherwise wrong defaults. 
   Copyright (C) 2001 Free Software Foundation, Inc.
   Contributed by Bo Thorsen <bo@suse.de>.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#define TARGET_64BIT_DEFAULT
#define TARGET_BI_ARCH
