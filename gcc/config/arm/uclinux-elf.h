/* Definitions for ARM running ucLinux using ELF
   Copyright (C) 1999, 2001 Free Software Foundation, Inc.
   Contributed by Philip Blundell <pb@nexus.co.uk>

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
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* We don't want a PLT.  */
#undef  NEED_PLT_RELOC
#define NEED_PLT_RELOC 0

#undef  TARGET_VERSION
#define TARGET_VERSION fputs (" (ARM/ELF ucLinux)", stderr);

#undef  TARGET_DEFAULT
#define TARGET_DEFAULT (ARM_FLAG_APCS_32 | ARM_FLAG_MMU_TRAPS | ARM_FLAG_SINGLE_PIC_BASE)
