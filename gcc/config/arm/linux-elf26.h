/* Definitions for 26-bit ARM running Linux-based GNU systems using ELF
   Copyright (C) 1998 Free Software Foundation, Inc.
   Contributed by Philip Blundell <philb@gnu.org>

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

#define SUBTARGET_DEFAULT_APCS26

#define SUBTARGET_LINK_SPEC	\
	" %{mapcs-32:-m elf32arm} %{!mapcs-32:-m elf32arm26}"

#define SUBTARGET_EXTRA_ASM_SPEC	\
	" %{mapcs-32:-mapcs-32} %(!mapcs-32:-mapcs-26}"

#define TARGET_DEFAULT (ARM_FLAG_SHORT_BYTE)

#include "arm/linux-elf.h"
