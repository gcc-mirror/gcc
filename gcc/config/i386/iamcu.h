/* Definitions of target machine for Intel MCU psABI.
   Copyright (C) 2015 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* Intel MCU has no 80387.  Default to Intel MCU psABI.  */
#undef TARGET_SUBTARGET_DEFAULT
#define TARGET_SUBTARGET_DEFAULT MASK_IAMCU

#undef ASM_SPEC
#define ASM_SPEC "--32 -march=iamcu"

#undef LINK_SPEC
#define LINK_SPEC "-m elf_iamcu"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC ""

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "crt0.o%s"

#undef LIB_SPEC
#define LIB_SPEC "--start-group -lc -lgloss --end-group"
