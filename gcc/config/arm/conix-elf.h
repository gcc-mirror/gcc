/* Definitions of target machine for GNU compiler,
   for ARM with ConiX OS.
   Copyright (C) 2000, 2001 Free Software Foundation, Inc.
   Contributed by Philip Blundell <pb@futuretv.com>
   
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
`Boston, MA 02111-1307, USA.  */

/* elfos.h should have already been included.  Now just override
   any conflicting definitions and add any extras.  */

/* Run-time Target Specification.  */
#undef  TARGET_VERSION
#define TARGET_VERSION	fputs (" (ARM/ELF ConiX)", stderr);

/* Default to using APCS-32 and software floating point.  */
#undef  TARGET_DEFAULT
#define TARGET_DEFAULT	(ARM_FLAG_SOFT_FLOAT | ARM_FLAG_APCS_32)

#ifndef CPP_APCS_PC_DEFAULT_SPEC
#define CPP_APCS_PC_DEFAULT_SPEC	"-D__APCS_32__"
#endif
     
#ifndef SUBTARGET_CPU_DEFAULT
#define SUBTARGET_CPU_DEFAULT 		TARGET_CPU_arm7tdmi
#endif
     
#define TARGET_OS_CPP_BUILTINS()		\
    do {					\
	builtin_define ("__CONIX__");		\
	builtin_define ("__ELF__");		\
    } while (0)
