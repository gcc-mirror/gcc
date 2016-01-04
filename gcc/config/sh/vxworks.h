/* Definitions of target machine for GCC,
   for SuperH with targeting the VXWorks run time environment. 
   Copyright (C) 2003-2016 Free Software Foundation, Inc.
   Contributed by CodeSourcery, LLC.
   
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


#define TARGET_OS_CPP_BUILTINS()	\
  do					\
    {					\
      builtin_define ("CPU=SH7000");	\
      VXWORKS_OS_CPP_BUILTINS ();	\
    }					\
  while (0)

#undef SUBTARGET_OVERRIDE_OPTIONS
#define SUBTARGET_OVERRIDE_OPTIONS				\
  do								\
    {								\
      VXWORKS_OVERRIDE_OPTIONS;					\
      /* The kernel loader cannot handle the relaxation		\
	 relocations, so it cannot load kernel modules		\
	 (which are ET_REL) or RTP executables (which are	\
	 linked with --emit-relocs).  No relaxation relocations	\
	 appear in shared libraries, so relaxation is OK	\
	 for RTP PIC.  */					\
      if (TARGET_RELAX && !(TARGET_VXWORKS_RTP && flag_pic))	\
	error ("-mrelax is only supported for RTP PIC");	\
    }								\
  while (0)

#undef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC VXWORKS_ADDITIONAL_CPP_SPEC

#undef SUBTARGET_LINK_EMUL_SUFFIX
#define SUBTARGET_LINK_EMUL_SUFFIX "_vxworks"

#undef LIB_SPEC
#define LIB_SPEC VXWORKS_LIB_SPEC
#undef LINK_SPEC
#define LINK_SPEC VXWORKS_LINK_SPEC " " SH_LINK_SPEC
#undef STARTFILE_SPEC
#define STARTFILE_SPEC VXWORKS_STARTFILE_SPEC
#undef ENDFILE_SPEC
#define ENDFILE_SPEC VXWORKS_ENDFILE_SPEC

/* There is no default multilib.  */
#undef MULTILIB_DEFAULTS

#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER VXWORKS_FUNCTION_PROFILER
