/* Definitions of target machine for GCC,
   for SuperH with targeting the VXWorks run time environment. 
   Copyright (C) 2003, 2004 Free Software Foundation, Inc.
   Contributed by CodeSourcery, LLC.
   
This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */


#define TARGET_OS_CPP_BUILTINS()	\
  do					\
    {					\
      builtin_define ("CPU=SH7000");	\
      VXWORKS_OS_CPP_BUILTINS ();	\
    }					\
  while (0)

#undef SUBTARGET_OVERRIDE_OPTIONS
#define SUBTARGET_OVERRIDE_OPTIONS VXWORKS_OVERRIDE_OPTIONS

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

#undef TARGET_VERSION
#define TARGET_VERSION	fputs (" (SH/VxWorks)", stderr);

/* There is no default multilib.  */
#undef MULTILIB_DEFAULTS

#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER VXWORKS_FUNCTION_PROFILER
