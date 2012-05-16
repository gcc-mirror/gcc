/* Definitions for rtems targeting a PowerPC using elf.
   Copyright (C) 1996, 1997, 2000, 2001, 2002, 2003, 2004, 2005, 2007
   Free Software Foundation, Inc.
   Contributed by Joel Sherrill (joel@OARcorp.com).

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* Specify predefined symbols in preprocessor.  */

#undef TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS()          \
  do                                      \
    {                                     \
      builtin_define_std ("PPC");         \
      builtin_define ("__rtems__");       \
      builtin_define ("__USE_INIT_FINI__"); \
      builtin_assert ("system=rtems");    \
      builtin_assert ("cpu=powerpc");     \
      builtin_assert ("machine=powerpc"); \
      TARGET_OS_SYSV_CPP_BUILTINS ();     \
    }                                     \
  while (0)

#undef CPP_OS_DEFAULT_SPEC
#define CPP_OS_DEFAULT_SPEC "%(cpp_os_rtems)"

#define CPP_OS_RTEMS_SPEC "\
%{!mcpu*:  %{!Dppc*: %{!Dmpc*: -Dmpc750} } }\
%{mcpu=403:  %{!Dppc*: %{!Dmpc*: -Dppc403}  } } \
%{mcpu=505:  %{!Dppc*: %{!Dmpc*: -Dmpc505}  } } \
%{mcpu=601:  %{!Dppc*: %{!Dmpc*: -Dppc601}  } } \
%{mcpu=602:  %{!Dppc*: %{!Dmpc*: -Dppc602}  } } \
%{mcpu=603:  %{!Dppc*: %{!Dmpc*: -Dppc603}  } } \
%{mcpu=603e: %{!Dppc*: %{!Dmpc*: -Dppc603e} } } \
%{mcpu=604:  %{!Dppc*: %{!Dmpc*: -Dmpc604}  } } \
%{mcpu=750:  %{!Dppc*: %{!Dmpc*: -Dmpc750}  } } \
%{mcpu=821:  %{!Dppc*: %{!Dmpc*: -Dmpc821}  } } \
%{mcpu=860:  %{!Dppc*: %{!Dmpc*: -Dmpc860}  } } \
%{mcpu=8540: %{!Dppc*: %{!Dmpc*: -Dppc8540}  } }" 

#undef  SUBSUBTARGET_EXTRA_SPECS
#define SUBSUBTARGET_EXTRA_SPECS \
  { "cpp_os_rtems",		CPP_OS_RTEMS_SPEC }
