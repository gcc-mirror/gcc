/* Definitions for rtems targeting a SPARC using ELF.
   Copyright (C) 1996-2021 Free Software Foundation, Inc.
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

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* Target OS builtins.  */
#undef TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
	builtin_define ("__rtems__");		\
	builtin_define ("__USE_INIT_FINI__");	\
	builtin_assert ("system=rtems");	\
	if (sparc_fix_b2bst)			\
	  builtin_define ("__FIX_LEON3FT_B2BST"); \
	if (sparc_fix_gr712rc || sparc_fix_ut700 || sparc_fix_ut699) \
	  builtin_define ("__FIX_LEON3FT_TN0018"); \
    }						\
  while (0)

/* Use the default */
#undef LINK_GCC_C_SEQUENCE_SPEC

#define SPARC_GCOV_TYPE_SIZE 32
