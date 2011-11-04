/* Definitions for RTEMS based ARM systems using ELF
   Copyright (C) 2000, 2002, 2005, 2007, 2008, 2009, 2011
   Free Software Foundation, Inc.
 
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

/* Run-time Target Specification.  */

#define HAS_INIT_SECTION

#define TARGET_OS_CPP_BUILTINS()		\
    do {					\
	builtin_define ("__rtems__");		\
	builtin_define ("__USE_INIT_FINI__");	\
	builtin_assert ("system=rtems");	\
    } while (0)

/*
 * The default in gcc now is soft-float, but gcc misses it to 
 * pass it to the assembler.
 */
#undef SUBTARGET_EXTRA_ASM_SPEC
#define SUBTARGET_EXTRA_ASM_SPEC "\
  %{!mfloat-abi=hard: %{!mfpu=vfp: %{!mfloat-abi=soft:-mfpu=softfpa}}}"

/*
 *  The default includes --start-group and --end-group which conflicts
 *  with how this used to be defined.
 */
#undef LINK_GCC_C_SEQUENCE_SPEC
