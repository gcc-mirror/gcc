/* Definitions of target machine for GNU compiler, for HI-UX.
   Copyright (C) 1993, 1995, 1996, 2002 Free Software Foundation, Inc.

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

/* HIUX is just a HPUX variant.  We can simply use the HPUX configuration
   for just about everything.  */

/* OS cpp builtins are the one noteworthy difference between HPUX and HIUX.
   The following defines are similar to those for hpux10 with the addition
   of __H3050R and __H3050RX.  */
#undef TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS()				\
  do								\
    {								\
	builtin_assert ("system=hiux");				\
	builtin_assert ("system=unix");				\
	builtin_define ("__hp9000s800");			\
	builtin_define ("__hp9000s800__");			\
	builtin_define ("__hiux");				\
	builtin_define ("__hiux__");				\
	builtin_define ("__unix");				\
	builtin_define ("__unix__");				\
	builtin_define ("__H3050R");				\
	builtin_define ("__H3050RX");				\
	if (c_language == clk_cplusplus)			\
	  {							\
	    builtin_define ("_HIUX_SOURCE");			\
	    builtin_define ("_INCLUDE_LONGLONG");		\
	  }							\
	else if (!flag_iso)					\
	  {							\
	    builtin_define ("_HIUX_SOURCE");			\
	    if (preprocessing_trad_p ())			\
	      {							\
		builtin_define ("hp9000s800");			\
		builtin_define ("hppa");			\
		builtin_define ("hiux");			\
		builtin_define ("unix");			\
		builtin_define ("__CLASSIC_C__");		\
		builtin_define ("_PWB");			\
		builtin_define ("PWB");				\
	      }							\
	    else						\
	      builtin_define ("__STDC_EXT__");			\
	  }							\
	if (TARGET_SIO)						\
	  builtin_define ("_SIO");				\
	else							\
	  {							\
	    builtin_define ("__hp9000s700");			\
	    builtin_define ("__hp9000s700__");			\
	    builtin_define ("_WSIO");				\
	  }							\
    }								\
  while (0)

#undef SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES \
  { "sio",	 MASK_SIO,	N_("Generate cpp defines for server IO") }, \
  { "wsio",	-MASK_SIO,	N_("Generate cpp defines for workstation IO") },
