/* Definitions for Intel 386 running SCO Unix System V,
   using dbx-in-coff encapsulation.
   Copyright (C) 1992, 1995, 1996, 1999 Free Software Foundation, Inc.

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

#include "i386/svr3dbx.h"

/* Overridden defines for SCO systems from sco.h. */

/* By default, target has a 80387, uses IEEE compatible arithmetic,
   and returns float values in the 387, ie,
   (TARGET_80387 | TARGET_FLOAT_RETURNS_IN_80387)

   SCO's software emulation of a 387 fails to handle the `fucomp'
   opcode.  fucomp is only used when generating IEEE compliant code.
   So don't make TARGET_IEEE_FP default for SCO. */

#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_80387 | MASK_FLOAT_RETURNS)

/* Use crt1.o as a startup file and crtn.o as a closing file.  */

#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
  "%{!r:%{!z:svr3.ifile%s}%{z:svr3z.ifile%s}}\
   %{pg:gcrt1.o%s}%{!pg:%{p:mcrt1.o%s}%{!p:crt1.o%s}}"

/* Library spec, including SCO international language support. */

#undef LIB_SPEC
#define LIB_SPEC \
 "%{p:-L/usr/lib/libp}%{pg:-L/usr/lib/libp} %{scointl:libintl.a%s} -lc"

/* Specify predefined symbols in preprocessor.  */

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dunix -DM_UNIX -DM_I386 -DM_COFF -DM_WORDSWAP -Asystem(svr3)"

#undef CPP_SPEC
#define CPP_SPEC "%(cpp_cpu) %{scointl:-DM_INTERNAT}"

/* This spec is used for telling cpp whether char is signed or not.  */

#undef SIGNED_CHAR_SPEC
#if DEFAULT_SIGNED_CHAR
#define SIGNED_CHAR_SPEC \
 "%{funsigned-char:-D__CHAR_UNSIGNED__ -D_CHAR_UNSIGNED}"
#else
#define SIGNED_CHAR_SPEC \
 "%{!fsigned-char:-D__CHAR_UNSIGNED__ -D_CHAR_UNSIGNED}"
#endif

/* caller has to pop the extra argument passed to functions that return
   structures. */

#undef RETURN_POPS_ARGS
#define RETURN_POPS_ARGS(FUNDECL,FUNTYPE,SIZE)   \
  ((FUNDECL) && TREE_CODE (FUNDECL) == IDENTIFIER_NODE ? 0	\
   : (TARGET_RTD						\
      && (TYPE_ARG_TYPES (FUNTYPE) == 0				\
	  || (TREE_VALUE (tree_last (TYPE_ARG_TYPES (FUNTYPE)))	\
	      == void_type_node))) ? (SIZE)			\
   : 0)
/* On other 386 systems, the last line looks like this:
   : (aggregate_value_p (TREE_TYPE (FUNTYPE))) ? GET_MODE_SIZE (Pmode) : 0)  */

/* Use periods rather than dollar signs in special g++ assembler names.  */

#define NO_DOLLAR_IN_LABEL

/* Handle #pragma pack. */
#define HANDLE_SYSV_PRAGMA
