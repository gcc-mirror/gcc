/* Definitions of target machine for GNU compiler,
   for IBM RS/6000 running AIX version 3.1.
   Copyright (C) 1996, 1997 Free Software Foundation, Inc.
   Contributed by David Reese (Dave.Reese@East.Sun.COM)

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

#include "rs6000/sysv4le.h"

/* Default ABI to use */
#undef	RS6000_ABI_NAME
#define RS6000_ABI_NAME "solaris"

#undef ASM_CPU_SPEC
#define ASM_CPU_SPEC "-le -s"

#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_POWERPC | \
			MASK_NEW_MNEMONICS | \
			MASK_LITTLE_ENDIAN | \
			MASK_REGNAMES)

#undef	LIB_DEFAULT_SPEC
#define LIB_DEFAULT_SPEC "%(lib_solaris)"

#undef	STARTFILE_DEFAULT_SPEC
#define STARTFILE_DEFAULT_SPEC "%(startfile_solaris)"

#undef	ENDFILE_DEFAULT_SPEC
#define ENDFILE_DEFAULT_SPEC "%(endfile_solaris)"

#undef	LINK_START_DEFAULT_SPEC
#define LINK_START_DEFAULT_SPEC "%(link_start_solaris)"

#undef CPP_SPEC
#define CPP_SPEC "%{posix: -D_POSIX_SOURCE}\
%(cpp_sysv) %(cpp_endian) %(cpp_cpu) \
%{mmvme: %(cpp_os_mvme) } \
%{msim: %(cpp_os_sim) } \
%{mcall-linux: %(cpp_os_linux) } \
%{mcall-solaris: %(cpp_os_solaris) } \
%{!mmvme: %{!msim: %{!mcall-linux: %{!mcall-solaris: %(cpp_os_default) }}}}"

#undef	CPP_OS_DEFAULT_SPEC
#define	CPP_OS_DEFAULT_SPEC "%(cpp_os_solaris)"

#undef	LINK_OS_DEFAULT_SPEC
#define	LINK_OS_DEFAULT_SPEC "%(link_os_solaris)"

#undef	CPP_ENDIAN_LITTLE_SPEC
#define	CPP_ENDIAN_LITTLE_SPEC CPP_ENDIAN_SOLARIS_SPEC

/* Don't turn -B into -L if the argument specifies a relative file name.  */
#undef	RELATIVE_PREFIX_NOT_LINKDIR

#define	DEFAULT_PCC_STRUCT_RETURN 0

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (PowerPC Solaris)");


/* Macros to check register numbers against specific register classes.  */

#undef	PREFERRED_DEBUGGING_TYPE
#define	PREFERRED_DEBUGGING_TYPE DBX_DEBUG


#if 0
#undef	ASM_OUTPUT_ALIGNED_LOCAL
#define ASM_OUTPUT_ALIGNED_LOCAL(FILE, NAME, SIZE, ALIGN)		\
do {									\
  fprintf ((FILE), "\t%s\t", ".lcomm");					\
  assemble_name ((FILE), (NAME));					\
  fprintf ((FILE), ",%u,%u\n", (SIZE), (ALIGN) / BITS_PER_UNIT);	\
} while (0)
#endif

/* Like block addresses, stabs line numbers are relative to the
   current function.  */

/* use .stabd instead of .stabn */

#define	ASM_STABN_OP	".stabd"

#undef  ASM_OUTPUT_SOURCE_LINE
#define ASM_OUTPUT_SOURCE_LINE(file, line)				\
do									\
  {									\
    static int sym_lineno = 1;						\
    char *_p;								\
    fprintf (file, "\t.stabd 68,0,%d,.LM%d-",				\
	     line, sym_lineno);						\
    STRIP_NAME_ENCODING (_p, XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0)); \
    assemble_name (file, _p);						\
    fprintf (file, "\n.LM%d:\n", sym_lineno);				\
    sym_lineno += 1;							\
  }									\
while (0)

/* This is how to output an assembler line defining a `double' constant.  */

#undef	ASM_OUTPUT_DOUBLE
#define ASM_OUTPUT_DOUBLE(FILE, VALUE)					\
  {									\
    if (REAL_VALUE_ISINF (VALUE)					\
        || REAL_VALUE_ISNAN (VALUE)					\
	|| REAL_VALUE_MINUS_ZERO (VALUE))				\
      {									\
	long t[2];							\
	REAL_VALUE_TO_TARGET_DOUBLE ((VALUE), t);			\
	fprintf (FILE, "\t.long 0x%lx\n\t.long 0x%lx\n",		\
		t[0] & 0xffffffff, t[1] & 0xffffffff);			\
      }									\
    else								\
      {									\
	char str[30];							\
	REAL_VALUE_TO_DECIMAL (VALUE, "%.20e", str);			\
	fprintf (FILE, "\t.double %s\n", str);				\
      }									\
  }

/* This is how to output an assembler line defining a `float' constant.  */

#undef	ASM_OUTPUT_FLOAT
#define ASM_OUTPUT_FLOAT(FILE, VALUE)					\
  {									\
    if (REAL_VALUE_ISINF (VALUE)					\
        || REAL_VALUE_ISNAN (VALUE)					\
	|| REAL_VALUE_MINUS_ZERO (VALUE))				\
      {									\
	long t;								\
	REAL_VALUE_TO_TARGET_SINGLE ((VALUE), t);			\
	fprintf (FILE, "\t.long 0x%lx\n", t & 0xffffffff);		\
      }									\
    else								\
      {									\
	char str[30];							\
	REAL_VALUE_TO_DECIMAL ((VALUE), "%.20e", str);			\
	fprintf (FILE, "\t.float %s\n", str);				\
      }									\
  }


/* Sun-ppc assembler does not permit '.' in some symbol names.
   Use 'name_.labelno' instead. */
#undef ASM_FORMAT_PRIVATE_NAME
#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)	\
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 10),	\
  sprintf ((OUTPUT), "%s_.%d", (NAME), (LABELNO))) 


/* Define this macro as a C expression for the initializer of an
   array of string to tell the driver program which options are
   defaults for this target and thus do not need to be handled
   specially when using `MULTILIB_OPTIONS'.

   Do not define this macro if `MULTILIB_OPTIONS' is not defined in
   the target makefile fragment or if none of the options listed in
   `MULTILIB_OPTIONS' are set by default.  *Note Target Fragment::.  */

#undef	MULTILIB_DEFAULTS
#define	MULTILIB_DEFAULTS { "mlittle", "mcall-solaris" }

#define STDC_0_IN_SYSTEM_HEADERS
