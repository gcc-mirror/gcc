/* Definitions of target machine for GNU compiler,
   for IBM RS/6000 running AIX version 3.1.
   Copyright (C) 1996 Free Software Foundation, Inc.
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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "rs6000/sysv4le.h"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES \
 "-Dsun=1 -Dunix -D__svr4__ -DSVR4 -DPPC \
  -D__ppc -Asystem(unix) -Asystem(svr4) -Acpu(powerpc) -Amachine(prep)"

#undef ASM_CPU_SPEC
#define ASM_CPU_SPEC "%{fpic:-K PIC} %{fPIC:-K PIC} -le -s"

/* This should be the same as in svr4.h, except with -R added.  */
#undef LINK_SPEC
#define LINK_SPEC \
  "%{h*} %{v:-V} %{G*} \
   %{b} %{Wl,*:%*} \
   %{static:-dn -Bstatic} \
   %{shared:-G -dy -z text %{!h*:%{o*:-h %*}}} \
   %{symbolic:-Bsymbolic -G -dy -z text %{!h*:%{o*:-h %*}}} \
   %{G:-G} \
   %{YP,*} \
   %{R*} %{!static:%{!R*:%{L*:-R %*}}} \
   %{compat-bsd: \
     %{!YP,*:%{p:-Y P,/usr/ucblib:/usr/ccs/lib/libp:/usr/lib/libp:/usr/ccs/lib:/usr/lib} \
       %{!p:-Y P,/usr/ucblib:/usr/ccs/lib:/usr/lib}} \
     -R /usr/ucblib} \
   %{!compat-bsd: \
     %{!YP,*:%{p:-Y P,/usr/ccs/lib/libp:/usr/lib/libp:/usr/ccs/lib:/usr/lib} \
       %{!p:-Y P,/usr/ccs/lib:/usr/lib}}} \
   %{Qy:} %{!Qn:-Qy}"

#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_POWERPC | \
			MASK_NEW_MNEMONICS | \
			MASK_LITTLE_ENDIAN | \
			MASK_REGNAMES)

#undef	LIB_DEFAULT_SPEC
#define LIB_DEFAULT_SPEC LIB_SOLARIS_SPEC

#undef	STARTFILE_DEFAULT_SPEC
#define STARTFILE_DEFAULT_SPEC STARTFILE_SOLARIS_SPEC

#undef	ENDFILE_DEFAULT_SPEC
#define ENDFILE_DEFAULT_SPEC ENDFILE_SOLARIS_SPEC

#undef	LINK_START_DEFAULT_SPEC
#define LINK_START_DEFAULT_SPEC LINK_START_SOLARIS_SPEC

/* Don't turn -B into -L if the argument specifies a relative file name.  */
#undef	RELATIVE_PREFIX_NOT_LINKDIR

#define	DEFAULT_PCC_STRUCT_RETURN 0

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (PowerPC Solaris)");


/* Macros to check register numbers against specific register classes.  */

#undef	PREFERRED_DEBUGGING_TYPE
#define	PREFERRED_DEBUGGING_TYPE DBX_DEBUG


#undef	ASM_OUTPUT_ALIGNED_LOCAL
#define ASM_OUTPUT_ALIGNED_LOCAL(FILE, NAME, SIZE, ALIGN)		\
do {									\
  fprintf ((FILE), "\t%s\t", ".lcomm");					\
  assemble_name ((FILE), (NAME));					\
  fprintf ((FILE), ",%u,%u\n", (SIZE), (ALIGN) / BITS_PER_UNIT);	\
} while (0)


/* Like block addresses, stabs line numbers are relative to the
   current function.  */

/* use .stabd instead of .stabn */

#define	ASM_STABN_OP	".stabd"

#undef	SKIP_ASM_OP
#define	SKIP_ASM_OP	".skip"

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

#define ASM_RELOCATION_EXPRESSIONS 1

