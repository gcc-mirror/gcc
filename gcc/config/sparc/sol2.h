/* Definitions of target machine for GNU compiler, for SPARC running Solaris 2
   Copyright 1992, 1995 Free Software Foundation, Inc.

   Written by Ron Guilmette (rfg@netcom.com).
   Additional changes by David V. Henkel-Wallace (gumby@cygnus.com).

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

/* Supposedly the same as vanilla sparc svr4, except for the stuff below: */
#include "sparc/sysv4.h"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES \
 "-Dsun -Dsparc -Dunix -D__svr4__ -D__SVR4 \
  -Asystem(unix) -Asystem(svr4) -Acpu(sparc) -Amachine(sparc)\
  -D__GCC_NEW_VARARGS__"

#undef CPP_SPEC
#define CPP_SPEC "\
   %{compat-bsd:-iwithprefixbefore ucbinclude -I/usr/ucbinclude}\
   %{msparclite:-D__sparclite__} %{mv8:-D__sparc_v8__}\
   %{msupersparc:-D__supersparc__ -D__sparc_v8__}"

/* The sun bundled assembler doesn't accept -Yd, (and neither does gas).
   It's safe to pass -s always, even if -g is not used. */
#undef ASM_SPEC
#define ASM_SPEC \
  "%{V} %{v:%{!V:-V}} %{Qy:} %{!Qn:-Qy} %{n} %{T} %{Ym,*} %{Wa,*:%*} -s \
   %{fpic:-K PIC} %{fPIC:-K PIC}"

/* However it appears that Solaris 2.0 uses the same reg numbering as
   the old BSD-style system did. */

#undef DBX_REGISTER_NUMBER
/* Same as sparc.h */
#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/* We use stabs-in-elf for debugging, because that is what the native
   toolchain uses.  */
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

/* The Solaris 2 assembler uses .skip, not .zero, so put this back. */
#undef ASM_OUTPUT_SKIP
#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.skip %u\n", (SIZE))

#undef ASM_OUTPUT_ALIGNED_LOCAL
#define ASM_OUTPUT_ALIGNED_LOCAL(FILE, NAME, SIZE, ALIGN)		\
do {									\
  fputs ("\t.local\t", (FILE));		\
  assemble_name ((FILE), (NAME));					\
  putc ('\n', (FILE));							\
  ASM_OUTPUT_ALIGNED_COMMON (FILE, NAME, SIZE, ALIGN);			\
} while (0)

#undef COMMON_ASM_OP
#define COMMON_ASM_OP "\t.common"

/* This is how to output a definition of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#undef  ASM_OUTPUT_INTERNAL_LABEL
#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  fprintf (FILE, ".L%s%d:\n", PREFIX, NUM)

/* This is how to output a reference to an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#undef  ASM_OUTPUT_INTERNAL_LABELREF
#define ASM_OUTPUT_INTERNAL_LABELREF(FILE,PREFIX,NUM)	\
  fprintf (FILE, ".L%s%d", PREFIX, NUM)

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#undef  ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf (LABEL, "*.L%s%d", PREFIX, NUM)


/* We don't use the standard svr4 STARTFILE_SPEC because it's wrong for us.
   We don't use the standard LIB_SPEC only because we don't yet support c++ */

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "%{!shared: \
			 %{!symbolic: \
			  %{p:mcrt1.o%s}%{!p:crt1.o%s} %{pg:gmon.o%s}}} \
			crti.o%s \
			%{ansi:values-Xc.o%s} \
			%{!ansi: \
			 %{traditional:values-Xt.o%s} \
			 %{!traditional:values-Xa.o%s}} \
			crtbegin.o%s"

/* ??? Note: in order for -compat-bsd to work fully,
   we must somehow arrange to fixincludes /usr/ucbinclude
   and put the result in $(libsubdir)/ucbinclude.  */

#undef LIB_SPEC
#define LIB_SPEC \
  "%{compat-bsd:-lucb -lsocket -lnsl -lelf -laio} %{!shared:%{!symbolic:-lc}}"

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s crtn.o%s"

/* This should be the same as in svr4.h, except with -R added.  */
#undef LINK_SPEC
#define LINK_SPEC \
  "%{h*} %{V} %{v:%{!V:-V}} \
   %{b} %{Wl,*:%*} \
   %{static:-dn -Bstatic} \
   %{shared:-G -dy -z text %{!h*:%{o*:-h %*}}} \
   %{symbolic:-Bsymbolic -G -dy -z text %{!h*:%{o*:-h %*}}} \
   %{G:-G} \
   %{YP,*} \
   %{R*} \
   %{compat-bsd: \
     %{!YP,*:%{p:-Y P,/usr/ucblib:/usr/ccs/lib/libp:/usr/lib/libp:/usr/ccs/lib:/usr/lib} \
       %{pg:-Y P,/usr/ucblib:/usr/ccs/lib/libp:/usr/lib/libp:/usr/ccs/lib:/usr/lib} \
       %{!p:%{!pg:-Y P,/usr/ucblib:/usr/ccs/lib:/usr/lib}}} \
     -R /usr/ucblib} \
   %{!compat-bsd: \
     %{!YP,*:%{p:-Y P,/usr/ccs/lib/libp:/usr/lib/libp:/usr/ccs/lib:/usr/lib} \
       %{pg:-Y P,/usr/ccs/lib/libp:/usr/lib/libp:/usr/ccs/lib:/usr/lib} \
       %{!p:%{!pg:-Y P,/usr/ccs/lib:/usr/lib}}}} \
   %{Qy:} %{!Qn:-Qy}"

/* This defines which switch letters take arguments.
   It is as in svr4.h but with -R added.  */

#undef SWITCH_TAKES_ARG
#define SWITCH_TAKES_ARG(CHAR) \
  (   (CHAR) == 'D' \
   || (CHAR) == 'U' \
   || (CHAR) == 'o' \
   || (CHAR) == 'e' \
   || (CHAR) == 'u' \
   || (CHAR) == 'I' \
   || (CHAR) == 'm' \
   || (CHAR) == 'L' \
   || (CHAR) == 'R' \
   || (CHAR) == 'A' \
   || (CHAR) == 'h' \
   || (CHAR) == 'z')

/* ??? This does not work in SunOS 4.x, so it is not enabled in sparc.h.
   Instead, it is enabled here, because it does work under Solaris.  */
/* Define for support of TFmode long double and REAL_ARITHMETIC.
   Sparc ABI says that long double is 4 words.  */
#define LONG_DOUBLE_TYPE_SIZE 128
