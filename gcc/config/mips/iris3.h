/* Definitions of target machine for GNU compiler.  Iris version.
   Copyright (C) 1991, 1993, 1995, 1996, 1998 Free Software Foundation, Inc.

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

#define SGI_TARGET 1		/* inform other mips files this is SGI */

/* Names to predefine in the preprocessor for this target machine.  */
/* Temporarily #if 0'd until Irix header consolidation.  */
#if 0
#define CPP_PREDEFINES	"\
-Dunix -Dmips -Dsgi -DSVR3 -Dhost_mips -DMIPSEB -DSYSTYPE_SYSV \
-Asystem=unix -Asystem=svr3 -Acpu=mips -Amachine=mips"
#define SUBTARGET_CPP_SPEC "\
%{!ansi:-D__EXTENSIONS__} -D_MIPSEB -D_SYSTYPE_SYSV"
#endif

#define STARTFILE_SPEC	"%{pg:gcrt1.o%s}%{!pg:%{p:mcrt1.o%s}%{!p:crt1.o%s}}"
#define LIB_SPEC	\
	"%{!p:%{!pg:%{!static:%{!g*:-lc_s}} -lc}}%{p:-lc_p}%{pg:-lc_p} crtn.o%s"

#define MACHINE_TYPE	"Silicon Graphics Mips"

/* Always use 1 for .file number.  I [meissner@osf.org] wonder why
   IRIS needs this.  */

#define SET_FILE_NUMBER() num_source_filenames = 1

/* Put out a label after a .loc.  I [meissner@osf.org] wonder why
   IRIS needs this.  */

#define LABEL_AFTER_LOC(STREAM) fprintf (STREAM, "LM%d:\n", ++sym_lineno)

#define STACK_ARGS_ADJUST(SIZE)                                         \
{                                                                       \
  SIZE.constant += 4;                                                   \
  if (SIZE.constant < 32)						\
    SIZE.constant = 32;                                                 \
}

/* Do not allow `$' in identifiers.  */

#define DOLLARS_IN_IDENTIFIERS 0

/* Tell G++ not to create constructors or destructors with $'s in them.  */

#define NO_DOLLAR_IN_LABEL 1

/* Specify wchar_t type.  */
#define WCHAR_TYPE	"unsigned char"
#define WCHAR_TYPE_SIZE BITS_PER_UNIT

/* Plain char is unsigned in the SGI compiler.  */
#define DEFAULT_SIGNED_CHAR 0
