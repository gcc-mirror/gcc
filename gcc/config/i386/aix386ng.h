/* Definitions for IBM PS2 running AIX/386.
   Copyright (C) 1988, 1996, 1998 Free Software Foundation, Inc.
   Contributed by Minh Tran-Le <TRANLE@intellicorp.com>.

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


#include "i386/i386.h"

/* Get the generic definitions for system V.3.  */

#include "svr3.h"

/* Use the ATT assembler syntax.
   This overrides at least one macro (USER_LABEL_PREFIX) from svr3.h.  */

#include "i386/att.h"

/* Use crt1.o as a startup file and crtn.o as a closing file.  */

#define STARTFILE_SPEC "%{pg:gcrt0.o%s}%{!pg:%{p:mcrt1.o%s}%{!p:crt1.o%s}}"
#define ENDFILE_SPEC   "crtn.o%s"

#define LIB_SPEC "%{shlib:-lc_s} -lc"

/* Special flags for the linker.  I don't know what they do.  */

#define LINK_SPEC "%{K} %{!K:-K} %{T*} %{z:-lm}"

/* Specify predefined symbols in preprocessor.  */

#define CPP_PREDEFINES "-Dps2 -Dunix -Asystem(aix)"

#define CPP_SPEC "%(cpp_cpu) \
  %{posix:-D_POSIX_SOURCE}%{!posix:-DAIX} -D_I386 -D_AIX -D_MBCS"

/* special flags for the aix assembler to generate the short form for all
   qualifying forward reference */
/* The buggy /bin/as of aix ps/2 1.2.x cannot always handle it.	*/
#if 0
#define ASM_SPEC "-s2"
#endif /* 0 */

#undef ASM_FILE_START
#define ASM_FILE_START(FILE) 					\
  do { output_file_directive (FILE, main_input_filename);	\
       if (optimize)						\
          ASM_FILE_START_1 (FILE); 				\
       else							\
          fprintf (FILE, "\t.noopt\n");				\
     } while (0)

/* This was suggested, but it shouldn't be right for DBX output. -- RMS
   #define ASM_OUTPUT_SOURCE_FILENAME(FILE, NAME) */

/* Writing `int' for a bitfield forces int alignment for the structure.  */

#define PCC_BITFIELD_TYPE_MATTERS 1

#ifndef USE_GAS
/* Don't write a `.optim' pseudo; this assembler
   is said to have a bug when .optim is used.  */

#undef ASM_FILE_START_1
#define ASM_FILE_START_1(FILE) fprintf (FILE, "\t.noopt\n")
#endif

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO)  \
   fprintf (FILE, "\tleal %sP%d,%%eax\n\tcall mcount\n", LPREFIX, (LABELNO));

/* Note that using bss_section here caused errors
   in building shared libraries on system V.3.
   but AIX 1.2 does not have yet shareable libraries on PS2 */
#undef ASM_OUTPUT_LOCAL
#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)  \
  (bss_section (),					\
   ASM_OUTPUT_LABEL ((FILE), (NAME)),			\
   fprintf ((FILE), "\t.set .,.+%u\n", (ROUNDED)))


/* Undef all the .init and .fini section stuff if we are not using gas and
 * gnu ld so that we can use collect because the standard /bin/as and /bin/ld
 * cannot handle those.
 */
#ifndef USE_GAS
# undef INIT_SECTION_ASM_OP
# undef FINI_SECTION_ASM_OP
# undef CTORS_SECTION_ASM_OP
# undef DTORS_SECTION_ASM_OP
# undef ASM_OUTPUT_CONSTRUCTOR
# undef ASM_OUTPUT_DESTRUCTOR
# undef DO_GLOBAL_CTORS_BODY

# undef CTOR_LIST_BEGIN
# define CTOR_LIST_BEGIN
# undef CTOR_LIST_END
# define CTOR_LIST_END
# undef DTOR_LIST_BEGIN
# define DTOR_LIST_BEGIN
# undef DTOR_LIST_END
# define DTOR_LIST_END

# undef CONST_SECTION_FUNCTION
# define CONST_SECTION_FUNCTION						\
void									\
const_section ()							\
{									\
  extern void text_section();						\
  text_section();							\
}

# undef EXTRA_SECTION_FUNCTIONS
# define EXTRA_SECTION_FUNCTIONS				\
  CONST_SECTION_FUNCTION

/* for collect2 */
# define OBJECT_FORMAT_COFF
# define MY_ISCOFF(magic) \
  ((magic) == I386MAGIC || (magic) == I386SVMAGIC)

#endif /* !USE_GAS */
