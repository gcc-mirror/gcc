/* Sequent DYNIX/ptx 1.x (SVr3) */

#include "i386/sysv3.h"

/* Sequent Symmetry SVr3 doesn't have crtn.o; crt1.o doesn't work
   but crt0.o does.  */

#undef STARTFILE_SPEC
#define STARTFILE_SPEC  \
"%{pg:gcrt0.o%s}\
 %{!pg:%{posix:%{p:mcrtp0.o%s}%{!p:crtp0.o%s}}\
       %{!posix:%{p:mcrt0.o%s}%{!p:crt0.o%s}}} crtbegin.o%s\
 %{p:-L/usr/lib/libp}%{pg:-L/usr/lib/libp}"

#undef LIB_SPEC
#define LIB_SPEC \
"%{posix:-lcposix}\
 %{shlib:-lc_s}\
 %{fshared-data:-lpps -lseq} -lc crtend.o%s"

#undef CPP_SPEC
#define CPP_SPEC "%(cpp_cpu) %{posix:-D_POSIX_SOURCE} -D_SEQUENT_=1"

/* Although the .init section is used, it is not automatically invoked.
   This because the _start() function in /lib/crt0.o never calls anything
   from the .init section */
#define INVOKE__main

/* Assembler pseudo-op for initialized shared variables (.shdata). */
#undef  SHARED_SECTION_ASM_OP
#define SHARED_SECTION_ASM_OP ".section .shdata, \"ws\""

/* Assembler pseudo-op for uninitialized shared global variables (.shbss). */
#undef  ASM_OUTPUT_SHARED_COMMON
#define ASM_OUTPUT_SHARED_COMMON(FILE, NAME, SIZE, ROUNDED) \
( fputs(".comm ", (FILE)),			\
  assemble_name((FILE), (NAME)),		\
   fprintf((FILE), ",%u,-3\n", (SIZE)))

/* Assembler pseudo-op for uninitialized shared local variables (.shbss). */
#undef  SHARED_BSS_SECTION_ASM_OP
#define SHARED_BSS_SECTION_ASM_OP ".section .shbss, \"bs\""

/* seq2-sysv3.h used to define HAVE_ATEXIT, so I assume ptx1 needs this...  */
#define NEED_ATEXIT
