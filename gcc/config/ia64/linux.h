/* Definitions for ia64-linux target.  */
#include "ia64/ia64.h"
#include <linux.h>
#include "sysv4.h"

/* ??? Maybe this should be in sysv4.h?  */
#define CPP_PREDEFINES "\
-D__ia64 -D__ia64__ -D__linux -D__linux__ -D_LONGLONG -Dlinux -Dunix \
-D__LP64__ -D__ELF__ -Asystem(linux) -Acpu(ia64) -Amachine(ia64)"

/* ??? ia64 gas doesn't accept standard svr4 assembler options?  */
#undef ASM_SPEC

/* Define this for shared library support because it isn't in the main
   linux.h file.  */

#undef LINK_SPEC
#define LINK_SPEC "\
  %{shared:-shared} \
  %{!shared: \
    %{!static: \
      %{rdynamic:-export-dynamic} \
      %{!dynamic-linker:-dynamic-linker /lib/ld-linux-ia64.so.1}} \
      %{static:-static}}"


#define DONT_USE_BUILTIN_SETJMP
#define JMP_BUF_SIZE  (8 * 76)

/* Output any profiling code before the prologue.  */

#undef PROFILE_BEFORE_PROLOGUE
#define PROFILE_BEFORE_PROLOGUE 1

/* A C statement or compound statement to output to FILE some assembler code to
   call the profiling subroutine `mcount'.  */

#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO)		\
do {							\
  char buf[20];						\
  ASM_GENERATE_INTERNAL_LABEL (buf, "LP", LABELNO);	\
  fputs ("\talloc out0 = ar.pfs, 8, 0, 4, 0\n", FILE);	\
  if (TARGET_AUTO_PIC)					\
    fputs ("\tmovl out3 = @gprel(", FILE);		\
  else							\
    fputs ("\taddl out3 = @ltoff(", FILE);		\
  assemble_name (FILE, buf);				\
  if (TARGET_AUTO_PIC)					\
    fputs (");;\n", FILE);				\
  else							\
    fputs ("), r1;;\n", FILE);				\
  fputs ("\tmov out1 = r1\n", FILE);			\
  fputs ("\tmov out2 = b0\n", FILE);			\
  fputs ("\tbr.call.sptk.many b0 = _mcount;;\n", FILE);	\
} while (0)

/* End of linux.h */
