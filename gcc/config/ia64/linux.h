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
      %{!dynamic-linker:-dynamic-linker /lib/ld-linux.so.2}} \
      %{static:-static}}"


#define DONT_USE_BUILTIN_SETJMP
#define JMP_BUF_SIZE  (8 * 76)
/* End of linux.h */
