#include "m68k/m68k.h"

/* See m68k.h.  7 means 68020 with 68881.  */

#define TARGET_DEFAULT 7

/* Define __HAVE_68881__ in preprocessor, unless -msoft-float is specified.
   This will control the use of inline 68881 insns in certain macros.  */

#define CPP_SPEC "%{!msoft-float:-D__HAVE_68881__ -D__HAVE_FPU__}"

/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES "-Dmc68000 -Dmc68020 -Dhp300 -Dhp9000 -Dunix -Asystem(unix)  -Asystem(bsd) -Acpu(m68k) -Amachine(m68k)"

/* Link with libg.a when debugging, for dbx's sake.  */

#define LIB_SPEC "%{g:-lg} %{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p} "

/* Every structure or union's size must be a multiple of 2 bytes.  */

#define STRUCTURE_SIZE_BOUNDARY 16

/* This is BSD, so it wants DBX format.  */

#define DBX_DEBUGGING_INFO

/* Do not break .stabs pseudos into continuations.  */

#define DBX_CONTIN_LENGTH 0

/* This is the char to use for continuation (in case we need to turn
   continuation back on).  */

#define DBX_CONTIN_CHAR '?'

/* Don't use the `xsfoo;' construct in DBX output; this system
   doesn't support it.  */

#define DBX_NO_XREFS

/* Don't default to pcc-struct-return, because gcc is the only compiler, and
   we want to retain compatibility with older gcc versions.  */
#define DEFAULT_PCC_STRUCT_RETURN 0
