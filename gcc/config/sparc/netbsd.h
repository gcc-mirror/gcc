#include <sparc/sparc.h>

/* Get generic NetBSD definitions.  */

#include <netbsd.h>

/* Names to predefine in the preprocessor for this target machine.  */

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dunix -Dsparc -D__NetBSD__ -Asystem(unix) -Asystem(NetBSD) -Acpu(sparc) -Amachine(sparc)"

/* Make gcc agree with <machine/ansi.h> */

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "int"

#undef WCHAR_UNSIGNED
#define WCHAR_UNSIGNED 0

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

/* This is BSD, so it wants DBX format.  */

#define DBX_DEBUGGING_INFO

/* This is the char to use for continuation (in case we need to turn
   continuation back on).  */

#define DBX_CONTIN_CHAR '?'

/* Don't use the `xsfoo;' construct in DBX output; this system
   doesn't support it.  */

#define DBX_NO_XREFS

/* Don't default to pcc-struct-return, because gcc is the only compiler, and
   we want to retain compatibility with older gcc versions.  */
#define DEFAULT_PCC_STRUCT_RETURN 0
