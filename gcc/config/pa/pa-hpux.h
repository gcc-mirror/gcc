#include "pa.h"

/* Make GCC agree with types.h.  */
#define SIZE_TYPE "unsigned int"

/* HPUX doesn't use any debugging format that GCC knows about.  */
#undef DBX_DEBUGGING_INFO

/* Like the default, except no -lg.  */
#define LIB_SPEC "%{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p}"

/* Control how gcc finds libgcc.a and how it passes -L options.  */
#define LINK_LIBGCC_SPECIAL
#define RELATIVE_PREFIX_NOT_LINKDIR
