/* Definitions of target machine for GNU compiler.  Tandem S2 w/ NonStop UX. */
#include "mips/svr4-5.h"

/* Use the default value for this.  */
#undef STANDARD_INCLUDE_DIR

#undef MACHINE_TYPE
#define MACHINE_TYPE "TANDEM System V.4 Mips"

/* Use the default values in mips.h.  */
#undef MD_STARTFILE_PREFIX
#undef MD_EXEC_PREFIX
