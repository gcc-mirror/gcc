/* Target definitions for GNU compiler for Intel 80386 running Solaris
   with gas and gdb.
   This file is added into the directory .../gcc-2.../config/i386
   Workability without "#undef DWARF_DEBUGGING_INFO" is not tested. */

/* Use stabs instead of DWARF debug format.  */
#ifdef PREFERRED_DEBUGGING_TYPE
#undef PREFERRED_DEBUGGING_TYPE
#endif
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

#include "i386/sol2.h"

#ifdef DWARF_DEBUGGING_INFO
#undef DWARF_DEBUGGING_INFO
#endif
