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

/*
  Changed from config/svr4.h in the following ways:

  - Added "%{V}".
  - Modified "{%v:-V}" to take into account "%{V}".
  - Added "-s" so that stabs are saved in the final executable.  */

#undef ASM_SPEC
#define ASM_SPEC \
  "%{V} %{v:%{!V:-V}} %{Qy:} %{!Qn:-Qy} %{n} %{T} %{Ym,*} %{Yd,*} %{Wa,*:%*} -s"
