#include "sparc/xm-sysv4.h"

/* If not compiled with GNU C, include the system's <alloca.h> header.  */
#ifndef __GNUC__
#include <alloca.h>
#endif

/* We do have _sys_siglist, but the declaration in <signal.h> conflicts with
   the declarations in collect2.c and mips-tfile.c, so just pretend that we
   don't have it.  */

#define NO_SYS_SIGLIST
