#include "sparc/xm-sysv4.h"

/* If not compiled with GNU C, include the system's <alloca.h> header.  */
#ifndef __GNUC__
#include <alloca.h>
#endif

/* We have _sys_siglist, but the declaration in <signal.h> conflicts with
   the declarations in collect2.c and mips-tfile.c, so disable the declarations
   in those files.  */

#define DONT_DECLARE_SYS_SIGLIST
