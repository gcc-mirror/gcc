/* Configuration for GCC for Intel i386 running System V Release 4.  */

#include "i386/xm-i386.h"
#include "xm-svr4.h"

/* If not compiled with GNU C, use the portable alloca.  */
#ifndef __GNUC__
#define USE_C_ALLOCA
#endif
#ifdef __HIGHC__
#include <alloca.h>		/* for MetaWare High-C on NCR System 3000 */
#endif

/* Univel, at least, has a small ARG_MAX.  Defining this is harmless
   except for causing extra stat calls in the driver program.  */
#define SMALL_ARG_MAX

/* We have _sys_siglist, but the declaration in <signal.h> conflicts with
   the declarations in collect2.c and mips-tfile.c, so disable the declarations
   in those files.  */

#define DONT_DECLARE_SYS_SIGLIST
