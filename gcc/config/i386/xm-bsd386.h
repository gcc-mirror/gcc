/* Configuration for GCC for Intel i386 running BSDI's BSD/386 as host.  */

#include "i386/xm-i386.h"

#define HAVE_STRERROR

/* We have _sys_siglist, but the declaration in <signal.h> conflicts with
   the declarations in collect2.c so disable the declarations
   in those files.  */

#define DONT_DECLARE_SYS_SIGLIST
