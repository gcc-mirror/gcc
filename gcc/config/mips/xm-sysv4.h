#include "mips/xm-sysv.h"

/* SVR4 provides no sys_siglist,
   but does offer the same data under another name.  */
#define sys_siglist _sys_siglist

/* There is a declaration in /usr/include/signal.h that conflicts with the
   declarations in collect2.c and mips-tfile.c, so disable gcc's declarations.
   This is at least true for CDC's EP/IX 2.1.1.  It is suspected to be true
   for RISC/OS 5.x also.  */
#define DONT_DECLARE_SYS_SIGLIST
