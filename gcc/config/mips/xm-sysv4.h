#include "mips/xm-sysv.h"

/* SVR4 provides no sys_siglist,
   but does offer the same data under another name.  */
#define sys_siglist _sys_siglist
#undef SYS_SIGLIST_DECLARED
#define SYS_SIGLIST_DECLARED
