/* Configuration for GCC for Intel i386 running SCO.  */

#include "i386/xm-sysv3.h"

/* Big buffers improve performance.  */

#define IO_BUFFER_SIZE (0x8000 - 1024)
/* OpenServer provides no sys_siglist,
   but does offer the same data under another name.  */
#define sys_siglist _sys_siglist
#undef SYS_SIGLIST_DECLARED
#define SYS_SIGLIST_DECLARED
