/* This file is for the Sony Mips News running "NewsOS Version 5",
   which is really System V.  */
#include "mips/xm-sysv.h"

/* Sony has a funny name for this symbol.  */
#define sys_siglist _sys_siglist
#undef SYS_SIGLIST_DECLARED
#define SYS_SIGLIST_DECLARED
