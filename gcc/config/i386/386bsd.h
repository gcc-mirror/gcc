/* Configuration for an i386 running 386BSD as the target machine.  */
#include "i386mach.h"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dunix -Di386 -D____386BSD____"

/* Specify extra dir to search for include files.  */
#undef SYSTEM_INCLUDE_DIR
