/* Configuration for an i860 running Mach as the target machine.  */

#include "i860.h"

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (i860 Mach3.x)");

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dunix -Di860 -DMACH"

/* Specify extra dir to search for include files.  */
#define SYSTEM_INCLUDE_DIR "/usr/mach/include"
