/* Configuration for an i386 running Mach as the target machine.  */

/* We do want to add an underscore to the front of each user symbol.
   i386/gas.h checks this.  */
#define YES_UNDERSCORES

#include "i386/gstabs.h"

/* Get perform_* macros to build libgcc.a.  */
#include "i386/perform.h"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dunix -Di386 -DMACH"

/* Specify extra dir to search for include files.  */
#define SYSTEM_INCLUDE_DIR "/usr/mach/include"

/* Make stddef.h agree with types.h.  */
#define SIZE_TYPE "long int"
