#include "sun3.h"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dmc68000 -Dsun -Dsun3 -Dunix -DMACH -DCMU -DMTXINU -DBIT_MSF -DBYTE_MSF"

/* Specify extra dir to search for include files.  */
#define SYSTEM_INCLUDE_DIR "/usr/mach/include"

/* LINK_SPEC is needed only for Sunos 4.  */

#undef LINK_SPEC
