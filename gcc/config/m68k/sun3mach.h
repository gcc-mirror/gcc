#include "m68k/sun3.h"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dmc68000 -Dsun -Dsun3 -Dunix -DMACH -DCMU -DMTXINU -DBIT_MSF -DBYTE_MSF -Asystem(unix)  -Asystem(mach) -Acpu(m68k) -Amachine(m68k)"

/* Specify extra dir to search for include files.  */
#define SYSTEM_INCLUDE_DIR "/usr/mach/include"

/* LINK_SPEC is needed only for Sunos 4.  */

#undef LINK_SPEC

/* Don't default to pcc-struct-return, because gcc is the only compiler, and
   we want to retain compatibility with older gcc versions.  */
#define DEFAULT_PCC_STRUCT_RETURN 0
