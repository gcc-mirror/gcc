/* Definitions of target machine for GNU compiler, for SPARC running Solaris 2
   using the system linker.  */

#include "sparc/sol2.h"

/* At least up through Solaris 2.6,
   the system linker does not work with DWARF or DWARF2,
   since it does not have working support for relocations
   to unaligned data.  */

#define LINKER_DOES_NOT_WORK_WITH_DWARF2
