/* Definitions of target machine for GNU compiler, for SPARC running Solaris 2
   using the system linker.  */

#include "sparc/sol2.h"

/* Disable any support for DWARF and DWARF2 if we are using the system linker.
   At least up through Solaris 2.6,
   the system linker does not work with DWARF or DWARF2,
   since it does not have working support for relocations
   to unaligned data.  */

#ifdef DWARF2_DEBUGGING_INFO
#undef DWARF2_DEBUGGING_INFO
#endif

#ifdef DWARF_DEBUGGING_INFO
#undef DWARF_DEBUGGING_INFO
#endif
