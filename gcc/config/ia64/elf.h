/* Definitions for embedded ia64-elf target.  */

#include "ia64/ia64.h"
#include "elfos.h"
#include "sysv4.h"

/* svr4.h links with crti.o/crtn.o, but elfos.h does not.  We override elfos.h
   so that we can use the standard ELF Unix method.  */
#undef  ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s crtn.o%s"

#undef	STARTFILE_SPEC
#define STARTFILE_SPEC "%{!shared: \
			 %{!symbolic: \
			  %{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}}}\
			crti.o%s crtbegin.o%s"

/* End of elf.h */
