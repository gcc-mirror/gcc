#include "sysv3.h"

/* Sequent Symmetry SVr3 doesn't have crtn.o; crt1.o doesn't work
   but crt0.o does.  */

#undef STARTFILE_SPEC
#define STARTFILE_SPEC  \
"%{pg:gcrt0.o%s}\
 %{!pg:%{posix:%{p:mcrtp0.o%s}%{!p:crtp0.o%s}}\
       %{!posix:%{p:mcrt0.o%s}%{!p:crt0.o%s}}} crtbegin.o%s\
 %{p:-L/usr/lib/libp}%{pg:-L/usr/lib/libp}"

#undef LIB_SPEC
#define LIB_SPEC "%{posix:-lcposix} %{shlib:-lc_s} -lc crtend.o%s"
