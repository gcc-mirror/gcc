/* Configuration for i386 interfacing with SCO's Universal Development Kit
   probably running on OpenServer 5, Unixware 2, or Unixware 5
 */


/* We're very much the SVR4 target with "/udk" prepended to everything that's
   interesting */

#include "i386/sysv5.h"

#undef MD_EXEC_PREFIX
#define MD_EXEC_PREFIX 		"/udk/usr/ccs/bin/" 

#undef MD_STARTFILE_PREFIX
#define MD_STARTFILE_PREFIX 	"/udk/usr/ccs/lib/" 

#define STANDARD_INCLUDE_DIR	"/udk/usr/include"

#undef LINK_SPEC
#define LINK_SPEC "%{h*} %{v:-V} \
	%{b} %{Wl,*:%*} \
	%{static:-dn -Bstatic} \
	%{shared:-G -dy -z text} \
	%{symbolic:-Bsymbolic -G -dy -z text} \
	%{G:-G} \
	%{YP,*} \
	%{!YP,*:%{p:-Y P,/udk/usr/ccs/lib/libp:/udk/usr/lib/libp:/udk/usr/ccs/lib:/udk/usr/lib} \
	%{!p:-Y P,/udk/usr/ccs/lib:/udk/usr/lib}} \
	%{Qy:} %{!Qn:-Qy}"

