/* Definitions for Intel 386 running Interactive Unix System V.
   Specifically, this is for recent versions that support POSIX;
   for version 2.0.2, use configuration option i386-sysv instead.  */

/* Mostly it's like AT&T Unix System V. */

#include "i386v.h"

/* Use crt0.o or crt1.o as a startup file and crtn.o as a closing file.  */
#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
  "%{!shlib:%{posix:%{pg:mcrtp1.o%s}%{!pg:%{p:mcrtp1.o%s}%{!p:crtp0.o%s}}}\
   %{!posix:%{pg:mcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}\
   %{p:-L/lib/libp} %{pg:-L/lib/libp}}}\
   %{shlib:%{posix:crtp1.o%s}%{!posix:crt1.o%s}} crtbegin.o%s"
  
#define ENDFILE_SPEC "crtend.o%s crtn.o%s"

/* Library spec */
#undef LIB_SPEC
#define LIB_SPEC "%{posix:-lcposix} %{shlib:-lc_s} -lc"

/* caller has to pop the extra argument passed to functions that return
   structures. */

#undef RETURN_POPS_ARGS
#define RETURN_POPS_ARGS(FUNTYPE,SIZE)   \
  (TREE_CODE (FUNTYPE) == IDENTIFIER_NODE ? 0			\
   : (TARGET_RTD						\
      && (TYPE_ARG_TYPES (FUNTYPE) == 0				\
	  || (TREE_VALUE (tree_last (TYPE_ARG_TYPES (FUNTYPE)))	\
	      == void_type_node))) ? (SIZE)			\
   : 0)
/* On other 386 systems, the last line looks like this:
   : (aggregate_value_p (FUNTYPE)) ? GET_MODE_SIZE (Pmode) : 0)  */

