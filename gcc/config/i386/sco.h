/* Definitions for Intel 386 running SCO Unix System V.  */


/* Mostly it's like AT&T Unix System V. */

#include "i386/sysv3.h"

/* By default, target has a 80387, uses IEEE compatible arithmetic,
   and returns float values in the 387, ie,
   (TARGET_80387 | TARGET_FLOAT_RETURNS_IN_80387)

   SCO's software emulation of a 387 fails to handle the `fucomp'
   opcode.  fucomp is only used when generating IEEE compliant code.
   So don't make TARGET_IEEE_FP default for SCO. */

#undef TARGET_DEFAULT
#define TARGET_DEFAULT 0201

/* Let's guess that the SCO software FPU emulator can't handle
   80-bit XFmode insns, so don't generate them.  */
#undef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE 64

/* Use crt1.o as a startup file and crtn.o as a closing file.  */

#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
 "%{pg:gcrt1.o%s}%{!pg:%{p:mcrt1.o%s}%{!p:crt1.o%s}} crtbegin.o%s"

#define ENDFILE_SPEC "crtend.o%s crtn.o%s"

/* Library spec, including SCO international language support. */

#undef LIB_SPEC
#define LIB_SPEC \
 "%{p:-L/usr/lib/libp}%{pg:-L/usr/lib/libp} %{scointl:libintl.a%s} -lc"

/* Specify predefined symbols in preprocessor.  */

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dunix -Di386 -DM_UNIX -DM_I386 -DM_COFF -DM_WORDSWAP -Asystem(unix) -Asystem(svr3) -Acpu(i386) -Amachine(i386)"

#undef CPP_SPEC
#define CPP_SPEC "%{scointl:-DM_INTERNAT}"

/* This spec is used for telling cpp whether char is signed or not.  */

#undef SIGNED_CHAR_SPEC
#if DEFAULT_SIGNED_CHAR
#define SIGNED_CHAR_SPEC \
 "%{funsigned-char:-D__CHAR_UNSIGNED__ -D_CHAR_UNSIGNED}"
#else
#define SIGNED_CHAR_SPEC \
 "%{!fsigned-char:-D__CHAR_UNSIGNED__ -D_CHAR_UNSIGNED}"
#endif

/* Use atexit for static destructors, instead of defining
   our own exit function.  */
#define HAVE_ATEXIT

/* Specify the size_t type.  */
#define SIZE_TYPE "unsigned int"

#if 0 /* Not yet certain whether this is needed.  */
/* If no 387, use the general regs to return floating values,
   since this system does not emulate the 80387.  */

#undef VALUE_REGNO
#define VALUE_REGNO(MODE) \
  ((TARGET_80387
     && ((MODE) == SFmode || (MODE) == DFmode || (MODE) == XFmode)
   ? FIRST_FLOAT_REG : 0)

#undef HARD_REGNO_MODE_OK
#define HARD_REGNO_MODE_OK(REGNO, MODE)					  \
  ((REGNO) < 2 ? 1							  \
   : (REGNO) < 4 ? 1							  \
   : FP_REGNO_P (REGNO) ? ((GET_MODE_CLASS (MODE) == MODE_FLOAT		  \
                          || GET_MODE_CLASS (MODE) == MODE_COMPLEX_FLOAT) \
                         && TARGET_80387				  \
                         && GET_MODE_UNIT_SIZE (MODE) <= 8)		  \
   : (MODE) != QImode)
#endif

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
   : (aggregate_value_p (TREE_TYPE (FUNTYPE))) ? GET_MODE_SIZE (Pmode) : 0)  */

/* Handle #pragma pack. */
#define HANDLE_SYSV_PRAGMA
