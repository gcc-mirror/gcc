#include "sparc.h"

/* Define the Sun-asm flag, which is necessary for Sun 4 with os version 3.  */
#undef TARGET_DEFAULT
#define TARGET_DEFAULT 7

#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO)  \
  fprintf (FILE, "\tsethi %%hi(LP%d),%%o0\n\tcall .mcount\n\tor %%lo(LP%d),%%o0,%%o0\n", \
	   (LABELNO), (LABELNO))

/* LINK_SPEC is needed only for Sunos 4.  */

#undef LINK_SPEC
