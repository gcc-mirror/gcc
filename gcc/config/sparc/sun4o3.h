#include "sparc/sparc.h"

/* Override the name of the mcount profiling function.  */

#undef MCOUNT_FUNCTION
#define MCOUNT_FUNCTION "*.mcount"

/* LINK_SPEC is needed only for SunOS 4.  */

#undef LINK_SPEC

/* Override MACHINE_STATE_{SAVE,RESTORE} because we have special
   traps available which can get and set the condition codes
   reliably.  */
#undef MACHINE_STATE_SAVE
#define MACHINE_STATE_SAVE(ID)				\
  unsigned long int ms_flags, ms_saveret;		\
  asm volatile("ta	0x20\n\t"			\
	       "mov	%%g1, %0\n\t"			\
	       "mov	%%g2, %1\n\t"			\
	       : "=r" (ms_flags), "=r" (ms_saveret));

#undef MACHINE_STATE_RESTORE
#define MACHINE_STATE_RESTORE(ID)			\
  asm volatile("mov	%0, %%g1\n\t"			\
	       "mov	%1, %%g2\n\t"			\
	       "ta	0x21\n\t"			\
	       : /* no outputs */			\
	       : "r" (ms_flags), "r" (ms_saveret));
