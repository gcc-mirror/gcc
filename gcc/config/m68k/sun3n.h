/* Define target machine as a Sun 3 with no 68881.  */

#define TARGET_DEFAULT (MASK_BITFIELD|MASK_68020)

#include "m68k/sun3.h"

/* Don't try using XFmode.  */
#undef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE 64
