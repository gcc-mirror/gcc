/* Define target machine as an ISI 68000/68020 with no 68881.  */

#define TARGET_DEFAULT 5

#include "m68k/isi.h"

/* Don't try using XFmode.  */
#undef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE 64
