/* Like iris4.h, but always inhibits assembler optimization for MIPS as.
   Use this via mips-sgi-iris4loser if you need it.  */

#define SUBTARGET_MIPS_AS_ASM_SPEC "-O0 %{v}"
#define SUBTARGET_ASM_OPTIMIZING_SPEC ""

#include "mips/iris4.h"
