/*
 * dpx2g.h - Bull DPX/2 200 and 300 systems (m68k, SysVr3) with gas
 */

#define USE_GAS
#include "m68k/dpx2.h"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
  "%{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}\
  huge.o%s"

/* Gas understands dollars in labels.  */
#undef NO_DOLLAR_IN_LABEL
/* GAS does not understand .ident so don't output anything for #ident.  */
#undef ASM_OUTPUT_IDENT

/* end of dpx2g.h */
