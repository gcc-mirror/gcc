/*
 * dpx2g.h - Bull DPX/2 200 and 300 systems (m68k, SysVr3) with gas
 */

#define USE_GAS
#include "dpx2.h"


/* GAS want's DBX debugging information.  */
#undef SDB_DEBUGGING_INFO
#ifndef DBX_DEBUGGING_INFO
#define DBX_DEBUGGING_INFO
#endif

/*
 * we are using GAS
 */
#undef EXTRA_SECTION_FUNCTIONS
#undef EXTRA_SECTIONS

/*
 * put const's in the text section
 */
#define const_section()  text_section()
#define fini_section() while (0)
			       
#if 0					/* this is fixed in 2.1 */
#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) >= 1)			\
    fprintf (FILE, "\t.even\n");	
#endif

#undef CTORS_SECTION_ASM_OP
#define CTORS_SECTION_ASM_OP "\t.data"
#undef DTORS_SECTION_ASM_OP
#define DTORS_SECTION_ASM_OP "\t.data"
#undef INIT_SECTION_ASM_OP

/* end of dpx2g.h */
