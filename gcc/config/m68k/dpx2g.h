/*
 * dpx2g.h - Bull DPX/2 200 and 300 systems (m68k, SysVr3) with gas
 */

#define USE_GAS
#include "m68k/dpx2.h"

#if 0 /* #ifndef USE_COLLECT2 */

/* We use set vectors for the constructors/destructors. */ 

#undef ASM_OUTPUT_CONSTRUCTOR
#undef ASM_OUTPUT_DESTRUCTOR

/* Although the gas we use can create .ctor and .dtor sections from N_SETT
   stabs, it does not support section directives, so we need to have the loader
   define the lists.
 */
#define CTOR_LISTS_DEFINED_EXTERNALLY

/* similar to default, but allows for the table defined by ld with gcc.ifile. 
   nptrs is always 0.  So we need to instead check that __DTOR_LIST__[1] != 0.
   The old check is left in so that the same macro can be used if and when  
   a future version of gas does support section directives. */

#define DO_GLOBAL_DTORS_BODY {int nptrs = *(int *)__DTOR_LIST__; int i; \
  if (nptrs == -1 || (__DTOR_LIST__[0] == 0 && __DTOR_LIST__[1] != 0))  \
    for (nptrs = 0; __DTOR_LIST__[nptrs + 1] != 0; nptrs++); 		\
  for (i = nptrs; i >= 1; i--)						\
    __DTOR_LIST__[i] (); }

#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
  "%{!r:gcc.ifile%s}\
   %{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}\
  huge.o%s"

#endif /* !USE_COLLECT2 */

#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
  "%{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}\
  huge.o%s"


/*
 * we are using GAS
 */
#undef EXTRA_SECTION_FUNCTIONS
#undef EXTRA_SECTIONS
/* Gas understands dollars in labels. */
#undef NO_DOLLAR_IN_LABEL
/* GAS does not understand .ident so don't output anything for #ident.  */
#undef ASM_OUTPUT_IDENT

#undef ASM_LONG
#define ASM_LONG "\t.long"

/*
 * put const's in the text section
 */
#define const_section()  text_section()
#define fini_section() while (0)
			       
#undef CTORS_SECTION_ASM_OP
#define CTORS_SECTION_ASM_OP "\t.data"
#undef DTORS_SECTION_ASM_OP
#define DTORS_SECTION_ASM_OP "\t.data"
#undef INIT_SECTION_ASM_OP

/* end of dpx2g.h */
