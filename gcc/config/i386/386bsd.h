/* Configuration for an i386 running 386BSD as the target machine.  */

/* This is tested by i386gas.h.  */
#define YES_UNDERSCORES

#include "i386/gstabs.h"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dunix -D____386BSD____ -D__386BSD__ -DBSD_NET2 -Asystem=unix -Asystem=bsd"

/* Like the default, except no -lg.  */
#define LIB_SPEC "%{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p}"

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "short unsigned int"

#define WCHAR_UNSIGNED 1

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 16

/* Redefine this to use %eax instead of %edx.  */
#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO)  \
{									\
  if (flag_pic)								\
    {									\
      fprintf (FILE, "\tleal %sP%d@GOTOFF(%%ebx),%%eax\n",		\
	       LPREFIX, (LABELNO));					\
      fprintf (FILE, "\tcall *mcount@GOT(%%ebx)\n");			\
    }									\
  else									\
    {									\
      fprintf (FILE, "\tmovl $%sP%d,%%eax\n", LPREFIX, (LABELNO));	\
      fprintf (FILE, "\tcall mcount\n");				\
    }									\
}

#undef ASM_APP_ON
#define ASM_APP_ON "#APP\n"

#undef ASM_APP_OFF
#define ASM_APP_OFF "#NO_APP\n"

/* Don't default to pcc-struct-return, because gcc is the only compiler, and
   we want to retain compatibility with older gcc versions.  */
#define DEFAULT_PCC_STRUCT_RETURN 0
