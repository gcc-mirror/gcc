/*
 * dpx2.h - Bull DPX/2 200 and 300 systems (m68k, SysVr3)
 */

#include "m68k.h"
#undef SELECT_RTX_SECTION
#include "svr3.h"

/* See m68k.h.  7 means 68020 with 68881. 
 * We really have 68030 and 68882,
 * but this will get us going.  
 */
#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT 7
#endif

#define OBJECT_FORMAT_COFF
#define NO_SYS_SIGLIST

#ifdef CPP_PREDEFINES
#undef CPP_PREDEFINES
#endif
/*
 * define all the things the compiler should
 */
#ifdef ncl_mr
# define CPP_PREDEFINES "-Dunix -Dbull -DDPX2 -DSVR3 -Dmc68000 -Dmc68020 -Dncl_mr=1"
#else
# ifdef ncl_el
# define CPP_PREDEFINES "-Dunix -Dbull -DDPX2 -DSVR3 -Dmc68000 -Dmc68020 -Dncl_el"
# else
#   define CPP_PREDEFINES "-Dunix -Dbull -DDPX2 -DSVR3 -Dmc68000 -Dmc68020"
# endif
#endif

#undef	CPP_SPEC
/*
 * use -ansi to imply POSIX and XOPEN and BULL source
 * no -ansi implies _SYSV
 */
/*
 * you can't get a DPX/2 without a 68882 but allow it
 * to be ignored...
 */
#if 0
# define CPP_SPEC "%{ansi:-D_POSIX_SOURCE -D_XOPEN_SOURCE -D_BULL_SOURCE}\
 %{!ansi:-D_SYSV}"
#else
# define __HAVE_68881__ 1
# define CPP_SPEC "%{!msoft-float:-D__HAVE_68881__ }\
 %{ansi:-D_POSIX_SOURCE -D_XOPEN_SOURCE -D_BULL_SOURCE}\
 %{!ansi:-D_SYSV}"
#endif

#undef ASM_LONG
#define ASM_LONG "\t.long"

#define HAVE_ATEXIT
#undef DO_GLOBAL_CTORS_BODY		/* don't use svr3.h version */
#undef DO_GLOBAL_DTORS_BODY

#if 0 /* Should be no need now that svr3.h defines BSS_SECTION_FUNCTION.  */
/* 
 * svr3.h says to use BSS_SECTION_FUNCTION
 * but no one appears to, and there is
 * no definition for m68k.
 */
#ifndef BSS_SECTION_FUNCTION
# undef EXTRA_SECTION_FUNCTIONS
# define EXTRA_SECTION_FUNCTIONS	\
  CONST_SECTION_FUNCTION		\
  INIT_SECTION_FUNCTION			\
  FINI_SECTION_FUNCTION
#endif
#endif /* 0 */

#ifndef USE_GAS
/*
 * handle the native assembler.
 * this does NOT yet work, there is much left to do.
 * use GAS for now...
 */
#undef ASM_OUTPUT_SOURCE_FILENAME
#define ASM_OUTPUT_SOURCE_FILENAME(FILE, NA)	\
  do { fprintf ((FILE), "\t.file\t'%s'\n", (NA)); } while (0)

/* 
 * we don't seem to support any of:
 * .globl
 * .even
 * .align
 * .ascii
 */
#undef ASM_GLOBALIZE_LABEL
#define ASM_GLOBALIZE_LABEL(FILE,NAME) while (0)
#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(asm_out_file, align) while (0)
#define STRING_LIMIT	(0)
#undef ASM_APP_ON
#define ASM_APP_ON ""
#undef ASM_APP_OFF
#define ASM_APP_OFF ""
/*
 * dc.b 'hello, world!'
 * dc.b 10,0
 * is how we have to output "hello, world!\n"
 */
#undef ASM_OUTPUT_ASCII
#define ASM_OUTPUT_ASCII(asm_out_file, p, thissize)	\
  do { register int i, c, f=0;			\
  for (i = 0; i < thissize; i++) { 		\
    c = p[i];					\
    if (c == '\'' || c < ' ' || c > 127) {	\
      switch(f) {				\
      case 0: /* need to output dc.b etc */	\
	fprintf(asm_out_file, "\tdc.b %d", c);	\
	f=1;					\
	break;					\
      case 1:					\
	fprintf(asm_out_file, ",%d", c);	\
	break;					\
      default:					\
	/* close a string */			\
	fprintf(asm_out_file, "'\n\tdc.b %d", c); \
	f=1;					\
	break;					\
      }						\
    } else {					\
      switch(f) {				\
      case 0:					\
	fprintf(asm_out_file, "\tdc.b '%c", c);	\
	f=2;					\
	break;					\
      case 2:					\
	fprintf(asm_out_file, "%c", c);		\
	break;					\
      default:					\
	fprintf(asm_out_file, "\n\tdc.b '%c", c); \
	f=2;					\
	break;					\
      }						\
    }						\
  }						\
  if (f==2)					\
    putc('\'', asm_out_file);			\
  putc('\n', asm_out_file); } while (0)

/* This is how to output an insn to push a register on the stack.
   It need not be very fast code.  */

#undef ASM_OUTPUT_REG_PUSH
#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)  \
  fprintf (FILE, "\tmove.l %s,-(sp)\n", reg_names[REGNO])

/* This is how to output an insn to pop a register from the stack.
   It need not be very fast code.  */

#undef ASM_OUTPUT_REG_POP
#define ASM_OUTPUT_REG_POP(FILE,REGNO)  \
  fprintf (FILE, "\tmove.l (sp)+,%s\n", reg_names[REGNO])
			
#endif /* ! use gas */			
