/* Target macros for the FRV port of GCC.
   Copyright (C) 1999, 2000, 2001, 2002 Free Software Foundation, Inc.
   Contributed by Red Hat Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING.  If not, write to the Free
   Software Foundation, 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.  */

#ifndef __FRV_H__
#define __FRV_H__

/* Set up System V.4 (aka ELF) defaults.  */
#include "svr4.h"


/* Frv general purpose macros.  */
/* Align an address.  */
#define ADDR_ALIGN(addr,align) (((addr) + (align) - 1) & ~((align) - 1))

/* Return true if a value is inside a range.  */
#define IN_RANGE_P(VALUE, LOW, HIGH)				\
  (   (((HOST_WIDE_INT)(VALUE)) >= (HOST_WIDE_INT)(LOW))	\
   && (((HOST_WIDE_INT)(VALUE)) <= ((HOST_WIDE_INT)(HIGH))))


/* Driver configuration.  */

/* A C expression which determines whether the option `-CHAR' takes arguments.
   The value should be the number of arguments that option takes-zero, for many
   options.

   By default, this macro is defined to handle the standard options properly.
   You need not define it unless you wish to add additional options which take
   arguments.

   Defined in svr4.h.  */
#undef  SWITCH_TAKES_ARG
#define SWITCH_TAKES_ARG(CHAR)                                          \
  (DEFAULT_SWITCH_TAKES_ARG (CHAR) || (CHAR) == 'G')

/* A C expression which determines whether the option `-NAME' takes arguments.
   The value should be the number of arguments that option takes-zero, for many
   options.  This macro rather than `SWITCH_TAKES_ARG' is used for
   multi-character option names.

   By default, this macro is defined as `DEFAULT_WORD_SWITCH_TAKES_ARG', which
   handles the standard options properly.  You need not define
   `WORD_SWITCH_TAKES_ARG' unless you wish to add additional options which take
   arguments.  Any redefinition should call `DEFAULT_WORD_SWITCH_TAKES_ARG' and
   then check for additional options.

   Defined in svr4.h.  */
#undef WORD_SWITCH_TAKES_ARG

/* A C string constant that tells the GNU CC driver program options to pass to
   the assembler.  It can also specify how to translate options you give to GNU
   CC into options for GNU CC to pass to the assembler.  See the file `sun3.h'
   for an example of this.

   Do not define this macro if it does not need to do anything.

   Defined in svr4.h.  */
#undef  ASM_SPEC
#define ASM_SPEC "\
%{G*} %{v} %{n} %{T} %{Ym,*} %{Yd,*} %{Wa,*:%*} \
%{mtomcat-stats} \
%{!mno-eflags: \
    %{mcpu=*} \
    %{mgpr-*} %{mfpr-*} \
    %{msoft-float} %{mhard-float} \
    %{mdword} %{mno-dword} \
    %{mdouble} %{mno-double} \
    %{mmedia} %{mno-media} \
    %{mmuladd} %{mno-muladd} \
    %{mpack} %{mno-pack} \
    %{fpic: -mpic} %{fPIC: -mPIC} %{mlibrary-pic}}"

/* Another C string constant used much like `LINK_SPEC'.  The difference
   between the two is that `STARTFILE_SPEC' is used at the very beginning of
   the command given to the linker.

   If this macro is not defined, a default is provided that loads the standard
   C startup file from the usual place.  See `gcc.c'.

   Defined in svr4.h.  */
#undef  STARTFILE_SPEC
#define STARTFILE_SPEC "crt0%O%s frvbegin%O%s"

/* Another C string constant used much like `LINK_SPEC'.  The difference
   between the two is that `ENDFILE_SPEC' is used at the very end of the
   command given to the linker.

   Do not define this macro if it does not need to do anything.

   Defined in svr4.h.  */
#undef  ENDFILE_SPEC
#define ENDFILE_SPEC "frvend%O%s"

/* A C string constant that tells the GNU CC driver program options to pass to
   CPP.  It can also specify how to translate options you give to GNU CC into
   options for GNU CC to pass to the CPP.

   Do not define this macro if it does not need to do anything.  */

/* The idea here is to use the -mcpu option to define macros based on the
   processor's features, using the features of the default processor if
   no -mcpu option is given.  These macros can then be overridden by
   other -m options.  */
#define CPP_SPEC "\
%{mcpu=frv: %(cpp_frv)} \
%{mcpu=fr500: %(cpp_fr500)} \
%{mcpu=fr400: %(cpp_fr400)} \
%{mcpu=fr300: %(cpp_simple)} \
%{mcpu=tomcat: %(cpp_fr500)} \
%{mcpu=simple: %(cpp_simple)} \
%{!mcpu*: %(cpp_cpu_default)} \
%{mno-media: -D__FRV_ACC__=0 %{msoft-float: -D__FRV_FPR__=0}} \
%{mhard-float: -D__FRV_HARD_FLOAT__} \
%{msoft-float: -U__FRV_HARD_FLOAT__} \
%{mgpr-32: -U__FRV_GPR__ -D__FRV_GPR__=32} \
%{mgpr-64: -U__FRV_GPR__ -D__FRV_GPR__=64} \
%{mfpr-32: -U__FRV_FPR__ -D__FRV_FPR__=32} \
%{mfpr-64: -U__FRV_FPR__ -D__FRV_FPR__=64} \
%{macc-4: -U__FRV_ACC__ -D__FRV_ACC__=4} \
%{macc-8: -U__FRV_ACC__ -D__FRV_ACC__=8} \
%{mdword: -D__FRV_DWORD__} \
%{mno-dword: -U__FRV_DWORD__} \
%{mno-pack: -U__FRV_VLIW__} \
%{fleading-underscore: -D__FRV_UNDERSCORE__}"

/* CPU defaults.  Each CPU has its own CPP spec that defines the default
   macros for that CPU.  Each CPU also has its own default target mask.

   CPU		GPRs	FPRs	ACCs	FPU	MulAdd  ldd/std  Issue rate
   ---		----    ----    ----    ---	------  -------  ----------
   FRV		64	64	8	double	yes	yes      4
   FR500	64	64	8	single	no	yes      4
   FR400	32	32	4	none	no	yes      2
   Simple	32	0	0	none	no	no       1 */


#define CPP_FRV_SPEC "\
-D__FRV_GPR__=64 \
-D__FRV_FPR__=64 \
-D__FRV_ACC__=8 \
-D__FRV_HARD_FLOAT__ \
-D__FRV_DWORD__ \
-D__FRV_VLIW__=4"

#define CPP_FR500_SPEC "\
-D__FRV_GPR__=64 \
-D__FRV_FPR__=64 \
-D__FRV_ACC__=8 \
-D__FRV_HARD_FLOAT__ \
-D__FRV_DWORD__ \
-D__FRV_VLIW__=4"

#define CPP_FR400_SPEC "\
-D__FRV_GPR__=32 \
-D__FRV_FPR__=32 \
-D__FRV_ACC__=4 \
-D__FRV_DWORD__ \
-D__FRV_VLIW__=2"

#define CPP_SIMPLE_SPEC "\
-D__FRV_GPR__=32 \
-D__FRV_FPR__=0 \
-D__FRV_ACC__=0 \
%{mmedia: -D__FRV_ACC__=8} \
%{mhard-float|mmedia: -D__FRV_FPR__=64}"

#define MASK_DEFAULT_FRV	\
  (MASK_MEDIA			\
   | MASK_DOUBLE		\
   | MASK_MULADD		\
   | MASK_DWORD			\
   | MASK_PACK)

#define MASK_DEFAULT_FR500 \
  (MASK_MEDIA | MASK_DWORD | MASK_PACK)

#define MASK_DEFAULT_FR400	\
  (MASK_GPR_32			\
   | MASK_FPR_32		\
   | MASK_MEDIA			\
   | MASK_ACC_4			\
   | MASK_SOFT_FLOAT		\
   | MASK_DWORD			\
   | MASK_PACK)

#define MASK_DEFAULT_SIMPLE \
  (MASK_GPR_32 | MASK_SOFT_FLOAT)

/* A C string constant that tells the GNU CC driver program options to pass to
   `cc1'.  It can also specify how to translate options you give to GNU CC into
   options for GNU CC to pass to the `cc1'.

   Do not define this macro if it does not need to do anything.  */
/* For ABI compliance, we need to put bss data into the normal data section.  */
#define CC1_SPEC "%{G*}"

/* A C string constant that tells the GNU CC driver program options to pass to
   the linker.  It can also specify how to translate options you give to GNU CC
   into options for GNU CC to pass to the linker.

   Do not define this macro if it does not need to do anything.

   Defined in svr4.h.  */
/* Override the svr4.h version with one that dispenses without the svr4
   shared library options, notably -G.  */
#undef	LINK_SPEC
#define LINK_SPEC "\
%{h*} %{v:-V} \
%{b} %{Wl,*:%*} \
%{static:-dn -Bstatic} \
%{shared:-Bdynamic} \
%{symbolic:-Bsymbolic} \
%{G*} \
%{YP,*} \
%{Qy:} %{!Qn:-Qy}"

/* Another C string constant used much like `LINK_SPEC'.  The difference
   between the two is that `LIB_SPEC' is used at the end of the command given
   to the linker.

   If this macro is not defined, a default is provided that loads the standard
   C library from the usual place.  See `gcc.c'.

   Defined in svr4.h.  */

#undef  LIB_SPEC
#define LIB_SPEC "--start-group -lc -lsim --end-group"

/* This macro defines names of additional specifications to put in the specs
   that can be used in various specifications like CC1_SPEC.  Its definition
   is an initializer with a subgrouping for each command option.

   Each subgrouping contains a string constant, that defines the
   specification name, and a string constant that used by the GNU CC driver
   program.

   Do not define this macro if it does not need to do anything.  */

#ifndef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS
#endif

#define EXTRA_SPECS							\
  { "cpp_frv",		CPP_FRV_SPEC },					\
  { "cpp_fr500",	CPP_FR500_SPEC },				\
  { "cpp_fr400",	CPP_FR400_SPEC },				\
  { "cpp_simple",	CPP_SIMPLE_SPEC },				\
  { "cpp_cpu_default",	CPP_CPU_DEFAULT_SPEC },				\
  SUBTARGET_EXTRA_SPECS

#ifndef CPP_CPU_DEFAULT_SPEC
#define CPP_CPU_DEFAULT_SPEC	CPP_FR500_SPEC
#define CPU_TYPE		FRV_CPU_FR500
#endif

/* Allow us to easily change the default for -malloc-cc.  */
#ifndef	DEFAULT_NO_ALLOC_CC
#define MASK_DEFAULT_ALLOC_CC	MASK_ALLOC_CC
#else
#define MASK_DEFAULT_ALLOC_CC	0
#endif

/* Run-time target specifications */

/* Define this to be a string constant containing `-D' options to define the
   predefined macros that identify this machine and system.  These macros will
   be predefined unless the `-ansi' option is specified.

   In addition, a parallel set of macros are predefined, whose names are made
   by appending `__' at the beginning and at the end.  These `__' macros are
   permitted by the ANSI standard, so they are predefined regardless of whether
   `-ansi' is specified.  */

#define CPP_PREDEFINES "-D__frv__ -Amachine(frv)"


/* This declaration should be present.  */
extern int target_flags;

/* This series of macros is to allow compiler command arguments to enable or
   disable the use of optional features of the target machine.  For example,
   one machine description serves both the 68000 and the 68020; a command
   argument tells the compiler whether it should use 68020-only instructions or
   not.  This command argument works by means of a macro `TARGET_68020' that
   tests a bit in `target_flags'.

   Define a macro `TARGET_FEATURENAME' for each such option.  Its definition
   should test a bit in `target_flags'; for example:

        #define TARGET_68020 (target_flags & 1)

   One place where these macros are used is in the condition-expressions of
   instruction patterns.  Note how `TARGET_68020' appears frequently in the
   68000 machine description file, `m68k.md'.  Another place they are used is
   in the definitions of the other macros in the `MACHINE.h' file.  */

#define MASK_GPR_32	     0x00000001	/* Limit gprs to 32 registers */
#define MASK_FPR_32	     0x00000002	/* Limit fprs to 32 registers */
#define MASK_SOFT_FLOAT	     0x00000004	/* Use software floating point */
#define MASK_ALLOC_CC	     0x00000008	/* Dynamically allocate icc/fcc's */
#define MASK_DWORD	     0x00000010	/* Change ABi to allow dbl word insns*/
#define MASK_DOUBLE	     0x00000020	/* Use double precision instructions */
#define MASK_MEDIA	     0x00000040	/* Use media instructions */
#define MASK_MULADD	     0x00000080	/* Use multiply add/subtract insns */
#define MASK_LIBPIC	     0x00000100	/* -fpic that can be linked w/o pic */
#define MASK_ACC_4	     0x00000200	/* Only use four media accumulators */
#define MASK_PACK	     0x00000400 /* Set to enable packed output */

			 		/* put debug masks up high */
#define MASK_DEBUG_ARG	     0x40000000	/* debug argument handling */
#define MASK_DEBUG_ADDR	     0x20000000	/* debug go_if_legitimate_address */
#define MASK_DEBUG_STACK     0x10000000	/* debug stack frame */
#define MASK_DEBUG	     0x08000000	/* general debugging switch */
#define MASK_DEBUG_LOC	     0x04000000	/* optimize line # table */
#define MASK_DEBUG_COND_EXEC 0x02000000	/* debug cond exec code */
#define MASK_NO_COND_MOVE    0x01000000	/* disable conditional moves */
#define MASK_NO_SCC	     0x00800000	/* disable set conditional codes */
#define MASK_NO_COND_EXEC    0x00400000	/* disable conditional execution */
#define MASK_NO_VLIW_BRANCH  0x00200000	/* disable repacking branches */
#define MASK_NO_MULTI_CE     0x00100000	/* disable multi-level cond exec */
#define MASK_NO_NESTED_CE    0x00080000	/* disable nested cond exec */

#define MASK_DEFAULT		MASK_DEFAULT_ALLOC_CC

#define TARGET_GPR_32		((target_flags & MASK_GPR_32) != 0)
#define TARGET_FPR_32		((target_flags & MASK_FPR_32) != 0)
#define TARGET_SOFT_FLOAT	((target_flags & MASK_SOFT_FLOAT) != 0)
#define TARGET_ALLOC_CC		((target_flags & MASK_ALLOC_CC) != 0)
#define TARGET_DWORD		((target_flags & MASK_DWORD) != 0)
#define TARGET_DOUBLE		((target_flags & MASK_DOUBLE) != 0)
#define TARGET_MEDIA		((target_flags & MASK_MEDIA) != 0)
#define TARGET_MULADD		((target_flags & MASK_MULADD) != 0)
#define TARGET_LIBPIC		((target_flags & MASK_LIBPIC) != 0)
#define TARGET_ACC_4		((target_flags & MASK_ACC_4) != 0)
#define TARGET_DEBUG_ARG	((target_flags & MASK_DEBUG_ARG) != 0)
#define TARGET_DEBUG_ADDR	((target_flags & MASK_DEBUG_ADDR) != 0)
#define TARGET_DEBUG_STACK	((target_flags & MASK_DEBUG_STACK) != 0)
#define TARGET_DEBUG		((target_flags & MASK_DEBUG) != 0)
#define TARGET_DEBUG_LOC	((target_flags & MASK_DEBUG_LOC) != 0)
#define TARGET_DEBUG_COND_EXEC	((target_flags & MASK_DEBUG_COND_EXEC) != 0)
#define TARGET_NO_COND_MOVE	((target_flags & MASK_NO_COND_MOVE) != 0)
#define TARGET_NO_SCC		((target_flags & MASK_NO_SCC) != 0)
#define TARGET_NO_COND_EXEC	((target_flags & MASK_NO_COND_EXEC) != 0)
#define TARGET_NO_VLIW_BRANCH	((target_flags & MASK_NO_VLIW_BRANCH) != 0)
#define TARGET_NO_MULTI_CE	((target_flags & MASK_NO_MULTI_CE) != 0)
#define TARGET_NO_NESTED_CE	((target_flags & MASK_NO_NESTED_CE) != 0)
#define TARGET_PACK		((target_flags & MASK_PACK) != 0)

#define TARGET_GPR_64		(! TARGET_GPR_32)
#define TARGET_FPR_64		(! TARGET_FPR_32)
#define TARGET_HARD_FLOAT	(! TARGET_SOFT_FLOAT)
#define TARGET_FIXED_CC		(! TARGET_ALLOC_CC)
#define TARGET_COND_MOVE	(! TARGET_NO_COND_MOVE)
#define TARGET_SCC		(! TARGET_NO_SCC)
#define TARGET_COND_EXEC	(! TARGET_NO_COND_EXEC)
#define TARGET_VLIW_BRANCH	(! TARGET_NO_VLIW_BRANCH)
#define TARGET_MULTI_CE		(! TARGET_NO_MULTI_CE)
#define TARGET_NESTED_CE	(! TARGET_NO_NESTED_CE)
#define TARGET_ACC_8		(! TARGET_ACC_4)

#define TARGET_HAS_FPRS		(TARGET_HARD_FLOAT || TARGET_MEDIA)

#define NUM_GPRS		(TARGET_GPR_32? 32 : 64)
#define NUM_FPRS		(!TARGET_HAS_FPRS? 0 : TARGET_FPR_32? 32 : 64)
#define NUM_ACCS		(!TARGET_MEDIA? 0 : TARGET_ACC_4? 4 : 8)

/* Macros to identify the blend of media instructions available.  Revision 1
   is the one found on the FR500.  Revision 2 includes the changes made for
   the FR400.

   Treat the generic processor as a revision 1 machine for now, for
   compatibility with earlier releases.  */

#define TARGET_MEDIA_REV1					\
  (TARGET_MEDIA							\
   && (frv_cpu_type == FRV_CPU_GENERIC				\
       || frv_cpu_type == FRV_CPU_FR500))

#define TARGET_MEDIA_REV2					\
  (TARGET_MEDIA && frv_cpu_type == FRV_CPU_FR400)

/* This macro defines names of command options to set and clear bits in
   `target_flags'.  Its definition is an initializer with a subgrouping for
   each command option.

   Each subgrouping contains a string constant, that defines the option name,
   a number, which contains the bits to set in `target_flags', and an optional
   second string which is the textual description that will be displayed when
   the user passes --help on the command line.  If the number entry is negative
   then the specified bits will be cleared instead of being set.  If the second
   string entry is present but empty, then no help information will be displayed
   for that option, but it will not count as an undocumented option.  The actual
   option name, asseen on the command line is made by appending `-m' to the
   specified name.

   One of the subgroupings should have a null string.  The number in this
   grouping is the default value for `target_flags'.  Any target options act
   starting with that value.

   Here is an example which defines `-m68000' and `-m68020' with opposite
   meanings, and picks the latter as the default:

        #define TARGET_SWITCHES \
          { { "68020",  1, ""},      \
            { "68000", -1, "Compile for the m68000"},     \
            { "",       1, }}

   This declaration must be present.  */

#define TARGET_SWITCHES							    \
{{ "gpr-32",		  MASK_GPR_32,		"Only use 32 gprs"},	    \
 { "gpr-64",		 -MASK_GPR_32,		"Use 64 gprs"},		    \
 { "fpr-32",		  MASK_FPR_32,		"Only use 32 fprs"},	    \
 { "fpr-64",		 -MASK_FPR_32,		"Use 64 fprs"},		    \
 { "hard-float",	 -MASK_SOFT_FLOAT,	"Use hardware floating point" },\
 { "soft-float",	  MASK_SOFT_FLOAT,	"Use software floating point" },\
 { "alloc-cc",		  MASK_ALLOC_CC,	"Dynamically allocate cc's" }, \
 { "fixed-cc",		 -MASK_ALLOC_CC,	"Just use icc0/fcc0" },	    \
 { "dword",		  MASK_DWORD,		"Change ABI to allow double word insns" }, \
 { "no-dword",		 -MASK_DWORD,		"Do not use double word insns" }, \
 { "double",		  MASK_DOUBLE,		"Use fp double instructions" }, \
 { "no-double",		 -MASK_DOUBLE,		"Do not use fp double insns" }, \
 { "media",		  MASK_MEDIA,		"Use media instructions" }, \
 { "no-media",		 -MASK_MEDIA,		"Do not use media insns" }, \
 { "muladd",		  MASK_MULADD,		"Use multiply add/subtract instructions" }, \
 { "no-muladd",		 -MASK_MULADD,		"Do not use multiply add/subtract insns" }, \
 { "library-pic",	  MASK_LIBPIC,		"PIC support for building libraries" }, \
 { "acc-4",		  MASK_ACC_4,		"Use 4 media accumulators" }, \
 { "acc-8",		 -MASK_ACC_4,		"Use 8 media accumulators" }, \
 { "pack",		  MASK_PACK,		"Pack VLIW instructions" }, \
 { "no-pack",		 -MASK_PACK,		"Do not pack VLIW instructions" }, \
 { "no-eflags",		  0,			"Do not mark ABI switches in e_flags" }, \
 { "debug-arg",		  MASK_DEBUG_ARG,	"Internal debug switch" },  \
 { "debug-addr",	  MASK_DEBUG_ADDR,	"Internal debug switch" },  \
 { "debug-stack",	  MASK_DEBUG_STACK,	"Internal debug switch" },  \
 { "debug",		  MASK_DEBUG,		"Internal debug switch" },  \
 { "debug-cond-exec",	  MASK_DEBUG_COND_EXEC,	"Internal debug switch" },  \
 { "debug-loc",		  MASK_DEBUG_LOC,	"Internal debug switch" },  \
 { "cond-move",		 -MASK_NO_COND_MOVE,	"Enable conditional moves" },  \
 { "no-cond-move",	  MASK_NO_COND_MOVE,	"Disable conditional moves" },  \
 { "scc",		 -MASK_NO_SCC,		"Enable setting gprs to the result of comparisons" },  \
 { "no-scc",		  MASK_NO_SCC,		"Disable setting gprs to the result of comparisons" },  \
 { "cond-exec",		 -MASK_NO_COND_EXEC,	"Enable conditional execution other than moves/scc" }, \
 { "no-cond-exec",	  MASK_NO_COND_EXEC,	"Disable conditional execution other than moves/scc" }, \
 { "vliw-branch",	 -MASK_NO_VLIW_BRANCH,	"Run pass to pack branches into VLIW insns" }, \
 { "no-vliw-branch",	  MASK_NO_VLIW_BRANCH,	"Do not run pass to pack branches into VLIW insns" }, \
 { "multi-cond-exec",	 -MASK_NO_MULTI_CE,	"Disable optimizing &&/|| in conditional execution" }, \
 { "no-multi-cond-exec",  MASK_NO_MULTI_CE,	"Enable optimizing &&/|| in conditional execution" }, \
 { "nested-cond-exec",	 -MASK_NO_NESTED_CE,	"Enable nested conditional execution optimizations" }, \
 { "no-nested-cond-exec" ,MASK_NO_NESTED_CE,	"Disable nested conditional execution optimizations" }, \
 { "tomcat-stats",	  0, 			"Cause gas to print tomcat statistics" }, \
 { "",			  MASK_DEFAULT,		"" }}			    \

/* This macro is similar to `TARGET_SWITCHES' but defines names of command
   options that have values.  Its definition is an initializer with a
   subgrouping for each command option.

   Each subgrouping contains a string constant, that defines the fixed part of
   the option name, the address of a variable, and an optional description string.
   The variable, of type `char *', is set to the text following the fixed part of
   the option as it is specified on the command line.  The actual option name is
   made by appending `-m' to the specified name.

   Here is an example which defines `-mshort-data-NUMBER'.  If the given option
   is `-mshort-data-512', the variable `m88k_short_data' will be set to the
   string `"512"'.

        extern char *m88k_short_data;
        #define TARGET_OPTIONS \
         { { "short-data-", & m88k_short_data, \
	 "Specify the size of the short data section"  } }

   This declaration is optional.  */
#define TARGET_OPTIONS							    \
{									    \
  { "cpu=",		&frv_cpu_string,	 "Set cpu type" },	    \
  { "branch-cost=",	&frv_branch_cost_string, "Internal debug switch" }, \
  { "cond-exec-insns=", &frv_condexec_insns_str, "Internal debug switch" }, \
  { "cond-exec-temps=", &frv_condexec_temps_str, "Internal debug switch" }, \
  { "sched-lookahead=", &frv_sched_lookahead_str,"Internal debug switch" }, \
}

/* This macro is a C statement to print on `stderr' a string describing the
   particular machine description choice.  Every machine description should
   define `TARGET_VERSION'.  For example:

        #ifdef MOTOROLA
        #define TARGET_VERSION \
          fprintf (stderr, " (68k, Motorola syntax)");
        #else
        #define TARGET_VERSION \
          fprintf (stderr, " (68k, MIT syntax)");
        #endif  */
#define TARGET_VERSION fprintf (stderr, _(" (frv)"))

/* Sometimes certain combinations of command options do not make sense on a
   particular target machine.  You can define a macro `OVERRIDE_OPTIONS' to
   take account of this.  This macro, if defined, is executed once just after
   all the command options have been parsed.

   Don't use this macro to turn on various extra optimizations for `-O'.  That
   is what `OPTIMIZATION_OPTIONS' is for.  */

#define OVERRIDE_OPTIONS frv_override_options ()

/* Some machines may desire to change what optimizations are performed for
   various optimization levels.  This macro, if defined, is executed once just
   after the optimization level is determined and before the remainder of the
   command options have been parsed.  Values set in this macro are used as the
   default values for the other command line options.

   LEVEL is the optimization level specified; 2 if `-O2' is specified, 1 if
   `-O' is specified, and 0 if neither is specified.

   SIZE is nonzero if `-Os' is specified, 0 otherwise.

   You should not use this macro to change options that are not
   machine-specific.  These should uniformly selected by the same optimization
   level on all supported machines.  Use this macro to enable machbine-specific
   optimizations.

   *Do not examine `write_symbols' in this macro!* The debugging options are
   *not supposed to alter the generated code.  */
#define OPTIMIZATION_OPTIONS(LEVEL,SIZE) frv_optimization_options (LEVEL, SIZE)


/* Define this macro if debugging can be performed even without a frame
   pointer.  If this macro is defined, GNU CC will turn on the
   `-fomit-frame-pointer' option whenever `-O' is specified.  */
/* Frv needs a specific frame layout that includes the frame pointer */

#define CAN_DEBUG_WITHOUT_FP


/* Small Data Area Support.  */
/* Maximum size of variables that go in .sdata/.sbss.
   The -msdata=foo switch also controls how small variables are handled.  */
#ifndef SDATA_DEFAULT_SIZE
#define SDATA_DEFAULT_SIZE 8
#endif

extern int g_switch_value;        /* value of the -G xx switch */
extern int g_switch_set;          /* whether -G xx was passed.  */


/* Storage Layout */

/* Define this macro to have the value 1 if the most significant bit in a byte
   has the lowest number; otherwise define it to have the value zero.  This
   means that bit-field instructions count from the most significant bit.  If
   the machine has no bit-field instructions, then this must still be defined,
   but it doesn't matter which value it is defined to.  This macro need not be
   a constant.

   This macro does not affect the way structure fields are packed into bytes or
   words; that is controlled by `BYTES_BIG_ENDIAN'.  */
#define BITS_BIG_ENDIAN 1

/* Define this macro to have the value 1 if the most significant byte in a word
   has the lowest number.  This macro need not be a constant.  */
#define BYTES_BIG_ENDIAN 1

/* Define this macro to have the value 1 if, in a multiword object, the most
   significant word has the lowest number.  This applies to both memory
   locations and registers; GNU CC fundamentally assumes that the order of
   words in memory is the same as the order in registers.  This macro need not
   be a constant.  */
#define WORDS_BIG_ENDIAN 1

/* Number of storage units in a word; normally 4.  */
#define UNITS_PER_WORD 4

/* A macro to update MODE and UNSIGNEDP when an object whose type is TYPE and
   which has the specified mode and signedness is to be stored in a register.
   This macro is only called when TYPE is a scalar type.

   On most RISC machines, which only have operations that operate on a full
   register, define this macro to set M to `word_mode' if M is an integer mode
   narrower than `BITS_PER_WORD'.  In most cases, only integer modes should be
   widened because wider-precision floating-point operations are usually more
   expensive than their narrower counterparts.

   For most machines, the macro definition does not change UNSIGNEDP.  However,
   some machines, have instructions that preferentially handle either signed or
   unsigned quantities of certain modes.  For example, on the DEC Alpha, 32-bit
   loads from memory and 32-bit add instructions sign-extend the result to 64
   bits.  On such machines, set UNSIGNEDP according to which kind of extension
   is more efficient.

   Do not define this macro if it would never modify MODE.  */
#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE)	\
  do						\
    {						\
      if (GET_MODE_CLASS (MODE) == MODE_INT	\
	  && GET_MODE_SIZE (MODE) < 4)		\
	(MODE) = SImode;			\
    }						\
  while (0)

/* Normal alignment required for function parameters on the stack, in bits.
   All stack parameters receive at least this much alignment regardless of data
   type.  On most machines, this is the same as the size of an integer.  */
#define PARM_BOUNDARY 32

/* Define this macro if you wish to preserve a certain alignment for the stack
   pointer.  The definition is a C expression for the desired alignment
   (measured in bits).

   If `PUSH_ROUNDING' is not defined, the stack will always be aligned to the
   specified boundary.  If `PUSH_ROUNDING' is defined and specifies a less
   strict alignment than `STACK_BOUNDARY', the stack may be momentarily
   unaligned while pushing arguments.  */
#define STACK_BOUNDARY 64

/* Alignment required for a function entry point, in bits.  */
#define FUNCTION_BOUNDARY 128

/* Biggest alignment that any data type can require on this machine,
   in bits.  */
#define BIGGEST_ALIGNMENT 64

/* @@@ A hack, needed because libobjc wants to use ADJUST_FIELD_ALIGN for
   some reason.  */
#ifdef IN_TARGET_LIBS
#define BIGGEST_FIELD_ALIGNMENT 64
#else
/* An expression for the alignment of a structure field FIELD if the
   alignment computed in the usual way is COMPUTED.  GNU CC uses this
   value instead of the value in `BIGGEST_ALIGNMENT' or
   `BIGGEST_FIELD_ALIGNMENT', if defined, for structure fields only.  */
#define ADJUST_FIELD_ALIGN(FIELD, COMPUTED) 				\
  frv_adjust_field_align (FIELD, COMPUTED)
#endif

/* If defined, a C expression to compute the alignment for a static variable.
   TYPE is the data type, and ALIGN is the alignment that the object
   would ordinarily have.  The value of this macro is used instead of that
   alignment to align the object.

   If this macro is not defined, then ALIGN is used.

   One use of this macro is to increase alignment of medium-size data to make
   it all fit in fewer cache lines.  Another is to cause character arrays to be
   word-aligned so that `strcpy' calls that copy constants to character arrays
   can be done inline.  */
#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

/* If defined, a C expression to compute the alignment given to a constant that
   is being placed in memory.  CONSTANT is the constant and ALIGN is the
   alignment that the object would ordinarily have.  The value of this macro is
   used instead of that alignment to align the object.

   If this macro is not defined, then ALIGN is used.

   The typical use of this macro is to increase alignment for string constants
   to be word aligned so that `strcpy' calls that copy constants can be done
   inline.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN)  \
  (TREE_CODE (EXP) == STRING_CST	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

/* Define this macro to be the value 1 if instructions will fail to work if
   given data not on the nominal alignment.  If instructions will merely go
   slower in that case, define this macro as 0.  */
#define STRICT_ALIGNMENT 1

/* Define this if you wish to imitate the way many other C compilers handle
   alignment of bitfields and the structures that contain them.

   The behavior is that the type written for a bit-field (`int', `short', or
   other integer type) imposes an alignment for the entire structure, as if the
   structure really did contain an ordinary field of that type.  In addition,
   the bit-field is placed within the structure so that it would fit within such
   a field, not crossing a boundary for it.

   Thus, on most machines, a bit-field whose type is written as `int' would not
   cross a four-byte boundary, and would force four-byte alignment for the
   whole structure.  (The alignment used may not be four bytes; it is
   controlled by the other alignment parameters.)

   If the macro is defined, its definition should be a C expression; a nonzero
   value for the expression enables this behavior.

   Note that if this macro is not defined, or its value is zero, some bitfields
   may cross more than one alignment boundary.  The compiler can support such
   references if there are `insv', `extv', and `extzv' insns that can directly
   reference memory.

   The other known way of making bitfields work is to define
   `STRUCTURE_SIZE_BOUNDARY' as large as `BIGGEST_ALIGNMENT'.  Then every
   structure can be accessed with fullwords.

   Unless the machine has bit-field instructions or you define
   `STRUCTURE_SIZE_BOUNDARY' that way, you must define
   `PCC_BITFIELD_TYPE_MATTERS' to have a nonzero value.

   If your aim is to make GNU CC use the same conventions for laying out
   bitfields as are used by another compiler, here is how to investigate what
   the other compiler does.  Compile and run this program:

        struct foo1
        {
          char x;
          char :0;
          char y;
        };

        struct foo2
        {
          char x;
          int :0;
          char y;
        };

        main ()
        {
          printf ("Size of foo1 is %d\n",
                  sizeof (struct foo1));
          printf ("Size of foo2 is %d\n",
                  sizeof (struct foo2));
          exit (0);
        }

   If this prints 2 and 5, then the compiler's behavior is what you would get
   from `PCC_BITFIELD_TYPE_MATTERS'.

   Defined in svr4.h.  */
#define PCC_BITFIELD_TYPE_MATTERS 1


/* Layout of Source Language Data Types.  */

#define CHAR_TYPE_SIZE         8
#define SHORT_TYPE_SIZE       16
#define INT_TYPE_SIZE         32
#define LONG_TYPE_SIZE        32
#define LONG_LONG_TYPE_SIZE   64
#define FLOAT_TYPE_SIZE       32
#define DOUBLE_TYPE_SIZE      64
#define LONG_DOUBLE_TYPE_SIZE 64

/* An expression whose value is 1 or 0, according to whether the type `char'
   should be signed or unsigned by default.  The user can always override this
   default with the options `-fsigned-char' and `-funsigned-char'.  */
#define DEFAULT_SIGNED_CHAR 1


/* General purpose registers.  */
#define GPR_FIRST       0                       /* First gpr */
#define GPR_LAST        (GPR_FIRST + 63)        /* Last gpr */
#define GPR_R0          GPR_FIRST               /* R0, constant 0 */
#define GPR_FP          (GPR_FIRST + 2)         /* Frame pointer */
#define GPR_SP          (GPR_FIRST + 1)         /* Stack pointer */
						/* small data register */
#define SDA_BASE_REG    ((unsigned)(flag_pic ? PIC_REGNO : (GPR_FIRST+16)))
#define PIC_REGNO       (GPR_FIRST + 17)        /* PIC register */

#define FPR_FIRST       64			/* First FP reg */
#define FPR_LAST        127			/* Last  FP reg */

#define DEFAULT_CONDEXEC_TEMPS 4		/* reserve 4 regs by default */
#define GPR_TEMP_NUM	frv_condexec_temps	/* # gprs to reserve for temps */

/* We reserve the last CR and CCR in each category to be used as a reload
   register to reload the CR/CCR registers.  This is a kludge.  */
#define CC_FIRST	128			/* First ICC/FCC reg */
#define CC_LAST		135			/* Last  ICC/FCC reg */
#define ICC_FIRST	(CC_FIRST + 4)		/* First ICC reg */
#define ICC_LAST	(CC_FIRST + 7)		/* Last  ICC reg */
#define ICC_TEMP	(CC_FIRST + 7)		/* Temporary ICC reg */
#define FCC_FIRST	(CC_FIRST)		/* First FCC reg */
#define FCC_LAST	(CC_FIRST + 3)		/* Last  FCC reg */

/* Amount to shift a value to locate a ICC or FCC register in the CCR
   register and shift it to the bottom 4 bits.  */
#define CC_SHIFT_RIGHT(REGNO) (((REGNO) - CC_FIRST) << 2)

/* Mask to isolate a single ICC/FCC value.  */
#define CC_MASK		0xf

/* Masks to isolate the various bits in an ICC field.  */
#define ICC_MASK_N	0x8	/* negative */
#define ICC_MASK_Z	0x4	/* zero */
#define ICC_MASK_V	0x2	/* overflow */
#define ICC_MASK_C	0x1	/* carry */

/* Mask to isolate the N/Z flags in an ICC.  */
#define ICC_MASK_NZ (ICC_MASK_N | ICC_MASK_Z)

/* Mask to isolate the Z/C flags in an ICC.  */
#define ICC_MASK_ZC (ICC_MASK_Z | ICC_MASK_C)

/* Masks to isolate the various bits in a FCC field.  */
#define FCC_MASK_E	0x8	/* equal */
#define FCC_MASK_L	0x4	/* less than */
#define FCC_MASK_G	0x2	/* greater than */
#define FCC_MASK_U	0x1	/* unordered */

/* For CCR registers, the machine wants CR4..CR7 to be used for integer
   code and CR0..CR3 to be used for floating point.  */
#define CR_FIRST	136			/* First CCR */
#define CR_LAST		143			/* Last  CCR */
#define CR_NUM		(CR_LAST-CR_FIRST+1)	/* # of CCRs (8) */
#define ICR_FIRST	(CR_FIRST + 4)		/* First integer CCR */
#define ICR_LAST	(CR_FIRST + 7)		/* Last  integer CCR */
#define ICR_TEMP	ICR_LAST		/* Temp  integer CCR */
#define FCR_FIRST	(CR_FIRST + 0)		/* First float CCR */
#define FCR_LAST	(CR_FIRST + 3)		/* Last  float CCR */

/* Amount to shift a value to locate a CR register in the CCCR special purpose
   register and shift it to the bottom 2 bits.  */
#define CR_SHIFT_RIGHT(REGNO) (((REGNO) - CR_FIRST) << 1)

/* Mask to isolate a single CR value.  */
#define CR_MASK		0x3

#define ACC_FIRST	144			/* First acc register */
#define ACC_LAST	151			/* Last  acc register */

#define ACCG_FIRST	152			/* First accg register */
#define ACCG_LAST	159			/* Last  accg register */

#define AP_FIRST	160			/* fake argument pointer */

#define SPR_FIRST	161
#define SPR_LAST	162
#define LR_REGNO	(SPR_FIRST)
#define LCR_REGNO	(SPR_FIRST + 1)

#define GPR_P(R)	IN_RANGE_P (R, GPR_FIRST, GPR_LAST)
#define GPR_OR_AP_P(R)	(GPR_P (R) || (R) == ARG_POINTER_REGNUM)
#define FPR_P(R)	IN_RANGE_P (R, FPR_FIRST, FPR_LAST)
#define CC_P(R)		IN_RANGE_P (R, CC_FIRST, CC_LAST)
#define ICC_P(R)	IN_RANGE_P (R, ICC_FIRST, ICC_LAST)
#define FCC_P(R)	IN_RANGE_P (R, FCC_FIRST, FCC_LAST)
#define CR_P(R)		IN_RANGE_P (R, CR_FIRST, CR_LAST)
#define ICR_P(R)	IN_RANGE_P (R, ICR_FIRST, ICR_LAST)
#define FCR_P(R)	IN_RANGE_P (R, FCR_FIRST, FCR_LAST)
#define ACC_P(R)	IN_RANGE_P (R, ACC_FIRST, ACC_LAST)
#define ACCG_P(R)	IN_RANGE_P (R, ACCG_FIRST, ACCG_LAST)
#define SPR_P(R)	IN_RANGE_P (R, SPR_FIRST, SPR_LAST)

#define GPR_OR_PSEUDO_P(R)	(GPR_P (R) || (R) >= FIRST_PSEUDO_REGISTER)
#define FPR_OR_PSEUDO_P(R)	(FPR_P (R) || (R) >= FIRST_PSEUDO_REGISTER)
#define GPR_AP_OR_PSEUDO_P(R)	(GPR_OR_AP_P (R) || (R) >= FIRST_PSEUDO_REGISTER)
#define CC_OR_PSEUDO_P(R)	(CC_P (R) || (R) >= FIRST_PSEUDO_REGISTER)
#define ICC_OR_PSEUDO_P(R)	(ICC_P (R) || (R) >= FIRST_PSEUDO_REGISTER)
#define FCC_OR_PSEUDO_P(R)	(FCC_P (R) || (R) >= FIRST_PSEUDO_REGISTER)
#define CR_OR_PSEUDO_P(R)	(CR_P (R) || (R) >= FIRST_PSEUDO_REGISTER)
#define ICR_OR_PSEUDO_P(R)	(ICR_P (R) || (R) >= FIRST_PSEUDO_REGISTER)
#define FCR_OR_PSEUDO_P(R)	(FCR_P (R) || (R) >= FIRST_PSEUDO_REGISTER)
#define ACC_OR_PSEUDO_P(R)	(ACC_P (R) || (R) >= FIRST_PSEUDO_REGISTER)
#define ACCG_OR_PSEUDO_P(R)	(ACCG_P (R) || (R) >= FIRST_PSEUDO_REGISTER)

#define MAX_STACK_IMMEDIATE_OFFSET 2047


/* Register Basics.  */

/* Number of hardware registers known to the compiler.  They receive numbers 0
   through `FIRST_PSEUDO_REGISTER-1'; thus, the first pseudo register's number
   really is assigned the number `FIRST_PSEUDO_REGISTER'.  */
#define FIRST_PSEUDO_REGISTER (SPR_LAST + 1)

/* The first/last register that can contain the arguments to a function.  */
#define FIRST_ARG_REGNUM	(GPR_FIRST + 8)
#define LAST_ARG_REGNUM		(FIRST_ARG_REGNUM + FRV_NUM_ARG_REGS - 1)

/* Registers used by the exception handling functions.  These should be
   registers that are not otherwised used by the calling sequence.  */
#define FIRST_EH_REGNUM		14
#define LAST_EH_REGNUM		15

/* Scratch registers used in the prologue, epilogue and thunks.
   OFFSET_REGNO is for loading constant addends that are too big for a
   single instruction.  TEMP_REGNO is used for transferring SPRs to and from
   the stack, and various other activities.  */
#define OFFSET_REGNO		4
#define TEMP_REGNO		5

/* Registers used in the prologue.  OLD_SP_REGNO is the old stack pointer,
   which is sometimes used to set up the frame pointer.  */
#define OLD_SP_REGNO		6

/* Registers used in the epilogue.  STACKADJ_REGNO stores the exception
   handler's stack adjustment.  */
#define STACKADJ_REGNO		6

/* Registers used in thunks.  JMP_REGNO is used for loading the target
   address.  */
#define JUMP_REGNO		6

#define EH_RETURN_DATA_REGNO(N)	((N) <= (LAST_EH_REGNUM - FIRST_EH_REGNUM)? \
				 (N) + FIRST_EH_REGNUM : INVALID_REGNUM)
#define EH_RETURN_STACKADJ_RTX	gen_rtx_REG (SImode, STACKADJ_REGNO)
#define EH_RETURN_HANDLER_RTX   RETURN_ADDR_RTX (0, frame_pointer_rtx)

/* An initializer that says which registers are used for fixed purposes all
   throughout the compiled code and are therefore not available for general
   allocation.  These would include the stack pointer, the frame pointer
   (except on machines where that can be used as a general register when no
   frame pointer is needed), the program counter on machines where that is
   considered one of the addressable registers, and any other numbered register
   with a standard use.

   This information is expressed as a sequence of numbers, separated by commas
   and surrounded by braces.  The Nth number is 1 if register N is fixed, 0
   otherwise.

   The table initialized from this macro, and the table initialized by the
   following one, may be overridden at run time either automatically, by the
   actions of the macro `CONDITIONAL_REGISTER_USAGE', or by the user with the
   command options `-ffixed-REG', `-fcall-used-REG' and `-fcall-saved-REG'.  */

/* gr0  -- Hard Zero
   gr1  -- Stack Pointer
   gr2  -- Frame Pointer
   gr3  -- Hidden Parameter
   gr16 -- Small Data reserved
   gr17 -- Pic reserved
   gr28 -- OS reserved
   gr29 -- OS reserved
   gr30 -- OS reserved
   gr31 -- OS reserved
   cr3  -- reserved to reload FCC registers.
   cr7  -- reserved to reload ICC registers.  */
#define FIXED_REGISTERS							\
{	/* Integer Registers */						\
	1, 1, 1, 1, 0, 0, 0, 0,		/* 000-007, gr0  - gr7  */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 008-015, gr8  - gr15 */	\
	1, 1, 0, 0, 0, 0, 0, 0,		/* 016-023, gr16 - gr23 */	\
	0, 0, 0, 0, 1, 1, 1, 1,		/* 024-031, gr24 - gr31 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 032-039, gr32 - gr39 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 040-040, gr48 - gr47 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 048-055, gr48 - gr55 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 056-063, gr56 - gr63 */	\
	/* Float Registers */						\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 064-071, fr0  - fr7  */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 072-079, fr8  - fr15 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 080-087, fr16 - fr23 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 088-095, fr24 - fr31 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 096-103, fr32 - fr39 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 104-111, fr48 - fr47 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 112-119, fr48 - fr55 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 120-127, fr56 - fr63 */	\
	/* Condition Code Registers */					\
	0, 0, 0, 0,			/* 128-131, fcc0 - fcc3  */	\
	0, 0, 0, 1,			/* 132-135, icc0 - icc3 */	\
	/* Conditional execution Registers (CCR) */			\
	0, 0, 0, 0, 0, 0, 0, 1,		/* 136-143, cr0 - cr7 */	\
	/* Accumulators */						\
	1, 1, 1, 1, 1, 1, 1, 1,		/* 144-151, acc0  - acc7 */	\
	1, 1, 1, 1, 1, 1, 1, 1,		/* 152-159, accg0 - accg7 */	\
	/* Other registers */						\
	1,				/* 160, AP   - fake arg ptr */	\
	0,				/* 161, LR   - Link register*/	\
	0,				/* 162, LCR  - Loop count reg*/	\
}

/* Like `FIXED_REGISTERS' but has 1 for each register that is clobbered (in
   general) by function calls as well as for fixed registers.  This macro
   therefore identifies the registers that are not available for general
   allocation of values that must live across function calls.

   If a register has 0 in `CALL_USED_REGISTERS', the compiler automatically
   saves it on function entry and restores it on function exit, if the register
   is used within the function.  */
#define CALL_USED_REGISTERS						\
{	/* Integer Registers */						\
	1, 1, 1, 1, 1, 1, 1, 1,		/* 000-007, gr0  - gr7  */	\
	1, 1, 1, 1, 1, 1, 1, 1,		/* 008-015, gr8  - gr15 */	\
	1, 1, 0, 0, 0, 0, 0, 0,		/* 016-023, gr16 - gr23 */	\
	0, 0, 0, 0, 1, 1, 1, 1,		/* 024-031, gr24 - gr31 */	\
	1, 1, 1, 1, 1, 1, 1, 1,		/* 032-039, gr32 - gr39 */	\
	1, 1, 1, 1, 1, 1, 1, 1,		/* 040-040, gr48 - gr47 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 048-055, gr48 - gr55 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 056-063, gr56 - gr63 */	\
	/* Float Registers */						\
	1, 1, 1, 1, 1, 1, 1, 1,		/* 064-071, fr0  - fr7  */	\
	1, 1, 1, 1, 1, 1, 1, 1,		/* 072-079, fr8  - fr15 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 080-087, fr16 - fr23 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 088-095, fr24 - fr31 */	\
	1, 1, 1, 1, 1, 1, 1, 1,		/* 096-103, fr32 - fr39 */	\
	1, 1, 1, 1, 1, 1, 1, 1,		/* 104-111, fr48 - fr47 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 112-119, fr48 - fr55 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 120-127, fr56 - fr63 */	\
	/* Condition Code Registers */					\
	1, 1, 1, 1,			/* 128-131, fcc0 - fcc3 */	\
	1, 1, 1, 1,			/* 132-135, icc0 - icc3  */	\
	/* Conditional execution Registers (CCR) */			\
	1, 1, 1, 1, 1, 1, 1, 1,		/* 136-143, cr0 - cr7 */	\
	/* Accumulators */						\
	1, 1, 1, 1, 1, 1, 1, 1,		/* 144-151, acc0 - acc7 */	\
	1, 1, 1, 1, 1, 1, 1, 1,		/* 152-159, accg0 - accg7 */	\
	/* Other registers */						\
	1,				/* 160, AP  - fake arg ptr */	\
	1,				/* 161, LR  - Link register*/	\
	1,				/* 162, LCR - Loop count reg */	\
}

/* Zero or more C statements that may conditionally modify two variables
   `fixed_regs' and `call_used_regs' (both of type `char []') after they have
   been initialized from the two preceding macros.

   This is necessary in case the fixed or call-clobbered registers depend on
   target flags.

   You need not define this macro if it has no work to do.

   If the usage of an entire class of registers depends on the target flags,
   you may indicate this to GCC by using this macro to modify `fixed_regs' and
   `call_used_regs' to 1 for each of the registers in the classes which should
   not be used by GCC.  Also define the macro `REG_CLASS_FROM_LETTER' to return
   `NO_REGS' if it is called with a letter for a class that shouldn't be used.

   (However, if this class is not included in `GENERAL_REGS' and all of the
   insn patterns whose constraints permit this class are controlled by target
   switches, then GCC will automatically avoid using these registers when the
   target switches are opposed to them.)  */

#define CONDITIONAL_REGISTER_USAGE frv_conditional_register_usage ()


/* Order of allocation of registers.  */

/* If defined, an initializer for a vector of integers, containing the numbers
   of hard registers in the order in which GNU CC should prefer to use them
   (from most preferred to least).

   If this macro is not defined, registers are used lowest numbered first (all
   else being equal).

   One use of this macro is on machines where the highest numbered registers
   must always be saved and the save-multiple-registers instruction supports
   only sequences of consecutive registers.  On such machines, define
   `REG_ALLOC_ORDER' to be an initializer that lists the highest numbered
   allocatable register first.  */

/* On the FRV, allocate GR16 and GR17 after other saved registers so that we
   have a better chance of allocating 2 registers at a time and can use the
   double word load/store instructions in the prologue.  */
#define REG_ALLOC_ORDER							\
{									\
  /* volatile registers */						\
  GPR_FIRST  +  4, GPR_FIRST  +  5, GPR_FIRST  +  6, GPR_FIRST 	+  7,	\
  GPR_FIRST  +  8, GPR_FIRST  +  9, GPR_FIRST  + 10, GPR_FIRST 	+ 11,	\
  GPR_FIRST  + 12, GPR_FIRST  + 13, GPR_FIRST  + 14, GPR_FIRST 	+ 15,	\
  GPR_FIRST  + 32, GPR_FIRST  + 33, GPR_FIRST  + 34, GPR_FIRST 	+ 35,	\
  GPR_FIRST  + 36, GPR_FIRST  + 37, GPR_FIRST  + 38, GPR_FIRST 	+ 39,	\
  GPR_FIRST  + 40, GPR_FIRST  + 41, GPR_FIRST  + 42, GPR_FIRST 	+ 43,	\
  GPR_FIRST  + 44, GPR_FIRST  + 45, GPR_FIRST  + 46, GPR_FIRST 	+ 47,	\
									\
  FPR_FIRST  +  0, FPR_FIRST  +  1, FPR_FIRST  +  2, FPR_FIRST 	+  3,	\
  FPR_FIRST  +  4, FPR_FIRST  +  5, FPR_FIRST  +  6, FPR_FIRST 	+  7,	\
  FPR_FIRST  +  8, FPR_FIRST  +  9, FPR_FIRST  + 10, FPR_FIRST 	+ 11,	\
  FPR_FIRST  + 12, FPR_FIRST  + 13, FPR_FIRST  + 14, FPR_FIRST 	+ 15,	\
  FPR_FIRST  + 32, FPR_FIRST  + 33, FPR_FIRST  + 34, FPR_FIRST 	+ 35,	\
  FPR_FIRST  + 36, FPR_FIRST  + 37, FPR_FIRST  + 38, FPR_FIRST 	+ 39,	\
  FPR_FIRST  + 40, FPR_FIRST  + 41, FPR_FIRST  + 42, FPR_FIRST 	+ 43,	\
  FPR_FIRST  + 44, FPR_FIRST  + 45, FPR_FIRST  + 46, FPR_FIRST 	+ 47,	\
									\
  ICC_FIRST  +  0, ICC_FIRST  +  1, ICC_FIRST  +  2, ICC_FIRST 	+  3,	\
  FCC_FIRST  +  0, FCC_FIRST  +  1, FCC_FIRST  +  2, FCC_FIRST 	+  3,	\
  CR_FIRST   +  0, CR_FIRST   +  1, CR_FIRST   +  2, CR_FIRST  	+  3,	\
  CR_FIRST   +  4, CR_FIRST   +  5, CR_FIRST   +  6, CR_FIRST  	+  7,	\
									\
  /* saved registers */							\
  GPR_FIRST  + 18, GPR_FIRST  + 19,					\
  GPR_FIRST  + 20, GPR_FIRST  + 21, GPR_FIRST  + 22, GPR_FIRST 	+ 23,	\
  GPR_FIRST  + 24, GPR_FIRST  + 25, GPR_FIRST  + 26, GPR_FIRST 	+ 27,	\
  GPR_FIRST  + 48, GPR_FIRST  + 49, GPR_FIRST  + 50, GPR_FIRST 	+ 51,	\
  GPR_FIRST  + 52, GPR_FIRST  + 53, GPR_FIRST  + 54, GPR_FIRST 	+ 55,	\
  GPR_FIRST  + 56, GPR_FIRST  + 57, GPR_FIRST  + 58, GPR_FIRST 	+ 59,	\
  GPR_FIRST  + 60, GPR_FIRST  + 61, GPR_FIRST  + 62, GPR_FIRST 	+ 63,	\
  GPR_FIRST  + 16, GPR_FIRST  + 17,					\
									\
  FPR_FIRST  + 16, FPR_FIRST  + 17, FPR_FIRST  + 18, FPR_FIRST 	+ 19,	\
  FPR_FIRST  + 20, FPR_FIRST  + 21, FPR_FIRST  + 22, FPR_FIRST 	+ 23,	\
  FPR_FIRST  + 24, FPR_FIRST  + 25, FPR_FIRST  + 26, FPR_FIRST 	+ 27,	\
  FPR_FIRST  + 28, FPR_FIRST  + 29, FPR_FIRST  + 30, FPR_FIRST 	+ 31,	\
  FPR_FIRST  + 48, FPR_FIRST  + 49, FPR_FIRST  + 50, FPR_FIRST 	+ 51,	\
  FPR_FIRST  + 52, FPR_FIRST  + 53, FPR_FIRST  + 54, FPR_FIRST 	+ 55,	\
  FPR_FIRST  + 56, FPR_FIRST  + 57, FPR_FIRST  + 58, FPR_FIRST 	+ 59,	\
  FPR_FIRST  + 60, FPR_FIRST  + 61, FPR_FIRST  + 62, FPR_FIRST 	+ 63,	\
									\
  /* special or fixed registers */					\
  GPR_FIRST  +  0, GPR_FIRST  +  1, GPR_FIRST  +  2, GPR_FIRST 	+  3,	\
  GPR_FIRST  + 28, GPR_FIRST  + 29, GPR_FIRST  + 30, GPR_FIRST 	+ 31,	\
  ACC_FIRST  +  0, ACC_FIRST  +  1, ACC_FIRST  +  2, ACC_FIRST 	+  3,	\
  ACC_FIRST  +  4, ACC_FIRST  +  5, ACC_FIRST  +  6, ACC_FIRST 	+  7,	\
  ACCG_FIRST +  0, ACCG_FIRST +  1, ACCG_FIRST +  2, ACCG_FIRST	+  3,	\
  ACCG_FIRST +  4, ACCG_FIRST +  5, ACCG_FIRST +  6, ACCG_FIRST	+  7,	\
  AP_FIRST, 	   LR_REGNO,       LCR_REGNO				\
}


/* How Values Fit in Registers.  */

/* A C expression for the number of consecutive hard registers, starting at
   register number REGNO, required to hold a value of mode MODE.

   On a machine where all registers are exactly one word, a suitable definition
   of this macro is

        #define HARD_REGNO_NREGS(REGNO, MODE)            \
           ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1)  \
            / UNITS_PER_WORD))  */

/* On the FRV, make the CC modes take 3 words in the integer registers, so that
   we can build the appropriate instructions to properly reload the values.  */
#define HARD_REGNO_NREGS(REGNO, MODE) frv_hard_regno_nregs (REGNO, MODE)

/* A C expression that is nonzero if it is permissible to store a value of mode
   MODE in hard register number REGNO (or in several registers starting with
   that one).  For a machine where all registers are equivalent, a suitable
   definition is

        #define HARD_REGNO_MODE_OK(REGNO, MODE) 1

   It is not necessary for this macro to check for the numbers of fixed
   registers, because the allocation mechanism considers them to be always
   occupied.

   On some machines, double-precision values must be kept in even/odd register
   pairs.  The way to implement that is to define this macro to reject odd
   register numbers for such modes.

   The minimum requirement for a mode to be OK in a register is that the
   `movMODE' instruction pattern support moves between the register and any
   other hard register for which the mode is OK; and that moving a value into
   the register and back out not alter it.

   Since the same instruction used to move `SImode' will work for all narrower
   integer modes, it is not necessary on any machine for `HARD_REGNO_MODE_OK'
   to distinguish between these modes, provided you define patterns `movhi',
   etc., to take advantage of this.  This is useful because of the interaction
   between `HARD_REGNO_MODE_OK' and `MODES_TIEABLE_P'; it is very desirable for
   all integer modes to be tieable.

   Many machines have special registers for floating point arithmetic.  Often
   people assume that floating point machine modes are allowed only in floating
   point registers.  This is not true.  Any registers that can hold integers
   can safely *hold* a floating point machine mode, whether or not floating
   arithmetic can be done on it in those registers.  Integer move instructions
   can be used to move the values.

   On some machines, though, the converse is true: fixed-point machine modes
   may not go in floating registers.  This is true if the floating registers
   normalize any value stored in them, because storing a non-floating value
   there would garble it.  In this case, `HARD_REGNO_MODE_OK' should reject
   fixed-point machine modes in floating registers.  But if the floating
   registers do not automatically normalize, if you can store any bit pattern
   in one and retrieve it unchanged without a trap, then any machine mode may
   go in a floating register, so you can define this macro to say so.

   The primary significance of special floating registers is rather that they
   are the registers acceptable in floating point arithmetic instructions.
   However, this is of no concern to `HARD_REGNO_MODE_OK'.  You handle it by
   writing the proper constraints for those instructions.

   On some machines, the floating registers are especially slow to access, so
   that it is better to store a value in a stack frame than in such a register
   if floating point arithmetic is not being done.  As long as the floating
   registers are not in class `GENERAL_REGS', they will not be used unless some
   pattern's constraint asks for one.  */
#define HARD_REGNO_MODE_OK(REGNO, MODE) frv_hard_regno_mode_ok (REGNO, MODE)

/* A C expression that is nonzero if it is desirable to choose register
   allocation so as to avoid move instructions between a value of mode MODE1
   and a value of mode MODE2.

   If `HARD_REGNO_MODE_OK (R, MODE1)' and `HARD_REGNO_MODE_OK (R, MODE2)' are
   ever different for any R, then `MODES_TIEABLE_P (MODE1, MODE2)' must be
   zero.  */
#define MODES_TIEABLE_P(MODE1, MODE2) (MODE1 == MODE2)

/* Define this macro if the compiler should avoid copies to/from CCmode
   registers.  You should only define this macro if support fo copying to/from
   CCmode is incomplete.  */
#define AVOID_CCMODE_COPIES


/* Register Classes.  */

/* An enumeral type that must be defined with all the register class names as
   enumeral values.  `NO_REGS' must be first.  `ALL_REGS' must be the last
   register class, followed by one more enumeral value, `LIM_REG_CLASSES',
   which is not a register class but rather tells how many classes there are.

   Each register class has a number, which is the value of casting the class
   name to type `int'.  The number serves as an index in many of the tables
   described below.  */
enum reg_class
{
  NO_REGS,
  ICC_REGS,
  FCC_REGS,
  CC_REGS,
  ICR_REGS,
  FCR_REGS,
  CR_REGS,
  LCR_REG,
  LR_REG,
  SPR_REGS,
  QUAD_ACC_REGS,
  EVEN_ACC_REGS,
  ACC_REGS,
  ACCG_REGS,
  QUAD_FPR_REGS,
  FEVEN_REGS,
  FPR_REGS,
  QUAD_REGS,
  EVEN_REGS,
  GPR_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

#define GENERAL_REGS GPR_REGS

/* The number of distinct register classes, defined as follows:

        #define N_REG_CLASSES (int) LIM_REG_CLASSES  */
#define N_REG_CLASSES ((int) LIM_REG_CLASSES)

/* An initializer containing the names of the register classes as C string
   constants.  These names are used in writing some of the debugging dumps.  */
#define REG_CLASS_NAMES {						\
   "NO_REGS",								\
   "ICC_REGS",								\
   "FCC_REGS",								\
   "CC_REGS",								\
   "ICR_REGS",								\
   "FCR_REGS",								\
   "CR_REGS",								\
   "LCR_REG",								\
   "LR_REG",								\
   "SPR_REGS",								\
   "QUAD_ACC_REGS",							\
   "EVEN_ACC_REGS",							\
   "ACC_REGS",								\
   "ACCG_REGS",								\
   "QUAD_FPR_REGS",							\
   "FEVEN_REGS",							\
   "FPR_REGS",								\
   "QUAD_REGS",								\
   "EVEN_REGS",								\
   "GPR_REGS",								\
   "ALL_REGS"								\
}

/* An initializer containing the contents of the register classes, as integers
   which are bit masks.  The Nth integer specifies the contents of class N.
   The way the integer MASK is interpreted is that register R is in the class
   if `MASK & (1 << R)' is 1.

   When the machine has more than 32 registers, an integer does not suffice.
   Then the integers are replaced by sub-initializers, braced groupings
   containing several integers.  Each sub-initializer must be suitable as an
   initializer for the type `HARD_REG_SET' which is defined in
   `hard-reg-set.h'.  */
#define REG_CLASS_CONTENTS						       \
{  /* gr0-gr31 gr32-gr63  fr0-fr31   fr32-fr-63 cc/ccr/acc ap/spr */	       \
  { 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x0}, /* NO_REGS  */\
  { 0x00000000,0x00000000,0x00000000,0x00000000,0x000000f0,0x0}, /* ICC_REGS */\
  { 0x00000000,0x00000000,0x00000000,0x00000000,0x0000000f,0x0}, /* FCC_REGS */\
  { 0x00000000,0x00000000,0x00000000,0x00000000,0x000000ff,0x0}, /* CC_REGS  */\
  { 0x00000000,0x00000000,0x00000000,0x00000000,0x0000f000,0x0}, /* ICR_REGS */\
  { 0x00000000,0x00000000,0x00000000,0x00000000,0x00000f00,0x0}, /* FCR_REGS */\
  { 0x00000000,0x00000000,0x00000000,0x00000000,0x0000ff00,0x0}, /* CR_REGS  */\
  { 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x4}, /* LCR_REGS */\
  { 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x2}, /* LR_REGS  */\
  { 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x6}, /* SPR_REGS */\
  { 0x00000000,0x00000000,0x00000000,0x00000000,0x00ff0000,0x0}, /* QUAD_ACC */\
  { 0x00000000,0x00000000,0x00000000,0x00000000,0x00ff0000,0x0}, /* EVEN_ACC */\
  { 0x00000000,0x00000000,0x00000000,0x00000000,0x00ff0000,0x0}, /* ACC_REGS */\
  { 0x00000000,0x00000000,0x00000000,0x00000000,0xff000000,0x0}, /* ACCG_REGS*/\
  { 0x00000000,0x00000000,0xffffffff,0xffffffff,0x00000000,0x0}, /* QUAD_FPR */\
  { 0x00000000,0x00000000,0xffffffff,0xffffffff,0x00000000,0x0}, /* FEVEN_REG*/\
  { 0x00000000,0x00000000,0xffffffff,0xffffffff,0x00000000,0x0}, /* FPR_REGS */\
  { 0x0ffffffc,0xffffffff,0x00000000,0x00000000,0x00000000,0x0}, /* QUAD_REGS*/\
  { 0xfffffffc,0xffffffff,0x00000000,0x00000000,0x00000000,0x0}, /* EVEN_REGS*/\
  { 0xffffffff,0xffffffff,0x00000000,0x00000000,0x00000000,0x1}, /* GPR_REGS */\
  { 0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0x7}, /* ALL_REGS */\
}

/* A C expression whose value is a register class containing hard register
   REGNO.  In general there is more than one such class; choose a class which
   is "minimal", meaning that no smaller class also contains the register.  */

extern enum reg_class regno_reg_class[];
#define REGNO_REG_CLASS(REGNO) regno_reg_class [REGNO]

/* A macro whose definition is the name of the class to which a valid base
   register must belong.  A base register is one used in an address which is
   the register value plus a displacement.  */
#define BASE_REG_CLASS GPR_REGS

/* A macro whose definition is the name of the class to which a valid index
   register must belong.  An index register is one used in an address where its
   value is either multiplied by a scale factor or added to another register
   (as well as added to a displacement).  */
#define INDEX_REG_CLASS GPR_REGS

/* A C expression which defines the machine-dependent operand constraint
   letters for register classes.  If CHAR is such a letter, the value should be
   the register class corresponding to it.  Otherwise, the value should be
   `NO_REGS'.  The register letter `r', corresponding to class `GENERAL_REGS',
   will not be passed to this macro; you do not need to handle it.

   The following letters are unavailable, due to being used as
   constraints:
	'0'..'9'
	'<', '>'
	'E', 'F', 'G', 'H'
	'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P'
	'Q', 'R', 'S', 'T', 'U'
	'V', 'X'
	'g', 'i', 'm', 'n', 'o', 'p', 'r', 's' */

extern enum reg_class reg_class_from_letter[];
#define REG_CLASS_FROM_LETTER(CHAR) reg_class_from_letter [(unsigned char)(CHAR)]

/* A C expression which is nonzero if register number NUM is suitable for use
   as a base register in operand addresses.  It may be either a suitable hard
   register or a pseudo register that has been allocated such a hard register.  */
#define REGNO_OK_FOR_BASE_P(NUM)           \
  ((NUM) < FIRST_PSEUDO_REGISTER           \
   ? GPR_P (NUM)                           \
   : (reg_renumber [NUM] >= 0 && GPR_P (reg_renumber [NUM])))

/* A C expression which is nonzero if register number NUM is suitable for use
   as an index register in operand addresses.  It may be either a suitable hard
   register or a pseudo register that has been allocated such a hard register.

   The difference between an index register and a base register is that the
   index register may be scaled.  If an address involves the sum of two
   registers, neither one of them scaled, then either one may be labeled the
   "base" and the other the "index"; but whichever labeling is used must fit
   the machine's constraints of which registers may serve in each capacity.
   The compiler will try both labelings, looking for one that is valid, and
   will reload one or both registers only if neither labeling works.  */
#define REGNO_OK_FOR_INDEX_P(NUM)                                       \
  ((NUM) < FIRST_PSEUDO_REGISTER                                        \
   ? GPR_P (NUM)                                                        \
   : (reg_renumber [NUM] >= 0 && GPR_P (reg_renumber [NUM])))

/* A C expression that places additional restrictions on the register class to
   use when it is necessary to copy value X into a register in class CLASS.
   The value is a register class; perhaps CLASS, or perhaps another, smaller
   class.  On many machines, the following definition is safe:

        #define PREFERRED_RELOAD_CLASS(X,CLASS) CLASS

   Sometimes returning a more restrictive class makes better code.  For
   example, on the 68000, when X is an integer constant that is in range for a
   `moveq' instruction, the value of this macro is always `DATA_REGS' as long
   as CLASS includes the data registers.  Requiring a data register guarantees
   that a `moveq' will be used.

   If X is a `const_double', by returning `NO_REGS' you can force X into a
   memory constant.  This is useful on certain machines where immediate
   floating values cannot be loaded into certain kinds of registers.

   This declaration must be present.  */
#define PREFERRED_RELOAD_CLASS(X, CLASS) CLASS

#define SECONDARY_INPUT_RELOAD_CLASS(CLASS, MODE, X) \
  frv_secondary_reload_class (CLASS, MODE, X, TRUE)

#define SECONDARY_OUTPUT_RELOAD_CLASS(CLASS, MODE, X) \
  frv_secondary_reload_class (CLASS, MODE, X, FALSE)

/* A C expression whose value is nonzero if pseudos that have been assigned to
   registers of class CLASS would likely be spilled because registers of CLASS
   are needed for spill registers.

   The default value of this macro returns 1 if CLASS has exactly one register
   and zero otherwise.  On most machines, this default should be used.  Only
   define this macro to some other expression if pseudo allocated by
   `local-alloc.c' end up in memory because their hard registers were needed
   for spill registers.  If this macro returns nonzero for those classes, those
   pseudos will only be allocated by `global.c', which knows how to reallocate
   the pseudo to another register.  If there would not be another register
   available for reallocation, you should not change the definition of this
   macro since the only effect of such a definition would be to slow down
   register allocation.  */
#define CLASS_LIKELY_SPILLED_P(CLASS) frv_class_likely_spilled_p (CLASS)

/* A C expression for the maximum number of consecutive registers of
   class CLASS needed to hold a value of mode MODE.

   This is closely related to the macro `HARD_REGNO_NREGS'.  In fact, the value
   of the macro `CLASS_MAX_NREGS (CLASS, MODE)' should be the maximum value of
   `HARD_REGNO_NREGS (REGNO, MODE)' for all REGNO values in the class CLASS.

   This macro helps control the handling of multiple-word values in
   the reload pass.

   This declaration is required.  */
#define CLASS_MAX_NREGS(CLASS, MODE) frv_class_max_nregs (CLASS, MODE)

#define ZERO_P(x) (x == CONST0_RTX (GET_MODE (x)))

/* 6 bit signed immediate.  */
#define CONST_OK_FOR_I(VALUE) IN_RANGE_P(VALUE, -32, 31)
/* 10 bit signed immediate.  */
#define CONST_OK_FOR_J(VALUE) IN_RANGE_P(VALUE, -512, 511)
/* Unused */
#define CONST_OK_FOR_K(VALUE)  0
/* 16 bit signed immediate.  */
#define CONST_OK_FOR_L(VALUE) IN_RANGE_P(VALUE, -32768, 32767)
/* 16 bit unsigned immediate.  */
#define CONST_OK_FOR_M(VALUE)  IN_RANGE_P (VALUE, 0, 65535)
/* 12 bit signed immediate that is negative.  */
#define CONST_OK_FOR_N(VALUE) IN_RANGE_P(VALUE, -2048, -1)
/* Zero */
#define CONST_OK_FOR_O(VALUE) ((VALUE) == 0)
/* 12 bit signed immediate that is negative.  */
#define CONST_OK_FOR_P(VALUE) IN_RANGE_P(VALUE, 1, 2047)

/* A C expression that defines the machine-dependent operand constraint letters
   (`I', `J', `K', .. 'P') that specify particular ranges of integer values.
   If C is one of those letters, the expression should check that VALUE, an
   integer, is in the appropriate range and return 1 if so, 0 otherwise.  If C
   is not one of those letters, the value should be 0 regardless of VALUE.  */
#define CONST_OK_FOR_LETTER_P(VALUE, C)		\
  (  (C) == 'I' ? CONST_OK_FOR_I (VALUE)        \
   : (C) == 'J' ? CONST_OK_FOR_J (VALUE)        \
   : (C) == 'K' ? CONST_OK_FOR_K (VALUE)        \
   : (C) == 'L' ? CONST_OK_FOR_L (VALUE)        \
   : (C) == 'M' ? CONST_OK_FOR_M (VALUE)        \
   : (C) == 'N' ? CONST_OK_FOR_N (VALUE)        \
   : (C) == 'O' ? CONST_OK_FOR_O (VALUE)        \
   : (C) == 'P' ? CONST_OK_FOR_P (VALUE)        \
   : 0)


/* A C expression that defines the machine-dependent operand constraint letters
   (`G', `H') that specify particular ranges of `const_double' values.

   If C is one of those letters, the expression should check that VALUE, an RTX
   of code `const_double', is in the appropriate range and return 1 if so, 0
   otherwise.  If C is not one of those letters, the value should be 0
   regardless of VALUE.

   `const_double' is used for all floating-point constants and for `DImode'
   fixed-point constants.  A given letter can accept either or both kinds of
   values.  It can use `GET_MODE' to distinguish between these kinds.  */

#define CONST_DOUBLE_OK_FOR_G(VALUE)					\
  ((GET_MODE (VALUE) == VOIDmode 					\
    && CONST_DOUBLE_LOW (VALUE) == 0					\
    && CONST_DOUBLE_HIGH (VALUE) == 0)					\
   || ((GET_MODE (VALUE) == SFmode					\
        || GET_MODE (VALUE) == DFmode)					\
       && (VALUE) == CONST0_RTX (GET_MODE (VALUE))))

#define CONST_DOUBLE_OK_FOR_H(VALUE) 0

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)				\
  (  (C) == 'G' ? CONST_DOUBLE_OK_FOR_G (VALUE)				\
   : (C) == 'H' ? CONST_DOUBLE_OK_FOR_H (VALUE)				\
   : 0)

/* A C expression that defines the optional machine-dependent constraint
   letters (`Q', `R', `S', `T', `U') that can be used to segregate specific
   types of operands, usually memory references, for the target machine.
   Normally this macro will not be defined.  If it is required for a particular
   target machine, it should return 1 if VALUE corresponds to the operand type
   represented by the constraint letter C.  If C is not defined as an extra
   constraint, the value returned should be 0 regardless of VALUE.

   For example, on the ROMP, load instructions cannot have their output in r0
   if the memory reference contains a symbolic address.  Constraint letter `Q'
   is defined as representing a memory address that does *not* contain a
   symbolic address.  An alternative is specified with a `Q' constraint on the
   input and `r' on the output.  The next alternative specifies `m' on the
   input and a register class that does not include r0 on the output.  */

/* Small data references */
#define EXTRA_CONSTRAINT_FOR_Q(VALUE)					\
  (small_data_symbolic_operand (VALUE, GET_MODE (VALUE)))

/* Double word memory ops that take one instruction.  */
#define EXTRA_CONSTRAINT_FOR_R(VALUE)					\
  (dbl_memory_one_insn_operand (VALUE, GET_MODE (VALUE)))

/* SYMBOL_REF */
#define EXTRA_CONSTRAINT_FOR_S(VALUE) (GET_CODE (VALUE) == SYMBOL_REF)

/* Double word memory ops that take two instructions.  */
#define EXTRA_CONSTRAINT_FOR_T(VALUE)					\
  (dbl_memory_two_insn_operand (VALUE, GET_MODE (VALUE)))

/* Memory operand for conditional execution.  */
#define EXTRA_CONSTRAINT_FOR_U(VALUE)					\
  (condexec_memory_operand (VALUE, GET_MODE (VALUE)))

#define EXTRA_CONSTRAINT(VALUE, C)					\
  (  (C) == 'Q'   ? EXTRA_CONSTRAINT_FOR_Q (VALUE)			\
   : (C) == 'R' ? EXTRA_CONSTRAINT_FOR_R (VALUE)			\
   : (C) == 'S' ? EXTRA_CONSTRAINT_FOR_S (VALUE)			\
   : (C) == 'T' ? EXTRA_CONSTRAINT_FOR_T (VALUE)			\
   : (C) == 'U' ? EXTRA_CONSTRAINT_FOR_U (VALUE)			\
   : 0)


/* Basic Stack Layout.  */

/* Structure to describe information about a saved range of registers */

typedef struct frv_stack_regs {
  const char * name;		/* name of the register ranges */
  int first;			/* first register in the range */
  int last;			/* last register in the range */
  int size_1word;		/* # of bytes to be stored via 1 word stores */
  int size_2words;		/* # of bytes to be stored via 2 word stores */
  unsigned char field_p;	/* true if the registers are a single SPR */
  unsigned char dword_p;	/* true if we can do dword stores */
  unsigned char special_p;	/* true if the regs have a fixed save loc.  */
} frv_stack_regs_t;

/* Register ranges to look into saving.  */
#define STACK_REGS_GPR		0	/* Gprs (normally gr16..gr31, gr48..gr63) */
#define STACK_REGS_FPR		1	/* Fprs (normally fr16..fr31, fr48..fr63) */
#define STACK_REGS_LR		2	/* LR register */
#define STACK_REGS_CC		3	/* CCrs (normally not saved) */
#define STACK_REGS_LCR		5	/* lcr register */
#define STACK_REGS_STDARG	6	/* stdarg registers */
#define STACK_REGS_STRUCT	7	/* structure return (gr3) */
#define STACK_REGS_FP		8	/* FP register */
#define STACK_REGS_MAX		9	/* # of register ranges */

/* Values for save_p field.  */
#define REG_SAVE_NO_SAVE	0	/* register not saved */
#define REG_SAVE_1WORD		1	/* save the register */
#define REG_SAVE_2WORDS		2	/* save register and register+1 */

/* Structure used to define the frv stack.  */

typedef struct frv_stack {
  int total_size;		/* total bytes allocated for stack */
  int vars_size;		/* variable save area size */
  int parameter_size;		/* outgoing parameter size */
  int stdarg_size;		/* size of regs needed to be saved for stdarg */
  int regs_size;		/* size of the saved registers */
  int regs_size_1word;		/* # of bytes to be stored via 1 word stores */
  int regs_size_2words;		/* # of bytes to be stored via 2 word stores */
  int header_size;		/* size of the old FP, struct ret., LR save */
  int pretend_size;		/* size of pretend args */
  int vars_offset;		/* offset to save local variables from new SP*/
  int regs_offset;		/* offset to save registers from new SP */
				/* register range information */
  frv_stack_regs_t regs[STACK_REGS_MAX];
				/* offset to store each register */
  int reg_offset[FIRST_PSEUDO_REGISTER];
				/* whether to save register (& reg+1) */
  unsigned char save_p[FIRST_PSEUDO_REGISTER];
} frv_stack_t;

/* Define this macro if pushing a word onto the stack moves the stack pointer
   to a smaller address.  */
#define STACK_GROWS_DOWNWARD 1

/* Define this macro if the addresses of local variable slots are at negative
   offsets from the frame pointer.  */
#define FRAME_GROWS_DOWNWARD

/* Offset from the frame pointer to the first local variable slot to be
   allocated.

   If `FRAME_GROWS_DOWNWARD', find the next slot's offset by subtracting the
   first slot's length from `STARTING_FRAME_OFFSET'.  Otherwise, it is found by
   adding the length of the first slot to the value `STARTING_FRAME_OFFSET'.  */
#define STARTING_FRAME_OFFSET 0

/* Offset from the stack pointer register to the first location at which
   outgoing arguments are placed.  If not specified, the default value of zero
   is used.  This is the proper value for most machines.

   If `ARGS_GROW_DOWNWARD', this is the offset to the location above the first
   location at which outgoing arguments are placed.  */
#define STACK_POINTER_OFFSET 0

/* Offset from the argument pointer register to the first argument's address.
   On some machines it may depend on the data type of the function.

   If `ARGS_GROW_DOWNWARD', this is the offset to the location above the first
   argument's address.  */
#define FIRST_PARM_OFFSET(FUNDECL) 0

/* A C expression whose value is RTL representing the address in a stack frame
   where the pointer to the caller's frame is stored.  Assume that FRAMEADDR is
   an RTL expression for the address of the stack frame itself.

   If you don't define this macro, the default is to return the value of
   FRAMEADDR--that is, the stack frame address is also the address of the stack
   word that points to the previous frame.  */
#define DYNAMIC_CHAIN_ADDRESS(FRAMEADDR) frv_dynamic_chain_address (FRAMEADDR)

/* A C expression whose value is RTL representing the value of the return
   address for the frame COUNT steps up from the current frame, after the
   prologue.  FRAMEADDR is the frame pointer of the COUNT frame, or the frame
   pointer of the COUNT - 1 frame if `RETURN_ADDR_IN_PREVIOUS_FRAME' is
   defined.

   The value of the expression must always be the correct address when COUNT is
   zero, but may be `NULL_RTX' if there is not way to determine the return
   address of other frames.  */
#define RETURN_ADDR_RTX(COUNT, FRAMEADDR) frv_return_addr_rtx (COUNT, FRAMEADDR)

/* This function contains machine specific function data.  */
struct machine_function GTY(())
{
  /* True if we have created an rtx that relies on the stack frame.  */
  int frame_needed;
};

#define RETURN_POINTER_REGNUM LR_REGNO

/* A C expression whose value is RTL representing the location of the incoming
   return address at the beginning of any function, before the prologue.  This
   RTL is either a `REG', indicating that the return value is saved in `REG',
   or a `MEM' representing a location in the stack.

   You only need to define this macro if you want to support call frame
   debugging information like that provided by DWARF 2.  */
#define INCOMING_RETURN_ADDR_RTX gen_rtx_REG (SImode, RETURN_POINTER_REGNUM)


/* Register That Address the Stack Frame.  */

/* The register number of the stack pointer register, which must also be a
   fixed register according to `FIXED_REGISTERS'.  On most machines, the
   hardware determines which register this is.  */
#define STACK_POINTER_REGNUM (GPR_FIRST + 1)

/* The register number of the frame pointer register, which is used to access
   automatic variables in the stack frame.  On some machines, the hardware
   determines which register this is.  On other machines, you can choose any
   register you wish for this purpose.  */
#define FRAME_POINTER_REGNUM (GPR_FIRST + 2)

/* The register number of the arg pointer register, which is used to access the
   function's argument list.  On some machines, this is the same as the frame
   pointer register.  On some machines, the hardware determines which register
   this is.  On other machines, you can choose any register you wish for this
   purpose.  If this is not the same register as the frame pointer register,
   then you must mark it as a fixed register according to `FIXED_REGISTERS', or
   arrange to be able to eliminate it.  */

/* On frv this is a fake register that is eliminated in
   terms of either the frame pointer or stack pointer.  */
#define ARG_POINTER_REGNUM AP_FIRST

/* Register numbers used for passing a function's static chain pointer.  If
   register windows are used, the register number as seen by the called
   function is `STATIC_CHAIN_INCOMING_REGNUM', while the register number as
   seen by the calling function is `STATIC_CHAIN_REGNUM'.  If these registers
   are the same, `STATIC_CHAIN_INCOMING_REGNUM' need not be defined.

   The static chain register need not be a fixed register.

   If the static chain is passed in memory, these macros should not be defined;
   instead, the next two macros should be defined.  */
#define STATIC_CHAIN_REGNUM (GPR_FIRST + 7)
#define STATIC_CHAIN_INCOMING_REGNUM (GPR_FIRST + 7)


/* Eliminating the Frame Pointer and the Arg Pointer.  */

/* A C expression which is nonzero if a function must have and use a frame
   pointer.  This expression is evaluated in the reload pass.  If its value is
   nonzero the function will have a frame pointer.

   The expression can in principle examine the current function and decide
   according to the facts, but on most machines the constant 0 or the constant
   1 suffices.  Use 0 when the machine allows code to be generated with no
   frame pointer, and doing so saves some time or space.  Use 1 when there is
   no possible advantage to avoiding a frame pointer.

   In certain cases, the compiler does not know how to produce valid code
   without a frame pointer.  The compiler recognizes those cases and
   automatically gives the function a frame pointer regardless of what
   `FRAME_POINTER_REQUIRED' says.  You don't need to worry about them.

   In a function that does not require a frame pointer, the frame pointer
   register can be allocated for ordinary usage, unless you mark it as a fixed
   register.  See `FIXED_REGISTERS' for more information.  */
#define FRAME_POINTER_REQUIRED frv_frame_pointer_required ()

/* If defined, this macro specifies a table of register pairs used to eliminate
   unneeded registers that point into the stack frame.  If it is not defined,
   the only elimination attempted by the compiler is to replace references to
   the frame pointer with references to the stack pointer.

   The definition of this macro is a list of structure initializations, each of
   which specifies an original and replacement register.

   On some machines, the position of the argument pointer is not known until
   the compilation is completed.  In such a case, a separate hard register must
   be used for the argument pointer.  This register can be eliminated by
   replacing it with either the frame pointer or the argument pointer,
   depending on whether or not the frame pointer has been eliminated.

   In this case, you might specify:
        #define ELIMINABLE_REGS  \
        {{ARG_POINTER_REGNUM, STACK_POINTER_REGNUM}, \
         {ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM}, \
         {FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}}

   Note that the elimination of the argument pointer with the stack pointer is
   specified first since that is the preferred elimination.  */

#define ELIMINABLE_REGS							\
{									\
  {ARG_POINTER_REGNUM,	 STACK_POINTER_REGNUM},				\
  {ARG_POINTER_REGNUM,	 FRAME_POINTER_REGNUM},				\
  {FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}				\
}

/* A C expression that returns nonzero if the compiler is allowed to try to
   replace register number FROM with register number TO.  This macro need only
   be defined if `ELIMINABLE_REGS' is defined, and will usually be the constant
   1, since most of the cases preventing register elimination are things that
   the compiler already knows about.  */

#define CAN_ELIMINATE(FROM, TO)						\
  ((FROM) == ARG_POINTER_REGNUM && (TO) == STACK_POINTER_REGNUM		\
   ? ! frame_pointer_needed						\
   : 1)

/* This macro is similar to `INITIAL_FRAME_POINTER_OFFSET'.  It specifies the
   initial difference between the specified pair of registers.  This macro must
   be defined if `ELIMINABLE_REGS' is defined.  */

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			\
  (OFFSET) = frv_initial_elimination_offset (FROM, TO)


/* Passing Function Arguments on the Stack.  */

/* If defined, the maximum amount of space required for outgoing arguments will
   be computed and placed into the variable
   `current_function_outgoing_args_size'.  No space will be pushed onto the
   stack for each call; instead, the function prologue should increase the
   stack frame size by this amount.

   Defining both `PUSH_ROUNDING' and `ACCUMULATE_OUTGOING_ARGS' is not
   proper.  */
#define ACCUMULATE_OUTGOING_ARGS 1

/* A C expression that should indicate the number of bytes of its own arguments
   that a function pops on returning, or 0 if the function pops no arguments
   and the caller must therefore pop them all after the function returns.

   FUNDECL is a C variable whose value is a tree node that describes the
   function in question.  Normally it is a node of type `FUNCTION_DECL' that
   describes the declaration of the function.  From this it is possible to
   obtain the DECL_ATTRIBUTES of the function.

   FUNTYPE is a C variable whose value is a tree node that describes the
   function in question.  Normally it is a node of type `FUNCTION_TYPE' that
   describes the data type of the function.  From this it is possible to obtain
   the data types of the value and arguments (if known).

   When a call to a library function is being considered, FUNTYPE will contain
   an identifier node for the library function.  Thus, if you need to
   distinguish among various library functions, you can do so by their names.
   Note that "library function" in this context means a function used to
   perform arithmetic, whose name is known specially in the compiler and was
   not mentioned in the C code being compiled.

   STACK-SIZE is the number of bytes of arguments passed on the stack.  If a
   variable number of bytes is passed, it is zero, and argument popping will
   always be the responsibility of the calling function.

   On the VAX, all functions always pop their arguments, so the definition of
   this macro is STACK-SIZE.  On the 68000, using the standard calling
   convention, no functions pop their arguments, so the value of the macro is
   always 0 in this case.  But an alternative calling convention is available
   in which functions that take a fixed number of arguments pop them but other
   functions (such as `printf') pop nothing (the caller pops all).  When this
   convention is in use, FUNTYPE is examined to determine whether a function
   takes a fixed number of arguments.  */
#define RETURN_POPS_ARGS(FUNDECL, FUNTYPE, STACK_SIZE) 0


/* Function Arguments in Registers.  */

/* Nonzero if we do not know how to pass TYPE solely in registers.
   We cannot do so in the following cases:

   - if the type has variable size
   - if the type is marked as addressable (it is required to be constructed
     into the stack)
   - if the type is a structure or union.  */

#define MUST_PASS_IN_STACK(MODE,TYPE)                           \
   (((MODE) == BLKmode)                                         \
    || ((TYPE) != 0                                             \
         && (TREE_CODE (TYPE_SIZE (TYPE)) != INTEGER_CST        \
             || TREE_CODE (TYPE) == RECORD_TYPE                 \
             || TREE_CODE (TYPE) == UNION_TYPE                  \
             || TREE_CODE (TYPE) == QUAL_UNION_TYPE             \
             || TREE_ADDRESSABLE (TYPE))))

/* The number of register assigned to holding function arguments.  */

#define FRV_NUM_ARG_REGS        6

/* A C expression that controls whether a function argument is passed in a
   register, and which register.

   The arguments are CUM, of type CUMULATIVE_ARGS, which summarizes (in a way
   defined by INIT_CUMULATIVE_ARGS and FUNCTION_ARG_ADVANCE) all of the previous
   arguments so far passed in registers; MODE, the machine mode of the argument;
   TYPE, the data type of the argument as a tree node or 0 if that is not known
   (which happens for C support library functions); and NAMED, which is 1 for an
   ordinary argument and 0 for nameless arguments that correspond to `...' in the
   called function's prototype.

   The value of the expression should either be a `reg' RTX for the hard
   register in which to pass the argument, or zero to pass the argument on the
   stack.

   For machines like the VAX and 68000, where normally all arguments are
   pushed, zero suffices as a definition.

   The usual way to make the ANSI library `stdarg.h' work on a machine where
   some arguments are usually passed in registers, is to cause nameless
   arguments to be passed on the stack instead.  This is done by making
   `FUNCTION_ARG' return 0 whenever NAMED is 0.

   You may use the macro `MUST_PASS_IN_STACK (MODE, TYPE)' in the definition of
   this macro to determine if this argument is of a type that must be passed in
   the stack.  If `REG_PARM_STACK_SPACE' is not defined and `FUNCTION_ARG'
   returns nonzero for such an argument, the compiler will abort.  If
   `REG_PARM_STACK_SPACE' is defined, the argument will be computed in the
   stack and then loaded into a register.  */
#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED)                    \
  frv_function_arg (&CUM, MODE, TYPE, NAMED, FALSE)

/* Define this macro if the target machine has "register windows", so that the
   register in which a function sees an arguments is not necessarily the same
   as the one in which the caller passed the argument.

   For such machines, `FUNCTION_ARG' computes the register in which the caller
   passes the value, and `FUNCTION_INCOMING_ARG' should be defined in a similar
   fashion to tell the function being called where the arguments will arrive.

   If `FUNCTION_INCOMING_ARG' is not defined, `FUNCTION_ARG' serves both
   purposes.  */

#define FUNCTION_INCOMING_ARG(CUM, MODE, TYPE, NAMED)			\
  frv_function_arg (&CUM, MODE, TYPE, NAMED, TRUE)

/* A C expression for the number of words, at the beginning of an argument,
   must be put in registers.  The value must be zero for arguments that are
   passed entirely in registers or that are entirely pushed on the stack.

   On some machines, certain arguments must be passed partially in registers
   and partially in memory.  On these machines, typically the first N words of
   arguments are passed in registers, and the rest on the stack.  If a
   multi-word argument (a `double' or a structure) crosses that boundary, its
   first few words must be passed in registers and the rest must be pushed.
   This macro tells the compiler when this occurs, and how many of the words
   should go in registers.

   `FUNCTION_ARG' for these arguments should return the first register to be
   used by the caller for this argument; likewise `FUNCTION_INCOMING_ARG', for
   the called function.  */
#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED)		\
  frv_function_arg_partial_nregs (&CUM, MODE, TYPE, NAMED)

/* extern int frv_function_arg_partial_nregs PARAMS ((CUMULATIVE_ARGS, int, Tree, int));  */

/* A C expression that indicates when an argument must be passed by reference.
   If nonzero for an argument, a copy of that argument is made in memory and a
   pointer to the argument is passed instead of the argument itself.  The
   pointer is passed in whatever way is appropriate for passing a pointer to
   that type.

   On machines where `REG_PARM_STACK_SPACE' is not defined, a suitable
   definition of this macro might be
        #define FUNCTION_ARG_PASS_BY_REFERENCE(CUM, MODE, TYPE, NAMED)  \
          MUST_PASS_IN_STACK (MODE, TYPE)  */
#define FUNCTION_ARG_PASS_BY_REFERENCE(CUM, MODE, TYPE, NAMED)		\
  frv_function_arg_pass_by_reference (&CUM, MODE, TYPE, NAMED)

/* If defined, a C expression that indicates when it is the called function's
   responsibility to make a copy of arguments passed by invisible reference.
   Normally, the caller makes a copy and passes the address of the copy to the
   routine being called.  When FUNCTION_ARG_CALLEE_COPIES is defined and is
   nonzero, the caller does not make a copy.  Instead, it passes a pointer to
   the "live" value.  The called function must not modify this value.  If it
   can be determined that the value won't be modified, it need not make a copy;
   otherwise a copy must be made.  */
#define FUNCTION_ARG_CALLEE_COPIES(CUM, MODE, TYPE, NAMED)		\
  frv_function_arg_callee_copies (&CUM, MODE, TYPE, NAMED)

/* If defined, a C expression that indicates when it is more desirable to keep
   an argument passed by invisible reference as a reference, rather than
   copying it to a pseudo register.  */
#define FUNCTION_ARG_KEEP_AS_REFERENCE(CUM, MODE, TYPE, NAMED)		\
  frv_function_arg_keep_as_reference (&CUM, MODE, TYPE, NAMED)

/* A C type for declaring a variable that is used as the first argument of
   `FUNCTION_ARG' and other related values.  For some target machines, the type
   `int' suffices and can hold the number of bytes of argument so far.

   There is no need to record in `CUMULATIVE_ARGS' anything about the arguments
   that have been passed on the stack.  The compiler has other variables to
   keep track of that.  For target machines on which all arguments are passed
   on the stack, there is no need to store anything in `CUMULATIVE_ARGS';
   however, the data structure must exist and should not be empty, so use
   `int'.  */
#define CUMULATIVE_ARGS int

/* A C statement (sans semicolon) for initializing the variable CUM for the
   state at the beginning of the argument list.  The variable has type
   `CUMULATIVE_ARGS'.  The value of FNTYPE is the tree node for the data type
   of the function which will receive the args, or 0 if the args are to a
   compiler support library function.  The value of INDIRECT is nonzero when
   processing an indirect call, for example a call through a function pointer.
   The value of INDIRECT is zero for a call to an explicitly named function, a
   library function call, or when `INIT_CUMULATIVE_ARGS' is used to find
   arguments for the function being compiled.

   When processing a call to a compiler support library function, LIBNAME
   identifies which one.  It is a `symbol_ref' rtx which contains the name of
   the function, as a string.  LIBNAME is 0 when an ordinary C function call is
   being processed.  Thus, each time this macro is called, either LIBNAME or
   FNTYPE is nonzero, but never both of them at once.  */

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT) \
  frv_init_cumulative_args (&CUM, FNTYPE, LIBNAME, INDIRECT, FALSE)

/* Like `INIT_CUMULATIVE_ARGS' but overrides it for the purposes of finding the
   arguments for the function being compiled.  If this macro is undefined,
   `INIT_CUMULATIVE_ARGS' is used instead.

   The value passed for LIBNAME is always 0, since library routines with
   special calling conventions are never compiled with GNU CC.  The argument
   LIBNAME exists for symmetry with `INIT_CUMULATIVE_ARGS'.  */

#define INIT_CUMULATIVE_INCOMING_ARGS(CUM, FNTYPE, LIBNAME) \
  frv_init_cumulative_args (&CUM, FNTYPE, LIBNAME, FALSE, TRUE)

/* A C statement (sans semicolon) to update the summarizer variable CUM to
   advance past an argument in the argument list.  The values MODE, TYPE and
   NAMED describe that argument.  Once this is done, the variable CUM is
   suitable for analyzing the *following* argument with `FUNCTION_ARG', etc.

   This macro need not do anything if the argument in question was passed on
   the stack.  The compiler knows how to track the amount of stack space used
   for arguments without any special help.  */
#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)			\
  frv_function_arg_advance (&CUM, MODE, TYPE, NAMED)

/* If defined, a C expression that gives the alignment boundary, in bits, of an
   argument with the specified mode and type.  If it is not defined,
   `PARM_BOUNDARY' is used for all arguments.  */

#define FUNCTION_ARG_BOUNDARY(MODE, TYPE) \
  frv_function_arg_boundary (MODE, TYPE)

/* A C expression that is nonzero if REGNO is the number of a hard register in
   which function arguments are sometimes passed.  This does *not* include
   implicit arguments such as the static chain and the structure-value address.
   On many machines, no registers can be used for this purpose since all
   function arguments are pushed on the stack.  */
#define FUNCTION_ARG_REGNO_P(REGNO) \
  ((REGNO) >= FIRST_ARG_REGNUM && ((REGNO) <= LAST_ARG_REGNUM))


/* How Scalar Function Values are Returned.  */

/* The number of the hard register that is used to return a scalar value from a
   function call.  */
#define RETURN_VALUE_REGNUM	(GPR_FIRST + 8)

/* A C expression to create an RTX representing the place where a function
   returns a value of data type VALTYPE.  VALTYPE is a tree node representing a
   data type.  Write `TYPE_MODE (VALTYPE)' to get the machine mode used to
   represent that type.  On many machines, only the mode is relevant.
   (Actually, on most machines, scalar values are returned in the same place
   regardless of mode).

   If `PROMOTE_FUNCTION_RETURN' is defined, you must apply the same promotion
   rules specified in `PROMOTE_MODE' if VALTYPE is a scalar type.

   If the precise function being called is known, FUNC is a tree node
   (`FUNCTION_DECL') for it; otherwise, FUNC is a null pointer.  This makes it
   possible to use a different value-returning convention for specific
   functions when all their calls are known.

   `FUNCTION_VALUE' is not used for return vales with aggregate data types,
   because these are returned in another way.  See `STRUCT_VALUE_REGNUM' and
   related macros, below.  */
#define FUNCTION_VALUE(VALTYPE, FUNC) \
  gen_rtx (REG, TYPE_MODE (VALTYPE), RETURN_VALUE_REGNUM)

/* A C expression to create an RTX representing the place where a library
   function returns a value of mode MODE.

   Note that "library function" in this context means a compiler support
   routine, used to perform arithmetic, whose name is known specially by the
   compiler and was not mentioned in the C code being compiled.

   The definition of `LIBRARY_VALUE' need not be concerned aggregate data
   types, because none of the library functions returns such types.  */
#define LIBCALL_VALUE(MODE) gen_rtx (REG, MODE, RETURN_VALUE_REGNUM)

/* A C expression that is nonzero if REGNO is the number of a hard register in
   which the values of called function may come back.

   A register whose use for returning values is limited to serving as the
   second of a pair (for a value of type `double', say) need not be recognized
   by this macro.  So for most machines, this definition suffices:

        #define FUNCTION_VALUE_REGNO_P(N) ((N) == RETURN)

   If the machine has register windows, so that the caller and the called
   function use different registers for the return value, this macro should
   recognize only the caller's register numbers.  */
#define FUNCTION_VALUE_REGNO_P(REGNO) ((REGNO) == RETURN_VALUE_REGNUM)


/* How Large Values are Returned.  */

/* If the structure value address is passed in a register, then
   `STRUCT_VALUE_REGNUM' should be the number of that register.  */
#define STRUCT_VALUE_REGNUM (GPR_FIRST + 3)


/* Function Entry and Exit.  */

/* Define this macro as a C expression that is nonzero if the return
   instruction or the function epilogue ignores the value of the stack pointer;
   in other words, if it is safe to delete an instruction to adjust the stack
   pointer before a return from the function.

   Note that this macro's value is relevant only for functions for which frame
   pointers are maintained.  It is never safe to delete a final stack
   adjustment in a function that has no frame pointer, and the compiler knows
   this regardless of `EXIT_IGNORE_STACK'.  */
#define EXIT_IGNORE_STACK 1

/* Generating Code for Profiling.  */

/* A C statement or compound statement to output to FILE some assembler code to
   call the profiling subroutine `mcount'.  Before calling, the assembler code
   must load the address of a counter variable into a register where `mcount'
   expects to find the address.  The name of this variable is `LP' followed by
   the number LABELNO, so you would generate the name using `LP%d' in a
   `fprintf'.

   The details of how the address should be passed to `mcount' are determined
   by your operating system environment, not by GNU CC.  To figure them out,
   compile a small program for profiling using the system's installed C
   compiler and look at the assembler code that results.

   This declaration must be present, but it can be an abort if profiling is
   not implemented.  */

#define FUNCTION_PROFILER(FILE, LABELNO) abort ()


/* Implementing the Varargs Macros.  */

/* If defined, is a C expression that produces the machine-specific code for a
   call to `__builtin_saveregs'.  This code will be moved to the very beginning
   of the function, before any parameter access are made.  The return value of
   this function should be an RTX that contains the value to use as the return
   of `__builtin_saveregs'.

   If this macro is not defined, the compiler will output an ordinary call to
   the library function `__builtin_saveregs'.  */

#define EXPAND_BUILTIN_SAVEREGS() frv_expand_builtin_saveregs ()

/* This macro offers an alternative to using `__builtin_saveregs' and defining
   the macro `EXPAND_BUILTIN_SAVEREGS'.  Use it to store the anonymous register
   arguments into the stack so that all the arguments appear to have been
   passed consecutively on the stack.  Once this is done, you can use the
   standard implementation of varargs that works for machines that pass all
   their arguments on the stack.

   The argument ARGS_SO_FAR is the `CUMULATIVE_ARGS' data structure, containing
   the values that obtain after processing of the named arguments.  The
   arguments MODE and TYPE describe the last named argument--its machine mode
   and its data type as a tree node.

   The macro implementation should do two things: first, push onto the stack
   all the argument registers *not* used for the named arguments, and second,
   store the size of the data thus pushed into the `int'-valued variable whose
   name is supplied as the argument PRETEND_ARGS_SIZE.  The value that you
   store here will serve as additional offset for setting up the stack frame.

   Because you must generate code to push the anonymous arguments at compile
   time without knowing their data types, `SETUP_INCOMING_VARARGS' is only
   useful on machines that have just a single category of argument register and
   use it uniformly for all data types.

   If the argument SECOND_TIME is nonzero, it means that the arguments of the
   function are being analyzed for the second time.  This happens for an inline
   function, which is not actually compiled until the end of the source file.
   The macro `SETUP_INCOMING_VARARGS' should not generate any instructions in
   this case.  */
#define SETUP_INCOMING_VARARGS(ARGS_SO_FAR, MODE, TYPE, PRETEND_ARGS_SIZE, SECOND_TIME) \
  frv_setup_incoming_varargs (& ARGS_SO_FAR, (int) MODE, TYPE, 	\
			      & PRETEND_ARGS_SIZE, SECOND_TIME)

/* Implement the stdarg/varargs va_start macro.  STDARG_P is nonzero if this
   is stdarg.h instead of varargs.h.  VALIST is the tree of the va_list
   variable to initialize.  NEXTARG is the machine independent notion of the
   'next' argument after the variable arguments.  If not defined, a standard
   implementation will be defined that works for arguments passed on the stack.  */

#define EXPAND_BUILTIN_VA_START(VALIST, NEXTARG)		\
  (frv_expand_builtin_va_start(VALIST, NEXTARG))

/* Implement the stdarg/varargs va_arg macro.  VALIST is the variable of type
   va_list as a tree, TYPE is the type passed to va_arg.  */

#define EXPAND_BUILTIN_VA_ARG(VALIST, TYPE)				\
  (frv_expand_builtin_va_arg (VALIST, TYPE))


/* Trampolines for Nested Functions.  */

/* A C expression for the size in bytes of the trampoline, as an integer.  */
#define TRAMPOLINE_SIZE frv_trampoline_size ()

/* Alignment required for trampolines, in bits.

   If you don't define this macro, the value of `BIGGEST_ALIGNMENT' is used for
   aligning trampolines.  */
#define TRAMPOLINE_ALIGNMENT 32

/* A C statement to initialize the variable parts of a trampoline.  ADDR is an
   RTX for the address of the trampoline; FNADDR is an RTX for the address of
   the nested function; STATIC_CHAIN is an RTX for the static chain value that
   should be passed to the function when it is called.  */
#define INITIALIZE_TRAMPOLINE(ADDR, FNADDR, STATIC_CHAIN) \
  frv_initialize_trampoline (ADDR, FNADDR, STATIC_CHAIN)

/* Define this macro if trampolines need a special subroutine to do their work.
   The macro should expand to a series of `asm' statements which will be
   compiled with GNU CC.  They go in a library function named
   `__transfer_from_trampoline'.

   If you need to avoid executing the ordinary prologue code of a compiled C
   function when you jump to the subroutine, you can do so by placing a special
   label of your own in the assembler code.  Use one `asm' statement to
   generate an assembler label, and another to make the label global.  Then
   trampolines can use that label to jump directly to your special assembler
   code.  */

#ifdef __FRV_UNDERSCORE__
#define TRAMPOLINE_TEMPLATE_NAME "___trampoline_template"
#else
#define TRAMPOLINE_TEMPLATE_NAME "__trampoline_template"
#endif

#define TRANSFER_FROM_TRAMPOLINE					\
extern int _write (int, const void *, unsigned);			\
									\
void									\
__trampoline_setup (addr, size, fnaddr, sc)				\
     short * addr;							\
     int size;								\
     int fnaddr;							\
     int sc;								\
{									\
  extern short __trampoline_template[];					\
  short * to = addr;							\
  short * from = &__trampoline_template[0];				\
  int i;								\
									\
  if (size < 20)							\
    {									\
      _write (2, "__trampoline_setup bad size\n",			\
	      sizeof ("__trampoline_setup bad size\n") - 1);		\
      exit (-1);							\
    }									\
									\
  to[0] = from[0];							\
  to[1] = (short)(fnaddr);						\
  to[2] = from[2];							\
  to[3] = (short)(sc);							\
  to[4] = from[4];							\
  to[5] = (short)(fnaddr >> 16);					\
  to[6] = from[6];							\
  to[7] = (short)(sc >> 16);						\
  to[8] = from[8];							\
  to[9] = from[9];							\
									\
  for (i = 0; i < 20; i++)						\
    __asm__ volatile ("dcf @(%0,%1)\n\tici @(%0,%1)" :: "r" (to), "r" (i)); \
}									\
									\
__asm__("\n"								\
	"\t.globl " TRAMPOLINE_TEMPLATE_NAME "\n"			\
	"\t.text\n"							\
	TRAMPOLINE_TEMPLATE_NAME ":\n"					\
	"\tsetlos #0, gr6\n"	/* jump register */			\
	"\tsetlos #0, gr7\n"	/* static chain */			\
	"\tsethi #0, gr6\n"						\
	"\tsethi #0, gr7\n"						\
	"\tjmpl @(gr0,gr6)\n");


/* Implicit Calls to Library Routines.  */

/* A C string constant giving the name of the function to call for the
   remainder in division of one signed full-word by another.  If you do not
   define this macro, the default name is used, which is `__modsi3', a function
   defined in `libgcc.a'.  */
#define MODSI3_LIBCALL "__modi"

/* A C string constant giving the name of the function to call for the
   remainder in division of one unsigned full-word by another.  If you do not
   define this macro, the default name is used, which is `__umodsi3', a
   function defined in `libgcc.a'.  */
#define UMODSI3_LIBCALL "__umodi"

/* A C string constant giving the name of the function to call for
   multiplication of one signed double-word by another.  If you do not define
   this macro, the default name is used, which is `__muldi3', a function
   defined in `libgcc.a'.  */
#define MULDI3_LIBCALL "__mulll"

/* A C string constant giving the name of the function to call for division of
   one signed double-word by another.  If you do not define this macro, the
   default name is used, which is `__divdi3', a function defined in `libgcc.a'.  */
#define DIVDI3_LIBCALL "__divll"

/* A C string constant giving the name of the function to call for division of
   one unsigned full-word by another.  If you do not define this macro, the
   default name is used, which is `__udivdi3', a function defined in
   `libgcc.a'.  */
#define UDIVDI3_LIBCALL "__udivll"

/* A C string constant giving the name of the function to call for the
   remainder in division of one signed double-word by another.  If you do not
   define this macro, the default name is used, which is `__moddi3', a function
   defined in `libgcc.a'.  */
#define MODDI3_LIBCALL "__modll"

/* A C string constant giving the name of the function to call for the
   remainder in division of one unsigned full-word by another.  If you do not
   define this macro, the default name is used, which is `__umoddi3', a
   function defined in `libgcc.a'.  */
#define UMODDI3_LIBCALL "__umodll"

/* Define this macro as a C statement that declares additional library routines
   renames existing ones. `init_optabs' calls this macro after initializing all
   the normal library routines.  */
#define INIT_TARGET_OPTABS 					\
  do								\
    {								\
      add_optab->handlers [(int) DImode].libfunc		\
	= init_one_libfunc ("__addll");				\
      sub_optab->handlers [(int) DImode].libfunc		\
	= init_one_libfunc ("__subll");				\
      and_optab->handlers [(int) DImode].libfunc		\
	= init_one_libfunc ("__andll");				\
      ior_optab->handlers [(int) DImode].libfunc		\
	= init_one_libfunc ("__orll");				\
      xor_optab->handlers [(int) DImode].libfunc		\
	= init_one_libfunc ("__xorll");				\
      one_cmpl_optab->handlers [(int) DImode].libfunc		\
	= init_one_libfunc ("__notll");				\
      add_optab->handlers [(int) SFmode].libfunc		\
	= init_one_libfunc ("__addf");				\
      sub_optab->handlers [(int) SFmode].libfunc		\
	= init_one_libfunc ("__subf");				\
      smul_optab->handlers [(int) SFmode].libfunc		\
	= init_one_libfunc ("__mulf");				\
      sdiv_optab->handlers [(int) SFmode].libfunc		\
	= init_one_libfunc ("__divf");				\
      add_optab->handlers [(int) DFmode].libfunc		\
	= init_one_libfunc ("__addd");				\
      sub_optab->handlers [(int) DFmode].libfunc		\
	= init_one_libfunc ("__subd");				\
      smul_optab->handlers [(int) DFmode].libfunc		\
	= init_one_libfunc ("__muld");				\
      sdiv_optab->handlers [(int) DFmode].libfunc		\
	= init_one_libfunc ("__divd");				\
      fixsfsi_libfunc = init_one_libfunc ("__ftoi");		\
      fixunssfsi_libfunc = init_one_libfunc ("__ftoui");	\
      fixsfdi_libfunc = init_one_libfunc ("__ftoll");		\
      fixunssfdi_libfunc = init_one_libfunc ("__ftoull");	\
      fixdfsi_libfunc = init_one_libfunc ("__dtoi");		\
      fixunsdfsi_libfunc = init_one_libfunc ("__dtoui");	\
      fixdfdi_libfunc = init_one_libfunc ("__dtoll");		\
      fixunsdfdi_libfunc = init_one_libfunc ("__dtoull");	\
      floatsisf_libfunc = init_one_libfunc ("__itof");		\
      floatdisf_libfunc = init_one_libfunc ("__lltof");		\
      floatsidf_libfunc = init_one_libfunc ("__itod");		\
      floatdidf_libfunc = init_one_libfunc ("__lltod");		\
      extendsfdf2_libfunc = init_one_libfunc ("__ftod");	\
      truncdfsf2_libfunc = init_one_libfunc ("__dtof");		\
    }								\
  while (0)


/* Addressing Modes.  */

/* A C expression that is 1 if the RTX X is a constant which is a valid
   address.  On most machines, this can be defined as `CONSTANT_P (X)', but a
   few machines are more restrictive in which constant addresses are supported.

   `CONSTANT_P' accepts integer-values expressions whose values are not
   explicitly known, such as `symbol_ref', `label_ref', and `high' expressions
   and `const' arithmetic expressions, in addition to `const_int' and
   `const_double' expressions.  */
#define CONSTANT_ADDRESS_P(X) CONSTANT_P (X)

/* A number, the maximum number of registers that can appear in a valid memory
   address.  Note that it is up to you to specify a value equal to the maximum
   number that `GO_IF_LEGITIMATE_ADDRESS' would ever accept.  */
#define MAX_REGS_PER_ADDRESS 2

/* A C compound statement with a conditional `goto LABEL;' executed if X (an
   RTX) is a legitimate memory address on the target machine for a memory
   operand of mode MODE.

   It usually pays to define several simpler macros to serve as subroutines for
   this one.  Otherwise it may be too complicated to understand.

   This macro must exist in two variants: a strict variant and a non-strict
   one.  The strict variant is used in the reload pass.  It must be defined so
   that any pseudo-register that has not been allocated a hard register is
   considered a memory reference.  In contexts where some kind of register is
   required, a pseudo-register with no hard register must be rejected.

   The non-strict variant is used in other passes.  It must be defined to
   accept all pseudo-registers in every context where some kind of register is
   required.

   Compiler source files that want to use the strict variant of this macro
   define the macro `REG_OK_STRICT'.  You should use an `#ifdef REG_OK_STRICT'
   conditional to define the strict variant in that case and the non-strict
   variant otherwise.

   Subroutines to check for acceptable registers for various purposes (one for
   base registers, one for index registers, and so on) are typically among the
   subroutines used to define `GO_IF_LEGITIMATE_ADDRESS'.  Then only these
   subroutine macros need have two variants; the higher levels of macros may be
   the same whether strict or not.

   Normally, constant addresses which are the sum of a `symbol_ref' and an
   integer are stored inside a `const' RTX to mark them as constant.
   Therefore, there is no need to recognize such sums specifically as
   legitimate addresses.  Normally you would simply recognize any `const' as
   legitimate.

   Usually `PRINT_OPERAND_ADDRESS' is not prepared to handle constant sums that
   are not marked with `const'.  It assumes that a naked `plus' indicates
   indexing.  If so, then you *must* reject such naked constant sums as
   illegitimate addresses, so that none of them will be given to
   `PRINT_OPERAND_ADDRESS'.

   On some machines, whether a symbolic address is legitimate depends on the
   section that the address refers to.  On these machines, define the macro
   `ENCODE_SECTION_INFO' to store the information into the `symbol_ref', and
   then check for it here.  When you see a `const', you will have to look
   inside it to find the `symbol_ref' in order to determine the section.

   The best way to modify the name string is by adding text to the beginning,
   with suitable punctuation to prevent any ambiguity.  Allocate the new name
   in `saveable_obstack'.  You will have to modify `ASM_OUTPUT_LABELREF' to
   remove and decode the added text and output the name accordingly, and define
   `(* targetm.strip_name_encoding)' to access the original name string.

   You can check the information stored here into the `symbol_ref' in the
   definitions of the macros `GO_IF_LEGITIMATE_ADDRESS' and
   `PRINT_OPERAND_ADDRESS'.  */

#ifdef REG_OK_STRICT
#define REG_OK_STRICT_P 1
#else
#define REG_OK_STRICT_P 0
#endif

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, LABEL)			\
  do									\
    {									\
      if (frv_legitimate_address_p (MODE, X, REG_OK_STRICT_P, FALSE))	\
	goto LABEL;							\
    }									\
  while (0)

/* A C expression that is nonzero if X (assumed to be a `reg' RTX) is valid for
   use as a base register.  For hard registers, it should always accept those
   which the hardware permits and reject the others.  Whether the macro accepts
   or rejects pseudo registers must be controlled by `REG_OK_STRICT' as
   described above.  This usually requires two variant definitions, of which
   `REG_OK_STRICT' controls the one actually used.  */
#ifdef REG_OK_STRICT
#define REG_OK_FOR_BASE_P(X) GPR_P (REGNO (X))
#else
#define REG_OK_FOR_BASE_P(X) GPR_AP_OR_PSEUDO_P (REGNO (X))
#endif

/* A C expression that is nonzero if X (assumed to be a `reg' RTX) is valid for
   use as an index register.

   The difference between an index register and a base register is that the
   index register may be scaled.  If an address involves the sum of two
   registers, neither one of them scaled, then either one may be labeled the
   "base" and the other the "index"; but whichever labeling is used must fit
   the machine's constraints of which registers may serve in each capacity.
   The compiler will try both labelings, looking for one that is valid, and
   will reload one or both registers only if neither labeling works.  */
#define REG_OK_FOR_INDEX_P(X) REG_OK_FOR_BASE_P (X)

/* A C compound statement that attempts to replace X with a valid memory
   address for an operand of mode MODE.  WIN will be a C statement label
   elsewhere in the code; the macro definition may use

        GO_IF_LEGITIMATE_ADDRESS (MODE, X, WIN);

   to avoid further processing if the address has become legitimate.

   X will always be the result of a call to `break_out_memory_refs', and OLDX
   will be the operand that was given to that function to produce X.

   The code generated by this macro should not alter the substructure of X.  If
   it transforms X into a more legitimate form, it should assign X (which will
   always be a C variable) a new value.

   It is not necessary for this macro to come up with a legitimate address.
   The compiler has standard ways of doing so in all cases.  In fact, it is
   safe for this macro to do nothing.  But often a machine-dependent strategy
   can generate better code.  */

/* On the FRV, we use it to convert small data and pic references into using
   the appropriate pointer in the address.  */
#define LEGITIMIZE_ADDRESS(X, OLDX, MODE, WIN)			\
  do								\
    {								\
      rtx newx = frv_legitimize_address (X, OLDX, MODE);	\
								\
      if (newx)							\
	{							\
	  (X) = newx;						\
	  goto WIN;						\
	}							\
    }								\
  while (0)

/* A C statement or compound statement with a conditional `goto LABEL;'
   executed if memory address X (an RTX) can have different meanings depending
   on the machine mode of the memory reference it is used for or if the address
   is valid for some modes but not others.

   Autoincrement and autodecrement addresses typically have mode-dependent
   effects because the amount of the increment or decrement is the size of the
   operand being addressed.  Some machines have other mode-dependent addresses.
   Many RISC machines have no mode-dependent addresses.

   You may assume that ADDR is a valid address for the machine.  */
#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR, LABEL)

/* A C expression that is nonzero if X is a legitimate constant for an
   immediate operand on the target machine.  You can assume that X satisfies
   `CONSTANT_P', so you need not check this.  In fact, `1' is a suitable
   definition for this macro on machines where anything `CONSTANT_P' is valid.  */
#define LEGITIMATE_CONSTANT_P(X) frv_legitimate_constant_p (X)

/* The load-and-update commands allow pre-modification in addresses.
   The index has to be in a register.  */
#define HAVE_PRE_MODIFY_REG 1


/* Returns a mode from class `MODE_CC' to be used when comparison operation
   code OP is applied to rtx X and Y.  For example, on the SPARC,
   `SELECT_CC_MODE' is defined as (see *note Jump Patterns::.  for a
   description of the reason for this definition)

        #define SELECT_CC_MODE(OP,X,Y) \
          (GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT          \
           ? ((OP == EQ || OP == NE) ? CCFPmode : CCFPEmode)    \
           : ((GET_CODE (X) == PLUS || GET_CODE (X) == MINUS    \
               || GET_CODE (X) == NEG) \
              ? CC_NOOVmode : CCmode))

   You need not define this macro if `EXTRA_CC_MODES' is not defined.  */
#define SELECT_CC_MODE(OP, X, Y)					\
  (GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT				\
   ? CC_FPmode								\
   : (((OP) == LEU || (OP) == GTU || (OP) == LTU || (OP) == GEU)	\
      ? CC_UNSmode							\
      : CCmode))

/* A C expression whose value is one if it is always safe to reverse a
   comparison whose mode is MODE.  If `SELECT_CC_MODE' can ever return MODE for
   a floating-point inequality comparison, then `REVERSIBLE_CC_MODE (MODE)'
   must be zero.

   You need not define this macro if it would always returns zero or if the
   floating-point format is anything other than `IEEE_FLOAT_FORMAT'.  For
   example, here is the definition used on the SPARC, where floating-point
   inequality comparisons are always given `CCFPEmode':

        #define REVERSIBLE_CC_MODE(MODE)  ((MODE) != CCFPEmode)  */

/* On frv, don't consider floating point comparisons to be reversible.  In
   theory, fp equality comparisons can be reversible */
#define REVERSIBLE_CC_MODE(MODE) ((MODE) == CCmode || (MODE) == CC_UNSmode)

/* Frv CCR_MODE's are not reversible.  */
#define REVERSE_CONDEXEC_PREDICATES_P(x,y)      0


/* Describing Relative Costs of Operations.  */

/* A part of a C `switch' statement that describes the relative costs of
   constant RTL expressions.  It must contain `case' labels for expression
   codes `const_int', `const', `symbol_ref', `label_ref' and `const_double'.
   Each case must ultimately reach a `return' statement to return the relative
   cost of the use of that kind of constant value in an expression.  The cost
   may depend on the precise value of the constant, which is available for
   examination in X, and the rtx code of the expression in which it is
   contained, found in OUTER_CODE.

   CODE is the expression code--redundant, since it can be obtained with
   `GET_CODE (X)'.  */
#define CONST_COSTS(X, CODE, OUTER_CODE)				\
  case CONST:								\
  case LABEL_REF:							\
  case SYMBOL_REF:							\
  case CONST_DOUBLE:							\
    return COSTS_N_INSNS (2);						\
									\
  case CONST_INT:							\
    /* Make 12 bit integers really cheap */				\
    return IN_RANGE_P (INTVAL (X), -2048, 2047) ? 0 : COSTS_N_INSNS (2); \

/* Like `CONST_COSTS' but applies to nonconstant RTL expressions.  This can be
   used, for example, to indicate how costly a multiply instruction is.  In
   writing this macro, you can use the construct `COSTS_N_INSNS (N)' to specify
   a cost equal to N fast instructions.  OUTER_CODE is the code of the
   expression in which X is contained.

   This macro is optional; do not define it if the default cost assumptions are
   adequate for the target machine.  */
#define RTX_COSTS(X, CODE, OUTER_CODE)					\
  case PLUS:								\
  case MINUS:								\
  case AND:								\
  case IOR:								\
  case XOR:								\
  case ASHIFT:								\
  case ASHIFTRT:							\
  case LSHIFTRT:							\
  case NOT:								\
  case NEG:								\
  case COMPARE:								\
    if (GET_MODE (X) == SImode)						\
      return COSTS_N_INSNS (1);						\
    else if (GET_MODE (X) == DImode)					\
      return COSTS_N_INSNS (2);						\
    else								\
      return COSTS_N_INSNS (3);	/* guess */				\
									\
  case MULT:								\
    if (GET_MODE (X) == SImode)						\
      return COSTS_N_INSNS (2);						\
    else								\
      return COSTS_N_INSNS (6);	/* guess */				\
									\
  case DIV:								\
  case UDIV:								\
    return COSTS_N_INSNS (18);

/* A C expression for the cost of moving data from a register in class FROM to
   one in class TO.  The classes are expressed using the enumeration values
   such as `GENERAL_REGS'.  A value of 4 is the default; other values are
   interpreted relative to that.

   It is not required that the cost always equal 2 when FROM is the same as TO;
   on some machines it is expensive to move between registers if they are not
   general registers.

   If reload sees an insn consisting of a single `set' between two hard
   registers, and if `REGISTER_MOVE_COST' applied to their classes returns a
   value of 2, reload does not check to ensure that the constraints of the insn
   are met.  Setting a cost of other than 2 will allow reload to verify that
   the constraints are met.  You should do this if the `movM' pattern's
   constraints do not allow such copying.  */
#define REGISTER_MOVE_COST(MODE, FROM, TO) frv_register_move_cost (FROM, TO)

/* A C expression for the cost of moving data of mode M between a register and
   memory.  A value of 2 is the default; this cost is relative to those in
   `REGISTER_MOVE_COST'.

   If moving between registers and memory is more expensive than between two
   registers, you should define this macro to express the relative cost.  */
#define MEMORY_MOVE_COST(M,C,I) 4

/* A C expression for the cost of a branch instruction.  A value of 1 is the
   default; other values are interpreted relative to that.  */

/* Here are additional macros which do not specify precise relative costs, but
   only that certain actions are more expensive than GNU CC would ordinarily
   expect.  */

/* We used to default the branch cost to 2, but I changed it to 1, to avoid
   generating SCC instructions and or/and-ing them together, and then doing the
   branch on the result, which collectively generate much worse code.  */
#ifndef DEFAULT_BRANCH_COST
#define DEFAULT_BRANCH_COST 1
#endif

#define BRANCH_COST frv_branch_cost_int

/* Define this macro as a C expression which is nonzero if accessing less than
   a word of memory (i.e. a `char' or a `short') is no faster than accessing a
   word of memory, i.e., if such access require more than one instruction or if
   there is no difference in cost between byte and (aligned) word loads.

   When this macro is not defined, the compiler will access a field by finding
   the smallest containing object; when it is defined, a fullword load will be
   used if alignment permits.  Unless bytes accesses are faster than word
   accesses, using word accesses is preferable since it may eliminate
   subsequent memory access if subsequent accesses occur to other fields in the
   same word of the structure, but to different bytes.  */
#define SLOW_BYTE_ACCESS 1

/* Define this macro if it is as good or better to call a constant function
   address than to call an address kept in a register.  */
#define NO_FUNCTION_CSE

/* Define this macro if it is as good or better for a function to call itself
   with an explicit address than to call an address kept in a register.  */
#define NO_RECURSIVE_FUNCTION_CSE


/* Dividing the output into sections.  */

/* A C expression whose value is a string containing the assembler operation
   that should precede instructions and read-only data.  Normally `".text"' is
   right.  */
#define TEXT_SECTION_ASM_OP "\t.text"

/* A C expression whose value is a string containing the assembler operation to
   identify the following data as writable initialized data.  Normally
   `".data"' is right.  */
#define DATA_SECTION_ASM_OP "\t.data"

/* If defined, a C expression whose value is a string containing the
   assembler operation to identify the following data as
   uninitialized global data.  If not defined, and neither
   `ASM_OUTPUT_BSS' nor `ASM_OUTPUT_ALIGNED_BSS' are defined,
   uninitialized global data will be output in the data section if
   `-fno-common' is passed, otherwise `ASM_OUTPUT_COMMON' will be
   used.  */
#define BSS_SECTION_ASM_OP "\t.section .bss,\"aw\""

/* Short Data Support */
#define SDATA_SECTION_ASM_OP	"\t.section .sdata,\"aw\""
#define SBSS_SECTION_ASM_OP	"\t.section .sbss,\"aw\""

/* On svr4, we *do* have support for the .init and .fini sections, and we
   can put stuff in there to be executed before and after `main'.  We let
   crtstuff.c and other files know this by defining the following symbols.
   The definitions say how to change sections to the .init and .fini
   sections.  This is the same for all known svr4 assemblers.

   The standard System V.4 macros will work, but they look ugly in the
   assembly output, so redefine them.  */

#undef	INIT_SECTION_ASM_OP
#undef	FINI_SECTION_ASM_OP
#define INIT_SECTION_ASM_OP	"\t.section .init,\"ax\""
#define FINI_SECTION_ASM_OP	"\t.section .fini,\"ax\""

/* A C expression whose value is a string containing the assembler operation to
   switch to the fixup section that records all initialized pointers in a -fpic
   program so they can be changed program startup time if the program is loaded
   at a different address than linked for.  */
#define FIXUP_SECTION_ASM_OP	"\t.section .rofixup,\"a\""

/* A list of names for sections other than the standard two, which are
   `in_text' and `in_data'.  You need not define this macro
   on a system with no other sections (that GCC needs to use).  */
#undef  EXTRA_SECTIONS
#define EXTRA_SECTIONS in_sdata, in_sbss, in_const, in_fixup

/* One or more functions to be defined in "varasm.c".  These
   functions should do jobs analogous to those of `text_section' and
   `data_section', for your additional sections.  Do not define this
   macro if you do not define `EXTRA_SECTIONS'.  */
#undef  EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS                                         \
SDATA_SECTION_FUNCTION                                                  \
SBSS_SECTION_FUNCTION							\
FIXUP_SECTION_FUNCTION


#define SDATA_SECTION_FUNCTION						\
void									\
sdata_section ()							\
{									\
  if (in_section != in_sdata)						\
    {									\
      fprintf (asm_out_file, "%s\n", SDATA_SECTION_ASM_OP);		\
      in_section = in_sdata;						\
    }									\
}									\

#define SBSS_SECTION_FUNCTION						\
void									\
sbss_section ()								\
{									\
  if (in_section != in_sbss)						\
    {									\
      fprintf (asm_out_file, "%s\n", SBSS_SECTION_ASM_OP);		\
      in_section = in_sbss;						\
    }									\
}									\

#define FIXUP_SECTION_FUNCTION						\
void									\
fixup_section ()							\
{									\
  if (in_section != in_fixup)						\
    {									\
      fprintf (asm_out_file, "%s\n", FIXUP_SECTION_ASM_OP);		\
      in_section = in_fixup;						\
    }									\
}									\

#define SDATA_FLAG_CHAR '@'

#define SDATA_NAME_P(NAME) (*(NAME) == SDATA_FLAG_CHAR)

/* Position Independent Code.  */

/* A C expression that is nonzero if X is a legitimate immediate operand on the
   target machine when generating position independent code.  You can assume
   that X satisfies `CONSTANT_P', so you need not check this.  You can also
   assume FLAG_PIC is true, so you need not check it either.  You need not
   define this macro if all constants (including `SYMBOL_REF') can be immediate
   operands when generating position independent code.  */
#define LEGITIMATE_PIC_OPERAND_P(X)					\
  (   GET_CODE (X) == CONST_INT						\
   || GET_CODE (X) == CONST_DOUBLE					\
   || (GET_CODE (X) == HIGH && GET_CODE (XEXP (X, 0)) == CONST_INT)	\
   || GET_CODE (X) == CONSTANT_P_RTX)


/* The Overall Framework of an Assembler File.  */

/* A C string constant describing how to begin a comment in the target
   assembler language.  The compiler assumes that the comment will end at the
   end of the line.  */
#define ASM_COMMENT_START ";"

/* A C string constant for text to be output before each `asm' statement or
   group of consecutive ones.  Normally this is `"#APP"', which is a comment
   that has no effect on most assemblers but tells the GNU assembler that it
   must check the lines that follow for all valid assembler constructs.  */
#define ASM_APP_ON "#APP\n"

/* A C string constant for text to be output after each `asm' statement or
   group of consecutive ones.  Normally this is `"#NO_APP"', which tells the
   GNU assembler to resume making the time-saving assumptions that are valid
   for ordinary compiler output.  */
#define ASM_APP_OFF "#NO_APP\n"


/* Output of Data.  */

/* This is how to output a label to dwarf/dwarf2.  */
#define ASM_OUTPUT_DWARF_ADDR(STREAM, LABEL)				\
do {									\
  fprintf (STREAM, "\t.picptr\t");					\
  assemble_name (STREAM, LABEL);					\
} while (0)

/* Whether to emit the gas specific dwarf2 line number support.  */
#define DWARF2_ASM_LINE_DEBUG_INFO (TARGET_DEBUG_LOC)

/* Output of Uninitialized Variables.  */

/* A C statement (sans semicolon) to output to the stdio stream STREAM the
   assembler definition of a local-common-label named NAME whose size is SIZE
   bytes.  The variable ROUNDED is the size rounded up to whatever alignment
   the caller wants.

   Use the expression `assemble_name (STREAM, NAME)' to output the name itself;
   before and after that, output the additional assembler syntax for defining
   the name, and a newline.

   This macro controls how the assembler definitions of uninitialized static
   variables are output.  */
#undef ASM_OUTPUT_LOCAL

/* Like `ASM_OUTPUT_LOCAL' except takes the required alignment as a separate,
   explicit argument.  If you define this macro, it is used in place of
   `ASM_OUTPUT_LOCAL', and gives you more flexibility in handling the required
   alignment of the variable.  The alignment is specified as the number of
   bits.

   Defined in svr4.h.  */
#undef ASM_OUTPUT_ALIGNED_LOCAL

/* This is for final.c, because it is used by ASM_DECLARE_OBJECT_NAME.  */
extern int size_directive_output;

/* Like `ASM_OUTPUT_ALIGNED_LOCAL' except that it takes an additional
   parameter - the DECL of variable to be output, if there is one.
   This macro can be called with DECL == NULL_TREE.  If you define
   this macro, it is used in place of `ASM_OUTPUT_LOCAL' and
   `ASM_OUTPUT_ALIGNED_LOCAL', and gives you more flexibility in
   handling the destination of the variable.  */
#undef ASM_OUTPUT_ALIGNED_DECL_LOCAL
#define ASM_OUTPUT_ALIGNED_DECL_LOCAL(STREAM, DECL, NAME, SIZE, ALIGN)	\
do {                                                                   	\
  if (SDATA_NAME_P (NAME))                                             	\
    sbss_section ();                                                 	\
  else                                                                 	\
     bss_section ();                                                  	\
  ASM_OUTPUT_ALIGN (STREAM, floor_log2 ((ALIGN) / BITS_PER_UNIT));     	\
  ASM_DECLARE_OBJECT_NAME (STREAM, NAME, DECL);                        	\
  ASM_OUTPUT_SKIP (STREAM, (SIZE) ? (SIZE) : 1);                       	\
} while (0)


/* Output and Generation of Labels.  */

/* A C statement (sans semicolon) to output to the stdio stream STREAM the
   assembler definition of a label named NAME.  Use the expression
   `assemble_name (STREAM, NAME)' to output the name itself; before and after
   that, output the additional assembler syntax for defining the name, and a
   newline.  */
#define ASM_OUTPUT_LABEL(STREAM, NAME)					\
do {									\
  assemble_name (STREAM, NAME);						\
  fputs (":\n", STREAM);						\
} while (0)

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.globl "

/* A C statement (sans semicolon) to output to the stdio stream STREAM a
   reference in assembler syntax to a label named NAME.  This should add `_' to
   the front of the name, if that is customary on your operating system, as it
   is in most Berkeley Unix systems.  This macro is used in `assemble_name'.  */
#undef ASM_OUTPUT_LABELREF
#define ASM_OUTPUT_LABELREF(STREAM, NAME)				\
do {									\
  const char *_name = (NAME);						\
  while (*_name == '*' || *_name == SDATA_FLAG_CHAR)			\
    _name++;								\
  asm_fprintf (STREAM, "%U%s", _name);					\
} while (0)

/* A C statement to store into the string STRING a label whose name is made
   from the string PREFIX and the number NUM.

   This string, when output subsequently by `assemble_name', should produce the
   output that `ASM_OUTPUT_INTERNAL_LABEL' would produce with the same PREFIX
   and NUM.

   If the string begins with `*', then `assemble_name' will output the rest of
   the string unchanged.  It is often convenient for
   `ASM_GENERATE_INTERNAL_LABEL' to use `*' in this way.  If the string doesn't
   start with `*', then `ASM_OUTPUT_LABELREF' gets to output the string, and
   may change it.  (Of course, `ASM_OUTPUT_LABELREF' is also part of your
   machine description, so you should know what it does on your machine.)

   Defined in svr4.h.  */
#undef ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(LABEL, PREFIX, NUM)			\
do {									\
  sprintf (LABEL, "*.%s%ld", PREFIX, (long)NUM);			\
} while (0)

/* A C expression to assign to OUTVAR (which is a variable of type `char *') a
   newly allocated string made from the string NAME and the number NUMBER, with
   some suitable punctuation added.  Use `alloca' to get space for the string.

   The string will be used as an argument to `ASM_OUTPUT_LABELREF' to produce
   an assembler label for an internal static variable whose name is NAME.
   Therefore, the string must be such as to result in valid assembler code.
   The argument NUMBER is different each time this macro is executed; it
   prevents conflicts between similarly-named internal static variables in
   different scopes.

   Ideally this string should not be a valid C identifier, to prevent any
   conflict with the user's own symbols.  Most assemblers allow periods or
   percent signs in assembler symbols; putting at least one of these between
   the name and the number will suffice.  */
#define ASM_FORMAT_PRIVATE_NAME(OUTVAR, NAME, NUMBER)			\
do {									\
  (OUTVAR) = (char *) alloca (strlen ((NAME)) + 12);			\
  sprintf ((OUTVAR), "%s.%ld", (NAME), (long)(NUMBER));			\
} while (0)


/* Macros Controlling Initialization Routines.  */

/* If defined, a C string constant for the assembler operation to identify the
   following data as initialization code.  If not defined, GNU CC will assume
   such a section does not exist.  When you are using special sections for
   initialization and termination functions, this macro also controls how
   `crtstuff.c' and `libgcc2.c' arrange to run the initialization functions.

   Defined in svr4.h.  */
#undef INIT_SECTION_ASM_OP

/* If defined, `main' will call `__main' despite the presence of
   `INIT_SECTION_ASM_OP'.  This macro should be defined for systems where the
   init section is not actually run automatically, but is still useful for
   collecting the lists of constructors and destructors.  */
#define INVOKE__main

/* Output appropriate code tp call a static constructor.  */
#undef  ASM_OUTPUT_CONSTRUCTOR
#define ASM_OUTPUT_CONSTRUCTOR(STREAM,NAME)				\
do {									\
  ctors_section ();							\
  fprintf (STREAM, "\t.picptr\t");					\
  assemble_name (STREAM, NAME);						\
  fprintf (STREAM, "\n");							\
} while (0)

/* Output appropriate code tp call a static destructor.  */
#undef  ASM_OUTPUT_DESTRUCTOR
#define ASM_OUTPUT_DESTRUCTOR(STREAM,NAME)				\
do {									\
  dtors_section ();							\
  fprintf (STREAM, "\t.picptr\t");					\
  assemble_name (STREAM, NAME);						\
  fprintf (STREAM, "\n");							\
} while (0)


/* Output of Assembler Instructions.  */

/* A C initializer containing the assembler's names for the machine registers,
   each one as a C string constant.  This is what translates register numbers
   in the compiler into assembler language.  */
#define REGISTER_NAMES							\
{									\
 "gr0",  "sp",   "fp",   "gr3",  "gr4",  "gr5",  "gr6",  "gr7",		\
  "gr8",  "gr9",  "gr10", "gr11", "gr12", "gr13", "gr14", "gr15",	\
  "gr16", "gr17", "gr18", "gr19", "gr20", "gr21", "gr22", "gr23",	\
  "gr24", "gr25", "gr26", "gr27", "gr28", "gr29", "gr30", "gr31",	\
  "gr32", "gr33", "gr34", "gr35", "gr36", "gr37", "gr38", "gr39",	\
  "gr40", "gr41", "gr42", "gr43", "gr44", "gr45", "gr46", "gr47",	\
  "gr48", "gr49", "gr50", "gr51", "gr52", "gr53", "gr54", "gr55",	\
  "gr56", "gr57", "gr58", "gr59", "gr60", "gr61", "gr62", "gr63",	\
									\
  "fr0",  "fr1",  "fr2",  "fr3",  "fr4",  "fr5",  "fr6",  "fr7",	\
  "fr8",  "fr9",  "fr10", "fr11", "fr12", "fr13", "fr14", "fr15",	\
  "fr16", "fr17", "fr18", "fr19", "fr20", "fr21", "fr22", "fr23",	\
  "fr24", "fr25", "fr26", "fr27", "fr28", "fr29", "fr30", "fr31",	\
  "fr32", "fr33", "fr34", "fr35", "fr36", "fr37", "fr38", "fr39",	\
  "fr40", "fr41", "fr42", "fr43", "fr44", "fr45", "fr46", "fr47",	\
  "fr48", "fr49", "fr50", "fr51", "fr52", "fr53", "fr54", "fr55",	\
  "fr56", "fr57", "fr58", "fr59", "fr60", "fr61", "fr62", "fr63",	\
									\
  "fcc0", "fcc1", "fcc2", "fcc3", "icc0", "icc1", "icc2", "icc3",	\
  "cc0",  "cc1",  "cc2",  "cc3",  "cc4",  "cc5",  "cc6",  "cc7",	\
  "acc0", "acc1", "acc2", "acc3", "acc4", "acc5", "acc6", "acc7",	\
  "accg0","accg1","accg2","accg3","accg4","accg5","accg6","accg7",	\
  "ap",   "lr",   "lcr"							\
}

/* Define this macro if you are using an unusual assembler that
   requires different names for the machine instructions.

   The definition is a C statement or statements which output an
   assembler instruction opcode to the stdio stream STREAM.  The
   macro-operand PTR is a variable of type `char *' which points to
   the opcode name in its "internal" form--the form that is written
   in the machine description.  The definition should output the
   opcode name to STREAM, performing any translation you desire, and
   increment the variable PTR to point at the end of the opcode so
   that it will not be output twice.

   In fact, your macro definition may process less than the entire
   opcode name, or more than the opcode name; but if you want to
   process text that includes `%'-sequences to substitute operands,
   you must take care of the substitution yourself.  Just be sure to
   increment PTR over whatever text should not be output normally.

   If you need to look at the operand values, they can be found as the
   elements of `recog_operand'.

   If the macro definition does nothing, the instruction is output in
   the usual way.  */

#define ASM_OUTPUT_OPCODE(STREAM, PTR)\
   (PTR) = frv_asm_output_opcode (STREAM, PTR)

/* If defined, a C statement to be executed just prior to the output
   of assembler code for INSN, to modify the extracted operands so
   they will be output differently.

   Here the argument OPVEC is the vector containing the operands
   extracted from INSN, and NOPERANDS is the number of elements of
   the vector which contain meaningful data for this insn.  The
   contents of this vector are what will be used to convert the insn
   template into assembler code, so you can change the assembler
   output by changing the contents of the vector.

   This macro is useful when various assembler syntaxes share a single
   file of instruction patterns; by defining this macro differently,
   you can cause a large class of instructions to be output
   differently (such as with rearranged operands).  Naturally,
   variations in assembler syntax affecting individual insn patterns
   ought to be handled by writing conditional output routines in
   those patterns.

   If this macro is not defined, it is equivalent to a null statement.  */

#define FINAL_PRESCAN_INSN(INSN, OPVEC, NOPERANDS)\
  frv_final_prescan_insn (INSN, OPVEC, NOPERANDS)


/* A C compound statement to output to stdio stream STREAM the assembler syntax
   for an instruction operand X.  X is an RTL expression.

   CODE is a value that can be used to specify one of several ways of printing
   the operand.  It is used when identical operands must be printed differently
   depending on the context.  CODE comes from the `%' specification that was
   used to request printing of the operand.  If the specification was just
   `%DIGIT' then CODE is 0; if the specification was `%LTR DIGIT' then CODE is
   the ASCII code for LTR.

   If X is a register, this macro should print the register's name.  The names
   can be found in an array `reg_names' whose type is `char *[]'.  `reg_names'
   is initialized from `REGISTER_NAMES'.

   When the machine description has a specification `%PUNCT' (a `%' followed by
   a punctuation character), this macro is called with a null pointer for X and
   the punctuation character for CODE.  */
#define PRINT_OPERAND(STREAM, X, CODE) frv_print_operand (STREAM, X, CODE)

/* A C expression which evaluates to true if CODE is a valid punctuation
   character for use in the `PRINT_OPERAND' macro.  If
   `PRINT_OPERAND_PUNCT_VALID_P' is not defined, it means that no punctuation
   characters (except for the standard one, `%') are used in this way.  */
/* . == gr0
   # == hint operand -- always zero for now
   @ == small data base register (gr16)
   ~ == pic register (gr17)
   * == temporary integer CCR register (cr3)
   & == temporary integer ICC register (icc3)  */
#define PRINT_OPERAND_PUNCT_VALID_P(CODE)				\
((CODE) == '.' || (CODE) == '#' || (CODE) == SDATA_FLAG_CHAR || (CODE) == '~'	\
 || (CODE) == '*' || (CODE) == '&')

/* A C compound statement to output to stdio stream STREAM the assembler syntax
   for an instruction operand that is a memory reference whose address is X.  X
   is an RTL expression.

   On some machines, the syntax for a symbolic address depends on the section
   that the address refers to.  On these machines, define the macro
   `ENCODE_SECTION_INFO' to store the information into the `symbol_ref', and
   then check for it here.

   This declaration must be present.  */
#define PRINT_OPERAND_ADDRESS(STREAM, X) frv_print_operand_address (STREAM, X)

/* If defined, C string expressions to be used for the `%R', `%L', `%U', and
   `%I' options of `asm_fprintf' (see `final.c').  These are useful when a
   single `md' file must support multiple assembler formats.  In that case, the
   various `tm.h' files can define these macros differently.

   USER_LABEL_PREFIX is defined in svr4.h.  */
#undef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX ""
#define REGISTER_PREFIX ""
#define LOCAL_LABEL_PREFIX "."
#define IMMEDIATE_PREFIX "#"


/* Output of dispatch tables.  */

/* This macro should be provided on machines where the addresses in a dispatch
   table are relative to the table's own address.

   The definition should be a C statement to output to the stdio stream STREAM
   an assembler pseudo-instruction to generate a difference between two labels.
   VALUE and REL are the numbers of two internal labels.  The definitions of
   these labels are output using `ASM_OUTPUT_INTERNAL_LABEL', and they must be
   printed in the same way here.  For example,

        fprintf (STREAM, "\t.word L%d-L%d\n", VALUE, REL)  */
#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM, BODY, VALUE, REL) \
fprintf (STREAM, "\t.word .L%d-.L%d\n", VALUE, REL)

/* This macro should be provided on machines where the addresses in a dispatch
   table are absolute.

   The definition should be a C statement to output to the stdio stream STREAM
   an assembler pseudo-instruction to generate a reference to a label.  VALUE
   is the number of an internal label whose definition is output using
   `ASM_OUTPUT_INTERNAL_LABEL'.  For example,

        fprintf (STREAM, "\t.word L%d\n", VALUE)  */
#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM, VALUE) \
fprintf (STREAM, "\t.word .L%d\n", VALUE)

/* Define this if the label before a jump-table needs to be output specially.
   The first three arguments are the same as for `ASM_OUTPUT_INTERNAL_LABEL';
   the fourth argument is the jump-table which follows (a `jump_insn'
   containing an `addr_vec' or `addr_diff_vec').

   This feature is used on system V to output a `swbeg' statement for the
   table.

   If this macro is not defined, these labels are output with
   `ASM_OUTPUT_INTERNAL_LABEL'.

   Defined in svr4.h.  */
/* When generating embedded PIC or mips16 code we want to put the jump
   table in the .text section.  In all other cases, we want to put the
   jump table in the .rdata section.  Unfortunately, we can't use
   JUMP_TABLES_IN_TEXT_SECTION, because it is not conditional.
   Instead, we use ASM_OUTPUT_CASE_LABEL to switch back to the .text
   section if appropriate.  */

#undef  ASM_OUTPUT_CASE_LABEL
#define ASM_OUTPUT_CASE_LABEL(STREAM, PREFIX, NUM, TABLE)               \
do {                                                                    \
  if (flag_pic)                                                         \
    function_section (current_function_decl);                           \
  ASM_OUTPUT_INTERNAL_LABEL (STREAM, PREFIX, NUM);                      \
} while (0)

/* Define this to determine whether case statement labels are relative to
   the start of the case statement or not.  */

#define CASE_VECTOR_PC_RELATIVE (flag_pic)


/* Assembler Commands for Exception Regions.  */

/* Define this macro to 0 if your target supports DWARF 2 frame unwind
   information, but it does not yet work with exception handling.  Otherwise,
   if your target supports this information (if it defines
   `INCOMING_RETURN_ADDR_RTX' and either `UNALIGNED_INT_ASM_OP' or
   `OBJECT_FORMAT_ELF'), GCC will provide a default definition of 1.

   If this macro is defined to 1, the DWARF 2 unwinder will be the default
   exception handling mechanism; otherwise, setjmp/longjmp will be used by
   default.

   If this macro is defined to anything, the DWARF 2 unwinder will be used
   instead of inline unwinders and __unwind_function in the non-setjmp case.  */
#define DWARF2_UNWIND_INFO 1

#define DWARF_FRAME_RETURN_COLUMN DWARF_FRAME_REGNUM (LR_REGNO)

/* Assembler Commands for Alignment.  */

/* A C statement to output to the stdio stream STREAM an assembler instruction
   to advance the location counter by NBYTES bytes.  Those bytes should be zero
   when loaded.  NBYTES will be a C expression of type `int'.

   Defined in svr4.h.  */
#undef  ASM_OUTPUT_SKIP
#define ASM_OUTPUT_SKIP(STREAM, NBYTES) \
  fprintf (STREAM, "\t.zero\t%u\n", (NBYTES))

/* A C statement to output to the stdio stream STREAM an assembler command to
   advance the location counter to a multiple of 2 to the POWER bytes.  POWER
   will be a C expression of type `int'.  */
#define ASM_OUTPUT_ALIGN(STREAM, POWER) \
  fprintf ((STREAM), "\t.p2align %d\n", (POWER))


/* Macros Affecting all Debug Formats.  */

/* A C expression that returns the DBX register number for the compiler
   register number REGNO.  In simple cases, the value of this expression may be
   REGNO itself.  But sometimes there are some registers that the compiler
   knows about and DBX does not, or vice versa.  In such cases, some register
   may need to have one number in the compiler and another for DBX.

   If two registers have consecutive numbers inside GNU CC, and they can be
   used as a pair to hold a multiword value, then they *must* have consecutive
   numbers after renumbering with `DBX_REGISTER_NUMBER'.  Otherwise, debuggers
   will be unable to access such a pair, because they expect register pairs to
   be consecutive in their own numbering scheme.

   If you find yourself defining `DBX_REGISTER_NUMBER' in way that does not
   preserve register pairs, then what you must do instead is redefine the
   actual register numbering scheme.

   This declaration is required.  */
#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/* A C expression that returns the type of debugging output GNU CC produces
   when the user specifies `-g' or `-ggdb'.  Define this if you have arranged
   for GNU CC to support more than one format of debugging output.  Currently,
   the allowable values are `DBX_DEBUG', `SDB_DEBUG', `DWARF_DEBUG',
   `DWARF2_DEBUG', and `XCOFF_DEBUG'.

   The value of this macro only affects the default debugging output; the user
   can always get a specific type of output by using `-gstabs', `-gcoff',
   `-gdwarf-1', `-gdwarf-2', or `-gxcoff'.

   Defined in svr4.h.  */
#undef  PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

/* Miscellaneous Parameters.  */

/* Define this if you have defined special-purpose predicates in the file
   `MACHINE.c'.  This macro is called within an initializer of an array of
   structures.  The first field in the structure is the name of a predicate and
   the second field is an array of rtl codes.  For each predicate, list all rtl
   codes that can be in expressions matched by the predicate.  The list should
   have a trailing comma.  Here is an example of two entries in the list for a
   typical RISC machine:

        #define PREDICATE_CODES \
          {"gen_reg_rtx_operand", {SUBREG, REG}},  \
          {"reg_or_short_cint_operand", {SUBREG, REG, CONST_INT}},

   Defining this macro does not affect the generated code (however, incorrect
   definitions that omit an rtl code that may be matched by the predicate can
   cause the compiler to malfunction).  Instead, it allows the table built by
   `genrecog' to be more compact and efficient, thus speeding up the compiler.
   The most important predicates to include in the list specified by this macro
   are thoses used in the most insn patterns.  */
#define PREDICATE_CODES							\
  { "integer_register_operand",		{ REG, SUBREG }},		\
  { "frv_load_operand",			{ REG, SUBREG, MEM }},		\
  { "gpr_no_subreg_operand",		{ REG }},			\
  { "gpr_or_fpr_operand",		{ REG, SUBREG }},		\
  { "gpr_or_int12_operand",		{ REG, SUBREG, CONST_INT }},	\
  { "gpr_fpr_or_int12_operand",		{ REG, SUBREG, CONST_INT }},	\
  { "gpr_or_int10_operand",		{ REG, SUBREG, CONST_INT }},	\
  { "gpr_or_int_operand",		{ REG, SUBREG, CONST_INT }},	\
  { "move_source_operand",		{ REG, SUBREG, CONST_INT, MEM,	\
					  CONST_DOUBLE, CONST,		\
					  SYMBOL_REF, LABEL_REF }},	\
  { "move_destination_operand",		{ REG, SUBREG, MEM }},		\
  { "condexec_source_operand",		{ REG, SUBREG, CONST_INT, MEM,	\
					  CONST_DOUBLE }},		\
  { "condexec_dest_operand",		{ REG, SUBREG, MEM }},		\
  { "reg_or_0_operand",			{ REG, SUBREG, CONST_INT }},	\
  { "lr_operand",			{ REG }},			\
  { "gpr_or_memory_operand",		{ REG, SUBREG, MEM }},		\
  { "fpr_or_memory_operand",		{ REG, SUBREG, MEM }},		\
  { "int12_operand",			{ CONST_INT }},			\
  { "int_2word_operand",		{ CONST_INT, CONST_DOUBLE,	\
					  SYMBOL_REF, LABEL_REF, CONST }}, \
  { "pic_register_operand",		{ REG }},			\
  { "pic_symbolic_operand",		{ SYMBOL_REF, LABEL_REF, CONST }}, \
  { "small_data_register_operand",	{ REG }},			\
  { "small_data_symbolic_operand",	{ SYMBOL_REF, CONST }},		\
  { "icc_operand",			{ REG }},			\
  { "fcc_operand",			{ REG }},			\
  { "cc_operand",			{ REG }},			\
  { "icr_operand",			{ REG }},			\
  { "fcr_operand",			{ REG }},			\
  { "cr_operand",			{ REG }},			\
  { "fpr_operand",			{ REG, SUBREG }},		\
  { "even_reg_operand",			{ REG, SUBREG }},		\
  { "odd_reg_operand",			{ REG, SUBREG }},		\
  { "even_gpr_operand",			{ REG, SUBREG }},		\
  { "odd_gpr_operand",			{ REG, SUBREG }},		\
  { "quad_fpr_operand",			{ REG, SUBREG }},		\
  { "even_fpr_operand",			{ REG, SUBREG }},		\
  { "odd_fpr_operand",			{ REG, SUBREG }},		\
  { "dbl_memory_one_insn_operand",	{ MEM }},			\
  { "dbl_memory_two_insn_operand",	{ MEM }},			\
  { "call_operand",			{ REG, SUBREG, PLUS, CONST_INT,	\
					  SYMBOL_REF, LABEL_REF, CONST }}, \
  { "upper_int16_operand",		{ CONST_INT }},			\
  { "uint16_operand",			{ CONST_INT }},			\
  { "relational_operator",		{ EQ, NE, LE, LT, GE, GT,	\
					  LEU, LTU, GEU, GTU }},	\
  { "signed_relational_operator",	{ EQ, NE, LE, LT, GE, GT }},	\
  { "unsigned_relational_operator",	{ LEU, LTU, GEU, GTU }},	\
  { "float_relational_operator",	{ EQ, NE, LE, LT, GE, GT }},	\
  { "ccr_eqne_operator",		{ EQ, NE }},			\
  { "minmax_operator",			{ SMIN, SMAX, UMIN, UMAX }},	\
  { "condexec_si_binary_operator",	{ PLUS, MINUS, AND, IOR, XOR,	\
					  ASHIFT, ASHIFTRT, LSHIFTRT }}, \
  { "condexec_si_divide_operator",	{ DIV, UDIV }},			\
  { "condexec_si_unary_operator",	{ NOT, NEG }},			\
  { "condexec_sf_binary_operator",	{ PLUS, MINUS, MULT, DIV }},	\
  { "condexec_sf_unary_operator",	{ ABS, NEG, SQRT }},		\
  { "intop_compare_operator",		{ PLUS, MINUS, AND, IOR, XOR,	\
					  ASHIFT, ASHIFTRT, LSHIFTRT }}, \
  { "condexec_intop_cmp_operator",	{ PLUS, MINUS, AND, IOR, XOR,	\
					  ASHIFT, ASHIFTRT, LSHIFTRT }}, \
  { "fpr_or_int6_operand",		{ REG, SUBREG, CONST_INT }},	\
  { "int6_operand",			{ CONST_INT }},			\
  { "int5_operand",			{ CONST_INT }},			\
  { "uint5_operand",			{ CONST_INT }},			\
  { "uint4_operand",			{ CONST_INT }},			\
  { "uint1_operand",			{ CONST_INT }},			\
  { "acc_operand",			{ REG, SUBREG }},		\
  { "even_acc_operand",			{ REG, SUBREG }},		\
  { "quad_acc_operand",			{ REG, SUBREG }},		\
  { "accg_operand",			{ REG, SUBREG }},

/* An alias for a machine mode name.  This is the machine mode that elements of
   a jump-table should have.  */
#define CASE_VECTOR_MODE SImode

/* Define this macro if operations between registers with integral mode smaller
   than a word are always performed on the entire register.  Most RISC machines
   have this property and most CISC machines do not.  */
#define WORD_REGISTER_OPERATIONS

/* Define this macro to be a C expression indicating when insns that read
   memory in MODE, an integral mode narrower than a word, set the bits outside
   of MODE to be either the sign-extension or the zero-extension of the data
   read.  Return `SIGN_EXTEND' for values of MODE for which the insn
   sign-extends, `ZERO_EXTEND' for which it zero-extends, and `NIL' for other
   modes.

   This macro is not called with MODE non-integral or with a width greater than
   or equal to `BITS_PER_WORD', so you may return any value in this case.  Do
   not define this macro if it would always return `NIL'.  On machines where
   this macro is defined, you will normally define it as the constant
   `SIGN_EXTEND' or `ZERO_EXTEND'.  */
#define LOAD_EXTEND_OP(MODE) SIGN_EXTEND

/* Define if loading short immediate values into registers sign extends.  */
#define SHORT_IMMEDIATES_SIGN_EXTEND

/* The maximum number of bytes that a single instruction can move quickly from
   memory to memory.  */
#define MOVE_MAX 8

/* A C expression which is nonzero if on this machine it is safe to "convert"
   an integer of INPREC bits to one of OUTPREC bits (where OUTPREC is smaller
   than INPREC) by merely operating on it as if it had only OUTPREC bits.

   On many machines, this expression can be 1.

   When `TRULY_NOOP_TRUNCATION' returns 1 for a pair of sizes for modes for
   which `MODES_TIEABLE_P' is 0, suboptimal code can result.  If this is the
   case, making `TRULY_NOOP_TRUNCATION' return 0 in such cases may improve
   things.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* An alias for the machine mode for pointers.  On most machines, define this
   to be the integer mode corresponding to the width of a hardware pointer;
   `SImode' on 32-bit machine or `DImode' on 64-bit machines.  On some machines
   you must define this to be one of the partial integer modes, such as
   `PSImode'.

   The width of `Pmode' must be at least as large as the value of
   `POINTER_SIZE'.  If it is not equal, you must define the macro
   `POINTERS_EXTEND_UNSIGNED' to specify how pointers are extended to `Pmode'.  */
#define Pmode SImode

/* An alias for the machine mode used for memory references to functions being
   called, in `call' RTL expressions.  On most machines this should be
   `QImode'.  */
#define FUNCTION_MODE QImode

/* Define this macro to handle System V style pragmas: #pragma pack and
   #pragma weak.  Note, #pragma weak will only be supported if SUPPORT_WEAK is
   defined.

   Defined in svr4.h.  */
#define HANDLE_SYSV_PRAGMA 1

/* A C expression for the maximum number of instructions to execute via
   conditional execution instructions instead of a branch.  A value of
   BRANCH_COST+1 is the default if the machine does not use
   cc0, and 1 if it does use cc0.  */
#define MAX_CONDITIONAL_EXECUTE frv_condexec_insns

/* Default value of MAX_CONDITIONAL_EXECUTE if no -mcond-exec-insns= */
#define DEFAULT_CONDEXEC_INSNS 8

/* A C expression to modify the code described by the conditional if
   information CE_INFO, possibly updating the tests in TRUE_EXPR, and
   FALSE_EXPR for converting if-then and if-then-else code to conditional
   instructions.  Set either TRUE_EXPR or FALSE_EXPR to a null pointer if the
   tests cannot be converted.  */
#define IFCVT_MODIFY_TESTS(CE_INFO, TRUE_EXPR, FALSE_EXPR)		\
frv_ifcvt_modify_tests (CE_INFO, &TRUE_EXPR, &FALSE_EXPR)

/* A C expression to modify the code described by the conditional if
   information CE_INFO, for the basic block BB, possibly updating the tests in
   TRUE_EXPR, and FALSE_EXPR for converting the && and || parts of if-then or
   if-then-else code to conditional instructions.  OLD_TRUE and OLD_FALSE are
   the previous tests.  Set either TRUE_EXPR or FALSE_EXPR to a null pointer if
   the tests cannot be converted.  */
#define IFCVT_MODIFY_MULTIPLE_TESTS(CE_INFO, BB, TRUE_EXPR, FALSE_EXPR) \
frv_ifcvt_modify_multiple_tests (CE_INFO, BB, &TRUE_EXPR, &FALSE_EXPR)

/* A C expression to modify the code described by the conditional if
   information CE_INFO with the new PATTERN in INSN.  If PATTERN is a null
   pointer after the IFCVT_MODIFY_INSN macro executes, it is assumed that that
   insn cannot be converted to be executed conditionally.  */
#define IFCVT_MODIFY_INSN(CE_INFO, PATTERN, INSN) \
(PATTERN) = frv_ifcvt_modify_insn (CE_INFO, PATTERN, INSN)

/* A C expression to perform any final machine dependent modifications in
   converting code to conditional execution in the code described by the
   conditional if information CE_INFO.  */
#define IFCVT_MODIFY_FINAL(CE_INFO) frv_ifcvt_modify_final (CE_INFO)

/* A C expression to cancel any machine dependent modifications in converting
   code to conditional execution in the code described by the conditional if
   information CE_INFO.  */
#define IFCVT_MODIFY_CANCEL(CE_INFO) frv_ifcvt_modify_cancel (CE_INFO)

/* Initialize the extra fields provided by IFCVT_EXTRA_FIELDS.  */
#define IFCVT_INIT_EXTRA_FIELDS(CE_INFO) frv_ifcvt_init_extra_fields (CE_INFO)

/* Indicate how many instructions can be issued at the same time.  */
#define ISSUE_RATE							\
(! TARGET_PACK ? 1							\
 : (frv_cpu_type == FRV_CPU_GENERIC					\
    || frv_cpu_type == FRV_CPU_FR500					\
    || frv_cpu_type == FRV_CPU_TOMCAT) ? 4				\
 : frv_cpu_type == FRV_CPU_FR400 ? 2 : 1)

/* Set and clear whether this insn begins a VLIW insn.  */
#define CLEAR_VLIW_START(INSN) PUT_MODE (INSN, VOIDmode)
#define SET_VLIW_START(INSN) PUT_MODE (INSN, TImode)

/* The definition of the following macro results in that the 2nd jump
   optimization (after the 2nd insn scheduling) is minimal.  It is
   necessary to define when start cycle marks of insns (TImode is used
   for this) is used for VLIW insn packing.  Some jump optimizations
   make such marks invalid.  These marks are corrected for some
   (minimal) optimizations.  ??? Probably the macro is temporary.
   Final solution could making the 2nd jump optimizations before the
   2nd instruction scheduling or corrections of the marks for all jump
   optimizations.  Although some jump optimizations are actually
   deoptimizations for VLIW (super-scalar) processors.  */

#define MINIMAL_SECOND_JUMP_OPTIMIZATION

/* Return true if parallel operations are expected to be emitted via the
   packing flag.  */
#define PACKING_FLAG_USED_P() \
(optimize && flag_schedule_insns_after_reload && ISSUE_RATE > 1)

/* If the following macro is defined and nonzero and deterministic
   finite state automata are used for pipeline hazard recognition, the
   code making resource-constrained software pipelining is on.  */
#define RCSP_SOFTWARE_PIPELINING 1

/* If the following macro is defined and nonzero and deterministic
   finite state automata are used for pipeline hazard recognition, we
   will try to exchange insns in queue ready to improve the schedule.
   The more macro value, the more tries will be made.  */
#define FIRST_CYCLE_MULTIPASS_SCHEDULING 1

/* The following macro is used only when value of
   FIRST_CYCLE_MULTIPASS_SCHEDULING is nonzero.  The more macro value,
   the more tries will be made to choose better schedule.  If the
   macro value is zero or negative there will be no multi-pass
   scheduling.  */
#define FIRST_CYCLE_MULTIPASS_SCHEDULING_LOOKAHEAD frv_sched_lookahead

/* Return true if a function is ok to be called as a sibcall.  */
#define FUNCTION_OK_FOR_SIBCALL(DECL) 0

enum frv_builtins
{
  FRV_BUILTIN_MAND,
  FRV_BUILTIN_MOR,
  FRV_BUILTIN_MXOR,
  FRV_BUILTIN_MNOT,
  FRV_BUILTIN_MAVEH,
  FRV_BUILTIN_MSATHS,
  FRV_BUILTIN_MSATHU,
  FRV_BUILTIN_MADDHSS,
  FRV_BUILTIN_MADDHUS,
  FRV_BUILTIN_MSUBHSS,
  FRV_BUILTIN_MSUBHUS,
  FRV_BUILTIN_MPACKH,
  FRV_BUILTIN_MQADDHSS,
  FRV_BUILTIN_MQADDHUS,
  FRV_BUILTIN_MQSUBHSS,
  FRV_BUILTIN_MQSUBHUS,
  FRV_BUILTIN_MUNPACKH,
  FRV_BUILTIN_MDPACKH,
  FRV_BUILTIN_MBTOH,
  FRV_BUILTIN_MHTOB,
  FRV_BUILTIN_MCOP1,
  FRV_BUILTIN_MCOP2,
  FRV_BUILTIN_MROTLI,
  FRV_BUILTIN_MROTRI,
  FRV_BUILTIN_MWCUT,
  FRV_BUILTIN_MSLLHI,
  FRV_BUILTIN_MSRLHI,
  FRV_BUILTIN_MSRAHI,
  FRV_BUILTIN_MEXPDHW,
  FRV_BUILTIN_MEXPDHD,
  FRV_BUILTIN_MMULHS,
  FRV_BUILTIN_MMULHU,
  FRV_BUILTIN_MMULXHS,
  FRV_BUILTIN_MMULXHU,
  FRV_BUILTIN_MMACHS,
  FRV_BUILTIN_MMACHU,
  FRV_BUILTIN_MMRDHS,
  FRV_BUILTIN_MMRDHU,
  FRV_BUILTIN_MQMULHS,
  FRV_BUILTIN_MQMULHU,
  FRV_BUILTIN_MQMULXHU,
  FRV_BUILTIN_MQMULXHS,
  FRV_BUILTIN_MQMACHS,
  FRV_BUILTIN_MQMACHU,
  FRV_BUILTIN_MCPXRS,
  FRV_BUILTIN_MCPXRU,
  FRV_BUILTIN_MCPXIS,
  FRV_BUILTIN_MCPXIU,
  FRV_BUILTIN_MQCPXRS,
  FRV_BUILTIN_MQCPXRU,
  FRV_BUILTIN_MQCPXIS,
  FRV_BUILTIN_MQCPXIU,
  FRV_BUILTIN_MCUT,
  FRV_BUILTIN_MCUTSS,
  FRV_BUILTIN_MWTACC,
  FRV_BUILTIN_MWTACCG,
  FRV_BUILTIN_MRDACC,
  FRV_BUILTIN_MRDACCG,
  FRV_BUILTIN_MTRAP,
  FRV_BUILTIN_MCLRACC,
  FRV_BUILTIN_MCLRACCA,
  FRV_BUILTIN_MDUNPACKH,
  FRV_BUILTIN_MBTOHE,
  FRV_BUILTIN_MQXMACHS,
  FRV_BUILTIN_MQXMACXHS,
  FRV_BUILTIN_MQMACXHS,
  FRV_BUILTIN_MADDACCS,
  FRV_BUILTIN_MSUBACCS,
  FRV_BUILTIN_MASACCS,
  FRV_BUILTIN_MDADDACCS,
  FRV_BUILTIN_MDSUBACCS,
  FRV_BUILTIN_MDASACCS,
  FRV_BUILTIN_MABSHS,
  FRV_BUILTIN_MDROTLI,
  FRV_BUILTIN_MCPLHI,
  FRV_BUILTIN_MCPLI,
  FRV_BUILTIN_MDCUTSSI,
  FRV_BUILTIN_MQSATHS,
  FRV_BUILTIN_MHSETLOS,
  FRV_BUILTIN_MHSETLOH,
  FRV_BUILTIN_MHSETHIS,
  FRV_BUILTIN_MHSETHIH,
  FRV_BUILTIN_MHDSETS,
  FRV_BUILTIN_MHDSETH
};

/* Enable prototypes on the call rtl functions.  */
#define MD_CALL_PROTOTYPES 1

extern GTY(()) rtx frv_compare_op0;			/* operand save for */
extern GTY(()) rtx frv_compare_op1;			/* comparison generation */

#endif /* __FRV_H__ */
