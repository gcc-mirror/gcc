/* Definitions of target machine for GNU compiler.  TMS320C[34]x
   Copyright (C) 1994-99, 2000 Free Software Foundation, Inc.

   Contributed by Michael Hayes (m.hayes@elec.canterbury.ac.nz)
              and Herman Ten Brugge (Haj.Ten.Brugge@net.HCC.nl).

   This file is part of GNU CC.

   GNU CC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GNU CC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU CC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* RUN-TIME TARGET SPECIFICATION */

#define C4x   1

/* Name of the c4x assembler */

#define ASM_PROG "c4x-as"

/* Name of the c4x linker */

#define LD_PROG "c4x-ld"

/* Define assembler options.  */

#define ASM_SPEC "\
%{!mcpu=30:%{!mcpu=31:%{!mcpu=32:%{!mcpu=40:%{!mcpu=44:\
%{!m30:%{!m40:-m40}}}}}}} \
%{mcpu=30:-m30} \
%{mcpu=31:-m31} \
%{mcpu=32:-m32} \
%{mcpu=40:-m40} \
%{mcpu=44:-m44} \
%{m30:-m30} \
%{m31:-m31} \
%{m32:-m32} \
%{m40:-m40} \
%{m44:-m44} \
%{mmemparm:-p} %{mregparm:-r} \
%{!mmemparm:%{!mregparm:-r}} \
%{mbig:-b} %{msmall:-s} \
%{!msmall:%{!mbig:-b}}"

/* Define linker options.  */

#define LINK_SPEC "\
%{m30:--architecture c3x} \
%{m31:--architecture c3x} \
%{m32:--architecture c3x} \
%{mcpu=30:--architecture c3x} \
%{mcpu=31:--architecture c3x} \
%{mcpu=32:--architecture c3x}"

/* Define C preprocessor options.  */

#define CPP_SPEC "\
%{!m30:%{!m31:%{!m32:%{!mcpu=30:%{!mcpu=31:%{!mcpu=32:%{!mcpu=40:%{!mcpu=44:\
  %{!m40:%{!m44:-D_TMS320C4x -D_C4x -D_TMS320C40 -D_C40 }}}}}}}}}} \
%{mcpu=30:-D_TMS320C3x -D_C3x -D_TMS320C30 -D_C30 } \
%{m30:-D_TMS320C3x -D_C3x -D_TMS320C30 -D_C30 } \
%{mcpu=31:-D_TMS320C3x -D_C3x -D_TMS320C31 -D_C31 } \
%{m31:-D_TMS320C3x -D_C3x -D_TMS320C31 -D_C31 } \
%{mcpu=32:-D_TMS320C3x -D_C3x -D_TMS320C32 -D_C32 } \
%{m32:-D_TMS320C3x -D_C3x -D_TMS320C32 -D_C32 } \
%{mcpu=40:-D_TMS320C4x -D_C4x -D_TMS320C40 -D_C40 } \
%{m40:-D_TMS320C4x -D_C4x -D_TMS320C40 -D_C40 } \
%{mcpu=44:-D_TMS320C4x -D_C4x -D_TMS320C44 -D_C44 } \
%{m44:-D_TMS320C4x -D_C4x -D_TMS320C44 -D_C44 } \
%{mmemparm:-U_REGPARM }%{mregparm:-D_REGPARM } \
%{!mmemparm:%{!mregparm:-D_REGPARM }} \
%{msmall:-U_BIGMODEL } %{mbig:-D_BIGMODEL } \
%{!msmall:%{!mbig:-D_BIGMODEL }} \
%{finline-functions:-D_INLINE }"

/* Specify the end file to link with.  */

#define ENDFILE_SPEC ""

/* Target compilation option flags.  */

#define SMALL_MEMORY_FLAG   0x0000001 /* small memory model */
#define MPYI_FLAG           0x0000002 /* use 24-bit MPYI for C3x */
#define FAST_FIX_FLAG       0x0000004 /* fast fixing of floats */
#define RPTS_FLAG           0x0000008 /* allow use of RPTS */
#define C3X_FLAG            0x0000010 /* emit C3x code */
#define TI_FLAG             0x0000020 /* be compatible with TI assembler */
#define PARANOID_FLAG       0x0000040 /* be paranoid about DP reg. in ISRs */
#define MEMPARM_FLAG        0x0000080 /* pass arguments on stack */
#define DEVEL_FLAG          0x0000100 /* enable features under development */
#define RPTB_FLAG           0x0000200 /* enable repeat block */
#define BK_FLAG             0x0000400 /* use BK as general register */
#define DB_FLAG             0x0000800 /* use decrement and branch for C3x */
#define DEBUG_FLAG          0x0001000 /* enable debugging of GCC */
#define HOIST_FLAG          0x0002000 /* force constants into registers */
#define LOOP_UNSIGNED_FLAG  0x0004000 /* allow unsigned loop counters */
#define FORCE_FLAG          0x0008000 /* force op0 and op1 to be same */
#define PRESERVE_FLOAT_FLAG 0x0010000 /* save all 40 bits for floats */
#define PARALLEL_PACK_FLAG  0x0020000 /* allow parallel insn packing */
#define PARALLEL_MPY_FLAG   0x0040000 /* allow MPY||ADD, MPY||SUB insns */
#define ALIASES_FLAG	    0x0080000 /* assume mem refs possibly aliased */

#define C30_FLAG            0x0100000 /* emit C30 code */
#define C31_FLAG            0x0200000 /* emit C31 code */
#define C32_FLAG            0x0400000 /* emit C32 code */
#define C40_FLAG            0x1000000 /* emit C40 code */
#define C44_FLAG            0x2000000 /* emit C44 code */

/* Run-time compilation parameters selecting different hardware subsets.

   Macro to define tables used to set the flags.
   This is a list in braces of triplets in braces,
   each pair being { "NAME", VALUE, "DESCRIPTION" }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

#define TARGET_SWITCHES \
{ { "small", SMALL_MEMORY_FLAG, \
    "Small memory model" }, \
  { "big", -SMALL_MEMORY_FLAG, \
    "Big memory model" }, \
  { "mpyi", MPYI_FLAG, \
    "Use MPYI instruction for C3x" }, \
  { "no-mpyi", -MPYI_FLAG, \
    "Do not use MPYI instruction for C3x" }, \
  { "fast-fix", FAST_FIX_FLAG, \
    "Use fast but approximate float to integer conversion" }, \
  { "no-fast-fix", -FAST_FIX_FLAG, \
    "Use slow but accurate float to integer conversion" }, \
  { "rpts", RPTS_FLAG, \
    "Enable use of RTPS instruction" }, \
  { "no-rpts", -RPTS_FLAG, \
    "Disable use of RTPS instruction" }, \
  { "rptb", RPTB_FLAG, \
    "Enable use of RTPB instruction" }, \
  { "no-rptb", -RPTB_FLAG, \
    "Disable use of RTPB instruction" }, \
  { "30", C30_FLAG, \
    "Generate code for C30 CPU"}, \
  { "31", C31_FLAG, \
    "Generate code for C31 CPU"}, \
  { "32", C32_FLAG, \
    "Generate code for C32 CPU"}, \
  { "40", C40_FLAG, \
    "Generate code for C40 CPU"}, \
  { "44", C44_FLAG, \
    "Generate code for C44 CPU"}, \
  { "ti", TI_FLAG, \
    "Emit code compatible with TI tools"}, \
  { "no-ti", -TI_FLAG, \
    "Emit code to use GAS extensions"}, \
  { "paranoid", PARANOID_FLAG, \
    "Save DP across ISR in small memory model" }, \
  { "no-paranoid", -PARANOID_FLAG, \
    "Don't save DP across ISR in small memory model" }, \
  { "isr-dp-reload", PARANOID_FLAG, \
    "Save DP across ISR in small memory model" }, \
  { "no-isr-dp-reload", -PARANOID_FLAG, \
    "Don't save DP across ISR in small memory model" }, \
  { "memparm", MEMPARM_FLAG, \
    "Pass arguments on the stack" }, \
  { "regparm", -MEMPARM_FLAG,  \
    "Pass arguments in registers" }, \
  { "devel", DEVEL_FLAG, \
    "Enable new features under development" }, \
  { "no-devel", -DEVEL_FLAG, \
    "Disable new features under development" }, \
  { "bk", BK_FLAG, \
    "Use the BK register as a general purpose register" }, \
  { "no-bk", -BK_FLAG, \
    "Do not allocate BK register" }, \
  { "db", DB_FLAG, \
    "Enable use of DB instruction" }, \
  { "no-db", -DB_FLAG, \
    "Disable use of DB instruction" }, \
  { "debug", DEBUG_FLAG, \
    "Enable debugging" }, \
  { "no-debug", -DEBUG_FLAG, \
    "Disable debugging" }, \
  { "hoist", HOIST_FLAG, \
    "Force constants into registers to improve hoisting" }, \
  { "no-hoist", -HOIST_FLAG, \
    "Don't force constants into registers" }, \
  { "force", FORCE_FLAG, \
    "Force RTL generation to emit valid 3 operand insns" }, \
  { "no-force", -FORCE_FLAG, \
    "Allow RTL generation to emit invalid 3 operand insns" }, \
  { "loop-unsigned", LOOP_UNSIGNED_FLAG, \
    "Allow unsigned interation counts for RPTB/DB" }, \
  { "no-loop-unsigned", -LOOP_UNSIGNED_FLAG, \
    "Disallow unsigned iteration counts for RPTB/DB" }, \
  { "preserve-float", PRESERVE_FLOAT_FLAG, \
    "Preserve all 40 bits of FP reg across call" }, \
  { "no-preserve-float", -PRESERVE_FLOAT_FLAG, \
    "Only preserve 32 bits of FP reg across call" }, \
  { "parallel-insns", PARALLEL_PACK_FLAG, \
    "Enable parallel instructions" }, \
  { "no-parallel-mpy", -PARALLEL_MPY_FLAG, \
    "Disable parallel instructions" }, \
  { "parallel-mpy", PARALLEL_MPY_FLAG, \
    "Enable MPY||ADD and MPY||SUB instructions" }, \
  { "no-parallel-insns", -PARALLEL_PACK_FLAG, \
    "Disable MPY||ADD and MPY||SUB instructions" }, \
  { "aliases", ALIASES_FLAG, \
    "Assume that pointers may be aliased" }, \
  { "no-aliases", -ALIASES_FLAG, \
    "Assume that pointers not aliased" }, \
  { "", TARGET_DEFAULT, ""} }

/* Default target switches */

/* Play safe, not the fastest code.  */
#define TARGET_DEFAULT		ALIASES_FLAG | PARALLEL_PACK_FLAG \
				| PARALLEL_MPY_FLAG | RPTB_FLAG

/* Caveats:
   Max iteration count for RPTB/RPTS is 2^31 + 1.
   Max iteration count for DB is 2^31 + 1 for C40, but 2^23 + 1 for C30.
   RPTS blocks interrupts.  */


extern int target_flags;

#define TARGET_INLINE		(! optimize_size) /* Inline MPYI.  */
#define TARGET_PARALLEL	        1 /* Enable parallel insns in MD.  */
#define TARGET_SMALL_REG_CLASS	0

#define TARGET_SMALL		(target_flags & SMALL_MEMORY_FLAG)
#define TARGET_MPYI		(!TARGET_C3X || (target_flags & MPYI_FLAG))
#define TARGET_FAST_FIX		(target_flags & FAST_FIX_FLAG)
#define TARGET_RPTS		(target_flags & RPTS_FLAG)
#define TARGET_TI		(target_flags & TI_FLAG)
#define TARGET_PARANOID		(target_flags & PARANOID_FLAG)
#define TARGET_MEMPARM		(target_flags & MEMPARM_FLAG)
#define TARGET_DEVEL		(target_flags & DEVEL_FLAG)
#define TARGET_RPTB		(target_flags & RPTB_FLAG \
				 && optimize >= 2)
#define TARGET_BK		(target_flags & BK_FLAG)
#define TARGET_DB		(! TARGET_C3X || (target_flags & DB_FLAG))
#define TARGET_DEBUG		(target_flags & DEBUG_FLAG)
#define TARGET_HOIST		(target_flags & HOIST_FLAG)
#define TARGET_LOOP_UNSIGNED	(target_flags & LOOP_UNSIGNED_FLAG)
#define TARGET_FORCE		(target_flags & FORCE_FLAG)
#define	TARGET_PRESERVE_FLOAT	(target_flags & PRESERVE_FLOAT_FLAG)
#define TARGET_PARALLEL_PACK	(TARGET_RPTB \
				 && (target_flags & PARALLEL_PACK_FLAG) \
				 && optimize >= 2)
#define TARGET_PARALLEL_MPY	(TARGET_PARALLEL_PACK \
				 && (target_flags & PARALLEL_MPY_FLAG))
#define	TARGET_ALIASES		(target_flags & ALIASES_FLAG)

#define TARGET_C3X		(target_flags & C3X_FLAG)
#define TARGET_C30		(target_flags & C30_FLAG)
#define TARGET_C31		(target_flags & C31_FLAG)
#define TARGET_C32		(target_flags & C32_FLAG)
#define TARGET_C40		(target_flags & C40_FLAG)
#define TARGET_C44		(target_flags & C44_FLAG)

/* Define some options to control code generation.  */
#define TARGET_LOAD_ADDRESS	(1 || (! TARGET_C3X && ! TARGET_SMALL))
#define TARGET_EXPOSE_LDP	0

/* -mrpts            allows the use of the RPTS instruction irregardless.
   -mrpts=max-cycles will use RPTS if the number of cycles is constant
   and less than max-cycles.  */

#define TARGET_RPTS_CYCLES(CYCLES) (TARGET_RPTS || (CYCLES) < c4x_rpts_cycles)

#define	BCT_CHECK_LOOP_ITERATIONS  !(TARGET_LOOP_UNSIGNED)

/* -mcpu=XX    with XX = target DSP version number */

/* This macro is similar to `TARGET_SWITCHES' but defines names of
   command options that have values.  Its definition is an
   initializer with a subgrouping for each command option.

   Each subgrouping contains a string constant, that defines the
   fixed part of the option name, and the address of a variable.
   The variable, type `char *', is set to the variable part of the
   given option if the fixed part matches.  The actual option name
   is made by appending `-m' to the specified name.

   Here is an example which defines `-mshort-data-NUMBER'.  If the
   given option is `-mshort-data-512', the variable `m88k_short_data'
   will be set to the string `"512"'.

   extern char *m88k_short_data;
   #define TARGET_OPTIONS { { "short-data-", &m88k_short_data } }  */

extern const char *c4x_rpts_cycles_string, *c4x_cpu_version_string;

#define TARGET_OPTIONS		\
{ {"rpts=", &c4x_rpts_cycles_string, \
   "Specify maximum number of iterations for RPTS" }, \
  {"cpu=", &c4x_cpu_version_string, \
   "Select CPU to generate code for" } }

/* Sometimes certain combinations of command options do not make sense
   on a particular target machine.  You can define a macro
   `OVERRIDE_OPTIONS' to take account of this.  This macro, if
   defined, is executed once just after all the command options have
   been parsed.  */

#define OVERRIDE_OPTIONS c4x_override_options ()

/* Define this to change the optimizations performed by default.  */

#define OPTIMIZATION_OPTIONS(LEVEL,SIZE) c4x_optimization_options(LEVEL, SIZE)

/* Run Time Target Specification  */

#define TARGET_VERSION fprintf (stderr, " (TMS320C[34]x, TI syntax)");

/* Storage Layout  */

#define BITS_BIG_ENDIAN		0
#define BYTES_BIG_ENDIAN	0
#define WORDS_BIG_ENDIAN	0

/* Technically, we are little endian, but we put the floats out as
   whole longs and this makes GCC put them out in the right order.  */

#define FLOAT_WORDS_BIG_ENDIAN	1

/* Note the ANSI C standard requires sizeof(char) = 1.  On the C[34]x
   all integral and floating point data types are stored in memory as
   32-bits (floating point types can be stored as 40-bits in the
   extended precision registers), so sizeof(char) = sizeof(short) =
   sizeof(int) = sizeof(long) = sizeof(float) = sizeof(double) = 1.  */

#define BITS_PER_UNIT		32
#define BITS_PER_WORD		32
#define UNITS_PER_WORD		1
#define POINTER_SIZE		32
#define PARM_BOUNDARY	        32
#define STACK_BOUNDARY		32
#define FUNCTION_BOUNDARY	32
#define BIGGEST_ALIGNMENT	32
#define EMPTY_FIELD_BOUNDARY	32
#define STRICT_ALIGNMENT	0
#define TARGET_FLOAT_FORMAT	C4X_FLOAT_FORMAT
#define MAX_FIXED_MODE_SIZE	64 /* HImode */

/* Number of bits in the high and low parts of a two stage
   load of an immediate constant.  */
#define BITS_PER_HIGH 16
#define BITS_PER_LO_SUM 16

/* Use the internal floating point stuff in the compiler and not the
   host floating point stuff.  */

#define REAL_ARITHMETIC

/* Define register numbers.  */

/* Extended-precision registers.  */

#define R0_REGNO   0
#define R1_REGNO   1
#define R2_REGNO   2
#define R3_REGNO   3
#define R4_REGNO   4
#define R5_REGNO   5
#define R6_REGNO   6
#define R7_REGNO   7

/* Auxiliary (address) registers.  */

#define AR0_REGNO  8
#define AR1_REGNO  9
#define AR2_REGNO 10
#define AR3_REGNO 11
#define AR4_REGNO 12
#define AR5_REGNO 13
#define AR6_REGNO 14
#define AR7_REGNO 15

/* Data page register.  */

#define DP_REGNO  16

/* Index registers.  */

#define IR0_REGNO 17
#define IR1_REGNO 18

/* Block size register.  */

#define BK_REGNO  19

/* Stack pointer.  */

#define SP_REGNO  20

/* Status register.  */

#define ST_REGNO  21

/* Misc. interrupt registers.  */

#define DIE_REGNO 22		/* C4x only.  */
#define IE_REGNO  22		/* C3x only.  */
#define IIE_REGNO 23		/* C4x only.  */
#define IF_REGNO  23		/* C3x only.  */
#define IIF_REGNO 24		/* C4x only.  */
#define IOF_REGNO 24		/* C3x only.  */

/* Repeat block registers.  */

#define RS_REGNO  25
#define RE_REGNO  26
#define RC_REGNO  27

/* Additional extended-precision registers.  */

#define R8_REGNO  28		/* C4x only.  */
#define R9_REGNO  29		/* C4x only.  */
#define R10_REGNO 30		/* C4x only.  */
#define R11_REGNO 31		/* C4x only.  */

#define FIRST_PSEUDO_REGISTER	32

/* Extended precision registers (low set).  */

#define IS_R0R1_REGNO(r)           ((((r) >= R0_REGNO) && ((r) <= R1_REGNO)))
#define IS_R2R3_REGNO(r)           ((((r) >= R2_REGNO) && ((r) <= R3_REGNO)))
#define IS_EXT_LOW_REGNO(r)        ((((r) >= R0_REGNO) && ((r) <= R7_REGNO)))

/* Extended precision registers (high set).  */

#define IS_EXT_HIGH_REGNO(r)       (! TARGET_C3X \
			            && ((r) >= R8_REGNO) && ((r) <= R11_REGNO))
/* Address registers.  */

#define IS_AUX_REGNO(r)    (((r) >= AR0_REGNO) && ((r) <= AR7_REGNO))
#define IS_ADDR_REGNO(r)   IS_AUX_REGNO(r)
#define IS_DP_REGNO(r)     ((r) == DP_REGNO)
#define IS_INDEX_REGNO(r)  (((r) == IR0_REGNO) || ((r) == IR1_REGNO))
#define IS_SP_REGNO(r)     ((r) == SP_REGNO)
#define IS_BK_REGNO(r)     (TARGET_BK && (r) == BK_REGNO)

/* Misc registers.  */

#define IS_ST_REGNO(r)     ((r) == ST_REGNO)
#define IS_RC_REGNO(r)     ((r) == RC_REGNO)
#define IS_REPEAT_REGNO(r) (((r) >= RS_REGNO) && ((r) <= RC_REGNO))

/* Composite register sets.  */

#define IS_ADDR_OR_INDEX_REGNO(r) (IS_ADDR_REGNO(r) || IS_INDEX_REGNO(r))
#define IS_EXT_REGNO(r)           (IS_EXT_LOW_REGNO(r) || IS_EXT_HIGH_REGNO(r))
#define IS_STD_REGNO(r)           (IS_ADDR_OR_INDEX_REGNO(r) \
				   || IS_REPEAT_REGNO(r) \
                                   || IS_SP_REGNO(r) \
		       		   || IS_BK_REGNO(r))
#define IS_INT_REGNO(r)           (IS_EXT_REGNO(r) || IS_STD_REGNO(r))
#define IS_GROUP1_REGNO(r)        (IS_ADDR_OR_INDEX_REGNO(r) || IS_BK_REGNO(r))

#define IS_PSEUDO_REGNO(r)            ((r) >= FIRST_PSEUDO_REGISTER)
#define IS_R0R1_OR_PSEUDO_REGNO(r)    (IS_R0R1_REGNO(r) || IS_PSEUDO_REGNO(r))
#define IS_R2R3_OR_PSEUDO_REGNO(r)    (IS_R2R3_REGNO(r) || IS_PSEUDO_REGNO(r))
#define IS_EXT_OR_PSEUDO_REGNO(r)     (IS_EXT_REGNO(r) || IS_PSEUDO_REGNO(r))
#define IS_STD_OR_PSEUDO_REGNO(r)     (IS_STD_REGNO(r) || IS_PSEUDO_REGNO(r))
#define IS_INT_OR_PSEUDO_REGNO(r)     (IS_INT_REGNO(r) || IS_PSEUDO_REGNO(r))
#define IS_ADDR_OR_PSEUDO_REGNO(r)    (IS_ADDR_REGNO(r) || IS_PSEUDO_REGNO(r))
#define IS_INDEX_OR_PSEUDO_REGNO(r)   (IS_INDEX_REGNO(r) || IS_PSEUDO_REGNO(r))
#define IS_EXT_LOW_OR_PSEUDO_REGNO(r) (IS_EXT_LOW_REGNO(r) \
				       || IS_PSEUDO_REGNO(r))
#define IS_DP_OR_PSEUDO_REGNO(r)      (IS_DP_REGNO(r) || IS_PSEUDO_REGNO(r))
#define IS_SP_OR_PSEUDO_REGNO(r)      (IS_SP_REGNO(r) || IS_PSEUDO_REGNO(r))
#define IS_ST_OR_PSEUDO_REGNO(r)      (IS_ST_REGNO(r) || IS_PSEUDO_REGNO(r))
#define IS_RC_OR_PSEUDO_REGNO(r)      (IS_RC_REGNO(r) || IS_PSEUDO_REGNO(r))

#define IS_PSEUDO_REG(op)          (IS_PSEUDO_REGNO(REGNO(op)))
#define IS_ADDR_REG(op)            (IS_ADDR_REGNO(REGNO(op)))
#define IS_INDEX_REG(op)           (IS_INDEX_REGNO(REGNO(op)))
#define IS_GROUP1_REG(r)           (IS_GROUP1_REGNO(REGNO(op)))
#define IS_SP_REG(op)              (IS_SP_REGNO(REGNO(op)))
#define IS_STD_REG(op)             (IS_STD_REGNO(REGNO(op)))
#define IS_EXT_REG(op)             (IS_EXT_REGNO(REGNO(op)))

#define IS_R0R1_OR_PSEUDO_REG(op)  (IS_R0R1_OR_PSEUDO_REGNO(REGNO(op)))
#define IS_R2R3_OR_PSEUDO_REG(op)  (IS_R2R3_OR_PSEUDO_REGNO(REGNO(op)))
#define IS_EXT_OR_PSEUDO_REG(op)   (IS_EXT_OR_PSEUDO_REGNO(REGNO(op)))
#define IS_STD_OR_PSEUDO_REG(op)   (IS_STD_OR_PSEUDO_REGNO(REGNO(op)))
#define IS_EXT_LOW_OR_PSEUDO_REG(op) (IS_EXT_LOW_OR_PSEUDO_REGNO(REGNO(op)))
#define IS_INT_OR_PSEUDO_REG(op)   (IS_INT_OR_PSEUDO_REGNO(REGNO(op)))

#define IS_ADDR_OR_PSEUDO_REG(op)  (IS_ADDR_OR_PSEUDO_REGNO(REGNO(op)))
#define IS_INDEX_OR_PSEUDO_REG(op) (IS_INDEX_OR_PSEUDO_REGNO(REGNO(op)))
#define IS_DP_OR_PSEUDO_REG(op)    (IS_DP_OR_PSEUDO_REGNO(REGNO(op)))
#define IS_SP_OR_PSEUDO_REG(op)    (IS_SP_OR_PSEUDO_REGNO(REGNO(op)))
#define IS_ST_OR_PSEUDO_REG(op)    (IS_ST_OR_PSEUDO_REGNO(REGNO(op)))
#define IS_RC_OR_PSEUDO_REG(op)    (IS_RC_OR_PSEUDO_REGNO(REGNO(op)))

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.  */

#define FIXED_REGISTERS \
{									\
/* R0  R1  R2  R3  R4  R5  R6  R7 AR0 AR1 AR2 AR3 AR4 AR5 AR6 AR7 */	\
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,	\
/* DP IR0 IR1  BK  SP  ST DIE IIE IIF  RS  RE  RC  R8  R9 R10 R11 */	\
    1,  0,  0,  0,  1,  1,  1,  1,  1,  0,  0,  0,  0,  0,  0,  0	\
}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  
   
   Note that the extended precision registers are only saved in some
   modes.  The macro HARD_REGNO_CALL_CLOBBERED specifies which modes
   get clobbered for a given regno.  */

#define CALL_USED_REGISTERS \
{									\
/* R0  R1  R2  R3  R4  R5  R6  R7 AR0 AR1 AR2 AR3 AR4 AR5 AR6 AR7 */	\
    1,  1,  1,  1,  0,  0,  0,  0,  1,  1,  1,  0,  0,  0,  0,  0,	\
/* DP IR0 IR1  BK  SP  ST DIE IIE IIF  RS  RE  RC  R8  R9 R10 R11 */	\
    1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  0,  1,  1,  1	\
}

/* Macro to conditionally modify fixed_regs/call_used_regs.  */

#define CONDITIONAL_REGISTER_USAGE			\
  {							\
    if (! TARGET_BK)					\
      {							\
	fixed_regs[BK_REGNO] = 1;			\
        call_used_regs[BK_REGNO] = 1;			\
        c4x_regclass_map[BK_REGNO] = NO_REGS;		\
      }							\
    if (TARGET_C3X)					\
      {							\
	 int i;                                          \
							 \
	 reg_names[DIE_REGNO] = "ie";  /* clobber die */ \
	 reg_names[IF_REGNO] = "if";   /* clobber iie */ \
	 reg_names[IOF_REGNO] = "iof"; /* clobber iif */ \
	 						\
	 for (i = R8_REGNO; i <= R11_REGNO; i++)	\
	 {						\
	     fixed_regs[i] = call_used_regs[i] = 1;	\
	     c4x_regclass_map[i] = NO_REGS;		\
	 }						\
      }							\
    if (TARGET_PRESERVE_FLOAT)				\
      {							\
	c4x_caller_save_map[R6_REGNO] = HFmode;		\
	c4x_caller_save_map[R7_REGNO] = HFmode;		\
      }							\
   }

/* Order of Allocation of Registers  */

/* List the order in which to allocate registers.  Each register must be
   listed once, even those in FIXED_REGISTERS.

   First allocate registers that don't need preservation across calls,
   except index and address registers.  Then allocate data registers
   that require preservation across calls (even though this invokes an
   extra overhead of having to save/restore these registers).  Next
   allocate the address and index registers, since using these
   registers for arithmetic can cause pipeline stalls.  Finally
   allocated the fixed registers which won't be allocated anyhow.  */

#define REG_ALLOC_ORDER					\
{R0_REGNO, R1_REGNO, R2_REGNO, R3_REGNO, 		\
 R9_REGNO, R10_REGNO, R11_REGNO,			\
 RS_REGNO, RE_REGNO, RC_REGNO, BK_REGNO,		\
 R4_REGNO, R5_REGNO, R6_REGNO, R7_REGNO, R8_REGNO,	\
 AR0_REGNO, AR1_REGNO, AR2_REGNO, AR3_REGNO,		\
 AR4_REGNO, AR5_REGNO, AR6_REGNO, AR7_REGNO,		\
 IR0_REGNO, IR1_REGNO,					\
 SP_REGNO, DP_REGNO, ST_REGNO, IE_REGNO, IF_REGNO, IOF_REGNO}


/* Determine which register classes are very likely used by spill registers.
   local-alloc.c won't allocate pseudos that have these classes as their
   preferred class unless they are "preferred or nothing".  */

#define CLASS_LIKELY_SPILLED_P(CLASS) ((CLASS) == INDEX_REGS)

/* CCmode is wrongly defined in machmode.def  It should have a size
   of UNITS_PER_WORD.  */

#define HARD_REGNO_NREGS(REGNO, MODE)				\
(((MODE) == CCmode || (MODE) == CC_NOOVmode) ? 1 : ((MODE) == HFmode) ? 1 : \
((GET_MODE_SIZE(MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))


/* A C expression that is nonzero if the hard register REGNO is preserved
   across a call in mode MODE.  This does not have to include the call used
   registers.  */

#define HARD_REGNO_CALL_PART_CLOBBERED(REGNO, MODE)		              \
     ((((REGNO) == R6_REGNO || (REGNO) == R7_REGNO) && ! ((MODE) == QFmode))  \
      || (((REGNO) == R4_REGNO || (REGNO) == R5_REGNO || (REGNO == R8_REGNO)) \
	  && ! ((MODE) == QImode || (MODE) == HImode || (MODE) == Pmode)))

/* Specify the modes required to caller save a given hard regno.  */

#define HARD_REGNO_CALLER_SAVE_MODE(REGNO, NREGS) (c4x_caller_save_map[REGNO])

#define HARD_REGNO_MODE_OK(REGNO, MODE) c4x_hard_regno_mode_ok(REGNO, MODE)

/* A C expression that is nonzero if it is desirable to choose
   register allocation so as to avoid move instructions between a
   value of mode MODE1 and a value of mode MODE2.

   Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */

#define MODES_TIEABLE_P(MODE1, MODE2) 0


/* Define the classes of registers for register constraints in the
   machine description.  Also define ranges of constants.

   One of the classes must always be named ALL_REGS and include all hard regs.
   If there is more than one class, another class must be named NO_REGS
   and contain no registers.

   The name GENERAL_REGS must be the name of a class (or an alias for
   another name such as ALL_REGS).  This is the class of registers
   that is allowed by "g" or "r" in a register constraint.
   Also, registers outside this class are allocated only when
   instructions express preferences for them.

   The classes must be numbered in nondecreasing order; that is,
   a larger-numbered class must never be contained completely
   in a smaller-numbered class.

   For any two classes, it is very desirable that there be another
   class that represents their union.  */
   
enum reg_class
  {
    NO_REGS,
    R0R1_REGS,			/* 't' */
    R2R3_REGS,			/* 'u' */
    EXT_LOW_REGS,		/* 'q' */
    EXT_REGS,			/* 'f' */
    ADDR_REGS,			/* 'a' */
    INDEX_REGS,			/* 'x' */
    BK_REG,			/* 'k' */
    SP_REG,			/* 'b' */
    RC_REG,			/* 'v' */
    COUNTER_REGS,		/*  */
    INT_REGS,			/* 'c' */
    GENERAL_REGS,		/* 'r' */
    DP_REG,			/* 'z' */
    ST_REG,			/* 'y' */
    ALL_REGS,
    LIM_REG_CLASSES
  };

#define N_REG_CLASSES (int) LIM_REG_CLASSES

#define REG_CLASS_NAMES \
{			\
   "NO_REGS",		\
   "R0R1_REGS",		\
   "R2R3_REGS",		\
   "EXT_LOW_REGS",	\
   "EXT_REGS",		\
   "ADDR_REGS",		\
   "INDEX_REGS",	\
   "BK_REG",		\
   "SP_REG",		\
   "RC_REG",		\
   "COUNTER_REGS",	\
   "INT_REGS",		\
   "GENERAL_REGS",	\
   "DP_REG",		\
   "ST_REG",		\
   "ALL_REGS"		\
}

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  RC is not included in GENERAL_REGS
   since the register allocator will often choose a general register
   in preference to RC for the decrement_and_branch_on_count pattern.  */

#define REG_CLASS_CONTENTS \
{						\
 {0x00000000}, /*     No registers */		\
 {0x00000003}, /* 't' R0-R1	 */		\
 {0x0000000c}, /* 'u' R2-R3	 */		\
 {0x000000ff}, /* 'q' R0-R7	 */		\
 {0xf00000ff}, /* 'f' R0-R11       */		\
 {0x0000ff00}, /* 'a' AR0-AR7 */		\
 {0x00060000}, /* 'x' IR0-IR1 */		\
 {0x00080000}, /* 'k' BK */			\
 {0x00100000}, /* 'b' SP */			\
 {0x08000000}, /* 'v' RC */			\
 {0x0800ff00}, /*     RC,AR0-AR7 */		\
 {0x0e1eff00}, /* 'c' AR0-AR7, IR0-IR1, BK, SP, RS, RE, RC */	\
 {0xfe1effff}, /* 'r' R0-R11, AR0-AR7, IR0-IR1, BK, SP, RS, RE, RC */\
 {0x00010000}, /* 'z' DP */			\
 {0x00200000}, /* 'y' ST */			\
 {0xffffffff}, /*     All registers */		\
}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO) (c4x_regclass_map[REGNO])

/* When SMALL_REGISTER_CLASSES is defined, the lifetime of registers
   explicitly used in the rtl is kept as short as possible.

   We only need to define SMALL_REGISTER_CLASSES if TARGET_PARALLEL_MPY
   is defined since the MPY|ADD insns require the classes R0R1_REGS and
   R2R3_REGS which are used by the function return registers (R0,R1) and
   the register arguments (R2,R3), respectively.  I'm reluctant to define
   this macro since it stomps on many potential optimisations.  Ideally
   it should have a register class argument so that not all the register
   classes gets penalised for the sake of a naughty few...  For long
   double arithmetic we need two additional registers that we can use as
   spill registers.  */

#define SMALL_REGISTER_CLASSES (TARGET_SMALL_REG_CLASS && TARGET_PARALLEL_MPY)

#define BASE_REG_CLASS	ADDR_REGS
#define INDEX_REG_CLASS INDEX_REGS

/*
  Register constraints for the C4x
 
  a - address reg (ar0-ar7)
  b - stack reg (sp)
  c - other gp int-only reg
  d - data/int reg (equiv. to f)
  f - data/float reg
  h - data/long double reg (equiv. to f)
  k - block count (bk)
  q - r0-r7
  t - r0-r1
  u - r2-r3
  v - repeat count (rc)
  x - index register (ir0-ir1)
  y - status register (st)
  z - dp reg (dp) 

  Memory/constant constraints for the C4x

  G - short float 16-bit
  I - signed 16-bit constant (sign extended)
  J - signed 8-bit constant (sign extended)  (C4x only)
  K - signed 5-bit constant (sign extended)  (C4x only for stik)
  L - unsigned 16-bit constant
  M - unsigned 8-bit constant                (C4x only)
  N - ones complement of unsigned 16-bit constant
  Q - indirect arx + 9-bit signed displacement
      (a *-arx(n) or *+arx(n) is used to account for the sign bit)
  R - indirect arx + 5-bit unsigned displacement  (C4x only)
  S - indirect arx + 0, 1, or irn displacement
  T - direct symbol ref
  > - indirect with autoincrement
  < - indirect with autodecrement
  } - indirect with post-modify
  { - indirect with pre-modify
  */

#define REG_CLASS_FROM_LETTER(CC)				\
     ( ((CC) == 'a') ? ADDR_REGS				\
     : ((CC) == 'b') ? SP_REG					\
     : ((CC) == 'c') ? INT_REGS					\
     : ((CC) == 'd') ? EXT_REGS					\
     : ((CC) == 'f') ? EXT_REGS					\
     : ((CC) == 'h') ? EXT_REGS					\
     : ((CC) == 'k') ? BK_REG					\
     : ((CC) == 'q') ? EXT_LOW_REGS				\
     : ((CC) == 't') ? R0R1_REGS				\
     : ((CC) == 'u') ? R2R3_REGS				\
     : ((CC) == 'v') ? RC_REG					\
     : ((CC) == 'x') ? INDEX_REGS				\
     : ((CC) == 'y') ? ST_REG					\
     : ((CC) == 'z') ? DP_REG					\
     : NO_REGS )

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */

#define REGNO_OK_FOR_BASE_P(REGNO)  \
     (IS_ADDR_REGNO(REGNO) || IS_ADDR_REGNO((unsigned)reg_renumber[REGNO]))

#define REGNO_OK_FOR_INDEX_P(REGNO) \
     (IS_INDEX_REGNO(REGNO) || IS_INDEX_REGNO((unsigned)reg_renumber[REGNO]))

#define PREFERRED_RELOAD_CLASS(X, CLASS) (CLASS)

#define LIMIT_RELOAD_CLASS(X, CLASS) (CLASS)

#define SECONDARY_MEMORY_NEEDED(CLASS1, CLASS2, MODE) 0

#define CLASS_MAX_NREGS(CLASS, MODE)			\
(((MODE) == CCmode || (MODE) == CC_NOOVmode) ? 1 : ((MODE) == HFmode) ? 1 : \
((GET_MODE_SIZE(MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

#define IS_INT5_CONST(VAL) (((VAL) <= 15) && ((VAL) >= -16))	/* 'K' */

#define IS_UINT5_CONST(VAL) (((VAL) <= 31) && ((VAL) >= 0))	/* 'R' */

#define IS_INT8_CONST(VAL) (((VAL) <= 127) && ((VAL) >= -128))	/* 'J' */

#define IS_UINT8_CONST(VAL) (((VAL) <= 255) && ((VAL) >= 0))	/* 'M' */

#define IS_INT16_CONST(VAL) (((VAL) <= 32767) && ((VAL) >= -32768)) /* 'I' */

#define IS_UINT16_CONST(VAL) (((VAL) <= 65535) && ((VAL) >= 0))	/* 'L' */

#define IS_NOT_UINT16_CONST(VAL) IS_UINT16_CONST(~(VAL))	/* 'N' */

#define IS_HIGH_CONST(VAL) (! TARGET_C3X && (((VAL) & 0xffff) == 0)) /* 'O' */


#define IS_DISP1_CONST(VAL) (((VAL) <= 1) && ((VAL) >= -1)) /* 'S' */

#define IS_DISP8_CONST(VAL) (((VAL) <= 255) && ((VAL) >= -255))	/* 'Q' */

#define IS_DISP1_OFF_CONST(VAL) (IS_DISP1_CONST (VAL) \
				 && IS_DISP1_CONST (VAL + 1))

#define IS_DISP8_OFF_CONST(VAL) (IS_DISP8_CONST (VAL) \
				 && IS_DISP8_CONST (VAL + 1))

#define CONST_OK_FOR_LETTER_P(VAL, C)					\
        ( ((C) == 'I') ? (IS_INT16_CONST (VAL))				\
	: ((C) == 'J') ? (! TARGET_C3X && IS_INT8_CONST (VAL))		\
	: ((C) == 'K') ? (! TARGET_C3X && IS_INT5_CONST (VAL))		\
        : ((C) == 'L') ? (IS_UINT16_CONST (VAL))			\
	: ((C) == 'M') ? (! TARGET_C3X && IS_UINT8_CONST (VAL))		\
	: ((C) == 'N') ? (IS_NOT_UINT16_CONST (VAL))		        \
	: ((C) == 'O') ? (IS_HIGH_CONST (VAL))			        \
        : 0 )	

#define CONST_DOUBLE_OK_FOR_LETTER_P(OP, C) 				\
        ( ((C) == 'G') ? (fp_zero_operand (OP, QFmode))			\
	: ((C) == 'H') ? (c4x_H_constant (OP)) 				\
	: 0 )

#define EXTRA_CONSTRAINT(OP, C) \
        ( ((C) == 'Q') ? (c4x_Q_constraint (OP))			\
	: ((C) == 'R') ? (c4x_R_constraint (OP))			\
	: ((C) == 'S') ? (c4x_S_constraint (OP))			\
	: ((C) == 'T') ? (c4x_T_constraint (OP))			\
	: ((C) == 'U') ? (c4x_U_constraint (OP))			\
	: 0 )

#define SMALL_CONST(VAL, insn)						\
     (  ((insn == NULL_RTX) || (get_attr_data (insn) == DATA_INT16))	\
	? IS_INT16_CONST (VAL)						\
	: ( (get_attr_data (insn) == DATA_NOT_UINT16)			\
	    ? IS_NOT_UINT16_CONST (VAL)					\
	    :  ( (get_attr_data (insn) == DATA_HIGH_16)			\
	       ? IS_HIGH_CONST (VAL)					\
	       : IS_UINT16_CONST (VAL)					\
	    )								\
	  )								\
	)

/*
   I. Routine calling with arguments in registers
   ----------------------------------------------

   The TI C3x compiler has a rather unusual register passing algorithm.
   Data is passed in the following registers (in order):

   AR2, R2, R3, RC, RS, RE

   However, the first and second floating point values are always in R2
   and R3 (and all other floats are on the stack).  Structs are always
   passed on the stack.  If the last argument is an ellipsis, the
   previous argument is passed on the stack so that its address can be
   taken for the stdargs macros.

   Because of this, we have to pre-scan the list of arguments to figure
   out what goes where in the list.

   II. Routine calling with arguments on stack
   -------------------------------------------

   Let the subroutine declared as "foo(arg0, arg1, arg2);" have local
   variables loc0, loc1, and loc2.  After the function prologue has
   been executed, the stack frame will look like:

   [stack grows towards increasing addresses]
       I-------------I
   5   I saved reg1  I  <= SP points here
       I-------------I
   4   I saved reg0  I  
       I-------------I
   3   I       loc2  I  
       I-------------I  
   2   I       loc1  I  
       I-------------I  
   1   I       loc0  I  
       I-------------I
   0   I     old FP  I <= FP (AR3) points here
       I-------------I
   -1  I  return PC  I
       I-------------I
   -2  I       arg0  I  
       I-------------I  
   -3  I       arg1  I
       I-------------I  
   -4  I       arg2  I 
       I-------------I  

   All local variables (locn) are accessible by means of +FP(n+1)
   addressing, where n is the local variable number.

   All stack arguments (argn) are accessible by means of -FP(n-2).

   The stack pointer (SP) points to the last register saved in the
   prologue (regn).

   Note that a push instruction performs a preincrement of the stack
   pointer.  (STACK_PUSH_CODE == PRE_INC)

   III. Registers used in function calling convention
   --------------------------------------------------

   Preserved across calls: R4...R5 (only by PUSH,  i.e. lower 32 bits)
   R6...R7 (only by PUSHF, i.e. upper 32 bits)
   AR3...AR7

   (Because of this model, we only assign FP values in R6, R7 and
   only assign integer values in R4, R5.)

   These registers are saved at each function entry and restored at
   the exit. Also it is expected any of these not affected by any
   call to user-defined (not service) functions.

   Not preserved across calls: R0...R3
   R4...R5 (upper 8 bits)
   R6...R7 (lower 8 bits)
   AR0...AR2, IR0, IR1, BK, ST, RS, RE, RC

   These registers are used arbitrary in a function without being preserved.
   It is also expected that any of these can be clobbered by any call.

   Not used by GCC (except for in user "asm" statements):
   IE (DIE), IF (IIE), IOF (IIF)

   These registers are never used by GCC for any data, but can be used
   with "asm" statements.  */

#define C4X_ARG0 -2
#define C4X_LOC0 1

/* Basic Stack Layout  */
     
/* The stack grows upward, stack frame grows upward, and args grow
   downward.  */

#define STARTING_FRAME_OFFSET		C4X_LOC0
#define FIRST_PARM_OFFSET(FNDECL)      (C4X_ARG0 + 1)
#define ARGS_GROW_DOWNWARD
#define STACK_POINTER_OFFSET 1

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */

/* #define STACK_GROWS_DOWNWARD */
/* Like the dsp16xx, i370, i960, and we32k ports */

/* Define this if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.  */

/* #define FRAME_GROWS_DOWNWARD */


/* Registers That Address the Stack Frame  */

#define STACK_POINTER_REGNUM	SP_REGNO	/* SP */
#define FRAME_POINTER_REGNUM	AR3_REGNO	/* AR3 */
#define ARG_POINTER_REGNUM	AR3_REGNO	/* AR3 */
#define STATIC_CHAIN_REGNUM	AR0_REGNO	/* AR0 */

/* Eliminating Frame Pointer and Arg Pointer  */

#define FRAME_POINTER_REQUIRED	0

#define INITIAL_FRAME_POINTER_OFFSET(DEPTH)			\
{								\
 int regno;							\
 int offset = 0;						\
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)	\
    if (regs_ever_live[regno] && ! call_used_regs[regno])	\
      offset += TARGET_PRESERVE_FLOAT				\
		&& ((regno == R6_REGNO) || (regno == R7_REGNO)) \
		? 2 : 1;					\
  (DEPTH) = -(offset + get_frame_size ());			\
}

/* This is a hack... We need to specify a register.  */
#define	ELIMINABLE_REGS 					\
  {{ FRAME_POINTER_REGNUM, FRAME_POINTER_REGNUM }}

#define	CAN_ELIMINATE(FROM, TO)					\
  (! (((FROM) == FRAME_POINTER_REGNUM && (TO) == STACK_POINTER_REGNUM) \
  || ((FROM) == FRAME_POINTER_REGNUM && (TO) == FRAME_POINTER_REGNUM)))

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)	 	\
{								\
 int regno;							\
 int offset = 0;						\
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)	\
    if (regs_ever_live[regno] && ! call_used_regs[regno])	\
      offset += TARGET_PRESERVE_FLOAT				\
		&& ((regno == R6_REGNO) || (regno == R7_REGNO)) \
		? 2 : 1;					\
  (OFFSET) = -(offset + get_frame_size ());			\
}


/* Passing Function Arguments on the Stack  */

#if 0
#define PUSH_ROUNDING(BYTES) (BYTES)
#endif
#define RETURN_POPS_ARGS(FUNDECL, FUNTYPE, STACK_SIZE) 0

/* The following structure is used by calls.c, function.c, c4x.c  */

typedef struct c4x_args
{
  int floats;
  int ints;
  int maxfloats;
  int maxints;
  int init;
  int var;
  int prototype;
  int args;
}
CUMULATIVE_ARGS;

#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME,INDIRECT)	\
  (c4x_init_cumulative_args (&CUM, FNTYPE, LIBNAME))

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)	\
  (c4x_function_arg_advance (&CUM, MODE, TYPE, NAMED))

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
  (c4x_function_arg(&CUM, MODE, TYPE, NAMED))

/* Define the profitability of saving registers around calls.
   We disable caller save to avoid a bug in flow.c (this also affects
   other targets such as m68k).  Since we must use stf/sti,
   the profitability is marginal anyway.  */

#define CALLER_SAVE_PROFITABLE(REFS,CALLS) 0

/* Never pass data by reference.  */

#define FUNCTION_ARG_PASS_BY_REFERENCE(CUM, MODE, TYPE, NAMED) 0

#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) 0

/* 1 if N is a possible register number for function argument passing.  */

#define FUNCTION_ARG_REGNO_P(REGNO) \
	(  (   ((REGNO) == AR2_REGNO)	/* AR2 */	\
	    || ((REGNO) == R2_REGNO)	/* R2 */	\
	    || ((REGNO) == R3_REGNO)	/* R3 */	\
	    || ((REGNO) == RC_REGNO)	/* RC */	\
	    || ((REGNO) == RS_REGNO)	/* RS */	\
	    || ((REGNO) == RE_REGNO))	/* RE */	\
	 ? 1						\
	 : 0)

/* How Scalar Function Values Are Returned  */

#define FUNCTION_VALUE(VALTYPE, FUNC) \
	gen_rtx(REG, TYPE_MODE(VALTYPE), R0_REGNO)	/* Return in R0 */

#define LIBCALL_VALUE(MODE) \
	gen_rtx(REG, MODE, R0_REGNO)	/* Return in R0 */

#define FUNCTION_VALUE_REGNO_P(REGNO) ((REGNO) == R0_REGNO)

/* How Large Values Are Returned  */

#define DEFAULT_PCC_STRUCT_RETURN	0
#define STRUCT_VALUE_REGNUM		AR0_REGNO	/* AR0 */

/* Varargs handling.  */

#define	EXPAND_BUILTIN_VA_START(stdarg, valist, nextarg) \
  c4x_va_start (stdarg, valist, nextarg)

#define EXPAND_BUILTIN_VA_ARG(valist, type) \
  c4x_va_arg (valist, type)

/* Function Entry and Exit  */

#define FUNCTION_PROLOGUE(FILE, SIZE)	c4x_function_prologue(FILE, SIZE)
#define FUNCTION_EPILOGUE(FILE, SIZE)	c4x_function_epilogue(FILE, SIZE)


/* Generating Code for Profiling  */

/* Note that the generated assembly uses the ^ operator to load the 16
   MSBs of the address.  This is not supported by the TI assembler. 
   The FUNCTION profiler needs a function mcount which gets passed
   a pointer to the LABELNO.  */

#define FUNCTION_PROFILER(FILE, LABELNO) 			\
     if (! TARGET_C3X)						\
     {								\
	fprintf (FILE, "\tpush\tar2\n");			\
	fprintf (FILE, "\tldhi\t^LP%d,ar2\n", (LABELNO));	\
	fprintf (FILE, "\tor\t#LP%d,ar2\n", (LABELNO));		\
	fprintf (FILE, "\tcall\tmcount\n");			\
	fprintf (FILE, "\tpop\tar2\n");				\
     }								\
     else							\
     {								\
	fprintf (FILE, "\tpush\tar2\n");			\
	fprintf (FILE, "\tldiu\t^LP%d,ar2\n", (LABELNO));	\
	fprintf (FILE, "\tlsh\t16,ar2\n");			\
	fprintf (FILE, "\tor\t#LP%d,ar2\n", (LABELNO));		\
	fprintf (FILE, "\tcall\tmcount\n");			\
	fprintf (FILE, "\tpop\tar2\n");				\
     }

/* There are three profiling modes for basic blocks available.
   The modes are selected at compile time by using the options
   -a or -ax of the gnu compiler.
   The variable `profile_block_flag' will be set according to the
   selected option.

   profile_block_flag == 0, no option used:

      No profiling done.

   profile_block_flag == 1, -a option used.

      Count frequency of execution of every basic block.

   profile_block_flag == 2, -ax option used.

      Generate code to allow several different profiling modes at run time. 
      Available modes are:
             Produce a trace of all basic blocks.
             Count frequency of jump instructions executed.
      In every mode it is possible to start profiling upon entering
      certain functions and to disable profiling of some other functions.

    The result of basic-block profiling will be written to a file `bb.out'.
    If the -ax option is used parameters for the profiling will be read
    from file `bb.in'.

*/

#define FUNCTION_BLOCK_PROFILER(FILE, BLOCKNO) 			\
  if (profile_block_flag == 2)					\
    {								\
      if (! TARGET_C3X)						\
      {								\
	fprintf (FILE, "\tpush\tst\n");				\
	fprintf (FILE, "\tpush\tar2\n");			\
	fprintf (FILE, "\tpush\tr2\n");				\
	fprintf (FILE, "\tldhi\t^LPBX0,ar2\n");			\
	fprintf (FILE, "\tor\t#LPBX0,ar2\n");			\
	if (BLOCKNO > 32767)					\
	  {							\
	    fprintf (FILE, "\tldhi\t%d,r2\n", (BLOCKNO) >> 16);	\
	    fprintf (FILE, "\tor\t%d,r2\n", (BLOCKNO));		\
	  }							\
 	else							\
	  {							\
	    fprintf (FILE, "\tldiu\t%d,r2\n", (BLOCKNO));	\
	  }							\
	fprintf (FILE, "\tcall\t___bb_init_trace_func\n");	\
	fprintf (FILE, "\tpop\tr2\n");				\
	fprintf (FILE, "\tpop\tar2\n");				\
	fprintf (FILE, "\tpop\tst\n");				\
      }								\
      else							\
      {								\
	fprintf (FILE, "\tpush\tst\n");				\
	fprintf (FILE, "\tpush\tar2\n");			\
	fprintf (FILE, "\tpush\tr2\n");				\
	fprintf (FILE, "\tldiu\t^LPBX0,ar2\n");			\
	fprintf (FILE, "\tlsh\t16,ar2\n");			\
	fprintf (FILE, "\tor\t#LPBX0,ar2\n");			\
	if (BLOCKNO > 32767)					\
	  {							\
	    fprintf (FILE, "\tldi\t%d,r2\n", (BLOCKNO) >> 16);	\
	    fprintf (FILE, "\tlsh\t16,r2\n");			\
	    fprintf (FILE, "\tor\t%d,r2\n", (BLOCKNO));		\
	  }							\
 	else							\
	  {							\
	    fprintf (FILE, "\tldiu\t%d,r2\n", (BLOCKNO));	\
	  }							\
	fprintf (FILE, "\tcall\t___bb_init_trace_func\n");	\
	fprintf (FILE, "\tpop\tr2\n");				\
	fprintf (FILE, "\tpop\tar2\n");				\
	fprintf (FILE, "\tpop\tst\n");				\
      }								\
    }								\
  else								\
    {								\
      if (! TARGET_C3X)						\
      {								\
	fprintf (FILE, "\tpush\tst\n");				\
	fprintf (FILE, "\tpush\tar2\n");			\
	fprintf (FILE, "\tldhi\t^LPBX0,ar2\n");			\
	fprintf (FILE, "\tor\t#LPBX0,ar2\n");			\
	fprintf (FILE, "\tcmpi\t0,*ar2\n");			\
	fprintf (FILE, "\tbne\t$+2\n");				\
	fprintf (FILE, "\tcall\t___bb_init_func\n");		\
	fprintf (FILE, "\tpop\tar2\n");				\
	fprintf (FILE, "\tpop\tst\n");				\
      }								\
      else							\
      {								\
	fprintf (FILE, "\tpush\tst\n");				\
	fprintf (FILE, "\tpush\tar2\n");			\
	fprintf (FILE, "\tpush\tr2\n");				\
	fprintf (FILE, "\tldiu\t^LPBX0,ar2\n");			\
	fprintf (FILE, "\tlsh\t16,ar2\n");			\
	fprintf (FILE, "\tor\t#LPBX0,ar2\n");			\
	fprintf (FILE, "\tldi\t*ar2,r2\n");			\
	fprintf (FILE, "\tbne\t$+2\n");				\
	fprintf (FILE, "\tcall\t___bb_init_func\n");		\
	fprintf (FILE, "\tpop\tr2\n");				\
	fprintf (FILE, "\tpop\tar2\n");				\
	fprintf (FILE, "\tpop\tst\n");				\
      }								\
    }

#define BLOCK_PROFILER(FILE, BLOCKNO) 				\
  if (profile_block_flag == 2)					\
    {								\
      if (! TARGET_C3X)						\
      {								\
	fprintf (FILE, "\tpush\tst\n");				\
	fprintf (FILE, "\tpush\tar2\n");			\
	fprintf (FILE, "\tpush\tar0\n");			\
	fprintf (FILE, "\tldhi\t^___bb,ar2\n");			\
	fprintf (FILE, "\tor\t#___bb,ar2\n");			\
	if (BLOCKNO > 32767)					\
	  {							\
	    fprintf (FILE, "\tldhi\t%d,ar0\n", (BLOCKNO) >> 16);\
	    fprintf (FILE, "\tor\t%d,ar0\n", (BLOCKNO));	\
	  }							\
 	else							\
	  {							\
	    fprintf (FILE, "\tldiu\t%d,ar0\n", (BLOCKNO));	\
	  }							\
	fprintf (FILE, "\tsti\tar0,*ar2\n");			\
	fprintf (FILE, "\tldhi\t^LPBX0,ar0\n");			\
	fprintf (FILE, "\tor\t#LPBX0,ar0\n");			\
	fprintf (FILE, "\tsti\tar0,*+ar2(1)\n");		\
	fprintf (FILE, "\tcall\t___bb_trace_func\n");		\
	fprintf (FILE, "\tpop\tar0\n");				\
        fprintf (FILE, "\tpop\tar2\n");				\
	fprintf (FILE, "\tpop\tst\n");				\
      }								\
      else							\
      {								\
	fprintf (FILE, "\tpush\tst\n");				\
	fprintf (FILE, "\tpush\tar2\n");			\
	fprintf (FILE, "\tpush\tar0\n");			\
	fprintf (FILE, "\tldiu\t^___bb,ar2\n");			\
	fprintf (FILE, "\tlsh\t16,ar2\n");			\
	fprintf (FILE, "\tor\t#___bb,ar2\n");			\
	if (BLOCKNO > 32767)					\
	  {							\
	    fprintf (FILE, "\tldi\t%d,ar0\n", (BLOCKNO) >> 16);	\
	    fprintf (FILE, "\tlsh\t16,ar0\n");			\
	    fprintf (FILE, "\tor\t%d,ar0\n", (BLOCKNO));	\
	  }							\
 	else							\
	  {							\
	    fprintf (FILE, "\tldiu\t%d,ar0\n", (BLOCKNO));	\
	  }							\
	fprintf (FILE, "\tsti\tar0,*ar2\n");			\
	fprintf (FILE, "\tldiu\t^LPBX0,ar0\n");			\
	fprintf (FILE, "\tlsh\t16,ar0\n");			\
	fprintf (FILE, "\tor\t#LPBX0,ar0\n");			\
	fprintf (FILE, "\tsti\tar0,*+ar2(1)\n");		\
	fprintf (FILE, "\tcall\t___bb_trace_func\n");		\
	fprintf (FILE, "\tpop\tar0\n");				\
        fprintf (FILE, "\tpop\tar2\n");				\
	fprintf (FILE, "\tpop\tst\n");				\
      }								\
    }								\
  else								\
    {								\
      if (! TARGET_C3X)						\
      {								\
	fprintf (FILE, "\tpush\tar2\n");			\
	fprintf (FILE, "\tpush\tar0\n");			\
	fprintf (FILE, "\tldhi\t^LPBX2+%d,ar2\n", (BLOCKNO));	\
	fprintf (FILE, "\tor\t#LPBX2+%d,ar2\n", (BLOCKNO));	\
	fprintf (FILE, "\taddi3\t1,*ar2,ar0\n");		\
	fprintf (FILE, "\tsti\tar0,*ar2\n");			\
	fprintf (FILE, "\tpop\tar0\n");				\
        fprintf (FILE, "\tpop\tar2\n");				\
      }								\
      else							\
      {								\
	fprintf (FILE, "\tpush\tar2\n");			\
	fprintf (FILE, "\tpush\tar0\n");			\
	fprintf (FILE, "\tldiu\t^LPBX2+%d,ar2\n", (BLOCKNO));	\
	fprintf (FILE, "\tlsh\t16,ar2\n");			\
	fprintf (FILE, "\tor\t#LPBX2+%d,ar2\n", (BLOCKNO));	\
	fprintf (FILE, "\tldiu\t*ar2,ar0\n");			\
	fprintf (FILE, "\taddi\t1,ar0\n");			\
	fprintf (FILE, "\tsti\tar0,*ar2\n");			\
	fprintf (FILE, "\tpop\tar0\n");				\
        fprintf (FILE, "\tpop\tar2\n");				\
      }								\
    }

#define FUNCTION_BLOCK_PROFILER_EXIT(FILE)			\
    {								\
	fprintf (FILE, "\tpush\tst\n");				\
	fprintf (FILE, "\tpush\tar2\n");			\
	fprintf (FILE, "\tcall\t___bb_trace_ret\n");		\
	fprintf (FILE, "\tpop\tar2\n");				\
	fprintf (FILE, "\tpop\tst\n");				\
    }

#define	MACHINE_STATE_SAVE(ID)		\
	asm("	push	r0");		\
	asm("	pushf	r0");		\
	asm("	push	r1");		\
	asm("	pushf	r1");		\
	asm("	push	r2");		\
	asm("	pushf	r2");		\
	asm("	push	r3");		\
	asm("	pushf	r3");		\
	asm("	push	ar0");		\
	asm("	push	ar1");		\
	asm("	.if	.BIGMODEL");	\
	asm("	push	dp");		\
	asm("	.endif");		\
	asm("	push	ir0");		\
	asm("	push	ir1");		\
	asm("	push	bk");		\
	asm("	push	rs");		\
	asm("	push	re");		\
	asm("	push	rc");		\
	asm("	.if	.tms320C40");	\
	asm("	push	r9");		\
	asm("	pushf	r9");		\
	asm("	push	r10");		\
	asm("	pushf	r10");		\
	asm("	push	r11");		\
	asm("	pushf	r11");		\
	asm("	.endif");

#define	MACHINE_STATE_RESTORE(ID)	\
	asm("	.if	.tms320C40");	\
	asm("	popf	r11");		\
	asm("	pop	r11");		\
	asm("	popf	r10");		\
	asm("	pop	r10");		\
	asm("	popf	r9");		\
	asm("	pop	r9");		\
	asm("	.endif");		\
	asm("	pop	rc");		\
	asm("	pop	re");		\
	asm("	pop	rs");		\
	asm("	pop	bk");		\
	asm("	pop	ir1");		\
	asm("	pop	ir0");		\
	asm("	.if	.BIGMODEL");	\
	asm("	pop	dp");		\
	asm("	.endif");		\
	asm("	pop	ar1");		\
	asm("	pop	ar0");		\
	asm("	popf	r3");		\
	asm("	pop	r3");		\
	asm("	popf	r2");		\
	asm("	pop	r2");		\
	asm("	popf	r1");		\
	asm("	pop	r1");		\
	asm("	popf	r0");		\
	asm("	pop	r0");		\

/* Implicit Calls to Library Routines  */

#define MULQI3_LIBCALL      "__mulqi3"
#define DIVQI3_LIBCALL      "__divqi3"
#define UDIVQI3_LIBCALL     "__udivqi3"
#define MODQI3_LIBCALL      "__modqi3"
#define UMODQI3_LIBCALL     "__umodqi3"

#define DIVQF3_LIBCALL      "__divqf3"

#define MULHF3_LIBCALL      "__mulhf3"
#define DIVHF3_LIBCALL      "__divhf3"

#define MULHI3_LIBCALL      "__mulhi3"
#define SMULHI3_LIBCALL     "__smulhi3_high"
#define UMULHI3_LIBCALL     "__umulhi3_high"
#define DIVHI3_LIBCALL      "__divhi3"
#define UDIVHI3_LIBCALL     "__udivhi3"
#define MODHI3_LIBCALL      "__modhi3"
#define UMODHI3_LIBCALL     "__umodhi3"

#define FLOATHIQF2_LIBCALL  "__floathiqf2"
#define FLOATUNSHIQF2_LIBCALL  "__ufloathiqf2"
#define FIX_TRUNCQFHI2_LIBCALL "__fix_truncqfhi2"
#define FIXUNS_TRUNCQFHI2_LIBCALL "__ufix_truncqfhi2"

#define FLOATHIHF2_LIBCALL  "__floathihf2"
#define FLOATUNSHIHF2_LIBCALL  "__ufloathihf2"
#define FIX_TRUNCHFHI2_LIBCALL "__fix_trunchfhi2"
#define FIXUNS_TRUNCHFHI2_LIBCALL "__ufix_trunchfhi2"

#define FFS_LIBCALL	    "__ffs"


#define INIT_TARGET_OPTABS \
  do { \
    smul_optab->handlers[(int) QImode].libfunc		\
      = init_one_libfunc (MULQI3_LIBCALL);		\
    sdiv_optab->handlers[(int) QImode].libfunc		\
      = init_one_libfunc (DIVQI3_LIBCALL);		\
    udiv_optab->handlers[(int) QImode].libfunc		\
      = init_one_libfunc (UDIVQI3_LIBCALL);		\
    smod_optab->handlers[(int) QImode].libfunc		\
      = init_one_libfunc (MODQI3_LIBCALL);		\
    umod_optab->handlers[(int) QImode].libfunc		\
      = init_one_libfunc (UMODQI3_LIBCALL);		\
    flodiv_optab->handlers[(int) QFmode].libfunc	\
      = init_one_libfunc (DIVQF3_LIBCALL);		\
    smul_optab->handlers[(int) HFmode].libfunc		\
      = init_one_libfunc (MULHF3_LIBCALL);		\
    flodiv_optab->handlers[(int) HFmode].libfunc	\
      = init_one_libfunc (DIVHF3_LIBCALL);		\
    smul_optab->handlers[(int) HImode].libfunc		\
      = init_one_libfunc (MULHI3_LIBCALL);		\
    sdiv_optab->handlers[(int) HImode].libfunc		\
      = init_one_libfunc (DIVHI3_LIBCALL);		\
    udiv_optab->handlers[(int) HImode].libfunc		\
      = init_one_libfunc (UDIVHI3_LIBCALL);		\
    smod_optab->handlers[(int) HImode].libfunc		\
      = init_one_libfunc (MODHI3_LIBCALL);		\
    umod_optab->handlers[(int) HImode].libfunc		\
      = init_one_libfunc (UMODHI3_LIBCALL);		\
    ffs_optab->handlers[(int) QImode].libfunc		\
      = init_one_libfunc (FFS_LIBCALL);			\
  } while (0)

#define TARGET_MEM_FUNCTIONS

/* Add any extra modes needed to represent the condition code.

   On the C4x, we have a "no-overflow" mode which is used when an ADD,
   SUB, NEG, or MPY insn is used to set the condition code.  This is
   to prevent the combiner from optimising away a following CMP of the
   result with zero when a signed conditional branch or load insn
   follows.

   The problem is a subtle one and deals with the manner in which the
   negative condition (N) flag is used on the C4x.  This flag does not
   reflect the status of the actual result but of the ideal result had
   no overflow occured (when considering signed operands).

   For example, 0x7fffffff + 1 => 0x80000000 Z=0 V=1 N=0 C=0.  Here
   the flags reflect the untruncated result, not the actual result.
   While the actual result is less than zero, the N flag is not set
   since the ideal result of the addition without truncation would
   have been positive.
   
   Note that the while the N flag is handled differently to most other
   architectures, the use of it is self consistent and is not the
   cause of the problem.

   Logical operations set the N flag to the MSB of the result so if
   the result is negative, N is 1.  However, integer and floating
   point operations set the N flag to be the MSB of the result
   exclusive ored with the overflow (V) flag.  Thus if an overflow
   occurs and the result does not have the MSB set (i.e., the result
   looks like a positive number), the N flag is set.  Conversely, if
   an overflow occurs and the MSB of the result is set, N is set to 0.
   Thus the N flag represents the sign of the result if it could have
   been stored without overflow but does not represent the apparent
   sign of the result.  Note that most architectures set the N flag to
   be the MSB of the result.

   The C4x approach to setting the N flag simplifies signed
   conditional branches and loads which only have to test the state of
   the N flag, whereas most architectures have to look at both the N
   and V flags.  The disadvantage is that there is no flag giving the
   status of the sign bit of the operation.  However, there are no
   conditional load or branch instructions that make use of this
   feature (e.g., BMI---branch minus) instruction.  Note that BN and
   BLT are identical in the C4x.
   
   To handle the problem where the N flag is set differently whenever
   there is an overflow we use a different CC mode, CC_NOOVmode which
   says that the CC reflects the comparison of the result against zero
   if no overflow occured.

   For example, 

   [(set (reg:CC_NOOV 21)
         (compare:CC_NOOV (minus:QI (match_operand:QI 1 "src_operand" "")
                                    (match_operand:QI 2 "src_operand" ""))
                          (const_int 0)))
    (set (match_operand:QI 0 "ext_reg_operand" "")
         (minus:QI (match_dup 1)
                   (match_dup 2)))]

   Note that there is no problem for insns that don't return a result
   like CMP, since the CC reflects the effect of operation.

   An example of a potential problem is when GCC
   converts   (LTU (MINUS (0x80000000) (0x7fffffff) (0x80000000)))
   to         (LEU (MINUS (0x80000000) (0x7fffffff) (0x7fffffff)))
   to         (GE  (MINUS (0x80000000) (0x7fffffff) (0x00000000)))

   Now (MINUS (0x80000000) (0x7fffffff)) returns 0x00000001 but the
   C4x sets the N flag since the result without overflow would have
   been 0xffffffff when treating the operands as signed integers.
   Thus (GE (MINUS (0x80000000) (0x7fffffff) (0x00000000))) sets the N
   flag but (GE (0x00000001)) does not set the N flag.

   The upshot is that we can not use signed branch and conditional
   load instructions after an add, subtract, neg, abs or multiply.
   We must emit a compare insn to check the result against 0.  */

#define EXTRA_CC_MODES CC(CC_NOOVmode, "CC_NOOV")

/* CC_NOOVmode should be used when the first operand is a PLUS, MINUS, NEG
   or MULT.
   CCmode should be used when no special processing is needed.  */
#define SELECT_CC_MODE(OP,X,Y) \
  ((GET_CODE (X) == PLUS || GET_CODE (X) == MINUS		\
    || GET_CODE (X) == NEG || GET_CODE (X) == MULT		\
    || GET_MODE (X) == ABS					\
    || GET_CODE (Y) == PLUS || GET_CODE (Y) == MINUS		\
    || GET_CODE (Y) == NEG || GET_CODE (Y) == MULT		\
    || GET_MODE (Y) == ABS)					\
    ? CC_NOOVmode : CCmode)

/* Addressing Modes  */

#define HAVE_POST_INCREMENT 1
#define HAVE_PRE_INCREMENT 1
#define HAVE_POST_DECREMENT 1
#define HAVE_PRE_DECREMENT 1
#define HAVE_PRE_MODIFY_REG 1
#define HAVE_POST_MODIFY_REG 1
#define HAVE_PRE_MODIFY_DISP 1
#define HAVE_POST_MODIFY_DISP 1

/* The number of insns that can be packed into a single opcode.  */
#define PACK_INSNS 2

/* Recognize any constant value that is a valid address. 
   We could allow arbitrary constant addresses in the large memory
   model but for the small memory model we can only accept addresses
   within the data page.  I suppose we could also allow
   CONST PLUS SYMBOL_REF.  */
#define CONSTANT_ADDRESS_P(X) (GET_CODE (X) == SYMBOL_REF)

/* Maximum number of registers that can appear in a valid memory
   address.  */
#define MAX_REGS_PER_ADDRESS	2

/* The macros REG_OK_FOR..._P assume that the arg is a REG rtx
   and check its validity for a certain class.
   We have two alternate definitions for each of them.
   The usual definition accepts all pseudo regs; the other rejects
   them unless they have been allocated suitable hard regs.
   The symbol REG_OK_STRICT causes the latter definition to be used.

   Most source files want to accept pseudo regs in the hope that
   they will get allocated to the class that the insn wants them to be in.
   Source files for reload pass need to be strict.
   After reload, it makes no difference, since pseudo regs have
   been eliminated by then.  */

#ifndef REG_OK_STRICT

/* Nonzero if X is a hard or pseudo reg that can be used as an base.  */

#define REG_OK_FOR_BASE_P(X) IS_ADDR_OR_PSEUDO_REG(X)

/* Nonzero if X is a hard or pseudo reg that can be used as an index.  */

#define REG_OK_FOR_INDEX_P(X) IS_INDEX_OR_PSEUDO_REG(X)

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)				\
{									\
  if (c4x_check_legit_addr (MODE, X, 0))				\
    goto ADDR;								\
}

#else

/* Nonzero if X is a hard reg that can be used as an index.  */

#define REG_OK_FOR_INDEX_P(X) REGNO_OK_FOR_INDEX_P (REGNO (X))

/* Nonzero if X is a hard reg that can be used as a base reg.  */

#define REG_OK_FOR_BASE_P(X) REGNO_OK_FOR_BASE_P (REGNO (X))

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)				\
{									\
  if (c4x_check_legit_addr (MODE, X, 1))				\
    goto ADDR;								\
}

#endif

#define LEGITIMIZE_ADDRESS(X, OLDX, MODE, WIN) \
{									\
  rtx new;								\
  new = c4x_legitimize_address (X, MODE);				\
  if (new != NULL_RTX)							\
  {									\
    (X) = new;								\
    goto WIN;								\
  }									\
}

#define LEGITIMIZE_RELOAD_ADDRESS(X,MODE,OPNUM,TYPE,IND_LEVELS,WIN)     \
{									\
  rtx new;								\
  new = c4x_legitimize_reload_address (X, MODE, insn);			\
  if (new != NULL_RTX)							\
  {									\
    (X) = new;								\
   /* We do not have to call push_reload because we do not require      \
      any more reloads.  */						\
    goto WIN;								\
  }									\
}


/* No mode-dependent addresses on the C4x are autoincrements.  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR, LABEL)	\
  if (GET_CODE (ADDR) == PRE_DEC	\
      || GET_CODE (ADDR) == POST_DEC	\
      || GET_CODE (ADDR) == PRE_INC	\
      || GET_CODE (ADDR) == POST_INC	\
      || GET_CODE (ADDR) == POST_MODIFY	\
      || GET_CODE (ADDR) == PRE_MODIFY)	\
    goto LABEL


/* Nonzero if the constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE. 

   The C4x can only load 16-bit immediate values, so we only allow a
   restricted subset of CONST_INT and CONST_DOUBLE.  Disallow
   LABEL_REF and SYMBOL_REF (except on the C40 with the big memory
   model) so that the symbols will be forced into the constant pool.
   On second thoughts, let's do this with the move expanders since
   the alias analysis has trouble if we force constant addresses
   into memory.
*/

#define LEGITIMATE_CONSTANT_P(X)				\
  ((GET_CODE (X) == CONST_DOUBLE && c4x_H_constant (X))		\
  || (GET_CODE (X) == CONST_INT)				\
  || (GET_CODE (X) == SYMBOL_REF)				\
  || (GET_CODE (X) == LABEL_REF)				\
  || (GET_CODE (X) == CONST)					\
  || (GET_CODE (X) == HIGH && ! TARGET_C3X)			\
  || (GET_CODE (X) == LO_SUM && ! TARGET_C3X))

#define LEGITIMATE_DISPLACEMENT_P(X) IS_DISP8_CONST (INTVAL (X))

/* Define this macro if references to a symbol must be treated
   differently depending on something about the variable or
   function named by the symbol (such as what section it is in).

   The macro definition, if any, is executed immediately after the
   rtl for DECL or other node is created.
   The value of the rtl will be a `mem' whose address is a
   `symbol_ref'.

   The usual thing for this macro to do is to a flag in the
   `symbol_ref' (such as `SYMBOL_REF_FLAG') or to store a modified
   name string in the `symbol_ref' (if one bit is not enough
   information).

   On the C4x we use this to indicate if a symbol is in text or
   data space.  */

#define ENCODE_SECTION_INFO(DECL) c4x_encode_section_info (DECL);

/* Descripting Relative Cost of Operations  */

/* Provide the costs of a rtl expression.  This is in the body of a
   switch on CODE. 

   Note that we return, rather than break so that rtx_cost doesn't
   include CONST_COSTS otherwise expand_mult will think that it is
   cheaper to synthesise a multiply rather than to use a multiply
   instruction.  I think this is because the algorithm synth_mult
   doesn't take into account the loading of the operands, whereas the
   calculation of mult_cost does. 
*/


#define RTX_COSTS(RTX, CODE, OUTER_CODE)				\
    case PLUS:								\
    case MINUS:								\
    case AND:								\
    case IOR:								\
    case XOR:								\
    case ASHIFT:							\
    case ASHIFTRT:							\
    case LSHIFTRT:							\
    return COSTS_N_INSNS (1);						\
    case MULT:								\
    return COSTS_N_INSNS (GET_MODE_CLASS (GET_MODE (RTX)) == MODE_FLOAT \
			  || TARGET_MPYI ? 1 : 14);			\
    case DIV:								\
    case UDIV:								\
    case MOD: 								\
    case UMOD:								\
    return COSTS_N_INSNS (GET_MODE_CLASS (GET_MODE (RTX)) == MODE_FLOAT	\
			  ? 15 : 50);

/* Compute the cost of computing a constant rtl expression RTX
   whose rtx-code is CODE.  The body of this macro is a portion
   of a switch statement.  If the code is computed here,
   return it with a return statement.  Otherwise, break from the switch.

   An insn is assumed to cost 4 units.
   COSTS_N_INSNS (N) is defined as (N) * 4 - 2.

   Some small integers are effectively free for the C40.  We should
   also consider if we are using the small memory model.  With
   the big memory model we require an extra insn for a constant
   loaded from memory.  

   This is used by expand_binop to decide whether to force a constant
   into a register.  If the cost is greater than 2 and the constant
   is used within a short loop, it gets forced into a register.  
   Ideally, there should be some weighting as to how mnay times it is used
   within the loop.  */

#define SHIFT_CODE_P(C) ((C) == ASHIFT || (C) == ASHIFTRT || (C) == LSHIFTRT)

#define LOGICAL_CODE_P(C) ((C) == NOT || (C) == AND \
                           || (C) == IOR || (C) == XOR)

#define NON_COMMUTATIVE_CODE_P ((C) == MINUS || (C) == COMPARE)

#define CONST_COSTS(RTX,CODE,OUTER_CODE)			\
	case CONST_INT:						\
           if (c4x_J_constant (RTX))				\
	     return 0;						\
	   if (! TARGET_C3X					\
	       && OUTER_CODE == AND				\
               && GET_CODE (RTX) == CONST_INT			\
	       && (INTVAL (RTX) == 255 || INTVAL (RTX) == 65535))	\
	     return 0;						\
	   if (! TARGET_C3X					\
	       && (OUTER_CODE == ASHIFTRT || OUTER_CODE == LSHIFTRT)	\
               && GET_CODE (RTX) == CONST_INT			\
	       && (INTVAL (RTX) == 16 || INTVAL (RTX) == 24))	\
	     return 0;						\
           if (TARGET_C3X && SHIFT_CODE_P (OUTER_CODE))		\
	     return 3;						\
           if (LOGICAL_CODE_P (OUTER_CODE) 			\
               ? c4x_L_constant (RTX) : c4x_I_constant (RTX))	\
	     return 2;						\
	case CONST:						\
	case LABEL_REF:						\
	case SYMBOL_REF:					\
	   return 4;						\
	case CONST_DOUBLE:					\
	   if (c4x_H_constant (RTX))				\
	     return 2;						\
           if (GET_MODE (RTX) == QFmode)			\
	     return 4;						\
           else							\
	     return 8;

/* Compute the cost of an address.  This is meant to approximate the size
   and/or execution delay of an insn using that address.  If the cost is
   approximated by the RTL complexity, including CONST_COSTS above, as
   is usually the case for CISC machines, this macro should not be defined.
   For aggressively RISCy machines, only one insn format is allowed, so
   this macro should be a constant.  The value of this macro only matters
   for valid addresses.  We handle the most common address without 
   a call to c4x_address_cost.  */

#define ADDRESS_COST(ADDR) (REG_P (ADDR) ? 1 : c4x_address_cost (ADDR))

#define	CANONICALIZE_COMPARISON(CODE, OP0, OP1)		\
if (REG_P (OP1) && ! REG_P (OP0))			\
{							\
  rtx tmp = OP0; OP0 = OP1 ; OP1 = tmp;			\
  CODE = swap_condition (CODE);				\
}

#define EXT_CLASS_P(CLASS) (reg_class_subset_p (CLASS, EXT_REGS))
#define ADDR_CLASS_P(CLASS) (reg_class_subset_p (CLASS, ADDR_REGS))
#define INDEX_CLASS_P(CLASS) (reg_class_subset_p (CLASS, INDEX_REGS))
#define EXPENSIVE_CLASS_P(CLASS) (ADDR_CLASS_P(CLASS) \
                          || INDEX_CLASS_P(CLASS) || (CLASS) == SP_REG)

/* Compute extra cost of moving data between one register class
   and another.  */

#define REGISTER_MOVE_COST(FROM, TO)	2

/* Memory move cost is same as fast register move.  Maybe this should
   be bumped up? */

#define MEMORY_MOVE_COST(M,C,I)		4

/* Branches are kind of expensive (even with delayed branching) so
   make their cost higher.  */

#define BRANCH_COST			8

/* Adjust the cost of dependencies.  */

#define ADJUST_COST(INSN,LINK,DEP,COST) \
  (COST) = c4x_adjust_cost (INSN, LINK, DEP, COST)

#define	WORD_REGISTER_OPERATIONS

/* Dividing the Output into Sections.  */

#define TEXT_SECTION_ASM_OP "\t.text"

#define DATA_SECTION_ASM_OP "\t.data"

#define USE_CONST_SECTION 1

#define CONST_SECTION_ASM_OP "\t.sect\t\".const\""

/* Do not use .init section so __main will be called on startup. This will
   call __do_global_ctors and prepare for __do_global_dtors on exit.  */

#if 0
#define INIT_SECTION_ASM_OP  "\t.sect\t\".init\""
#endif

#define FINI_SECTION_ASM_OP  "\t.sect\t\".fini\""

/* Support const sections and the ctors and dtors sections for g++.
   Note that there appears to be two different ways to support const
   sections at the moment.  You can either #define the symbol
   READONLY_DATA_SECTION (giving it some code which switches to the
   readonly data section) or else you can #define the symbols
   EXTRA_SECTIONS, EXTRA_SECTION_FUNCTIONS, SELECT_SECTION, and
   SELECT_RTX_SECTION.  We do both here just to be on the safe side.  */

/* Define a few machine-specific details of the implementation of
   constructors.

   The __CTORS_LIST__ goes in the .ctors section.  Define CTOR_LIST_BEGIN
   and CTOR_LIST_END to contribute to the .ctors section an instruction to
   push a word containing 0 (or some equivalent of that).

   Define ASM_OUTPUT_CONSTRUCTOR to push the address of the constructor.  */

#define CTORS_SECTION_ASM_OP	"\t.sect\t\".ctors\""
#define DTORS_SECTION_ASM_OP    "\t.sect\t\".dtors\""

/* Constructor list on stack is in reverse order.  Go to the end of the
   list and go backwards to call constructors in the right order.  */

#define DO_GLOBAL_CTORS_BODY					\
do {								\
  extern func_ptr __CTOR_LIST__[];				\
  func_ptr *p, *beg = __CTOR_LIST__ + 1;			\
  for (p = beg; *p ; p++) ;					\
  while (p != beg)						\
    (*--p) ();							\
} while (0)

#undef EXTRA_SECTIONS
#define EXTRA_SECTIONS in_const, in_init, in_fini, in_ctors, in_dtors

#undef EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS					\
  CONST_SECTION_FUNCTION					\
  INIT_SECTION_FUNCTION						\
  FINI_SECTION_FUNCTION						\
  CTORS_SECTION_FUNCTION					\
  DTORS_SECTION_FUNCTION

#define INIT_SECTION_FUNCTION					\
void								\
init_section ()							\
{								\
  if (in_section != in_init)					\
    {								\
      fprintf (asm_out_file, ";\t.init\n");			\
      in_section = in_init;					\
    }								\
}

#define FINI_SECTION_FUNCTION					\
void								\
fini_section ()							\
{								\
  if (in_section != in_fini)					\
    {								\
      fprintf (asm_out_file, "\t%s\n", FINI_SECTION_ASM_OP);	\
      in_section = in_fini;					\
    }								\
}

#define READONLY_DATA_SECTION() const_section ()

#define CONST_SECTION_FUNCTION						\
void									\
const_section ()							\
{									\
  if (! USE_CONST_SECTION)						\
    text_section();							\
  else if (in_section != in_const)					\
    {									\
      fprintf (asm_out_file, "%s\n", CONST_SECTION_ASM_OP);		\
      in_section = in_const;						\
    }									\
}

#define ASM_STABS_OP "\t.stabs"

/* The ctors and dtors sections are not normally put into use 
   by EXTRA_SECTIONS and EXTRA_SECTION_FUNCTIONS as defined in svr3.h,
   but it can't hurt to define these macros for whatever systems use them.  */

#define CTORS_SECTION_FUNCTION						\
void									\
ctors_section ()							\
{									\
  if (in_section != in_ctors)						\
    {									\
      fprintf (asm_out_file, "%s\n", CTORS_SECTION_ASM_OP);		\
      in_section = in_ctors;						\
    }									\
}

#define DTORS_SECTION_FUNCTION						\
void									\
dtors_section ()							\
{									\
  if (in_section != in_dtors)						\
    {									\
      fprintf (asm_out_file, "%s\n", DTORS_SECTION_ASM_OP);		\
      in_section = in_dtors;						\
    }									\
}

#define ASM_OUTPUT_SECTION_NAME(FILE, DECL, NAME, RELOC) \
   fprintf (FILE, "\t.sect\t\"%s\"\n", NAME);

/* This is machine-dependent because it needs to push something
   on the stack.  */

/* A C statement (sans semicolon) to output an element in the table of
   global constructors.  */
#define ASM_OUTPUT_CONSTRUCTOR(FILE,NAME)				\
  do {									\
    ctors_section ();							\
    fprintf (FILE, "\t.word\t ");					\
    assemble_name (FILE, NAME);						\
    fprintf (FILE, "\n");						\
  } while (0)

/* A C statement (sans semicolon) to output an element in the table of
   global destructors.  */
#define ASM_OUTPUT_DESTRUCTOR(FILE,NAME)       				\
  do {									\
    dtors_section ();                   				\
    fprintf (FILE, "\t.word\t ");					\
    assemble_name (FILE, NAME);              				\
    fprintf (FILE, "\n");						\
  } while (0)

/* A C statement or statements to switch to the appropriate
   section for output of DECL.  DECL is either a `VAR_DECL' node
   or a constant of some sort.  RELOC indicates whether forming
   the initial value of DECL requires link-time relocations.  */

#define SELECT_SECTION(DECL, RELOC)					\
{									\
  if (TREE_CODE (DECL) == STRING_CST)					\
    {									\
      if (! flag_writable_strings)					\
	const_section ();						\
      else								\
	data_section ();						\
    }									\
  else if (TREE_CODE (DECL) == VAR_DECL)				\
    {									\
      if ((0 && RELOC)	/* should be (flag_pic && RELOC) */		\
	  || ! TREE_READONLY (DECL) || TREE_SIDE_EFFECTS (DECL)		\
	  || ! DECL_INITIAL (DECL)					\
	  || (DECL_INITIAL (DECL) != error_mark_node			\
	      && ! TREE_CONSTANT (DECL_INITIAL (DECL))))		\
	data_section ();						\
      else								\
	const_section ();						\
    }									\
  else									\
    const_section ();							\
}

/* A C statement or statements to switch to the appropriate
   section for output of RTX in mode MODE.  RTX is some kind
   of constant in RTL.  The argument MODE is redundant except
   in the case of a `const_int' rtx.  Currently, these always
   go into the const section.  */

#define SELECT_RTX_SECTION(MODE, RTX) const_section()


/* Overall Framework of an Assembler File  */

#define ASM_FILE_START(FILE)					\
{								\
    int dspversion = 0;						\
    if (TARGET_C30) dspversion = 30;				\
    if (TARGET_C31) dspversion = 31;				\
    if (TARGET_C32) dspversion = 32;				\
    if (TARGET_C40) dspversion = 40;				\
    if (TARGET_C44) dspversion = 44;				\
    fprintf (FILE, "\t.version\t%d\n", dspversion);		\
    fprintf (FILE, "\t.file\t");				\
    if (TARGET_TI)						\
      {								\
        char *p;						\
        char *after_dir = main_input_filename;			\
	for (p = main_input_filename; *p; p++)			\
	  if (*p == '/')					\
	    after_dir = p + 1;					\
	output_quoted_string (FILE, after_dir);			\
      }								\
    else							\
      output_quoted_string (FILE, main_input_filename);		\
    fprintf (FILE, "\n");					\
}

#define ASM_FILE_END(FILE) fprintf (FILE, "\t.end\n")

/* We need to have a data section we can identify so that we can set
   the DP register back to a data pointer in the small memory model.
   This is only required for ISRs if we are paranoid that someone
   may have quietly changed this register on the sly.  */

#define ASM_IDENTIFY_GCC(FILE) \
    if (! TARGET_TI) fputs ("gcc2_compiled.:\n", FILE);	\
      fputs ("\t.data\ndata_sec:\n", FILE);

#define ASM_COMMENT_START ";"

#define ASM_APP_ON ""
#define ASM_APP_OFF ""

/* Output float/double constants  QFmode.  */

#define ASM_OUTPUT_BYTE_FLOAT(FILE, VALUE)		\
  do {							\
    long l;						\
    char str[30];					\
    REAL_VALUE_TO_TARGET_SINGLE (VALUE, l);		\
    REAL_VALUE_TO_DECIMAL (VALUE, "%20lf", str);	\
    if (sizeof (int) == sizeof (long))			\
      fprintf (FILE, "\t.word\t0%08xh\t; %s\n", l, str);\
    else						\
      fprintf (FILE, "\t.word\t0%08lxh\t; %s\n", l, str);\
  } while (0);

/* Output long double constants  HFmode. 
   The first word contains the exponent and first part of the mantissa
   in the same manner as QFmode.  The second word contains the full
   mantissa.  We should ensure that the two words are allocated within
   the same page for the large memory model since we only output a single
   LDP instruction.  FIXME.  The simplest solution probably is to output
   a LDP for each load.  */

#define ASM_OUTPUT_SHORT_FLOAT(FILE, VALUE)		\
  do {							\
    long l[2];						\
    char str[30];					\
    REAL_VALUE_TO_TARGET_DOUBLE (VALUE, l);		\
    REAL_VALUE_TO_DECIMAL (VALUE, "%20lf", str);	\
    l[1] = (l[0] << 8) | ((l[1] >> 24) & 0xff);		\
    if (sizeof (int) == sizeof (long))			\
      fprintf (FILE, "\t.word\t0%08xh\t; %s\n\t.word\t0%08xh\n", \
               l[0], str, l[1]);				\
    else							\
      fprintf (FILE, "\t.word\t0%08lxh\t; %s\n\t.word\t0%08lxh\n", \
               l[0], str, l[1]);				\
  } while (0);

#define ASM_OUTPUT_CHAR(FILE, VALUE)			\
  do {							\
    fprintf (FILE, "\t.word\t");			\
     output_addr_const (FILE, VALUE);			\
     if (GET_CODE (VALUE) != SYMBOL_REF)		\
       fprintf (FILE, " ; 0%08xh\n", INTVAL (VALUE));	\
     else						\
       fputc ('\n', FILE);				\
  } while (0);

#define ASM_OUTPUT_BYTE(FILE, VALUE)  \
  fprintf (FILE, "\t.word\t0%xh\n", (VALUE))

#define ASM_OUTPUT_ASCII(FILE, PTR, LEN) c4x_output_ascii (FILE, PTR, LEN)

#define ASM_OPEN_PAREN "("
#define ASM_CLOSE_PAREN ")"


/* Output and Generation of Labels  */

#define NO_DOT_IN_LABEL		/* Only required for TI format */

#define ASM_OUTPUT_LABEL(FILE, NAME)	\
do { assemble_name (FILE, NAME); fputs (":\n", FILE); } while (0);

#define ASM_GLOBALIZE_LABEL(FILE, NAME) \
  do {                                  \
    fprintf (FILE, "\t.global\t");	\
    assemble_name (FILE, NAME);		\
    fputs ("\n", FILE); 	        \
  } while (0);

#define ASM_OUTPUT_EXTERNAL(FILE, DECL, NAME)	\
  do {                                         	\
    fprintf (FILE, "\t.ref\t");			\
    assemble_name (FILE, NAME);	             	\
    fputc ('\n', FILE);  	               	\
  } while (0);

/* A C statement to output on FILE an assembler pseudo-op to
   declare a library function named external.
   (Only needed to keep asm30 happy for ___divqf3 etc.)  */

#define ASM_OUTPUT_EXTERNAL_LIBCALL(FILE, FUN)  \
  do {						\
    fprintf (FILE, "\t.ref\t");			\
    assemble_name (FILE, XSTR (FUN, 0));	\
    fprintf (FILE, "\n");			\
  } while (0);

/* The prefix to add to user-visible assembler symbols.  */

#define USER_LABEL_PREFIX "_"

/* This is how to output an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#define ASM_OUTPUT_INTERNAL_LABEL(FILE, PREFIX, NUM)	\
asm_fprintf (FILE, "%s%d:\n", PREFIX, NUM)

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#define ASM_GENERATE_INTERNAL_LABEL(BUFFER, PREFIX, NUM) \
    sprintf (BUFFER, "*%s%d", PREFIX, NUM)

/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */

#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)  \
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 10),    \
  sprintf ((OUTPUT), "%s%d", (NAME), (LABELNO)))


/* Output of Dispatch Tables  */

/* This is how to output an element of a case-vector that is absolute.  */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE) \
    fprintf (FILE, "\t.long\tL%d\n", VALUE);

/* This is how to output an element of a case-vector that is relative.  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL) \
    fprintf (FILE, "\t.long\tL%d-L%d\n", VALUE, REL);

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "long int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

#define INT_TYPE_SIZE		32
#define LONG_LONG_TYPE_SIZE	64
#define FLOAT_TYPE_SIZE		32
#define DOUBLE_TYPE_SIZE	32
#define LONG_DOUBLE_TYPE_SIZE	64 /* actually only 40 */

/* Allow #sccs in preprocessor.  */

#define SCCS_DIRECTIVE

/* Output #ident as a .ident.  */

#define ASM_OUTPUT_IDENT(FILE, NAME) \
  fprintf (FILE, "\t.ident \"%s\"\n", NAME);

#define CPP_PREDEFINES ""

/* Output of Uninitialized Variables  */

/* This says how to output an assembler line to define a local
   uninitialized variable.  */

#undef ASM_OUTPUT_LOCAL
#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)  \
( fputs ("\t.bss\t", FILE),			\
  assemble_name (FILE, (NAME)),		\
  fprintf (FILE, ",%u\n", (ROUNDED)))

/* This says how to output an assembler line to define a global
   uninitialized variable.  */

#undef ASM_OUTPUT_COMMON
#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)  \
(  fputs ("\t.globl\t", FILE),	\
   assemble_name (FILE, (NAME)),	\
   fputs ("\n\t.bss\t", FILE),	\
   assemble_name (FILE, (NAME)),	\
   fprintf (FILE, ",%u\n", (ROUNDED)))

/* Macros Controlling Initialization Routines  */

#define OBJECT_FORMAT_COFF
#define REAL_NM_FILE_NAME "c4x-nm"

/* Output of Assembler Instructions  */

/* Register names when used for integer modes.  */

#define REGISTER_NAMES \
{								\
 "r0",   "r1", "r2",   "r3",  "r4",  "r5",  "r6",  "r7",	\
 "ar0", "ar1", "ar2", "ar3", "ar4", "ar5", "ar6", "ar7",	\
 "dp",  "ir0", "ir1",  "bk",  "sp",  "st", "die", "iie",	\
 "iif",	 "rs",  "re",  "rc",  "r8",  "r9", "r10", "r11"		\
}

/* Alternate register names when used for floating point modes.  */

#define FLOAT_REGISTER_NAMES \
{								\
 "f0",   "f1", "f2",   "f3",  "f4",  "f5",  "f6",  "f7",	\
 "ar0", "ar1", "ar2", "ar3", "ar4", "ar5", "ar6", "ar7",	\
 "dp",  "ir0", "ir1",  "bk",  "sp",  "st", "die", "iie",	\
 "iif",	 "rs",  "re",  "rc",  "f8",  "f9", "f10", "f11"		\
}

#define PRINT_OPERAND(FILE, X, CODE) c4x_print_operand(FILE, X, CODE)

/* Determine which codes are valid without a following integer.  These must
   not be alphabetic.  */

#define PRINT_OPERAND_PUNCT_VALID_P(CODE) ((CODE) == '#')

#define PRINT_OPERAND_ADDRESS(FILE, X) c4x_print_operand_address(FILE, X)

/* Define this macro if you want to implement any pragmas.  If defined, it
   should be a C expression to be executed when #pragma is seen.  The
   argument STREAM is the stdio input stream from which the source
   text can be read.  CH is the first character after the #pragma.  The
   result of the expression is the terminating character found
   (newline or EOF).  */
#define HANDLE_PRAGMA(GETC, UNGETC, NAME) \
  c4x_handle_pragma (GETC, UNGETC, NAME)

#define SET_DEFAULT_DECL_ATTRIBUTES(DECL, ATTRIBUTES) \
  c4x_set_default_attributes (DECL, &ATTRIBUTES)

#define VALID_MACHINE_TYPE_ATTRIBUTE(TYPE, ATTRIBUTES, NAME, ARGS) \
  (c4x_valid_type_attribute_p (TYPE, ATTRIBUTES, NAME, ARGS))

/* Assembler Commands for Alignment  */

#define ASM_OUTPUT_SKIP(FILE, SIZE) \
{ int c = SIZE; \
  for (; c > 0; --c) \
   fprintf (FILE,"\t.word\t0\n"); \
}

#define ASM_NO_SKIP_IN_TEXT 1

/* I'm not sure about this one.  FIXME.  */

#define ASM_OUTPUT_ALIGN(FILE, LOG)	\
  if ((LOG) != 0)			\
    fprintf (FILE, "\t.align\t%d\n", (1 << (LOG)))


/* Macros for SDB and DWARF Output  (use .sdef instead of .def
   to avoid conflict with TI's use of .def)  */

#define SDB_DELIM "\n"
#define SDB_DEBUGGING_INFO

#define PUT_SDB_DEF(A)				\
do { fprintf (asm_out_file, "\t.sdef\t");	\
     ASM_OUTPUT_LABELREF (asm_out_file, A); 	\
     fprintf (asm_out_file, SDB_DELIM); } while (0)

#define PUT_SDB_PLAIN_DEF(A)			\
  fprintf (asm_out_file,"\t.sdef\t.%s%s", A, SDB_DELIM)

#define PUT_SDB_BLOCK_START(LINE)		\
  fprintf (asm_out_file,			\
	   "\t.sdef\t.bb%s\t.val\t.%s\t.scl\t100%s\t.line\t%d%s\t.endef\n", \
	   SDB_DELIM, SDB_DELIM, SDB_DELIM, (LINE), SDB_DELIM)

#define PUT_SDB_BLOCK_END(LINE)			\
  fprintf (asm_out_file,			\
	   "\t.sdef\t.eb%s\t.val\t.%s\t.scl\t100%s\t.line\t%d%s\t.endef\n", \
	   SDB_DELIM, SDB_DELIM, SDB_DELIM, (LINE), SDB_DELIM)

#define PUT_SDB_FUNCTION_START(LINE)		\
  fprintf (asm_out_file,			\
	   "\t.sdef\t.bf%s\t.val\t.%s\t.scl\t101%s\t.line\t%d%s\t.endef\n", \
	   SDB_DELIM, SDB_DELIM, SDB_DELIM, (LINE), SDB_DELIM)

#define PUT_SDB_FUNCTION_END(LINE)		\
  fprintf (asm_out_file,			\
	   "\t.sdef\t.ef%s\t.val\t.%s\t.scl\t101%s\t.line\t%d%s\t.endef\n", \
	   SDB_DELIM, SDB_DELIM, SDB_DELIM, (LINE), SDB_DELIM)

#define PUT_SDB_EPILOGUE_END(NAME)			\
do { fprintf (asm_out_file, "\t.sdef\t");		\
     ASM_OUTPUT_LABELREF (asm_out_file, NAME);		\
     fprintf (asm_out_file,				\
	      "%s\t.val\t.%s\t.scl\t-1%s\t.endef\n",	\
	      SDB_DELIM, SDB_DELIM, SDB_DELIM); } while (0)


/* Define results of standard character escape sequences.  */

#define TARGET_BELL 007
#define TARGET_BS 010
#define TARGET_TAB 011
#define TARGET_NEWLINE 012
#define TARGET_VT 013
#define TARGET_FF 014
#define TARGET_CR 015

/* This is the kind of divide that is easiest to do in the general case.  */

#define EASY_DIV_EXPR TRUNC_DIV_EXPR

/* Define this as 1 if `char' should by default be signed; else as 0.  */

#define DEFAULT_SIGNED_CHAR 1

/* A function address in a call instruction is a byte address (for
   indexing purposes) so give the MEM rtx a byte's mode.  */

#define FUNCTION_MODE QImode

#define SLOW_BYTE_ACCESS 0

/* Specify the machine mode that pointers have.  After generation of
   RTL, the compiler makes no further distinction between pointers and
   any other objects of this machine mode.  */

#define Pmode QImode

/* On the C4x we can write the following code. We have to clear the cache
   every time we execute it because the data in the stack could change.

   laj   $+4
   addi3 4,r11,ar0
   lda   *ar0,ar1
   lda   *+ar0(1),ar0
   bud   ar1
   nop
   nop
   or   1000h,st
   .word FNADDR
   .word CXT

   On the c3x this is a bit more difficult. We have to write self
   modifying code here. So we have to clear the cache every time
   we execute it because the data in the stack could change.

   ldiu TOP_OF_FUNCTION,ar1
   lsh  16,ar1
   or   BOTTOM_OF_FUNCTION,ar1
   ldiu TOP_OF_STATIC,ar0
   bud  ar1
   lsh  16,ar0
   or   BOTTOM_OF_STATIC,ar0
   or   1000h,st
   
  */

#define TRAMPOLINE_SIZE (TARGET_C3X ? 8 : 10)

#define TRAMPOLINE_TEMPLATE(FILE)				\
{								\
  if (TARGET_C3X)						\
    {								\
      asm_fprintf (FILE, "\tldiu\t0,ar1\n");			\
      asm_fprintf (FILE, "\tlsh\t16,ar1\n");			\
      asm_fprintf (FILE, "\tor\t0,ar1\n");			\
      asm_fprintf (FILE, "\tldiu\t0,ar0\n");			\
      asm_fprintf (FILE, "\tbud\tar1\n");			\
      asm_fprintf (FILE, "\tlsh\t16,ar0\n");			\
      asm_fprintf (FILE, "\tor\t0,ar0\n");			\
      asm_fprintf (FILE, "\tor\t1000h,st\n");			\
    }								\
  else								\
    {								\
      asm_fprintf (FILE, "\tlaj\t$+4\n");			\
      asm_fprintf (FILE, "\taddi3\t4,r11,ar0\n");		\
      asm_fprintf (FILE, "\tlda\t*ar0,ar1\n");			\
      asm_fprintf (FILE, "\tlda\t*+ar0(1),ar0\n");		\
      asm_fprintf (FILE, "\tbud\tar1\n");			\
      asm_fprintf (FILE, "\tnop\n");				\
      asm_fprintf (FILE, "\tnop\n");				\
      asm_fprintf (FILE, "\tor\t1000h,st\n");			\
      asm_fprintf (FILE, "\t.word\t0\n");			\
      asm_fprintf (FILE, "\t.word\t0\n");			\
    }								\
}

#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT)			\
{									\
  if (TARGET_C3X)							\
    {									\
      rtx tmp1, tmp2;							\
      tmp1 = expand_shift (RSHIFT_EXPR, QImode, FNADDR,			\
			   size_int (16), 0, 1);			\
      tmp2 = expand_shift (LSHIFT_EXPR, QImode,				\
			   gen_rtx (CONST_INT, VOIDmode, 0x5069),	\
			   size_int (16), 0, 1);			\
      emit_insn (gen_iorqi3 (tmp1, tmp1, tmp2));			\
      emit_move_insn (gen_rtx (MEM, QImode,				\
			       plus_constant (tramp, 0)), tmp1);	\
      tmp1 = expand_and (FNADDR, gen_rtx (CONST_INT, VOIDmode,		\
					  0xffff), 0);			\
      tmp2 = expand_shift (LSHIFT_EXPR, QImode,				\
			   gen_rtx (CONST_INT, VOIDmode, 0x1069),	\
			   size_int (16), 0, 1);			\
      emit_insn (gen_iorqi3 (tmp1, tmp1, tmp2));			\
      emit_move_insn (gen_rtx (MEM, QImode,				\
			       plus_constant (tramp, 2)), tmp1);	\
      tmp1 = expand_shift (RSHIFT_EXPR, QImode, CXT,			\
			   size_int (16), 0, 1);			\
      tmp2 = expand_shift (LSHIFT_EXPR, QImode,				\
			   gen_rtx (CONST_INT, VOIDmode, 0x5068),	\
			   size_int (16), 0, 1);			\
      emit_insn (gen_iorqi3 (tmp1, tmp1, tmp2));			\
      emit_move_insn (gen_rtx (MEM, QImode,				\
			       plus_constant (tramp, 3)), tmp1);	\
      tmp1 = expand_and (CXT, gen_rtx (CONST_INT, VOIDmode,		\
				       0xffff), 0);			\
      tmp2 = expand_shift (LSHIFT_EXPR, QImode,				\
			   gen_rtx (CONST_INT, VOIDmode, 0x1068),	\
			   size_int (16), 0, 1);			\
      emit_insn (gen_iorqi3 (tmp1, tmp1, tmp2));			\
      emit_move_insn (gen_rtx (MEM, QImode,				\
			       plus_constant (tramp, 6)), tmp1);	\
    }									\
  else									\
    {									\
      emit_move_insn (gen_rtx (MEM, QImode,				\
			       plus_constant (TRAMP, 8)), FNADDR); 	\
      emit_move_insn (gen_rtx (MEM, QImode,				\
			       plus_constant (TRAMP, 9)), CXT); 	\
    }									\
}

/* Specify the machine mode that this machine uses for the index in
   the tablejump instruction.  */

#define CASE_VECTOR_MODE Pmode

/* Max number of (32-bit) bytes we can move from memory to memory
   in one reasonably fast instruction.  */

#define MOVE_MAX 1

/* MOVE_RATIO is the number of move instructions that is better than a
   block move.  */

#define MOVE_RATIO 2		/* Default value */

#define BSS_SECTION_ASM_OP ".bss"

#define ASM_OUTPUT_REG_PUSH(FILE, REGNO)  \
  asm_fprintf (FILE, "\tpush\t%s\n", reg_names[REGNO])

/* This is how to output an insn to pop a register from the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_POP(FILE, REGNO)  \
  asm_fprintf (FILE, "\tpop\t%s\n", reg_names[REGNO])

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */

#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/* We need to use direct addressing for large constants and addresses
   that cannot fit within an instruction.  We must check for these
   after after the final jump optimisation pass, since this may
   introduce a local_move insn for a SYMBOL_REF.  This pass
   must come before delayed branch slot filling since it can generate
   additional instructions.  */

#define MACHINE_DEPENDENT_REORG(INSNS) c4x_process_after_reload(INSNS)

#define DBR_OUTPUT_SEQEND(FILE)		\
if (final_sequence != NULL_RTX)		\
{					\
 int count;				\
 int laj = GET_CODE (XVECEXP (final_sequence, 0, 0)) == CALL_INSN; \
					\
 count = dbr_sequence_length();		\
 while (count < (laj ? 2 : 3))		\
 {					\
    fputs("\tnop\n", FILE);		\
    count++;				\
 }					\
 if (laj)				\
    fputs("\tpush\tr11\n", FILE);	\
}

#define NO_FUNCTION_CSE

/* We don't want a leading tab.  */

#define ASM_OUTPUT_ASM(FILE, STRING) fprintf (FILE, "%s\n", STRING)

/* Define the codes that are matched by predicates in c4x.c.  */

#define PREDICATE_CODES						\
  {"fp_zero_operand", {CONST_DOUBLE}},				\
  {"const_operand", {CONST_INT, CONST_DOUBLE}},			\
  {"stik_const_operand", {CONST_INT}},				\
  {"not_const_operand", {CONST_INT}},				\
  {"reg_operand", {REG, SUBREG}},				\
  {"reg_or_const_operand", {REG, SUBREG, CONST_INT, CONST_DOUBLE}},\
  {"r0r1_reg_operand", {REG, SUBREG}},				\
  {"r2r3_reg_operand", {REG, SUBREG}},				\
  {"ext_low_reg_operand", {REG, SUBREG}},			\
  {"ext_reg_operand", {REG, SUBREG}},				\
  {"std_reg_operand", {REG, SUBREG}},				\
  {"addr_reg_operand", {REG, SUBREG}},				\
  {"index_reg_operand", {REG, SUBREG}},				\
  {"dp_reg_operand", {REG}},					\
  {"sp_reg_operand", {REG}},					\
  {"st_reg_operand", {REG}},					\
  {"rc_reg_operand", {REG}},					\
  {"call_address_operand", {REG, SYMBOL_REF, LABEL_REF, CONST}}, \
  {"dst_operand", {SUBREG, REG, MEM}}, \
  {"src_operand", {SUBREG, REG, MEM, CONST_INT, CONST_DOUBLE}}, \
  {"src_hi_operand", {SUBREG, REG, MEM, CONST_DOUBLE}}, 	\
  {"lsrc_operand", {SUBREG, REG, MEM, CONST_INT, CONST_DOUBLE}}, \
  {"tsrc_operand", {SUBREG, REG, MEM, CONST_INT, CONST_DOUBLE}}, \
  {"any_operand", {SUBREG, REG, MEM, CONST_INT, CONST_DOUBLE}}, \
  {"par_ind_operand", {MEM}},					\
  {"parallel_operand", {SUBREG, REG, MEM}},			\
  {"symbolic_address_operand", {SYMBOL_REF, LABEL_REF, CONST}},	\
  {"mem_operand", {MEM}},					


/* Variables in c4x.c */

extern enum reg_class c4x_regclass_map[];/* smallest class containing REGNO */
extern enum machine_mode c4x_caller_save_map[];

extern struct rtx_def *c4x_compare_op0;	/* operand 0 for comparisons */
extern struct rtx_def *c4x_compare_op1;	/* operand 1 for comparisons */

extern int c4x_rpts_cycles;	        /* max cycles for RPTS */
extern int c4x_cpu_version;		/* cpu version C30/31/32/40/44 */
