/* Output routines for GCC for ARM.
   Copyright (C) 1991, 93, 94, 95, 96, 97, 98, 99, 2000 Free Software Foundation, Inc.
   Contributed by Pieter `Tiggr' Schoenmakers (rcpieter@win.tue.nl)
   and Martin Simmons (@harleqn.co.uk).
   More major hacks by Richard Earnshaw (rearnsha@arm.com).

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
    
#include "config.h"
#include "system.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "reload.h"
#include "tree.h"
#include "function.h"
#include "expr.h"
#include "toplev.h"
#include "recog.h"
#include "ggc.h"
#include "arm-protos.h"

/* The maximum number of insns skipped which will be conditionalised if
   possible.  */
static int max_insns_skipped = 5;

extern FILE * asm_out_file;
/* Some function declarations.  */

#ifndef Mmode
#define Mmode enum machine_mode
#endif

static HOST_WIDE_INT int_log2 		PARAMS ((HOST_WIDE_INT));
static char * output_multi_immediate 	PARAMS ((rtx *, char *, char *, int, HOST_WIDE_INT));
static int arm_gen_constant 		PARAMS ((enum rtx_code, Mmode, HOST_WIDE_INT, rtx, rtx, int, int));
static int arm_naked_function_p 	PARAMS ((tree));
static void init_fpa_table 		PARAMS ((void));
static enum machine_mode select_dominance_cc_mode PARAMS ((rtx, rtx, HOST_WIDE_INT));
static HOST_WIDE_INT add_minipool_constant PARAMS ((rtx, Mmode));
static void dump_minipool 		PARAMS ((rtx));
static rtx find_barrier 		PARAMS ((rtx, int));
static void push_minipool_fix 		PARAMS ((rtx, int, rtx *, Mmode, rtx));
static void push_minipool_barrier 	PARAMS ((rtx, int));
static void note_invalid_constants 	PARAMS ((rtx, int));
static char * fp_const_from_val 	PARAMS ((REAL_VALUE_TYPE *));
static int eliminate_lr2ip 		PARAMS ((rtx *));
static char * shift_op 			PARAMS ((rtx, HOST_WIDE_INT *));
static int pattern_really_clobbers_lr 	PARAMS ((rtx));
static int function_really_clobbers_lr 	PARAMS ((rtx));
static void emit_multi_reg_push 	PARAMS ((int));
static void emit_sfm 			PARAMS ((int, int));
static enum arm_cond_code get_arm_condition_code PARAMS ((rtx));
static int const_ok_for_op 		PARAMS ((HOST_WIDE_INT, enum rtx_code));
static void arm_add_gc_roots 		PARAMS ((void));

/* True if we are currently building a constant table. */
int making_const_table;

/* Define the information needed to generate branch insns.  This is
   stored from the compare operation. */
rtx arm_compare_op0, arm_compare_op1;

/* What type of floating point are we tuning for? */
enum floating_point_type arm_fpu;

/* What type of floating point instructions are available? */
enum floating_point_type arm_fpu_arch;

/* What program mode is the cpu running in? 26-bit mode or 32-bit mode */
enum prog_mode_type arm_prgmode;

/* Set by the -mfp=... option */
const char * target_fp_name = NULL;

/* Used to parse -mstructure_size_boundary command line option.  */
const char * structure_size_string = NULL;
int    arm_structure_size_boundary = DEFAULT_STRUCTURE_SIZE_BOUNDARY;

/* Bit values used to identify processor capabilities.  */
#define FL_CO_PROC    (1 << 0)        /* Has external co-processor bus */
#define FL_FAST_MULT  (1 << 1)        /* Fast multiply */
#define FL_MODE26     (1 << 2)        /* 26-bit mode support */
#define FL_MODE32     (1 << 3)        /* 32-bit mode support */
#define FL_ARCH4      (1 << 4)        /* Architecture rel 4 */
#define FL_ARCH5      (1 << 5)        /* Architecture rel 5 */
#define FL_THUMB      (1 << 6)        /* Thumb aware */
#define FL_LDSCHED    (1 << 7)	      /* Load scheduling necessary */
#define FL_STRONG     (1 << 8)	      /* StrongARM */

/* The bits in this mask specify which instructions we are allowed to
   generate.  */
static int insn_flags = 0;
/* The bits in this mask specify which instruction scheduling options should
   be used.  Note - there is an overlap with the FL_FAST_MULT.  For some
   hardware we want to be able to generate the multiply instructions, but to
   tune as if they were not present in the architecture.  */
static int tune_flags = 0;

/* The following are used in the arm.md file as equivalents to bits
   in the above two flag variables.  */

/* Nonzero if this is an "M" variant of the processor.  */
int arm_fast_multiply = 0;

/* Nonzero if this chip supports the ARM Architecture 4 extensions */
int arm_arch4 = 0;

/* Nonzero if this chip supports the ARM Architecture 5 extensions */
int arm_arch5 = 0;

/* Nonzero if this chip can benefit from load scheduling.  */
int arm_ld_sched = 0;

/* Nonzero if this chip is a StrongARM.  */
int arm_is_strong = 0;

/* Nonzero if this chip is a an ARM6 or an ARM7.  */
int arm_is_6_or_7 = 0;

/* In case of a PRE_INC, POST_INC, PRE_DEC, POST_DEC memory reference, we
   must report the mode of the memory reference from PRINT_OPERAND to
   PRINT_OPERAND_ADDRESS.  */
enum machine_mode output_memory_reference_mode;

/* Nonzero if the prologue must setup `fp'.  */
int current_function_anonymous_args;

/* The register number to be used for the PIC offset register.  */
const char * arm_pic_register_string = NULL;
int arm_pic_register = 9;

/* Set to one if we think that lr is only saved because of subroutine calls,
   but all of these can be `put after' return insns */
int lr_save_eliminated;

/* Set to 1 when a return insn is output, this means that the epilogue
   is not needed. */
static int return_used_this_function;

/* Set to 1 after arm_reorg has started.  Reset to start at the start of
   the next function.  */
static int after_arm_reorg = 0;

/* The maximum number of insns to be used when loading a constant.  */
static int arm_constant_limit = 3;

/* For an explanation of these variables, see final_prescan_insn below.  */
int arm_ccfsm_state;
enum arm_cond_code arm_current_cc;
rtx arm_target_insn;
int arm_target_label;

/* The condition codes of the ARM, and the inverse function.  */
char * arm_condition_codes[] =
{
  "eq", "ne", "cs", "cc", "mi", "pl", "vs", "vc",
  "hi", "ls", "ge", "lt", "gt", "le", "al", "nv"
};

static enum arm_cond_code get_arm_condition_code ();

#define streq(string1, string2) (strcmp (string1, string2) == 0)

/* Initialization code */

struct processors
{
  char *       name;
  unsigned int flags;
};

/* Not all of these give usefully different compilation alternatives,
   but there is no simple way of generalizing them.  */
static struct processors all_cores[] =
{
  /* ARM Cores */
  
  {"arm2",	FL_CO_PROC | FL_MODE26 },
  {"arm250",	FL_CO_PROC | FL_MODE26 },
  {"arm3",	FL_CO_PROC | FL_MODE26 },
  {"arm6",	FL_CO_PROC | FL_MODE26 | FL_MODE32 },
  {"arm60",	FL_CO_PROC | FL_MODE26 | FL_MODE32 },
  {"arm600",	FL_CO_PROC | FL_MODE26 | FL_MODE32 },
  {"arm610",	             FL_MODE26 | FL_MODE32 },
  {"arm620",	FL_CO_PROC | FL_MODE26 | FL_MODE32 },
  {"arm7",	FL_CO_PROC | FL_MODE26 | FL_MODE32 },
  /* arm7m doesn't exist on its own, but only with D, (and I), but
   those don't alter the code, so arm7m is sometimes used.  */
  {"arm7m",	FL_CO_PROC | FL_MODE26 | FL_MODE32 | FL_FAST_MULT },
  {"arm7d",	FL_CO_PROC | FL_MODE26 | FL_MODE32 },
  {"arm7dm",	FL_CO_PROC | FL_MODE26 | FL_MODE32 | FL_FAST_MULT },
  {"arm7di",	FL_CO_PROC | FL_MODE26 | FL_MODE32 },
  {"arm7dmi",	FL_CO_PROC | FL_MODE26 | FL_MODE32 | FL_FAST_MULT },
  {"arm70",	FL_CO_PROC | FL_MODE26 | FL_MODE32 },
  {"arm700",	FL_CO_PROC | FL_MODE26 | FL_MODE32 },
  {"arm700i",	FL_CO_PROC | FL_MODE26 | FL_MODE32 },
  {"arm710",	             FL_MODE26 | FL_MODE32 },
  {"arm720",	             FL_MODE26 | FL_MODE32 },
  {"arm710c",	             FL_MODE26 | FL_MODE32 },
  {"arm7100",	             FL_MODE26 | FL_MODE32 },
  {"arm7500",	             FL_MODE26 | FL_MODE32 },
  /* Doesn't have an external co-proc, but does have embedded fpu.  */
  {"arm7500fe",	FL_CO_PROC | FL_MODE26 | FL_MODE32 },
  {"arm7tdmi",	FL_CO_PROC |             FL_MODE32 | FL_FAST_MULT | FL_ARCH4 | FL_THUMB },
  {"arm8",	             FL_MODE26 | FL_MODE32 | FL_FAST_MULT | FL_ARCH4 |            FL_LDSCHED },
  {"arm810",	             FL_MODE26 | FL_MODE32 | FL_FAST_MULT | FL_ARCH4 |            FL_LDSCHED },
  {"arm9",	                         FL_MODE32 | FL_FAST_MULT | FL_ARCH4 | FL_THUMB | FL_LDSCHED },
  {"arm920",	                         FL_MODE32 | FL_FAST_MULT | FL_ARCH4 |            FL_LDSCHED },
  {"arm920t",	                         FL_MODE32 | FL_FAST_MULT | FL_ARCH4 | FL_THUMB | FL_LDSCHED },
  {"arm9tdmi",	                         FL_MODE32 | FL_FAST_MULT | FL_ARCH4 | FL_THUMB | FL_LDSCHED },
  {"strongarm",	             FL_MODE26 | FL_MODE32 | FL_FAST_MULT | FL_ARCH4 |            FL_LDSCHED | FL_STRONG },
  {"strongarm110",           FL_MODE26 | FL_MODE32 | FL_FAST_MULT | FL_ARCH4 |            FL_LDSCHED | FL_STRONG },
  {"strongarm1100",          FL_MODE26 | FL_MODE32 | FL_FAST_MULT | FL_ARCH4 |            FL_LDSCHED | FL_STRONG },
  
  {NULL, 0}
};

static struct processors all_architectures[] =
{
  /* ARM Architectures */
  
  { "armv2",     FL_CO_PROC | FL_MODE26 },
  { "armv2a",    FL_CO_PROC | FL_MODE26 },
  { "armv3",     FL_CO_PROC | FL_MODE26 | FL_MODE32 },
  { "armv3m",    FL_CO_PROC | FL_MODE26 | FL_MODE32 | FL_FAST_MULT },
  { "armv4",     FL_CO_PROC | FL_MODE26 | FL_MODE32 | FL_FAST_MULT | FL_ARCH4 },
  /* Strictly, FL_MODE26 is a permitted option for v4t, but there are no
     implementations that support it, so we will leave it out for now.  */
  { "armv4t",    FL_CO_PROC |             FL_MODE32 | FL_FAST_MULT | FL_ARCH4 | FL_THUMB },
  { "armv5",     FL_CO_PROC |             FL_MODE32 | FL_FAST_MULT | FL_ARCH4 | FL_THUMB | FL_ARCH5 },
  { NULL, 0 }
};

/* This is a magic stucture.  The 'string' field is magically filled in
   with a pointer to the value specified by the user on the command line
   assuming that the user has specified such a value.  */

struct arm_cpu_select arm_select[] =
{
  /* string	  name            processors  */	
  { NULL,	"-mcpu=",	all_cores  },
  { NULL,	"-march=",	all_architectures },
  { NULL,	"-mtune=",	all_cores }
};

/* Return the number of bits set in value' */
static unsigned int
bit_count (value)
     signed int value;
{
  unsigned int count = 0;
  
  while (value)
    {
      value &= ~(value & - value);
      ++ count;
    }

  return count;
}

/* Fix up any incompatible options that the user has specified.
   This has now turned into a maze.  */
void
arm_override_options ()
{
  unsigned i;
  
  /* Set up the flags based on the cpu/architecture selected by the user.  */
  for (i = sizeof (arm_select) / sizeof (arm_select[0]); i--;)
    {
      struct arm_cpu_select * ptr = arm_select + i;
      
      if (ptr->string != NULL && ptr->string[0] != '\0')
        {
	  const struct processors * sel;

          for (sel = ptr->processors; sel->name != NULL; sel ++)
            if (streq (ptr->string, sel->name))
              {
		if (i == 2)
		  tune_flags = sel->flags;
		else
		  {
		    /* If we have been given an architecture and a processor
		       make sure that they are compatible.  We only generate
		       a warning though, and we prefer the CPU over the
		       architecture. */
		    if (insn_flags != 0 && (insn_flags ^ sel->flags))
		      warning ("switch -mcpu=%s conflicts with -march= switch",
			       ptr->string);
		    
		    insn_flags = sel->flags;
		  }
		
                break;
              }

          if (sel->name == NULL)
            error ("bad value (%s) for %s switch", ptr->string, ptr->name);
        }
    }
  
  /* If the user did not specify a processor, choose one for them.  */
  if (insn_flags == 0)
    {
      struct processors * sel;
      unsigned int        sought;
      static struct cpu_default
      {
	int    cpu;
	char * name;
      }
      cpu_defaults[] =
      {
	{ TARGET_CPU_arm2,      "arm2" },
	{ TARGET_CPU_arm6,      "arm6" },
	{ TARGET_CPU_arm610,    "arm610" },
	{ TARGET_CPU_arm710,	"arm710" },
	{ TARGET_CPU_arm7m,     "arm7m" },
	{ TARGET_CPU_arm7500fe, "arm7500fe" },
	{ TARGET_CPU_arm7tdmi,  "arm7tdmi" },
	{ TARGET_CPU_arm8,      "arm8" },
	{ TARGET_CPU_arm810,    "arm810" },
	{ TARGET_CPU_arm9,      "arm9" },
	{ TARGET_CPU_strongarm, "strongarm" },
	{ TARGET_CPU_generic,   "arm" },
	{ 0, 0 }
      };
      struct cpu_default * def;
	  
      /* Find the default.  */
      for (def = cpu_defaults; def->name; def ++)
	if (def->cpu == TARGET_CPU_DEFAULT)
	  break;

      /* Make sure we found the default CPU.  */
      if (def->name == NULL)
	abort ();
      
      /* Find the default CPU's flags.  */
      for (sel = all_cores; sel->name != NULL; sel ++)
	if (streq (def->name, sel->name))
	  break;
      
      if (sel->name == NULL)
	abort ();

      insn_flags = sel->flags;
      
      /* Now check to see if the user has specified some command line
	 switch that require certain abilities from the cpu.  */
      sought = 0;
      
      if (TARGET_INTERWORK)
	{
	  sought |= (FL_THUMB | FL_MODE32);
	  
	  /* Force apcs-32 to be used for interworking.  */
	  target_flags |= ARM_FLAG_APCS_32;

	  /* There are no ARM processor that supports both APCS-26 and
	     interworking.  Therefore we force FL_MODE26 to be removed
	     from insn_flags here (if it was set), so that the search
	     below will always be able to find a compatible processor.  */
	  insn_flags &= ~ FL_MODE26;
	}
      
      if (! TARGET_APCS_32)
	sought |= FL_MODE26;

      if (sought != 0 && ((sought & insn_flags) != sought))
	{
	  /* Try to locate a CPU type that supports all of the abilities
	     of the default CPU, plus the extra abilities requested by
	     the user.  */
	  for (sel = all_cores; sel->name != NULL; sel ++)
	    if ((sel->flags & sought) == (sought | insn_flags))
	      break;

	  if (sel->name == NULL)
	    {
	      unsigned int        current_bit_count = 0;
	      struct processors * best_fit = NULL;
	      
	      /* Ideally we would like to issue an error message here
		 saying that it was not possible to find a CPU compatible
		 with the default CPU, but which also supports the command
		 line options specified by the programmer, and so they
		 ought to use the -mcpu=<name> command line option to
		 override the default CPU type.

		 Unfortunately this does not work with multilibing.  We
		 need to be able to support multilibs for -mapcs-26 and for
		 -mthumb-interwork and there is no CPU that can support both
		 options.  Instead if we cannot find a cpu that has both the
		 characteristics of the default cpu and the given command line
		 options we scan the array again looking for a best match.  */
	      for (sel = all_cores; sel->name != NULL; sel ++)
		if ((sel->flags & sought) == sought)
		  {
		    unsigned int count;

		    count = bit_count (sel->flags & insn_flags);

		    if (count >= current_bit_count)
		      {
			best_fit = sel;
			current_bit_count = count;
		      }
		  }

	      if (best_fit == NULL)
		abort ();
	      else
		sel = best_fit;
	    }

	  insn_flags = sel->flags;
	}
    }
  
  /* If tuning has not been specified, tune for whichever processor or
     architecture has been selected.  */
  if (tune_flags == 0)
    tune_flags = insn_flags;
  
  /* Make sure that the processor choice does not conflict with any of the
     other command line choices.  */
  if (TARGET_APCS_32 && !(insn_flags & FL_MODE32))
    {
      /* If APCS-32 was not the default then it must have been set by the
	 user, so issue a warning message.  If the user has specified
	 "-mapcs-32 -mcpu=arm2" then we loose here.  */
      if ((TARGET_DEFAULT & ARM_FLAG_APCS_32) == 0)
	warning ("target CPU does not support APCS-32" );
      target_flags &= ~ ARM_FLAG_APCS_32;
    }
  else if (! TARGET_APCS_32 && !(insn_flags & FL_MODE26))
    {
      warning ("target CPU does not support APCS-26" );
      target_flags |= ARM_FLAG_APCS_32;
    }
  
  if (TARGET_INTERWORK && !(insn_flags & FL_THUMB))
    {
      warning ("target CPU does not support interworking" );
      target_flags &= ~ARM_FLAG_INTERWORK;
    }
  
  /* If interworking is enabled then APCS-32 must be selected as well.  */
  if (TARGET_INTERWORK)
    {
      if (! TARGET_APCS_32)
	warning ("interworking forces APCS-32 to be used" );
      target_flags |= ARM_FLAG_APCS_32;
    }
  
  if (TARGET_APCS_STACK && ! TARGET_APCS)
    {
      warning ("-mapcs-stack-check incompatible with -mno-apcs-frame");
      target_flags |= ARM_FLAG_APCS_FRAME;
    }
  
  if (TARGET_POKE_FUNCTION_NAME)
    target_flags |= ARM_FLAG_APCS_FRAME;
  
  if (TARGET_APCS_REENT && flag_pic)
    fatal ("-fpic and -mapcs-reent are incompatible");
  
  if (TARGET_APCS_REENT)
    warning ("APCS reentrant code not supported.  Ignored");
  
  if (write_symbols != NO_DEBUG && flag_omit_frame_pointer)
    warning ("-g with -fomit-frame-pointer may not give sensible debugging");
  
  /* If stack checking is disabled, we can use r10 as the PIC register,
     which keeps r9 available.  */
  if (flag_pic && ! TARGET_APCS_STACK)
    arm_pic_register = 10;
  
  if (TARGET_APCS_FLOAT)
    warning ("Passing floating point arguments in fp regs not yet supported");
  
  /* Initialise boolean versions of the flags, for use in the arm.md file.  */
  arm_fast_multiply = (insn_flags & FL_FAST_MULT) != 0;
  arm_arch4         = (insn_flags & FL_ARCH4) != 0;
  arm_arch5         = (insn_flags & FL_ARCH5) != 0;
  
  arm_ld_sched      = (tune_flags & FL_LDSCHED) != 0;
  arm_is_strong     = (tune_flags & FL_STRONG) != 0;
  arm_is_6_or_7     = ((tune_flags & (FL_MODE26 | FL_MODE32))
		       && !(tune_flags & FL_ARCH4)) != 0;
  
  /* Default value for floating point code... if no co-processor
     bus, then schedule for emulated floating point.  Otherwise,
     assume the user has an FPA.
     Note: this does not prevent use of floating point instructions,
     -msoft-float does that.  */
  arm_fpu = (tune_flags & FL_CO_PROC) ? FP_HARD : FP_SOFT3;
  
  if (target_fp_name)
    {
      if (streq (target_fp_name, "2"))
	arm_fpu_arch = FP_SOFT2;
      else if (streq (target_fp_name, "3"))
	arm_fpu_arch = FP_SOFT3;
      else
	fatal ("Invalid floating point emulation option: -mfpe-%s",
	       target_fp_name);
    }
  else
    arm_fpu_arch = FP_DEFAULT;
  
  if (TARGET_FPE && arm_fpu != FP_HARD)
    arm_fpu = FP_SOFT2;
  
  /* For arm2/3 there is no need to do any scheduling if there is only
     a floating point emulator, or we are doing software floating-point.  */
  if ((TARGET_SOFT_FLOAT || arm_fpu != FP_HARD)
      && (tune_flags & FL_MODE32) == 0)
    flag_schedule_insns = flag_schedule_insns_after_reload = 0;
  
  arm_prog_mode = TARGET_APCS_32 ? PROG_MODE_PROG32 : PROG_MODE_PROG26;
  
  if (structure_size_string != NULL)
    {
      int size = strtol (structure_size_string, NULL, 0);
      
      if (size == 8 || size == 32)
	arm_structure_size_boundary = size;
      else
	warning ("Structure size boundary can only be set to 8 or 32");
    }

  if (arm_pic_register_string != NULL)
    {
      int pic_register;

      if (! flag_pic)
	warning ("-mpic-register= is useless without -fpic");

      pic_register = decode_reg_name (arm_pic_register_string);
      
      /* Prevent the user from choosing an obviously stupid PIC register.  */
      if (pic_register < 0 || call_used_regs[pic_register]
	  || pic_register == HARD_FRAME_POINTER_REGNUM
	  || pic_register == STACK_POINTER_REGNUM
	  || pic_register >= PC_REGNUM)
	error ("Unable to use '%s' for PIC register", arm_pic_register_string);
      else
	arm_pic_register = pic_register;
    }
  
  /* If optimizing for space, don't synthesize constants.
     For processors with load scheduling, it never costs more than 2 cycles
     to load a constant, and the load scheduler may well reduce that to 1.  */
  if (optimize_size || (tune_flags & FL_LDSCHED))
    arm_constant_limit = 1;
  
  /* If optimizing for size, bump the number of instructions that we
     are prepared to conditionally execute (even on a StrongARM). 
     Otherwise for the StrongARM, which has early execution of branches,
     a sequence that is worth skipping is shorter.  */
  if (optimize_size)
    max_insns_skipped = 6;
  else if (arm_is_strong)
    max_insns_skipped = 3;

  /* Register global variables with the garbage collector.  */
  arm_add_gc_roots ();
}

static void
arm_add_gc_roots ()
{
  ggc_add_rtx_root (&arm_compare_op0, 1);
  ggc_add_rtx_root (&arm_compare_op1, 1);
  ggc_add_rtx_root (&arm_target_insn, 1); /* Not sure this is really a root */
  /* XXX: What about the minipool tables?  */
}


/* Return 1 if it is possible to return using a single instruction */

int
use_return_insn (iscond)
     int iscond;
{
  int regno;

  if (!reload_completed
      || current_function_pretend_args_size
      || current_function_anonymous_args
      || ((get_frame_size () + current_function_outgoing_args_size != 0)
	  && !(TARGET_APCS && frame_pointer_needed)))
    return 0;

  /* Can't be done if interworking with Thumb, and any registers have been
     stacked.  Similarly, on StrongARM, conditional returns are expensive
     if they aren't taken and registers have been stacked.  */
  if (iscond && arm_is_strong && frame_pointer_needed)
    return 0;
  if ((iscond && arm_is_strong)
      || TARGET_INTERWORK)
    {
      for (regno = 0; regno < 16; regno++)
	if (regs_ever_live[regno] && ! call_used_regs[regno])
	  return 0;

      if (flag_pic && regs_ever_live[PIC_OFFSET_TABLE_REGNUM])
	return 0;
    }
      
  /* Can't be done if any of the FPU regs are pushed, since this also
     requires an insn */
  for (regno = 16; regno < 24; regno++)
    if (regs_ever_live[regno] && ! call_used_regs[regno])
      return 0;

  /* If a function is naked, don't use the "return" insn.  */
  if (arm_naked_function_p (current_function_decl))
    return 0;

  return 1;
}

/* Return TRUE if int I is a valid immediate ARM constant.  */

int
const_ok_for_arm (i)
     HOST_WIDE_INT i;
{
  unsigned HOST_WIDE_INT mask = ~(unsigned HOST_WIDE_INT)0xFF;

  /* For machines with >32 bit HOST_WIDE_INT, the bits above bit 31 must 
     be all zero, or all one.  */
  if ((i & ~(unsigned HOST_WIDE_INT) 0xffffffffUL) != 0
      && ((i & ~(unsigned HOST_WIDE_INT) 0xffffffffUL) 
	  != ((~(unsigned HOST_WIDE_INT) 0)
	      & ~(unsigned HOST_WIDE_INT) 0xffffffffUL)))
    return FALSE;
  
  /* Fast return for 0 and powers of 2 */
  if ((i & (i - 1)) == 0)
    return TRUE;

  do
    {
      if ((i & mask & (unsigned HOST_WIDE_INT) 0xffffffffUL) == 0)
        return TRUE;
      mask =
	  (mask << 2) | ((mask & (unsigned HOST_WIDE_INT) 0xffffffffUL)
			 >> (32 - 2)) | ~((unsigned HOST_WIDE_INT) 0xffffffffUL);
    } while (mask != ~(unsigned HOST_WIDE_INT) 0xFF);

  return FALSE;
}

/* Return true if I is a valid constant for the operation CODE. */
static int
const_ok_for_op (i, code)
     HOST_WIDE_INT i;
     enum rtx_code code;
{
  if (const_ok_for_arm (i))
    return 1;

  switch (code)
    {
    case PLUS:
      return const_ok_for_arm (ARM_SIGN_EXTEND (-i));

    case MINUS:		/* Should only occur with (MINUS I reg) => rsb */
    case XOR:
    case IOR:
      return 0;

    case AND:
      return const_ok_for_arm (ARM_SIGN_EXTEND (~i));

    default:
      abort ();
    }
}

/* Emit a sequence of insns to handle a large constant.
   CODE is the code of the operation required, it can be any of SET, PLUS,
   IOR, AND, XOR, MINUS;
   MODE is the mode in which the operation is being performed;
   VAL is the integer to operate on;
   SOURCE is the other operand (a register, or a null-pointer for SET);
   SUBTARGETS means it is safe to create scratch registers if that will
   either produce a simpler sequence, or we will want to cse the values.
   Return value is the number of insns emitted.  */

int
arm_split_constant (code, mode, val, target, source, subtargets)
     enum rtx_code code;
     enum machine_mode mode;
     HOST_WIDE_INT val;
     rtx target;
     rtx source;
     int subtargets;
{
  if (subtargets || code == SET
      || (GET_CODE (target) == REG && GET_CODE (source) == REG
	  && REGNO (target) != REGNO (source)))
    {
      /* After arm_reorg has been called, we can't fix up expensive
	 constants by pushing them into memory so we must synthesise
	 them in-line, regardless of the cost.  This is only likely to
	 be more costly on chips that have load delay slots and we are
	 compiling without running the scheduler (so no splitting
	 occurred before the final instruction emission).

	 Ref: gcc -O1 -mcpu=strongarm gcc.c-torture/compile/980506-2.c
      */
      if (! after_arm_reorg
	  && (arm_gen_constant (code, mode, val, target, source, 1, 0)
	      > arm_constant_limit + (code != SET)))
	{
	  if (code == SET)
	    {
	      /* Currently SET is the only monadic value for CODE, all
		 the rest are diadic.  */
	      emit_insn (gen_rtx_SET (VOIDmode, target, GEN_INT (val)));
	      return 1;
	    }
	  else
	    {
	      rtx temp = subtargets ? gen_reg_rtx (mode) : target;

	      emit_insn (gen_rtx_SET (VOIDmode, temp, GEN_INT (val)));
	      /* For MINUS, the value is subtracted from, since we never
		 have subtraction of a constant.  */
	      if (code == MINUS)
		emit_insn (gen_rtx_SET (VOIDmode, target,
					gen_rtx (code, mode, temp, source)));
	      else
		emit_insn (gen_rtx_SET (VOIDmode, target,
					gen_rtx (code, mode, source, temp)));
	      return 2;
	    }
	}
    }

  return arm_gen_constant (code, mode, val, target, source, subtargets, 1);
}

/* As above, but extra parameter GENERATE which, if clear, suppresses
   RTL generation.  */
int
arm_gen_constant (code, mode, val, target, source, subtargets, generate)
     enum rtx_code code;
     enum machine_mode mode;
     HOST_WIDE_INT val;
     rtx target;
     rtx source;
     int subtargets;
     int generate;
{
  int can_invert = 0;
  int can_negate = 0;
  int can_negate_initial = 0;
  int can_shift = 0;
  int i;
  int num_bits_set = 0;
  int set_sign_bit_copies = 0;
  int clear_sign_bit_copies = 0;
  int clear_zero_bit_copies = 0;
  int set_zero_bit_copies = 0;
  int insns = 0;
  unsigned HOST_WIDE_INT temp1, temp2;
  unsigned HOST_WIDE_INT remainder = val & 0xffffffffUL;

  /* find out which operations are safe for a given CODE.  Also do a quick
     check for degenerate cases; these can occur when DImode operations
     are split.  */
  switch (code)
    {
    case SET:
      can_invert = 1;
      can_shift = 1;
      can_negate = 1;
      break;

    case PLUS:
      can_negate = 1;
      can_negate_initial = 1;
      break;

    case IOR:
      if (remainder == 0xffffffffUL)
	{
	  if (generate)
	    emit_insn (gen_rtx_SET (VOIDmode, target,
				    GEN_INT (ARM_SIGN_EXTEND (val))));
	  return 1;
	}
      if (remainder == 0)
	{
	  if (reload_completed && rtx_equal_p (target, source))
	    return 0;
	  if (generate)
	    emit_insn (gen_rtx_SET (VOIDmode, target, source));
	  return 1;
	}
      break;

    case AND:
      if (remainder == 0)
	{
	  if (generate)
	    emit_insn (gen_rtx_SET (VOIDmode, target, const0_rtx));
	  return 1;
	}
      if (remainder == 0xffffffffUL)
	{
	  if (reload_completed && rtx_equal_p (target, source))
	    return 0;
	  if (generate)
	    emit_insn (gen_rtx_SET (VOIDmode, target, source));
	  return 1;
	}
      can_invert = 1;
      break;

    case XOR:
      if (remainder == 0)
	{
	  if (reload_completed && rtx_equal_p (target, source))
	    return 0;
	  if (generate)
	    emit_insn (gen_rtx_SET (VOIDmode, target, source));
	  return 1;
	}
      if (remainder == 0xffffffffUL)
	{
	  if (generate)
	    emit_insn (gen_rtx_SET (VOIDmode, target,
				    gen_rtx_NOT (mode, source)));
	  return 1;
	}

      /* We don't know how to handle this yet below.  */
      abort ();

    case MINUS:
      /* We treat MINUS as (val - source), since (source - val) is always
	 passed as (source + (-val)).  */
      if (remainder == 0)
	{
	  if (generate)
	    emit_insn (gen_rtx_SET (VOIDmode, target,
				    gen_rtx_NEG (mode, source)));
	  return 1;
	}
      if (const_ok_for_arm (val))
	{
	  if (generate)
	    emit_insn (gen_rtx_SET (VOIDmode, target, 
				    gen_rtx_MINUS (mode, GEN_INT (val),
						   source)));
	  return 1;
	}
      can_negate = 1;

      break;

    default:
      abort ();
    }

  /* If we can do it in one insn get out quickly */
  if (const_ok_for_arm (val)
      || (can_negate_initial && const_ok_for_arm (-val))
      || (can_invert && const_ok_for_arm (~val)))
    {
      if (generate)
	emit_insn (gen_rtx_SET (VOIDmode, target,
				(source ? gen_rtx (code, mode, source,
						   GEN_INT (val))
				 : GEN_INT (val))));
      return 1;
    }


  /* Calculate a few attributes that may be useful for specific
     optimizations. */

  for (i = 31; i >= 0; i--)
    {
      if ((remainder & (1 << i)) == 0)
	clear_sign_bit_copies++;
      else
	break;
    }

  for (i = 31; i >= 0; i--)
    {
      if ((remainder & (1 << i)) != 0)
	set_sign_bit_copies++;
      else
	break;
    }

  for (i = 0; i <= 31; i++)
    {
      if ((remainder & (1 << i)) == 0)
	clear_zero_bit_copies++;
      else
	break;
    }

  for (i = 0; i <= 31; i++)
    {
      if ((remainder & (1 << i)) != 0)
	set_zero_bit_copies++;
      else
	break;
    }

  switch (code)
    {
    case SET:
      /* See if we can do this by sign_extending a constant that is known
	 to be negative.  This is a good, way of doing it, since the shift
	 may well merge into a subsequent insn.  */
      if (set_sign_bit_copies > 1)
	{
	  if (const_ok_for_arm
	      (temp1 = ARM_SIGN_EXTEND (remainder 
					<< (set_sign_bit_copies - 1))))
	    {
	      if (generate)
		{
		  rtx new_src = subtargets ? gen_reg_rtx (mode) : target;
		  emit_insn (gen_rtx_SET (VOIDmode, new_src, 
					  GEN_INT (temp1)));
		  emit_insn (gen_ashrsi3 (target, new_src, 
					  GEN_INT (set_sign_bit_copies - 1)));
		}
	      return 2;
	    }
	  /* For an inverted constant, we will need to set the low bits,
	     these will be shifted out of harm's way.  */
	  temp1 |= (1 << (set_sign_bit_copies - 1)) - 1;
	  if (const_ok_for_arm (~temp1))
	    {
	      if (generate)
		{
		  rtx new_src = subtargets ? gen_reg_rtx (mode) : target;
		  emit_insn (gen_rtx_SET (VOIDmode, new_src,
					  GEN_INT (temp1)));
		  emit_insn (gen_ashrsi3 (target, new_src, 
					  GEN_INT (set_sign_bit_copies - 1)));
		}
	      return 2;
	    }
	}

      /* See if we can generate this by setting the bottom (or the top)
	 16 bits, and then shifting these into the other half of the
	 word.  We only look for the simplest cases, to do more would cost
	 too much.  Be careful, however, not to generate this when the
	 alternative would take fewer insns.  */
      if (val & 0xffff0000UL)
	{
	  temp1 = remainder & 0xffff0000UL;
	  temp2 = remainder & 0x0000ffff;

	  /* Overlaps outside this range are best done using other methods. */
	  for (i = 9; i < 24; i++)
	    {
	      if ((((temp2 | (temp2 << i)) & 0xffffffffUL) == remainder)
		  && ! const_ok_for_arm (temp2))
		{
		  rtx new_src = (subtargets
				 ? (generate ? gen_reg_rtx (mode) : NULL_RTX)
				 : target);
		  insns = arm_gen_constant (code, mode, temp2, new_src,
					    source, subtargets, generate);
		  source = new_src;
		  if (generate)
		    emit_insn (gen_rtx_SET
			       (VOIDmode, target,
				gen_rtx_IOR (mode,
					     gen_rtx_ASHIFT (mode, source,
							     GEN_INT (i)),
					     source)));
		  return insns + 1;
		}
	    }

	  /* Don't duplicate cases already considered. */
	  for (i = 17; i < 24; i++)
	    {
	      if (((temp1 | (temp1 >> i)) == remainder)
		  && ! const_ok_for_arm (temp1))
		{
		  rtx new_src = (subtargets
				 ? (generate ? gen_reg_rtx (mode) : NULL_RTX)
				 : target);
		  insns = arm_gen_constant (code, mode, temp1, new_src,
					    source, subtargets, generate);
		  source = new_src;
		  if (generate)
		    emit_insn
		      (gen_rtx_SET (VOIDmode, target,
				    gen_rtx_IOR
				    (mode,
				     gen_rtx_LSHIFTRT (mode, source,
						       GEN_INT (i)),
				     source)));
		  return insns + 1;
		}
	    }
	}
      break;

    case IOR:
    case XOR:
      /* If we have IOR or XOR, and the constant can be loaded in a
	 single instruction, and we can find a temporary to put it in,
	 then this can be done in two instructions instead of 3-4.  */
      if (subtargets
	  /* TARGET can't be NULL if SUBTARGETS is 0 */
	  || (reload_completed && ! reg_mentioned_p (target, source)))
	{
	  if (const_ok_for_arm (ARM_SIGN_EXTEND (~ val)))
	    {
	      if (generate)
		{
		  rtx sub = subtargets ? gen_reg_rtx (mode) : target;

		  emit_insn (gen_rtx_SET (VOIDmode, sub, GEN_INT (val)));
		  emit_insn (gen_rtx_SET (VOIDmode, target, 
					  gen_rtx (code, mode, source, sub)));
		}
	      return 2;
	    }
	}

      if (code == XOR)
	break;

      if (set_sign_bit_copies > 8
	  && (val & (-1 << (32 - set_sign_bit_copies))) == val)
	{
	  if (generate)
	    {
	      rtx sub = subtargets ? gen_reg_rtx (mode) : target;
	      rtx shift = GEN_INT (set_sign_bit_copies);

	      emit_insn (gen_rtx_SET (VOIDmode, sub,
				      gen_rtx_NOT (mode, 
						   gen_rtx_ASHIFT (mode,
								   source, 
								   shift))));
	      emit_insn (gen_rtx_SET (VOIDmode, target,
				      gen_rtx_NOT (mode,
						   gen_rtx_LSHIFTRT (mode, sub,
								     shift))));
	    }
	  return 2;
	}

      if (set_zero_bit_copies > 8
	  && (remainder & ((1 << set_zero_bit_copies) - 1)) == remainder)
	{
	  if (generate)
	    {
	      rtx sub = subtargets ? gen_reg_rtx (mode) : target;
	      rtx shift = GEN_INT (set_zero_bit_copies);

	      emit_insn (gen_rtx_SET (VOIDmode, sub,
				      gen_rtx_NOT (mode,
						   gen_rtx_LSHIFTRT (mode,
								     source,
								     shift))));
	      emit_insn (gen_rtx_SET (VOIDmode, target,
				      gen_rtx_NOT (mode,
						   gen_rtx_ASHIFT (mode, sub,
								   shift))));
	    }
	  return 2;
	}

      if (const_ok_for_arm (temp1 = ARM_SIGN_EXTEND (~ val)))
	{
	  if (generate)
	    {
	      rtx sub = subtargets ? gen_reg_rtx (mode) : target;
	      emit_insn (gen_rtx_SET (VOIDmode, sub,
				      gen_rtx_NOT (mode, source)));
	      source = sub;
	      if (subtargets)
		sub = gen_reg_rtx (mode);
	      emit_insn (gen_rtx_SET (VOIDmode, sub,
				      gen_rtx_AND (mode, source, 
						   GEN_INT (temp1))));
	      emit_insn (gen_rtx_SET (VOIDmode, target,
				      gen_rtx_NOT (mode, sub)));
	    }
	  return 3;
	}
      break;

    case AND:
      /* See if two shifts will do 2 or more insn's worth of work.  */
      if (clear_sign_bit_copies >= 16 && clear_sign_bit_copies < 24)
	{
	  HOST_WIDE_INT shift_mask = ((0xffffffffUL
				       << (32 - clear_sign_bit_copies))
				      & 0xffffffffUL);

	  if ((remainder | shift_mask) != 0xffffffffUL)
	    {
	      if (generate)
		{
		  rtx new_src = subtargets ? gen_reg_rtx (mode) : target;
		  insns = arm_gen_constant (AND, mode, remainder | shift_mask,
					    new_src, source, subtargets, 1);
		  source = new_src;
		}
	      else
		{
		  rtx targ = subtargets ? NULL_RTX : target;
		  insns = arm_gen_constant (AND, mode, remainder | shift_mask,
					    targ, source, subtargets, 0);
		}
	    }

	  if (generate)
	    {
	      rtx new_src = subtargets ? gen_reg_rtx (mode) : target;
	      rtx shift = GEN_INT (clear_sign_bit_copies);

	      emit_insn (gen_ashlsi3 (new_src, source, shift));
	      emit_insn (gen_lshrsi3 (target, new_src, shift));
	    }

	  return insns + 2;
	}

      if (clear_zero_bit_copies >= 16 && clear_zero_bit_copies < 24)
	{
	  HOST_WIDE_INT shift_mask = (1 << clear_zero_bit_copies) - 1;
	  
	  if ((remainder | shift_mask) != 0xffffffffUL)
	    {
	      if (generate)
		{
		  rtx new_src = subtargets ? gen_reg_rtx (mode) : target;

		  insns = arm_gen_constant (AND, mode, remainder | shift_mask,
					    new_src, source, subtargets, 1);
		  source = new_src;
		}
	      else
		{
		  rtx targ = subtargets ? NULL_RTX : target;

		  insns = arm_gen_constant (AND, mode, remainder | shift_mask,
					    targ, source, subtargets, 0);
		}
	    }

	  if (generate)
	    {
	      rtx new_src = subtargets ? gen_reg_rtx (mode) : target;
	      rtx shift = GEN_INT (clear_zero_bit_copies);

	      emit_insn (gen_lshrsi3 (new_src, source, shift));
	      emit_insn (gen_ashlsi3 (target, new_src, shift));
	    }

	  return insns + 2;
	}

      break;

    default:
      break;
    }

  for (i = 0; i < 32; i++)
    if (remainder & (1 << i))
      num_bits_set++;

  if (code == AND || (can_invert && num_bits_set > 16))
    remainder = (~remainder) & 0xffffffffUL;
  else if (code == PLUS && num_bits_set > 16)
    remainder = (-remainder) & 0xffffffffUL;
  else
    {
      can_invert = 0;
      can_negate = 0;
    }

  /* Now try and find a way of doing the job in either two or three
     instructions.
     We start by looking for the largest block of zeros that are aligned on
     a 2-bit boundary, we then fill up the temps, wrapping around to the
     top of the word when we drop off the bottom.
     In the worst case this code should produce no more than four insns. */
  {
    int best_start = 0;
    int best_consecutive_zeros = 0;

    for (i = 0; i < 32; i += 2)
      {
	int consecutive_zeros = 0;

	if (! (remainder & (3 << i)))
	  {
	    while ((i < 32) && ! (remainder & (3 << i)))
	      {
		consecutive_zeros += 2;
		i += 2;
	      }
	    if (consecutive_zeros > best_consecutive_zeros)
	      {
		best_consecutive_zeros = consecutive_zeros;
		best_start = i - consecutive_zeros;
	      }
	    i -= 2;
	  }
      }

    /* Now start emitting the insns, starting with the one with the highest
       bit set: we do this so that the smallest number will be emitted last;
       this is more likely to be combinable with addressing insns. */
    i = best_start;
    do
      {
	int end;

	if (i <= 0)
	  i += 32;
	if (remainder & (3 << (i - 2)))
	  {
	    end = i - 8;
	    if (end < 0)
	      end += 32;
	    temp1 = remainder & ((0x0ff << end)
				 | ((i < end) ? (0xff >> (32 - end)) : 0));
	    remainder &= ~temp1;

	    if (generate)
	      {
		rtx new_src;

		if (code == SET)
		  emit_insn (gen_rtx_SET (VOIDmode,
					  new_src = (subtargets
						     ? gen_reg_rtx (mode)
						     : target),
					  GEN_INT (can_invert
						   ? ~temp1 : temp1)));
		else if (code == MINUS)
		  emit_insn (gen_rtx_SET (VOIDmode,
					  new_src = (subtargets
						     ? gen_reg_rtx (mode)
						     : target),
					  gen_rtx (code, mode, GEN_INT (temp1),
						   source)));
		else
		  emit_insn (gen_rtx_SET (VOIDmode,
					  new_src = (remainder
						     ? (subtargets
							? gen_reg_rtx (mode)
							: target)
						     : target),
					  gen_rtx (code, mode, source,
						   GEN_INT (can_invert ? ~temp1
							    : (can_negate
							       ? -temp1
							       : temp1)))));
		source = new_src;
	      }

	    if (code == SET)
	      {
		can_invert = 0;
		code = PLUS;
	      }
	    else if (code == MINUS)
	      code = PLUS;

	    insns++;
	    i -= 6;
	  }
	i -= 2;
      } while (remainder);
  }
  return insns;
}

/* Canonicalize a comparison so that we are more likely to recognize it.
   This can be done for a few constant compares, where we can make the
   immediate value easier to load.  */
enum rtx_code
arm_canonicalize_comparison (code, op1)
     enum rtx_code code;
     rtx * op1;
{
  unsigned HOST_WIDE_INT i = INTVAL (*op1);

  switch (code)
    {
    case EQ:
    case NE:
      return code;

    case GT:
    case LE:
      if (i != ((((unsigned HOST_WIDE_INT) 1) << (HOST_BITS_PER_WIDE_INT - 1))
		- 1)
	  && (const_ok_for_arm (i+1) || const_ok_for_arm (- (i+1))))
	{
	  *op1 = GEN_INT (i+1);
	  return code == GT ? GE : LT;
	}
      break;

    case GE:
    case LT:
      if (i != (((unsigned HOST_WIDE_INT) 1) << (HOST_BITS_PER_WIDE_INT - 1))
	  && (const_ok_for_arm (i-1) || const_ok_for_arm (- (i-1))))
	{
	  *op1 = GEN_INT (i-1);
	  return code == GE ? GT : LE;
	}
      break;

    case GTU:
    case LEU:
      if (i != ~((unsigned HOST_WIDE_INT) 0)
	  && (const_ok_for_arm (i+1) || const_ok_for_arm (- (i+1))))
	{
	  *op1 = GEN_INT (i + 1);
	  return code == GTU ? GEU : LTU;
	}
      break;

    case GEU:
    case LTU:
      if (i != 0
	  && (const_ok_for_arm (i - 1) || const_ok_for_arm (- (i - 1))))
	{
	  *op1 = GEN_INT (i - 1);
	  return code == GEU ? GTU : LEU;
	}
      break;

    default:
      abort ();
    }

  return code;
}

/* Decide whether a type should be returned in memory (true)
   or in a register (false).  This is called by the macro
   RETURN_IN_MEMORY.  */
int
arm_return_in_memory (type)
     tree type;
{
  if (! AGGREGATE_TYPE_P (type))
    {
      /* All simple types are returned in registers. */
      return 0;
    }
  else if (int_size_in_bytes (type) > 4)
    {
      /* All structures/unions bigger than one word are returned in memory. */
      return 1;
    }
  else if (TREE_CODE (type) == RECORD_TYPE)
    {
      tree field;

      /* For a struct the APCS says that we only return in a register
	 if the type is 'integer like' and every addressable element
	 has an offset of zero.  For practical purposes this means
	 that the structure can have at most one non bit-field element
	 and that this element must be the first one in the structure.  */
      
      /* Find the first field, ignoring non FIELD_DECL things which will
	 have been created by C++.  */
      for (field = TYPE_FIELDS (type);
	   field && TREE_CODE (field) != FIELD_DECL;
	   field = TREE_CHAIN (field))
	continue;
      
      if (field == NULL)
	return 0; /* An empty structure.  Allowed by an extension to ANSI C. */

      /* Check that the first field is valid for returning in a register...  */

      /* ... Floats are not allowed */
      if (FLOAT_TYPE_P (TREE_TYPE (field)))
	return 1;

      /* ... Aggregates that are not themselves valid for returning in
	 a register are not allowed.  */
      if (RETURN_IN_MEMORY (TREE_TYPE (field)))
	return 1;

      /* Now check the remaining fields, if any.  Only bitfields are allowed,
	 since they are not addressable.  */
      for (field = TREE_CHAIN (field);
	   field;
	   field = TREE_CHAIN (field))
	{
	  if (TREE_CODE (field) != FIELD_DECL)
	    continue;
	  
	  if (! DECL_BIT_FIELD_TYPE (field))
	    return 1;
	}

      return 0;
    }
  else if (TREE_CODE (type) == UNION_TYPE)
    {
      tree field;

      /* Unions can be returned in registers if every element is
	 integral, or can be returned in an integer register.  */
      for (field = TYPE_FIELDS (type);
	   field;
	   field = TREE_CHAIN (field))
	{
	  if (TREE_CODE (field) != FIELD_DECL)
	    continue;

	  if (FLOAT_TYPE_P (TREE_TYPE (field)))
	    return 1;
	  
	  if (RETURN_IN_MEMORY (TREE_TYPE (field)))
	    return 1;
	}
      
      return 0;
    }
  
  /* XXX Not sure what should be done for other aggregates, so put them in
     memory. */
  return 1;
}

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is NULL.  */
void
arm_init_cumulative_args (pcum, fntype, libname, indirect)
     CUMULATIVE_ARGS * pcum;
     tree fntype;
     rtx libname  ATTRIBUTE_UNUSED;
     int indirect ATTRIBUTE_UNUSED;
{
  /* On the ARM, the offset starts at 0.  */
  pcum->nregs = ((fntype && aggregate_value_p (TREE_TYPE (fntype)))
		 ? 1 : 0);

  pcum->call_cookie = CALL_NORMAL;

  if (TARGET_LONG_CALLS)
    pcum->call_cookie = CALL_LONG;
    
  /* Check for long call/short call attributes.  The attributes
     override any command line option.  */
  if (fntype)
    {
      if (lookup_attribute ("short_call", TYPE_ATTRIBUTES (fntype)))
	pcum->call_cookie = CALL_SHORT;
      else if (lookup_attribute ("long_call", TYPE_ATTRIBUTES (fntype)))
	pcum->call_cookie = CALL_LONG;
    }
}

/* Determine where to put an argument to a function.
   Value is zero to push the argument on the stack,
   or a hard register in which to store the argument.

   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
    This is null for libcalls where that information may
    not be available.
   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   NAMED is nonzero if this argument is a named parameter
    (otherwise it is an extra parameter matching an ellipsis).  */
rtx
arm_function_arg (pcum, mode, type, named)
     CUMULATIVE_ARGS * pcum;
     enum machine_mode mode;
     tree type ATTRIBUTE_UNUSED;
     int named;
{
  if (mode == VOIDmode)
    /* Compute operand 2 of the call insn.  */
    return GEN_INT (pcum->call_cookie);
  
  if (! named || pcum->nregs >= NUM_ARG_REGS)
    return NULL_RTX;
  
  return gen_rtx_REG (mode, pcum->nregs);
}


/* Return 1 if the operand is a SYMBOL_REF for a function
   known to be defined in the current compilation unit.  */
static int
current_file_function_operand (sym_ref)
  rtx sym_ref;
{
  /* XXX FIXME - we need some way to determine if SYMREF has already been
     compiled.  We wanted to used SYMBOL_REF_FLAG but this is already in use
     by the constant pool generation code.  */
  return sym_ref == XEXP (DECL_RTL (current_function_decl), 0);
}

/* Return non-zero if a 32 bit "long call" should be generated for this
   call.

   We generate a long call if the function is not declared
   __attribute__ ((short_call),

   AND:
   
     (1)  the function is declared __attribute__ ((long_call))

   OR

     (2) -mlong-calls is enabled and we don't know whether the target
         function is declared in this file.
	 
   This function will typically be called by C fragments in the machine
   description file.  CALL_REF is the matched rtl operand.  CALL_COOKIE
   describes the value of the long_call and short_call attributes for
   the called functiion.  CALL_SYMBOL is used to distinguish between
   two different callers of the function.  It is set to 1 in the "call_symbol"
   and "call_symbol_value" patterns in arm.md and to 0 in the "call" and
   "call_value" patterns.  This is because of the difference of SYM_REFs passed
   from "call_symbol" and "call"  patterns.  */
int
arm_is_longcall_p (sym_ref, call_cookie, call_symbol)
  rtx sym_ref;
  int call_cookie;
  int call_symbol;
{
  if (! call_symbol)
    {
      if (GET_CODE (sym_ref) != MEM)
	return 0;

      sym_ref = XEXP (sym_ref, 0);
    }

  if (GET_CODE (sym_ref) != SYMBOL_REF)
    return 0;

  if (call_cookie & CALL_SHORT)
    return 0;

  if (TARGET_LONG_CALLS && flag_function_sections)
    return 1;
  
  if (current_file_function_operand (sym_ref, VOIDmode))
    return 0;
  
  return (call_cookie & CALL_LONG) || TARGET_LONG_CALLS;
}

/* Return nonzero if IDENTIFIER with arguments ARGS is a valid machine specific
   attribute for TYPE.  The attributes in ATTRIBUTES have previously been
   assigned to TYPE.  */
int
arm_valid_type_attribute_p (type, attributes, identifier, args)
     tree type;
     tree attributes ATTRIBUTE_UNUSED;
     tree identifier;
     tree args;
{
  if (   TREE_CODE (type) != FUNCTION_TYPE
      && TREE_CODE (type) != METHOD_TYPE
      && TREE_CODE (type) != FIELD_DECL
      && TREE_CODE (type) != TYPE_DECL)
    return 0;

  /* Function calls made to this symbol must be done indirectly, because
     it may lie outside of the 26 bit addressing range of a normal function
     call.  */
  if (is_attribute_p ("long_call", identifier))
    return (args == NULL_TREE);

  /* Whereas these functions are always known to reside within the 26 bit
     addressing range.  */
  if (is_attribute_p ("short_call", identifier))
    return (args == NULL_TREE);
  
  return 0;
}

/* Return 0 if the attributes for two types are incompatible, 1 if they
   are compatible, and 2 if they are nearly compatible (which causes a
   warning to be generated).  */
int
arm_comp_type_attributes (type1, type2)
     tree type1;
     tree type2;
{
  int l1, l2, s1, s2;
  /* Check for mismatch of non-default calling convention.  */
  if (TREE_CODE (type1) != FUNCTION_TYPE)
    return 1;

  /* Check for mismatched call attributes.  */
  l1 = ! lookup_attribute ("long_call", TYPE_ATTRIBUTES (type1));
  l2 = ! lookup_attribute ("long_call", TYPE_ATTRIBUTES (type2));
  s1 = ! lookup_attribute ("short_call", TYPE_ATTRIBUTES (type1));
  s2 = ! lookup_attribute ("short_call", TYPE_ATTRIBUTES (type2));

  return ! ((l1 ^ l2) || (s1 ^s2) || (l1 | s2) || (s1 | l2));
}


int
legitimate_pic_operand_p (x)
     rtx x;
{
  if (CONSTANT_P (x) && flag_pic
      && (GET_CODE (x) == SYMBOL_REF
	  || (GET_CODE (x) == CONST
	      && GET_CODE (XEXP (x, 0)) == PLUS
	      && GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF)))
    return 0;

  return 1;
}

rtx
legitimize_pic_address (orig, mode, reg)
     rtx orig;
     enum machine_mode mode;
     rtx reg;
{
  if (GET_CODE (orig) == SYMBOL_REF)
    {
      rtx pic_ref, address;
      rtx insn;
      int subregs = 0;

      if (reg == 0)
	{
	  if (reload_in_progress || reload_completed)
	    abort ();
	  else
	    reg = gen_reg_rtx (Pmode);

	  subregs = 1;
	}

#ifdef AOF_ASSEMBLER
      /* The AOF assembler can generate relocations for these directly, and
	 understands that the PIC register has to be added into the offset.
	 */
      insn = emit_insn (gen_pic_load_addr_based (reg, orig));
#else
      if (subregs)
	address = gen_reg_rtx (Pmode);
      else
	address = reg;

      emit_insn (gen_pic_load_addr (address, orig));

      pic_ref = gen_rtx_MEM (Pmode,
			     gen_rtx_PLUS (Pmode, pic_offset_table_rtx,
					   address));
      RTX_UNCHANGING_P (pic_ref) = 1;
      insn = emit_move_insn (reg, pic_ref);
#endif
      current_function_uses_pic_offset_table = 1;
      /* Put a REG_EQUAL note on this insn, so that it can be optimized
	 by loop.  */
      REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_EQUAL, orig,
					    REG_NOTES (insn));
      return reg;
    }
  else if (GET_CODE (orig) == CONST)
    {
      rtx base, offset;

      if (GET_CODE (XEXP (orig, 0)) == PLUS
	  && XEXP (XEXP (orig, 0), 0) == pic_offset_table_rtx)
	return orig;

      if (reg == 0)
	{
	  if (reload_in_progress || reload_completed)
	    abort ();
	  else
	    reg = gen_reg_rtx (Pmode);
	}

      if (GET_CODE (XEXP (orig, 0)) == PLUS)
	{
	  base = legitimize_pic_address (XEXP (XEXP (orig, 0), 0), Pmode, reg);
	  offset = legitimize_pic_address (XEXP (XEXP (orig, 0), 1), Pmode,
					   base == reg ? 0 : reg);
	}
      else
	abort ();

      if (GET_CODE (offset) == CONST_INT)
	{
	  /* The base register doesn't really matter, we only want to
	     test the index for the appropriate mode.  */
	  GO_IF_LEGITIMATE_INDEX (mode, 0, offset, win);

	  if (! reload_in_progress && ! reload_completed)
	    offset = force_reg (Pmode, offset);
	  else
	    abort ();

	win:
	  if (GET_CODE (offset) == CONST_INT)
	    return plus_constant_for_output (base, INTVAL (offset));
	}

      if (GET_MODE_SIZE (mode) > 4
	  && (GET_MODE_CLASS (mode) == MODE_INT
	      || TARGET_SOFT_FLOAT))
	{
	  emit_insn (gen_addsi3 (reg, base, offset));
	  return reg;
	}

      return gen_rtx_PLUS (Pmode, base, offset);
    }
  else if (GET_CODE (orig) == LABEL_REF)
    {
      current_function_uses_pic_offset_table = 1;
      
      if (NEED_GOT_RELOC)
        {
          rtx pic_ref, address = gen_reg_rtx (Pmode);
          
          emit_insn (gen_pic_load_addr (address, orig));
          pic_ref = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, address);
          
          emit_move_insn (address, pic_ref);
          return address;
        }
    }

  return orig;
}

static rtx pic_rtx;

int
is_pic (x)
     rtx x;
{
  if (x == pic_rtx)
    return 1;
  return 0;
}

void
arm_finalize_pic ()
{
#ifndef AOF_ASSEMBLER
  rtx l1, pic_tmp, pic_tmp2, seq;
  rtx global_offset_table;

  if (current_function_uses_pic_offset_table == 0 || TARGET_SINGLE_PIC_BASE)
    return;

  if (! flag_pic)
    abort ();

  start_sequence ();
  l1 = gen_label_rtx ();

  global_offset_table = gen_rtx_SYMBOL_REF (Pmode, "_GLOBAL_OFFSET_TABLE_");
  /* On the ARM the PC register contains 'dot + 8' at the time of the
     addition.  */
  pic_tmp = plus_constant (gen_rtx_LABEL_REF (Pmode, l1), 8);
  if (GOT_PCREL)
    pic_tmp2 = gen_rtx_CONST (VOIDmode,
			    gen_rtx_PLUS (Pmode, global_offset_table, pc_rtx));
  else
    pic_tmp2 = gen_rtx_CONST (VOIDmode, global_offset_table);

  pic_rtx = gen_rtx_CONST (Pmode, gen_rtx_MINUS (Pmode, pic_tmp2, pic_tmp));
  
  emit_insn (gen_pic_load_addr (pic_offset_table_rtx, pic_rtx));
  emit_insn (gen_pic_add_dot_plus_eight (pic_offset_table_rtx, l1));

  seq = gen_sequence ();
  end_sequence ();
  emit_insn_after (seq, get_insns ());

  /* Need to emit this whether or not we obey regdecls,
     since setjmp/longjmp can cause life info to screw up.  */
  emit_insn (gen_rtx_USE (VOIDmode, pic_offset_table_rtx));
#endif /* AOF_ASSEMBLER */
}

#define REG_OR_SUBREG_REG(X)						\
  (GET_CODE (X) == REG							\
   || (GET_CODE (X) == SUBREG && GET_CODE (SUBREG_REG (X)) == REG))

#define REG_OR_SUBREG_RTX(X)			\
   (GET_CODE (X) == REG ? (X) : SUBREG_REG (X))

#define ARM_FRAME_RTX(X)				\
  ((X) == frame_pointer_rtx || (X) == stack_pointer_rtx	\
   || (X) == arg_pointer_rtx)

int
arm_rtx_costs (x, code)
     rtx x;
     enum rtx_code code;
{
  enum machine_mode mode = GET_MODE (x);
  enum rtx_code subcode;
  int extra_cost;

  switch (code)
    {
    case MEM:
      /* Memory costs quite a lot for the first word, but subsequent words
	 load at the equivalent of a single insn each.  */
      return (10 + 4 * ((GET_MODE_SIZE (mode) - 1) / UNITS_PER_WORD)
	      + (CONSTANT_POOL_ADDRESS_P (x) ? 4 : 0));

    case DIV:
    case MOD:
      return 100;

    case ROTATE:
      if (mode == SImode && GET_CODE (XEXP (x, 1)) == REG)
	return 4;
      /* Fall through */
    case ROTATERT:
      if (mode != SImode)
	return 8;
      /* Fall through */
    case ASHIFT: case LSHIFTRT: case ASHIFTRT:
      if (mode == DImode)
	return (8 + (GET_CODE (XEXP (x, 1)) == CONST_INT ? 0 : 8)
		+ ((GET_CODE (XEXP (x, 0)) == REG 
		    || (GET_CODE (XEXP (x, 0)) == SUBREG
			&& GET_CODE (SUBREG_REG (XEXP (x, 0))) == REG))
		   ? 0 : 8));
      return (1 + ((GET_CODE (XEXP (x, 0)) == REG
		    || (GET_CODE (XEXP (x, 0)) == SUBREG
			&& GET_CODE (SUBREG_REG (XEXP (x, 0))) == REG))
		   ? 0 : 4)
	      + ((GET_CODE (XEXP (x, 1)) == REG
		  || (GET_CODE (XEXP (x, 1)) == SUBREG
		      && GET_CODE (SUBREG_REG (XEXP (x, 1))) == REG)
		  || (GET_CODE (XEXP (x, 1)) == CONST_INT))
		 ? 0 : 4));

    case MINUS:
      if (mode == DImode)
	return (4 + (REG_OR_SUBREG_REG (XEXP (x, 1)) ? 0 : 8)
		+ ((REG_OR_SUBREG_REG (XEXP (x, 0))
		    || (GET_CODE (XEXP (x, 0)) == CONST_INT
		       && const_ok_for_arm (INTVAL (XEXP (x, 0)))))
		   ? 0 : 8));

      if (GET_MODE_CLASS (mode) == MODE_FLOAT)
	return (2 + ((REG_OR_SUBREG_REG (XEXP (x, 1))
		      || (GET_CODE (XEXP (x, 1)) == CONST_DOUBLE
			  && const_double_rtx_ok_for_fpu (XEXP (x, 1))))
		     ? 0 : 8)
		+ ((REG_OR_SUBREG_REG (XEXP (x, 0))
		    || (GET_CODE (XEXP (x, 0)) == CONST_DOUBLE
			&& const_double_rtx_ok_for_fpu (XEXP (x, 0))))
		   ? 0 : 8));

      if (((GET_CODE (XEXP (x, 0)) == CONST_INT
	    && const_ok_for_arm (INTVAL (XEXP (x, 0)))
	    && REG_OR_SUBREG_REG (XEXP (x, 1))))
	  || (((subcode = GET_CODE (XEXP (x, 1))) == ASHIFT
	       || subcode == ASHIFTRT || subcode == LSHIFTRT
	       || subcode == ROTATE || subcode == ROTATERT
	       || (subcode == MULT
		   && GET_CODE (XEXP (XEXP (x, 1), 1)) == CONST_INT
		   && ((INTVAL (XEXP (XEXP (x, 1), 1)) &
			(INTVAL (XEXP (XEXP (x, 1), 1)) - 1)) == 0)))
	      && REG_OR_SUBREG_REG (XEXP (XEXP (x, 1), 0))
	      && (REG_OR_SUBREG_REG (XEXP (XEXP (x, 1), 1))
		  || GET_CODE (XEXP (XEXP (x, 1), 1)) == CONST_INT)
	      && REG_OR_SUBREG_REG (XEXP (x, 0))))
	return 1;
      /* Fall through */

    case PLUS: 
      if (GET_MODE_CLASS (mode) == MODE_FLOAT)
	return (2 + (REG_OR_SUBREG_REG (XEXP (x, 0)) ? 0 : 8)
		+ ((REG_OR_SUBREG_REG (XEXP (x, 1))
		    || (GET_CODE (XEXP (x, 1)) == CONST_DOUBLE
			&& const_double_rtx_ok_for_fpu (XEXP (x, 1))))
		   ? 0 : 8));

      /* Fall through */
    case AND: case XOR: case IOR: 
      extra_cost = 0;

      /* Normally the frame registers will be spilt into reg+const during
	 reload, so it is a bad idea to combine them with other instructions,
	 since then they might not be moved outside of loops.  As a compromise
	 we allow integration with ops that have a constant as their second
	 operand.  */
      if ((REG_OR_SUBREG_REG (XEXP (x, 0))
	   && ARM_FRAME_RTX (REG_OR_SUBREG_RTX (XEXP (x, 0)))
	   && GET_CODE (XEXP (x, 1)) != CONST_INT)
	  || (REG_OR_SUBREG_REG (XEXP (x, 0))
	      && ARM_FRAME_RTX (REG_OR_SUBREG_RTX (XEXP (x, 0)))))
	extra_cost = 4;

      if (mode == DImode)
	return (4 + extra_cost + (REG_OR_SUBREG_REG (XEXP (x, 0)) ? 0 : 8)
		+ ((REG_OR_SUBREG_REG (XEXP (x, 1))
		    || (GET_CODE (XEXP (x, 1)) == CONST_INT
			&& const_ok_for_op (INTVAL (XEXP (x, 1)), code)))
		   ? 0 : 8));

      if (REG_OR_SUBREG_REG (XEXP (x, 0)))
	return (1 + (GET_CODE (XEXP (x, 1)) == CONST_INT ? 0 : extra_cost)
		+ ((REG_OR_SUBREG_REG (XEXP (x, 1))
		    || (GET_CODE (XEXP (x, 1)) == CONST_INT
			&& const_ok_for_op (INTVAL (XEXP (x, 1)), code)))
		   ? 0 : 4));

      else if (REG_OR_SUBREG_REG (XEXP (x, 1)))
	return (1 + extra_cost
		+ ((((subcode = GET_CODE (XEXP (x, 0))) == ASHIFT
		     || subcode == LSHIFTRT || subcode == ASHIFTRT
		     || subcode == ROTATE || subcode == ROTATERT
		     || (subcode == MULT
			 && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
			 && ((INTVAL (XEXP (XEXP (x, 0), 1)) &
			      (INTVAL (XEXP (XEXP (x, 0), 1)) - 1)) == 0)))
		    && (REG_OR_SUBREG_REG (XEXP (XEXP (x, 0), 0)))
		    && ((REG_OR_SUBREG_REG (XEXP (XEXP (x, 0), 1)))
			|| GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT))
		   ? 0 : 4));

      return 8;

    case MULT:
      /* There is no point basing this on the tuning, since it is always the
	 fast variant if it exists at all */
      if (arm_fast_multiply && mode == DImode
	  && (GET_CODE (XEXP (x, 0)) == GET_CODE (XEXP (x, 1)))
	  && (GET_CODE (XEXP (x, 0)) == ZERO_EXTEND
	      || GET_CODE (XEXP (x, 0)) == SIGN_EXTEND))
	return 8;

      if (GET_MODE_CLASS (mode) == MODE_FLOAT
	  || mode == DImode)
	return 30;

      if (GET_CODE (XEXP (x, 1)) == CONST_INT)
	{
	  unsigned HOST_WIDE_INT i = (INTVAL (XEXP (x, 1))
				      & (unsigned HOST_WIDE_INT) 0xffffffffUL);
	  int add_cost = const_ok_for_arm (i) ? 4 : 8;
	  int j;
	  /* Tune as appropriate */ 
	  int booth_unit_size = ((tune_flags & FL_FAST_MULT) ? 8 : 2);
	  
	  for (j = 0; i && j < 32; j += booth_unit_size)
	    {
	      i >>= booth_unit_size;
	      add_cost += 2;
	    }

	  return add_cost;
	}

      return (((tune_flags & FL_FAST_MULT) ? 8 : 30)
	      + (REG_OR_SUBREG_REG (XEXP (x, 0)) ? 0 : 4)
	      + (REG_OR_SUBREG_REG (XEXP (x, 1)) ? 0 : 4));

    case TRUNCATE:
      if (arm_fast_multiply && mode == SImode
	  && GET_CODE (XEXP (x, 0)) == LSHIFTRT
	  && GET_CODE (XEXP (XEXP (x, 0), 0)) == MULT
	  && (GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 0))
	      == GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 1)))
	  && (GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 0)) == ZERO_EXTEND
	      || GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 0)) == SIGN_EXTEND))
	return 8;
      return 99;

    case NEG:
      if (GET_MODE_CLASS (mode) == MODE_FLOAT)
	return 4 + (REG_OR_SUBREG_REG (XEXP (x, 0)) ? 0 : 6);
      /* Fall through */
    case NOT:
      if (mode == DImode)
	return 4 + (REG_OR_SUBREG_REG (XEXP (x, 0)) ? 0 : 4);

      return 1 + (REG_OR_SUBREG_REG (XEXP (x, 0)) ? 0 : 4);

    case IF_THEN_ELSE:
      if (GET_CODE (XEXP (x, 1)) == PC || GET_CODE (XEXP (x, 2)) == PC)
	return 14;
      return 2;

    case COMPARE:
      return 1;

    case ABS:
      return 4 + (mode == DImode ? 4 : 0);

    case SIGN_EXTEND:
      if (GET_MODE (XEXP (x, 0)) == QImode)
	return (4 + (mode == DImode ? 4 : 0)
		+ (GET_CODE (XEXP (x, 0)) == MEM ? 10 : 0));
      /* Fall through */
    case ZERO_EXTEND:
      switch (GET_MODE (XEXP (x, 0)))
	{
	case QImode:
	  return (1 + (mode == DImode ? 4 : 0)
		  + (GET_CODE (XEXP (x, 0)) == MEM ? 10 : 0));

	case HImode:
	  return (4 + (mode == DImode ? 4 : 0)
		  + (GET_CODE (XEXP (x, 0)) == MEM ? 10 : 0));

	case SImode:
	  return (1 + (GET_CODE (XEXP (x, 0)) == MEM ? 10 : 0));

	default:
	  break;
	}
      abort ();

    default:
      return 99;
    }
}

int
arm_adjust_cost (insn, link, dep, cost)
     rtx insn;
     rtx link;
     rtx dep;
     int cost;
{
  rtx i_pat, d_pat;

  /* XXX This is not strictly true for the FPA. */
  if (REG_NOTE_KIND(link) == REG_DEP_ANTI
      || REG_NOTE_KIND(link) == REG_DEP_OUTPUT)
    return 0;

  if ((i_pat = single_set (insn)) != NULL
      && GET_CODE (SET_SRC (i_pat)) == MEM
      && (d_pat = single_set (dep)) != NULL
      && GET_CODE (SET_DEST (d_pat)) == MEM)
    {
      /* This is a load after a store, there is no conflict if the load reads
	 from a cached area.  Assume that loads from the stack, and from the
	 constant pool are cached, and that others will miss.  This is a 
	 hack. */
      
      if (CONSTANT_POOL_ADDRESS_P (XEXP (SET_SRC (i_pat), 0))
	  || reg_mentioned_p (stack_pointer_rtx, XEXP (SET_SRC (i_pat), 0))
	  || reg_mentioned_p (frame_pointer_rtx, XEXP (SET_SRC (i_pat), 0))
	  || reg_mentioned_p (hard_frame_pointer_rtx, 
			      XEXP (SET_SRC (i_pat), 0)))
	return 1;
    }

  return cost;
}

/* This code has been fixed for cross compilation. */

static int fpa_consts_inited = 0;

char * strings_fpa[8] =
{
  "0",   "1",   "2",   "3",
  "4",   "5",   "0.5", "10"
};

static REAL_VALUE_TYPE values_fpa[8];

static void
init_fpa_table ()
{
  int i;
  REAL_VALUE_TYPE r;

  for (i = 0; i < 8; i++)
    {
      r = REAL_VALUE_ATOF (strings_fpa[i], DFmode);
      values_fpa[i] = r;
    }

  fpa_consts_inited = 1;
}

/* Return TRUE if rtx X is a valid immediate FPU constant. */

int
const_double_rtx_ok_for_fpu (x)
     rtx x;
{
  REAL_VALUE_TYPE r;
  int i;
  
  if (!fpa_consts_inited)
    init_fpa_table ();
  
  REAL_VALUE_FROM_CONST_DOUBLE (r, x);
  if (REAL_VALUE_MINUS_ZERO (r))
    return 0;

  for (i = 0; i < 8; i++)
    if (REAL_VALUES_EQUAL (r, values_fpa[i]))
      return 1;

  return 0;
}

/* Return TRUE if rtx X is a valid immediate FPU constant. */

int
neg_const_double_rtx_ok_for_fpu (x)
     rtx x;
{
  REAL_VALUE_TYPE r;
  int i;
  
  if (!fpa_consts_inited)
    init_fpa_table ();
  
  REAL_VALUE_FROM_CONST_DOUBLE (r, x);
  r = REAL_VALUE_NEGATE (r);
  if (REAL_VALUE_MINUS_ZERO (r))
    return 0;

  for (i = 0; i < 8; i++)
    if (REAL_VALUES_EQUAL (r, values_fpa[i]))
      return 1;

  return 0;
}

/* Predicates for `match_operand' and `match_operator'.  */

/* s_register_operand is the same as register_operand, but it doesn't accept
   (SUBREG (MEM)...).

   This function exists because at the time it was put in it led to better
   code.  SUBREG(MEM) always needs a reload in the places where
   s_register_operand is used, and this seemed to lead to excessive
   reloading.  */

int
s_register_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (GET_MODE (op) != mode && mode != VOIDmode)
    return 0;

  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  /* We don't consider registers whose class is NO_REGS
     to be a register operand.  */
  return (GET_CODE (op) == REG
	  && (REGNO (op) >= FIRST_PSEUDO_REGISTER
	      || REGNO_REG_CLASS (REGNO (op)) != NO_REGS));
}

/* Only accept reg, subreg(reg), const_int.  */

int
reg_or_int_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT)
    return 1;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return 0;

  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  /* We don't consider registers whose class is NO_REGS
     to be a register operand.  */
  return (GET_CODE (op) == REG
	  && (REGNO (op) >= FIRST_PSEUDO_REGISTER
	      || REGNO_REG_CLASS (REGNO (op)) != NO_REGS));
}

/* Return 1 if OP is an item in memory, given that we are in reload.  */

int
reload_memory_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  int regno = true_regnum (op);

  return (! CONSTANT_P (op)
	  && (regno == -1
	      || (GET_CODE (op) == REG
		  && REGNO (op) >= FIRST_PSEUDO_REGISTER)));
}

/* Return 1 if OP is a valid memory address, but not valid for a signed byte
   memory access (architecture V4) */
int
bad_signed_byte_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (! memory_operand (op, mode) || GET_CODE (op) != MEM)
    return 0;

  op = XEXP (op, 0);

  /* A sum of anything more complex than reg + reg or reg + const is bad */
  if ((GET_CODE (op) == PLUS || GET_CODE (op) == MINUS)
      && (! s_register_operand (XEXP (op, 0), VOIDmode)
	  || (! s_register_operand (XEXP (op, 1), VOIDmode)
	      && GET_CODE (XEXP (op, 1)) != CONST_INT)))
    return 1;

  /* Big constants are also bad */
  if (GET_CODE (op) == PLUS && GET_CODE (XEXP (op, 1)) == CONST_INT
      && (INTVAL (XEXP (op, 1)) > 0xff
	  || -INTVAL (XEXP (op, 1)) > 0xff))
    return 1;

  /* Everything else is good, or can will automatically be made so. */
  return 0;
}

/* Return TRUE for valid operands for the rhs of an ARM instruction.  */

int
arm_rhs_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (s_register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT && const_ok_for_arm (INTVAL (op))));
}

/* Return TRUE for valid operands for the rhs of an ARM instruction, or a load.
 */

int
arm_rhsm_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (s_register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT && const_ok_for_arm (INTVAL (op)))
	  || memory_operand (op, mode));
}

/* Return TRUE for valid operands for the rhs of an ARM instruction, or if a
   constant that is valid when negated.  */

int
arm_add_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (s_register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT
	      && (const_ok_for_arm (INTVAL (op))
		  || const_ok_for_arm (-INTVAL (op)))));
}

int
arm_not_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (s_register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT
	      && (const_ok_for_arm (INTVAL (op))
		  || const_ok_for_arm (~INTVAL (op)))));
}

/* Return TRUE if the operand is a memory reference which contains an
   offsettable address.  */
int
offsettable_memory_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (mode == VOIDmode)
    mode = GET_MODE (op);

  return (mode == GET_MODE (op)
	  && GET_CODE (op) == MEM
	  && offsettable_address_p (reload_completed | reload_in_progress,
				    mode, XEXP (op, 0)));
}

/* Return TRUE if the operand is a memory reference which is, or can be
   made word aligned by adjusting the offset.  */
int
alignable_memory_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  rtx reg;

  if (mode == VOIDmode)
    mode = GET_MODE (op);

  if (mode != GET_MODE (op) || GET_CODE (op) != MEM)
    return 0;

  op = XEXP (op, 0);

  return ((GET_CODE (reg = op) == REG
	   || (GET_CODE (op) == SUBREG
	       && GET_CODE (reg = SUBREG_REG (op)) == REG)
	   || (GET_CODE (op) == PLUS
	       && GET_CODE (XEXP (op, 1)) == CONST_INT
	       && (GET_CODE (reg = XEXP (op, 0)) == REG
		   || (GET_CODE (XEXP (op, 0)) == SUBREG
		       && GET_CODE (reg = SUBREG_REG (XEXP (op, 0))) == REG))))
	  && REGNO_POINTER_ALIGN (REGNO (reg)) >= 4);
}

/* Similar to s_register_operand, but does not allow hard integer 
   registers.  */
int
f_register_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (GET_MODE (op) != mode && mode != VOIDmode)
    return 0;

  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  /* We don't consider registers whose class is NO_REGS
     to be a register operand.  */
  return (GET_CODE (op) == REG
	  && (REGNO (op) >= FIRST_PSEUDO_REGISTER
	      || REGNO_REG_CLASS (REGNO (op)) == FPU_REGS));
}

/* Return TRUE for valid operands for the rhs of an FPU instruction.  */

int
fpu_rhs_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (s_register_operand (op, mode))
    return TRUE;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) == CONST_DOUBLE)
    return const_double_rtx_ok_for_fpu (op);

  return FALSE;
}

int
fpu_add_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (s_register_operand (op, mode))
    return TRUE;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return FALSE;

  if (GET_CODE (op) == CONST_DOUBLE)
    return (const_double_rtx_ok_for_fpu (op) 
	    || neg_const_double_rtx_ok_for_fpu (op));

  return FALSE;
}

/* Return nonzero if OP is a constant power of two.  */

int
power_of_two_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) == CONST_INT)
    {
      HOST_WIDE_INT value = INTVAL(op);
      return value != 0  &&  (value & (value - 1)) == 0;
    }
  return FALSE;
}

/* Return TRUE for a valid operand of a DImode operation.
   Either: REG, SUBREG, CONST_DOUBLE or MEM(DImode_address).
   Note that this disallows MEM(REG+REG), but allows
   MEM(PRE/POST_INC/DEC(REG)).  */

int
di_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (s_register_operand (op, mode))
    return TRUE;

  if (mode != VOIDmode && GET_MODE (op) != VOIDmode && GET_MODE (op) != DImode)
    return FALSE;

  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  switch (GET_CODE (op))
    {
    case CONST_DOUBLE:
    case CONST_INT:
      return TRUE;

    case MEM:
      return memory_address_p (DImode, XEXP (op, 0));

    default:
      return FALSE;
    }
}

/* Return TRUE for a valid operand of a DFmode operation when -msoft-float.
   Either: REG, SUBREG, CONST_DOUBLE or MEM(DImode_address).
   Note that this disallows MEM(REG+REG), but allows
   MEM(PRE/POST_INC/DEC(REG)).  */

int
soft_df_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (s_register_operand (op, mode))
    return TRUE;

  if (mode != VOIDmode && GET_MODE (op) != mode)
    return FALSE;

  if (GET_CODE (op) == SUBREG && CONSTANT_P (SUBREG_REG (op)))
    return FALSE;
  
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  
  switch (GET_CODE (op))
    {
    case CONST_DOUBLE:
      return TRUE;

    case MEM:
      return memory_address_p (DFmode, XEXP (op, 0));

    default:
      return FALSE;
    }
}

/* Return TRUE for valid index operands. */

int
index_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (s_register_operand(op, mode)
	  || (immediate_operand (op, mode)
	      && INTVAL (op) < 4096 && INTVAL (op) > -4096));
}

/* Return TRUE for valid shifts by a constant. This also accepts any
   power of two on the (somewhat overly relaxed) assumption that the
   shift operator in this case was a mult. */

int
const_shift_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (power_of_two_operand (op, mode)
	  || (immediate_operand (op, mode)
	      && (INTVAL (op) < 32 && INTVAL (op) > 0)));
}

/* Return TRUE for arithmetic operators which can be combined with a multiply
   (shift).  */

int
shiftable_operator (x, mode)
     rtx x;
     enum machine_mode mode;
{
  if (GET_MODE (x) != mode)
    return FALSE;
  else
    {
      enum rtx_code code = GET_CODE (x);

      return (code == PLUS || code == MINUS
	      || code == IOR || code == XOR || code == AND);
    }
}

/* Return TRUE for binary logical operators.  */

int
logical_binary_operator (x, mode)
     rtx x;
     enum machine_mode mode;
{
  if (GET_MODE (x) != mode)
    return FALSE;
  else
    {
      enum rtx_code code = GET_CODE (x);

      return (code == IOR || code == XOR || code == AND);
    }
}

/* Return TRUE for shift operators. */

int
shift_operator (x, mode)
     rtx x;
     enum machine_mode mode;
{
  if (GET_MODE (x) != mode)
    return FALSE;
  else
    {
      enum rtx_code code = GET_CODE (x);

      if (code == MULT)
	return power_of_two_operand (XEXP (x, 1), mode);

      return (code == ASHIFT || code == ASHIFTRT || code == LSHIFTRT
	      || code == ROTATERT);
    }
}

int equality_operator (x, mode)
     rtx x;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return GET_CODE (x) == EQ || GET_CODE (x) == NE;
}

/* Return TRUE for SMIN SMAX UMIN UMAX operators. */

int
minmax_operator (x, mode)
     rtx x;
     enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (x);

  if (GET_MODE (x) != mode)
    return FALSE;

  return code == SMIN || code == SMAX || code == UMIN || code == UMAX;
}

/* return TRUE if x is EQ or NE */

/* Return TRUE if this is the condition code register, if we aren't given
   a mode, accept any class CCmode register */

int
cc_register (x, mode)
     rtx x;
     enum machine_mode mode;
{
  if (mode == VOIDmode)
    {
      mode = GET_MODE (x);
      if (GET_MODE_CLASS (mode) != MODE_CC)
	return FALSE;
    }

  if (mode == GET_MODE (x) && GET_CODE (x) == REG && REGNO (x) == 24)
    return TRUE;

  return FALSE;
}

/* Return TRUE if this is the condition code register, if we aren't given
   a mode, accept any class CCmode register which indicates a dominance
   expression.  */

int
dominant_cc_register (x, mode)
     rtx x;
     enum machine_mode mode;
{
  if (mode == VOIDmode)
    {
      mode = GET_MODE (x);
      if (GET_MODE_CLASS (mode) != MODE_CC)
	return FALSE;
    }

  if (mode != CC_DNEmode && mode != CC_DEQmode
      && mode != CC_DLEmode && mode != CC_DLTmode
      && mode != CC_DGEmode && mode != CC_DGTmode
      && mode != CC_DLEUmode && mode != CC_DLTUmode
      && mode != CC_DGEUmode && mode != CC_DGTUmode)
    return FALSE;

  if (mode == GET_MODE (x) && GET_CODE (x) == REG && REGNO (x) == 24)
    return TRUE;

  return FALSE;
}

/* Return TRUE if X references a SYMBOL_REF.  */
int
symbol_mentioned_p (x)
     rtx x;
{
  register const char * fmt;
  register int i;

  if (GET_CODE (x) == SYMBOL_REF)
    return 1;

  fmt = GET_RTX_FORMAT (GET_CODE (x));
  for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  register int j;

	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (symbol_mentioned_p (XVECEXP (x, i, j)))
	      return 1;
	}
      else if (fmt[i] == 'e' && symbol_mentioned_p (XEXP (x, i)))
	return 1;
    }

  return 0;
}

/* Return TRUE if X references a LABEL_REF.  */
int
label_mentioned_p (x)
     rtx x;
{
  register const char * fmt;
  register int i;

  if (GET_CODE (x) == LABEL_REF)
    return 1;

  fmt = GET_RTX_FORMAT (GET_CODE (x));
  for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  register int j;

	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (label_mentioned_p (XVECEXP (x, i, j)))
	      return 1;
	}
      else if (fmt[i] == 'e' && label_mentioned_p (XEXP (x, i)))
	return 1;
    }

  return 0;
}

enum rtx_code
minmax_code (x)
     rtx x;
{
  enum rtx_code code = GET_CODE (x);

  if (code == SMAX)
    return GE;
  else if (code == SMIN)
    return LE;
  else if (code == UMIN)
    return LEU;
  else if (code == UMAX)
    return GEU;

  abort ();
}

/* Return 1 if memory locations are adjacent */

int
adjacent_mem_locations (a, b)
     rtx a, b;
{
  int val0 = 0, val1 = 0;
  int reg0, reg1;
  
  if ((GET_CODE (XEXP (a, 0)) == REG
       || (GET_CODE (XEXP (a, 0)) == PLUS
	   && GET_CODE (XEXP (XEXP (a, 0), 1)) == CONST_INT))
      && (GET_CODE (XEXP (b, 0)) == REG
	  || (GET_CODE (XEXP (b, 0)) == PLUS
	      && GET_CODE (XEXP (XEXP (b, 0), 1)) == CONST_INT)))
    {
      if (GET_CODE (XEXP (a, 0)) == PLUS)
        {
	  reg0 = REGNO (XEXP (XEXP (a, 0), 0));
	  val0 = INTVAL (XEXP (XEXP (a, 0), 1));
        }
      else
	reg0 = REGNO (XEXP (a, 0));
      if (GET_CODE (XEXP (b, 0)) == PLUS)
        {
	  reg1 = REGNO (XEXP (XEXP (b, 0), 0));
	  val1 = INTVAL (XEXP (XEXP (b, 0), 1));
        }
      else
	reg1 = REGNO (XEXP (b, 0));
      return (reg0 == reg1) && ((val1 - val0) == 4 || (val0 - val1) == 4);
    }
  return 0;
}

/* Return 1 if OP is a load multiple operation.  It is known to be
   parallel and the first section will be tested. */

int
load_multiple_operation (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  HOST_WIDE_INT count = XVECLEN (op, 0);
  int dest_regno;
  rtx src_addr;
  HOST_WIDE_INT i = 1, base = 0;
  rtx elt;

  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET)
    return 0;

  /* Check to see if this might be a write-back */
  if (GET_CODE (SET_SRC (elt = XVECEXP (op, 0, 0))) == PLUS)
    {
      i++;
      base = 1;

      /* Now check it more carefully */
      if (GET_CODE (SET_DEST (elt)) != REG
          || GET_CODE (XEXP (SET_SRC (elt), 0)) != REG
          || REGNO (XEXP (SET_SRC (elt), 0)) != REGNO (SET_DEST (elt))
          || GET_CODE (XEXP (SET_SRC (elt), 1)) != CONST_INT
          || INTVAL (XEXP (SET_SRC (elt), 1)) != (count - 2) * 4
          || GET_CODE (XVECEXP (op, 0, count - 1)) != CLOBBER
          || GET_CODE (XEXP (XVECEXP (op, 0, count - 1), 0)) != REG
          || REGNO (XEXP (XVECEXP (op, 0, count - 1), 0))
              != REGNO (SET_DEST (elt)))
        return 0;

      count--;
    }

  /* Perform a quick check so we don't blow up below.  */
  if (count <= i
      || GET_CODE (XVECEXP (op, 0, i - 1)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, i - 1))) != REG
      || GET_CODE (SET_SRC (XVECEXP (op, 0, i - 1))) != MEM)
    return 0;

  dest_regno = REGNO (SET_DEST (XVECEXP (op, 0, i - 1)));
  src_addr = XEXP (SET_SRC (XVECEXP (op, 0, i - 1)), 0);

  for (; i < count; i++)
    {
      elt = XVECEXP (op, 0, i);

      if (GET_CODE (elt) != SET
          || GET_CODE (SET_DEST (elt)) != REG
          || GET_MODE (SET_DEST (elt)) != SImode
          || REGNO (SET_DEST (elt)) != dest_regno + i - base
          || GET_CODE (SET_SRC (elt)) != MEM
          || GET_MODE (SET_SRC (elt)) != SImode
          || GET_CODE (XEXP (SET_SRC (elt), 0)) != PLUS
          || ! rtx_equal_p (XEXP (XEXP (SET_SRC (elt), 0), 0), src_addr)
          || GET_CODE (XEXP (XEXP (SET_SRC (elt), 0), 1)) != CONST_INT
          || INTVAL (XEXP (XEXP (SET_SRC (elt), 0), 1)) != (i - base) * 4)
        return 0;
    }

  return 1;
}

/* Return 1 if OP is a store multiple operation.  It is known to be
   parallel and the first section will be tested. */

int
store_multiple_operation (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  HOST_WIDE_INT count = XVECLEN (op, 0);
  int src_regno;
  rtx dest_addr;
  HOST_WIDE_INT i = 1, base = 0;
  rtx elt;

  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET)
    return 0;

  /* Check to see if this might be a write-back */
  if (GET_CODE (SET_SRC (elt = XVECEXP (op, 0, 0))) == PLUS)
    {
      i++;
      base = 1;

      /* Now check it more carefully */
      if (GET_CODE (SET_DEST (elt)) != REG
          || GET_CODE (XEXP (SET_SRC (elt), 0)) != REG
          || REGNO (XEXP (SET_SRC (elt), 0)) != REGNO (SET_DEST (elt))
          || GET_CODE (XEXP (SET_SRC (elt), 1)) != CONST_INT
          || INTVAL (XEXP (SET_SRC (elt), 1)) != (count - 2) * 4
          || GET_CODE (XVECEXP (op, 0, count - 1)) != CLOBBER
          || GET_CODE (XEXP (XVECEXP (op, 0, count - 1), 0)) != REG
          || REGNO (XEXP (XVECEXP (op, 0, count - 1), 0))
              != REGNO (SET_DEST (elt)))
        return 0;

      count--;
    }

  /* Perform a quick check so we don't blow up below.  */
  if (count <= i
      || GET_CODE (XVECEXP (op, 0, i - 1)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, i - 1))) != MEM
      || GET_CODE (SET_SRC (XVECEXP (op, 0, i - 1))) != REG)
    return 0;

  src_regno = REGNO (SET_SRC (XVECEXP (op, 0, i - 1)));
  dest_addr = XEXP (SET_DEST (XVECEXP (op, 0, i - 1)), 0);

  for (; i < count; i++)
    {
      elt = XVECEXP (op, 0, i);

      if (GET_CODE (elt) != SET
          || GET_CODE (SET_SRC (elt)) != REG
          || GET_MODE (SET_SRC (elt)) != SImode
          || REGNO (SET_SRC (elt)) != src_regno + i - base
          || GET_CODE (SET_DEST (elt)) != MEM
          || GET_MODE (SET_DEST (elt)) != SImode
          || GET_CODE (XEXP (SET_DEST (elt), 0)) != PLUS
          || ! rtx_equal_p (XEXP (XEXP (SET_DEST (elt), 0), 0), dest_addr)
          || GET_CODE (XEXP (XEXP (SET_DEST (elt), 0), 1)) != CONST_INT
          || INTVAL (XEXP (XEXP (SET_DEST (elt), 0), 1)) != (i - base) * 4)
        return 0;
    }

  return 1;
}

int
load_multiple_sequence (operands, nops, regs, base, load_offset)
     rtx * operands;
     int nops;
     int * regs;
     int * base;
     HOST_WIDE_INT * load_offset;
{
  int unsorted_regs[4];
  HOST_WIDE_INT unsorted_offsets[4];
  int order[4];
  int base_reg = -1;
  int i;

  /* Can only handle 2, 3, or 4 insns at present, though could be easily
     extended if required.  */
  if (nops < 2 || nops > 4)
    abort ();

  /* Loop over the operands and check that the memory references are
     suitable (ie immediate offsets from the same base register).  At
     the same time, extract the target register, and the memory
     offsets.  */
  for (i = 0; i < nops; i++)
    {
      rtx reg;
      rtx offset;

      /* Convert a subreg of a mem into the mem itself.  */
      if (GET_CODE (operands[nops + i]) == SUBREG)
	operands[nops + i] = alter_subreg(operands[nops + i]);

      if (GET_CODE (operands[nops + i]) != MEM)
	abort ();

      /* Don't reorder volatile memory references; it doesn't seem worth
	 looking for the case where the order is ok anyway.  */
      if (MEM_VOLATILE_P (operands[nops + i]))
	return 0;

      offset = const0_rtx;

      if ((GET_CODE (reg = XEXP (operands[nops + i], 0)) == REG
	   || (GET_CODE (reg) == SUBREG
	       && GET_CODE (reg = SUBREG_REG (reg)) == REG))
	  || (GET_CODE (XEXP (operands[nops + i], 0)) == PLUS
	      && ((GET_CODE (reg = XEXP (XEXP (operands[nops + i], 0), 0))
		   == REG)
		  || (GET_CODE (reg) == SUBREG
		      && GET_CODE (reg = SUBREG_REG (reg)) == REG))
	      && (GET_CODE (offset = XEXP (XEXP (operands[nops + i], 0), 1))
		  == CONST_INT)))
	{
	  if (i == 0)
	    {
	      base_reg = REGNO(reg);
	      unsorted_regs[0] = (GET_CODE (operands[i]) == REG
				  ? REGNO (operands[i])
				  : REGNO (SUBREG_REG (operands[i])));
	      order[0] = 0;
	    }
	  else 
	    {
	      if (base_reg != REGNO (reg))
		/* Not addressed from the same base register.  */
		return 0;

	      unsorted_regs[i] = (GET_CODE (operands[i]) == REG
				  ? REGNO (operands[i])
				  : REGNO (SUBREG_REG (operands[i])));
	      if (unsorted_regs[i] < unsorted_regs[order[0]])
		order[0] = i;
	    }

	  /* If it isn't an integer register, or if it overwrites the
	     base register but isn't the last insn in the list, then
	     we can't do this.  */
	  if (unsorted_regs[i] < 0 || unsorted_regs[i] > 14
	      || (i != nops - 1 && unsorted_regs[i] == base_reg))
	    return 0;

	  unsorted_offsets[i] = INTVAL (offset);
	}
      else
	/* Not a suitable memory address.  */
	return 0;
    }

  /* All the useful information has now been extracted from the
     operands into unsorted_regs and unsorted_offsets; additionally,
     order[0] has been set to the lowest numbered register in the
     list.  Sort the registers into order, and check that the memory
     offsets are ascending and adjacent.  */

  for (i = 1; i < nops; i++)
    {
      int j;

      order[i] = order[i - 1];
      for (j = 0; j < nops; j++)
	if (unsorted_regs[j] > unsorted_regs[order[i - 1]]
	    && (order[i] == order[i - 1]
		|| unsorted_regs[j] < unsorted_regs[order[i]]))
	  order[i] = j;

      /* Have we found a suitable register? if not, one must be used more
	 than once.  */
      if (order[i] == order[i - 1])
	return 0;

      /* Is the memory address adjacent and ascending? */
      if (unsorted_offsets[order[i]] != unsorted_offsets[order[i - 1]] + 4)
	return 0;
    }

  if (base)
    {
      *base = base_reg;

      for (i = 0; i < nops; i++)
	regs[i] = unsorted_regs[order[i]];

      *load_offset = unsorted_offsets[order[0]];
    }

  if (unsorted_offsets[order[0]] == 0)
    return 1; /* ldmia */

  if (unsorted_offsets[order[0]] == 4)
    return 2; /* ldmib */

  if (unsorted_offsets[order[nops - 1]] == 0)
    return 3; /* ldmda */

  if (unsorted_offsets[order[nops - 1]] == -4)
    return 4; /* ldmdb */

  /* For ARM8,9 & StrongARM, 2 ldr instructions are faster than an ldm
     if the offset isn't small enough.  The reason 2 ldrs are faster
     is because these ARMs are able to do more than one cache access
     in a single cycle.  The ARM9 and StrongARM have Harvard caches,
     whilst the ARM8 has a double bandwidth cache.  This means that
     these cores can do both an instruction fetch and a data fetch in
     a single cycle, so the trick of calculating the address into a
     scratch register (one of the result regs) and then doing a load
     multiple actually becomes slower (and no smaller in code size).
     That is the transformation
 
 	ldr	rd1, [rbase + offset]
 	ldr	rd2, [rbase + offset + 4]
 
     to
 
 	add	rd1, rbase, offset
 	ldmia	rd1, {rd1, rd2}
 
     produces worse code -- '3 cycles + any stalls on rd2' instead of
     '2 cycles + any stalls on rd2'.  On ARMs with only one cache
     access per cycle, the first sequence could never complete in less
     than 6 cycles, whereas the ldm sequence would only take 5 and
     would make better use of sequential accesses if not hitting the
     cache.

     We cheat here and test 'arm_ld_sched' which we currently know to
     only be true for the ARM8, ARM9 and StrongARM.  If this ever
     changes, then the test below needs to be reworked.  */
  if (nops == 2 && arm_ld_sched)
    return 0;

  /* Can't do it without setting up the offset, only do this if it takes
     no more than one insn.  */
  return (const_ok_for_arm (unsorted_offsets[order[0]]) 
	  || const_ok_for_arm (-unsorted_offsets[order[0]])) ? 5 : 0;
}

char *
emit_ldm_seq (operands, nops)
     rtx * operands;
     int nops;
{
  int regs[4];
  int base_reg;
  HOST_WIDE_INT offset;
  char buf[100];
  int i;

  switch (load_multiple_sequence (operands, nops, regs, &base_reg, &offset))
    {
    case 1:
      strcpy (buf, "ldm%?ia\t");
      break;

    case 2:
      strcpy (buf, "ldm%?ib\t");
      break;

    case 3:
      strcpy (buf, "ldm%?da\t");
      break;

    case 4:
      strcpy (buf, "ldm%?db\t");
      break;

    case 5:
      if (offset >= 0)
	sprintf (buf, "add%%?\t%s%s, %s%s, #%ld", REGISTER_PREFIX,
		 reg_names[regs[0]], REGISTER_PREFIX, reg_names[base_reg],
		 (long) offset);
      else
	sprintf (buf, "sub%%?\t%s%s, %s%s, #%ld", REGISTER_PREFIX,
		 reg_names[regs[0]], REGISTER_PREFIX, reg_names[base_reg],
		 (long) -offset);
      output_asm_insn (buf, operands);
      base_reg = regs[0];
      strcpy (buf, "ldm%?ia\t");
      break;

    default:
      abort ();
    }

  sprintf (buf + strlen (buf), "%s%s, {%s%s", REGISTER_PREFIX, 
	   reg_names[base_reg], REGISTER_PREFIX, reg_names[regs[0]]);

  for (i = 1; i < nops; i++)
    sprintf (buf + strlen (buf), ", %s%s", REGISTER_PREFIX,
	     reg_names[regs[i]]);

  strcat (buf, "}\t%@ phole ldm");

  output_asm_insn (buf, operands);
  return "";
}

int
store_multiple_sequence (operands, nops, regs, base, load_offset)
     rtx * operands;
     int nops;
     int * regs;
     int * base;
     HOST_WIDE_INT * load_offset;
{
  int unsorted_regs[4];
  HOST_WIDE_INT unsorted_offsets[4];
  int order[4];
  int base_reg = -1;
  int i;

  /* Can only handle 2, 3, or 4 insns at present, though could be easily
     extended if required.  */
  if (nops < 2 || nops > 4)
    abort ();

  /* Loop over the operands and check that the memory references are
     suitable (ie immediate offsets from the same base register).  At
     the same time, extract the target register, and the memory
     offsets.  */
  for (i = 0; i < nops; i++)
    {
      rtx reg;
      rtx offset;

      /* Convert a subreg of a mem into the mem itself.  */
      if (GET_CODE (operands[nops + i]) == SUBREG)
	operands[nops + i] = alter_subreg(operands[nops + i]);

      if (GET_CODE (operands[nops + i]) != MEM)
	abort ();

      /* Don't reorder volatile memory references; it doesn't seem worth
	 looking for the case where the order is ok anyway.  */
      if (MEM_VOLATILE_P (operands[nops + i]))
	return 0;

      offset = const0_rtx;

      if ((GET_CODE (reg = XEXP (operands[nops + i], 0)) == REG
	   || (GET_CODE (reg) == SUBREG
	       && GET_CODE (reg = SUBREG_REG (reg)) == REG))
	  || (GET_CODE (XEXP (operands[nops + i], 0)) == PLUS
	      && ((GET_CODE (reg = XEXP (XEXP (operands[nops + i], 0), 0))
		   == REG)
		  || (GET_CODE (reg) == SUBREG
		      && GET_CODE (reg = SUBREG_REG (reg)) == REG))
	      && (GET_CODE (offset = XEXP (XEXP (operands[nops + i], 0), 1))
		  == CONST_INT)))
	{
	  if (i == 0)
	    {
	      base_reg = REGNO (reg);
	      unsorted_regs[0] = (GET_CODE (operands[i]) == REG
				  ? REGNO (operands[i])
				  : REGNO (SUBREG_REG (operands[i])));
	      order[0] = 0;
	    }
	  else 
	    {
	      if (base_reg != REGNO (reg))
		/* Not addressed from the same base register.  */
		return 0;

	      unsorted_regs[i] = (GET_CODE (operands[i]) == REG
				  ? REGNO (operands[i])
				  : REGNO (SUBREG_REG (operands[i])));
	      if (unsorted_regs[i] < unsorted_regs[order[0]])
		order[0] = i;
	    }

	  /* If it isn't an integer register, then we can't do this.  */
	  if (unsorted_regs[i] < 0 || unsorted_regs[i] > 14)
	    return 0;

	  unsorted_offsets[i] = INTVAL (offset);
	}
      else
	/* Not a suitable memory address.  */
	return 0;
    }

  /* All the useful information has now been extracted from the
     operands into unsorted_regs and unsorted_offsets; additionally,
     order[0] has been set to the lowest numbered register in the
     list.  Sort the registers into order, and check that the memory
     offsets are ascending and adjacent.  */

  for (i = 1; i < nops; i++)
    {
      int j;

      order[i] = order[i - 1];
      for (j = 0; j < nops; j++)
	if (unsorted_regs[j] > unsorted_regs[order[i - 1]]
	    && (order[i] == order[i - 1]
		|| unsorted_regs[j] < unsorted_regs[order[i]]))
	  order[i] = j;

      /* Have we found a suitable register? if not, one must be used more
	 than once.  */
      if (order[i] == order[i - 1])
	return 0;

      /* Is the memory address adjacent and ascending? */
      if (unsorted_offsets[order[i]] != unsorted_offsets[order[i - 1]] + 4)
	return 0;
    }

  if (base)
    {
      *base = base_reg;

      for (i = 0; i < nops; i++)
	regs[i] = unsorted_regs[order[i]];

      *load_offset = unsorted_offsets[order[0]];
    }

  if (unsorted_offsets[order[0]] == 0)
    return 1; /* stmia */

  if (unsorted_offsets[order[0]] == 4)
    return 2; /* stmib */

  if (unsorted_offsets[order[nops - 1]] == 0)
    return 3; /* stmda */

  if (unsorted_offsets[order[nops - 1]] == -4)
    return 4; /* stmdb */

  return 0;
}

char *
emit_stm_seq (operands, nops)
     rtx * operands;
     int nops;
{
  int regs[4];
  int base_reg;
  HOST_WIDE_INT offset;
  char buf[100];
  int i;

  switch (store_multiple_sequence (operands, nops, regs, &base_reg, &offset))
    {
    case 1:
      strcpy (buf, "stm%?ia\t");
      break;

    case 2:
      strcpy (buf, "stm%?ib\t");
      break;

    case 3:
      strcpy (buf, "stm%?da\t");
      break;

    case 4:
      strcpy (buf, "stm%?db\t");
      break;

    default:
      abort ();
    }

  sprintf (buf + strlen (buf), "%s%s, {%s%s", REGISTER_PREFIX, 
	   reg_names[base_reg], REGISTER_PREFIX, reg_names[regs[0]]);

  for (i = 1; i < nops; i++)
    sprintf (buf + strlen (buf), ", %s%s", REGISTER_PREFIX,
	     reg_names[regs[i]]);

  strcat (buf, "}\t%@ phole stm");

  output_asm_insn (buf, operands);
  return "";
}

int
multi_register_push (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) != PARALLEL
      || (GET_CODE (XVECEXP (op, 0, 0)) != SET)
      || (GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != UNSPEC)
      || (XINT (SET_SRC (XVECEXP (op, 0, 0)), 1) != 2))
    return 0;

  return 1;
}


/* Routines for use with attributes */

/* Return nonzero if ATTR is a valid attribute for DECL.
   ATTRIBUTES are any existing attributes and ARGS are the arguments
   supplied with ATTR.

   Supported attributes:

   naked: don't output any prologue or epilogue code, the user is assumed
   to do the right thing.  */

int
arm_valid_machine_decl_attribute (decl, attr, args)
     tree decl;
     tree attr;
     tree args;
{
  if (args != NULL_TREE)
    return 0;

  if (is_attribute_p ("naked", attr))
    return TREE_CODE (decl) == FUNCTION_DECL;
  return 0;
}

/* Return non-zero if FUNC is a naked function.  */

static int
arm_naked_function_p (func)
     tree func;
{
  tree a;

  if (TREE_CODE (func) != FUNCTION_DECL)
    abort ();
  
  a = lookup_attribute ("naked", DECL_MACHINE_ATTRIBUTES (func));
  return a != NULL_TREE;
}

/* Routines for use in generating RTL */

rtx
arm_gen_load_multiple (base_regno, count, from, up, write_back, unchanging_p,
		       in_struct_p, scalar_p)
     int base_regno;
     int count;
     rtx from;
     int up;
     int write_back;
     int unchanging_p;
     int in_struct_p;
     int scalar_p;
{
  int i = 0, j;
  rtx result;
  int sign = up ? 1 : -1;
  rtx mem;

  result = gen_rtx_PARALLEL (VOIDmode,
			     rtvec_alloc (count + (write_back ? 2 : 0)));
  if (write_back)
    {
      XVECEXP (result, 0, 0)
	= gen_rtx_SET (GET_MODE (from), from,
		       plus_constant (from, count * 4 * sign));
      i = 1;
      count++;
    }

  for (j = 0; i < count; i++, j++)
    {
      mem = gen_rtx_MEM (SImode, plus_constant (from, j * 4 * sign));
      RTX_UNCHANGING_P (mem) = unchanging_p;
      MEM_IN_STRUCT_P (mem) = in_struct_p;
      MEM_SCALAR_P (mem) = scalar_p;
      XVECEXP (result, 0, i)
	= gen_rtx_SET (VOIDmode, gen_rtx_REG (SImode, base_regno + j), mem);
    }

  if (write_back)
    XVECEXP (result, 0, i) = gen_rtx_CLOBBER (SImode, from);

  return result;
}

rtx
arm_gen_store_multiple (base_regno, count, to, up, write_back, unchanging_p,
			in_struct_p, scalar_p)
     int base_regno;
     int count;
     rtx to;
     int up;
     int write_back;
     int unchanging_p;
     int in_struct_p;
     int scalar_p;
{
  int i = 0, j;
  rtx result;
  int sign = up ? 1 : -1;
  rtx mem;

  result = gen_rtx_PARALLEL (VOIDmode,
			     rtvec_alloc (count + (write_back ? 2 : 0)));
  if (write_back)
    {
      XVECEXP (result, 0, 0)
	= gen_rtx_SET (GET_MODE (to), to,
		       plus_constant (to, count * 4 * sign));
      i = 1;
      count++;
    }

  for (j = 0; i < count; i++, j++)
    {
      mem = gen_rtx_MEM (SImode, plus_constant (to, j * 4 * sign));
      RTX_UNCHANGING_P (mem) = unchanging_p;
      MEM_IN_STRUCT_P (mem) = in_struct_p;
      MEM_SCALAR_P (mem) = scalar_p;

      XVECEXP (result, 0, i)
	= gen_rtx_SET (VOIDmode, mem, gen_rtx_REG (SImode, base_regno + j));
    }

  if (write_back)
    XVECEXP (result, 0, i) = gen_rtx_CLOBBER (SImode, to);

  return result;
}

int
arm_gen_movstrqi (operands)
     rtx * operands;
{
  HOST_WIDE_INT in_words_to_go, out_words_to_go, last_bytes;
  int i;
  rtx src, dst;
  rtx st_src, st_dst, fin_src, fin_dst;
  rtx part_bytes_reg = NULL;
  rtx mem;
  int dst_unchanging_p, dst_in_struct_p, src_unchanging_p, src_in_struct_p;
  int dst_scalar_p, src_scalar_p;

  if (GET_CODE (operands[2]) != CONST_INT
      || GET_CODE (operands[3]) != CONST_INT
      || INTVAL (operands[2]) > 64
      || INTVAL (operands[3]) & 3)
    return 0;

  st_dst = XEXP (operands[0], 0);
  st_src = XEXP (operands[1], 0);

  dst_unchanging_p = RTX_UNCHANGING_P (operands[0]);
  dst_in_struct_p = MEM_IN_STRUCT_P (operands[0]);
  dst_scalar_p = MEM_SCALAR_P (operands[0]);
  src_unchanging_p = RTX_UNCHANGING_P (operands[1]);
  src_in_struct_p = MEM_IN_STRUCT_P (operands[1]);
  src_scalar_p = MEM_SCALAR_P (operands[1]);

  fin_dst = dst = copy_to_mode_reg (SImode, st_dst);
  fin_src = src = copy_to_mode_reg (SImode, st_src);

  in_words_to_go = (INTVAL (operands[2]) + 3) / 4;
  out_words_to_go = INTVAL (operands[2]) / 4;
  last_bytes = INTVAL (operands[2]) & 3;

  if (out_words_to_go != in_words_to_go && ((in_words_to_go - 1) & 3) != 0)
    part_bytes_reg = gen_rtx_REG (SImode, (in_words_to_go - 1) & 3);

  for (i = 0; in_words_to_go >= 2; i+=4)
    {
      if (in_words_to_go > 4)
	emit_insn (arm_gen_load_multiple (0, 4, src, TRUE, TRUE,
					  src_unchanging_p,
					  src_in_struct_p,
					  src_scalar_p));
      else
	emit_insn (arm_gen_load_multiple (0, in_words_to_go, src, TRUE, 
					  FALSE, src_unchanging_p,
					  src_in_struct_p, src_scalar_p));

      if (out_words_to_go)
	{
	  if (out_words_to_go > 4)
	    emit_insn (arm_gen_store_multiple (0, 4, dst, TRUE, TRUE,
					       dst_unchanging_p,
					       dst_in_struct_p,
					       dst_scalar_p));
	  else if (out_words_to_go != 1)
	    emit_insn (arm_gen_store_multiple (0, out_words_to_go,
					       dst, TRUE, 
					       (last_bytes == 0
						? FALSE : TRUE),
					       dst_unchanging_p,
					       dst_in_struct_p,
					       dst_scalar_p));
	  else
	    {
	      mem = gen_rtx_MEM (SImode, dst);
	      RTX_UNCHANGING_P (mem) = dst_unchanging_p;
	      MEM_IN_STRUCT_P (mem) = dst_in_struct_p;
	      MEM_SCALAR_P (mem) = dst_scalar_p;
	      emit_move_insn (mem, gen_rtx_REG (SImode, 0));
	      if (last_bytes != 0)
		emit_insn (gen_addsi3 (dst, dst, GEN_INT (4)));
	    }
	}

      in_words_to_go -= in_words_to_go < 4 ? in_words_to_go : 4;
      out_words_to_go -= out_words_to_go < 4 ? out_words_to_go : 4;
    }

  /* OUT_WORDS_TO_GO will be zero here if there are byte stores to do.  */
  if (out_words_to_go)
    {
      rtx sreg;
      
      mem = gen_rtx_MEM (SImode, src);
      RTX_UNCHANGING_P (mem) = src_unchanging_p;
      MEM_IN_STRUCT_P (mem) = src_in_struct_p;
      MEM_SCALAR_P (mem) = src_scalar_p;
      emit_move_insn (sreg = gen_reg_rtx (SImode), mem);
      emit_move_insn (fin_src = gen_reg_rtx (SImode), plus_constant (src, 4));
      
      mem = gen_rtx_MEM (SImode, dst);
      RTX_UNCHANGING_P (mem) = dst_unchanging_p;
      MEM_IN_STRUCT_P (mem) = dst_in_struct_p;
      MEM_SCALAR_P (mem) = dst_scalar_p;
      emit_move_insn (mem, sreg);
      emit_move_insn (fin_dst = gen_reg_rtx (SImode), plus_constant (dst, 4));
      in_words_to_go--;
      
      if (in_words_to_go)	/* Sanity check */
	abort ();
    }

  if (in_words_to_go)
    {
      if (in_words_to_go < 0)
	abort ();

      mem = gen_rtx_MEM (SImode, src);
      RTX_UNCHANGING_P (mem) = src_unchanging_p;
      MEM_IN_STRUCT_P (mem) = src_in_struct_p;
      MEM_SCALAR_P (mem) = src_scalar_p;
      part_bytes_reg = copy_to_mode_reg (SImode, mem);
    }

  if (BYTES_BIG_ENDIAN && last_bytes)
    {
      rtx tmp = gen_reg_rtx (SImode);

      if (part_bytes_reg == NULL)
	abort ();

      /* The bytes we want are in the top end of the word */
      emit_insn (gen_lshrsi3 (tmp, part_bytes_reg,
			      GEN_INT (8 * (4 - last_bytes))));
      part_bytes_reg = tmp;
      
      while (last_bytes)
	{
	  mem = gen_rtx_MEM (QImode, plus_constant (dst, last_bytes - 1));
	  RTX_UNCHANGING_P (mem) = dst_unchanging_p;
	  MEM_IN_STRUCT_P (mem) = dst_in_struct_p;
	  MEM_SCALAR_P (mem) = dst_scalar_p;
	  emit_move_insn (mem, gen_rtx_SUBREG (QImode, part_bytes_reg, 0));
	  
	  if (--last_bytes)
	    {
	      tmp = gen_reg_rtx (SImode);
	      emit_insn (gen_lshrsi3 (tmp, part_bytes_reg, GEN_INT (8)));
	      part_bytes_reg = tmp;
	    }
	}
	  
    }
  else
    {
      while (last_bytes)
	{
	  if (part_bytes_reg == NULL)
	    abort ();

	  mem = gen_rtx_MEM (QImode, dst);
	  RTX_UNCHANGING_P (mem) = dst_unchanging_p;
	  MEM_IN_STRUCT_P (mem) = dst_in_struct_p;
	  MEM_SCALAR_P (mem) = dst_scalar_p;
	  emit_move_insn (mem, gen_rtx_SUBREG (QImode, part_bytes_reg, 0));
	  
	  if (--last_bytes)
	    {
	      rtx tmp = gen_reg_rtx (SImode);

	      emit_insn (gen_addsi3 (dst, dst, const1_rtx));
	      emit_insn (gen_lshrsi3 (tmp, part_bytes_reg, GEN_INT (8)));
	      part_bytes_reg = tmp;
	    }
	}
    }

  return 1;
}

/* Generate a memory reference for a half word, such that it will be loaded
   into the top 16 bits of the word.  We can assume that the address is
   known to be alignable and of the form reg, or plus (reg, const).  */
rtx
gen_rotated_half_load (memref)
     rtx memref;
{
  HOST_WIDE_INT offset = 0;
  rtx base = XEXP (memref, 0);

  if (GET_CODE (base) == PLUS)
    {
      offset = INTVAL (XEXP (base, 1));
      base = XEXP (base, 0);
    }

  /* If we aren't allowed to generate unaligned addresses, then fail.  */
  if (TARGET_MMU_TRAPS
      && ((BYTES_BIG_ENDIAN ? 1 : 0) ^ ((offset & 2) == 0)))
    return NULL;

  base = gen_rtx_MEM (SImode, plus_constant (base, offset & ~2));

  if ((BYTES_BIG_ENDIAN ? 1 : 0) ^ ((offset & 2) == 2))
    return base;

  return gen_rtx_ROTATE (SImode, base, GEN_INT (16));
}

static enum machine_mode
select_dominance_cc_mode (x, y, cond_or)
     rtx x;
     rtx y;
     HOST_WIDE_INT cond_or;
{
  enum rtx_code cond1, cond2;
  int swapped = 0;

  /* Currently we will probably get the wrong result if the individual
     comparisons are not simple.  This also ensures that it is safe to
     reverse a comparison if necessary.  */
  if ((arm_select_cc_mode (cond1 = GET_CODE (x), XEXP (x, 0), XEXP (x, 1))
       != CCmode)
      || (arm_select_cc_mode (cond2 = GET_CODE (y), XEXP (y, 0), XEXP (y, 1))
	  != CCmode))
    return CCmode;

  if (cond_or)
    cond1 = reverse_condition (cond1);

  /* If the comparisons are not equal, and one doesn't dominate the other,
     then we can't do this.  */
  if (cond1 != cond2 
      && ! comparison_dominates_p (cond1, cond2)
      && (swapped = 1, ! comparison_dominates_p (cond2, cond1)))
    return CCmode;

  if (swapped)
    {
      enum rtx_code temp = cond1;
      cond1 = cond2;
      cond2 = temp;
    }

  switch (cond1)
    {
    case EQ:
      if (cond2 == EQ || ! cond_or)
	return CC_DEQmode;

      switch (cond2)
	{
	case LE: return CC_DLEmode;
	case LEU: return CC_DLEUmode;
	case GE: return CC_DGEmode;
	case GEU: return CC_DGEUmode;
	default: break;
	}

      break;

    case LT:
      if (cond2 == LT || ! cond_or)
	return CC_DLTmode;
      if (cond2 == LE)
	return CC_DLEmode;
      if (cond2 == NE)
	return CC_DNEmode;
      break;

    case GT:
      if (cond2 == GT || ! cond_or)
	return CC_DGTmode;
      if (cond2 == GE)
	return CC_DGEmode;
      if (cond2 == NE)
	return CC_DNEmode;
      break;
      
    case LTU:
      if (cond2 == LTU || ! cond_or)
	return CC_DLTUmode;
      if (cond2 == LEU)
	return CC_DLEUmode;
      if (cond2 == NE)
	return CC_DNEmode;
      break;

    case GTU:
      if (cond2 == GTU || ! cond_or)
	return CC_DGTUmode;
      if (cond2 == GEU)
	return CC_DGEUmode;
      if (cond2 == NE)
	return CC_DNEmode;
      break;

    /* The remaining cases only occur when both comparisons are the
       same.  */
    case NE:
      return CC_DNEmode;

    case LE:
      return CC_DLEmode;

    case GE:
      return CC_DGEmode;

    case LEU:
      return CC_DLEUmode;

    case GEU:
      return CC_DGEUmode;

    default:
      break;
    }

  abort ();
}

enum machine_mode
arm_select_cc_mode (op, x, y)
     enum rtx_code op;
     rtx x;
     rtx y;
{
  /* All floating point compares return CCFP if it is an equality
     comparison, and CCFPE otherwise.  */
  if (GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT)
    return (op == EQ || op == NE) ? CCFPmode : CCFPEmode;
  
  /* A compare with a shifted operand.  Because of canonicalization, the
     comparison will have to be swapped when we emit the assembler.  */
  if (GET_MODE (y) == SImode && GET_CODE (y) == REG
      && (GET_CODE (x) == ASHIFT || GET_CODE (x) == ASHIFTRT
	  || GET_CODE (x) == LSHIFTRT || GET_CODE (x) == ROTATE
	  || GET_CODE (x) == ROTATERT))
    return CC_SWPmode;

  /* This is a special case that is used by combine to allow a 
     comparison of a shifted byte load to be split into a zero-extend
     followed by a comparison of the shifted integer (only valid for
     equalities and unsigned inequalities).  */
  if (GET_MODE (x) == SImode
      && GET_CODE (x) == ASHIFT
      && GET_CODE (XEXP (x, 1)) == CONST_INT && INTVAL (XEXP (x, 1)) == 24
      && GET_CODE (XEXP (x, 0)) == SUBREG
      && GET_CODE (SUBREG_REG (XEXP (x, 0))) == MEM
      && GET_MODE (SUBREG_REG (XEXP (x, 0))) == QImode
      && (op == EQ || op == NE
	  || op == GEU || op == GTU || op == LTU || op == LEU)
      && GET_CODE (y) == CONST_INT)
    return CC_Zmode;

  /* An operation that sets the condition codes as a side-effect, the
     V flag is not set correctly, so we can only use comparisons where
     this doesn't matter.  (For LT and GE we can use "mi" and "pl"
     instead.  */
  if (GET_MODE (x) == SImode
      && y == const0_rtx
      && (op == EQ || op == NE || op == LT || op == GE)
      && (GET_CODE (x) == PLUS || GET_CODE (x) == MINUS
	  || GET_CODE (x) == AND || GET_CODE (x) == IOR
	  || GET_CODE (x) == XOR || GET_CODE (x) == MULT
	  || GET_CODE (x) == NOT || GET_CODE (x) == NEG
	  || GET_CODE (x) == LSHIFTRT
	  || GET_CODE (x) == ASHIFT || GET_CODE (x) == ASHIFTRT
	  || GET_CODE (x) == ROTATERT || GET_CODE (x) == ZERO_EXTRACT))
    return CC_NOOVmode;

  /* A construct for a conditional compare, if the false arm contains
     0, then both conditions must be true, otherwise either condition
     must be true.  Not all conditions are possible, so CCmode is
     returned if it can't be done.  */
  if (GET_CODE (x) == IF_THEN_ELSE
      && (XEXP (x, 2) == const0_rtx
	  || XEXP (x, 2) == const1_rtx)
      && GET_RTX_CLASS (GET_CODE (XEXP (x, 0))) == '<'
      && GET_RTX_CLASS (GET_CODE (XEXP (x, 1))) == '<')
    return select_dominance_cc_mode (XEXP (x, 0), XEXP (x, 1), 
				     INTVAL (XEXP (x, 2)));

  if (GET_MODE (x) == QImode && (op == EQ || op == NE))
    return CC_Zmode;

  if (GET_MODE (x) == SImode && (op == LTU || op == GEU)
      && GET_CODE (x) == PLUS
      && (rtx_equal_p (XEXP (x, 0), y) || rtx_equal_p (XEXP (x, 1), y)))
    return CC_Cmode;

  return CCmode;
}

/* X and Y are two things to compare using CODE.  Emit the compare insn and
   return the rtx for register 0 in the proper mode.  FP means this is a
   floating point compare: I don't think that it is needed on the arm.  */

rtx
gen_compare_reg (code, x, y)
     enum rtx_code code;
     rtx x, y;
{
  enum machine_mode mode = SELECT_CC_MODE (code, x, y);
  rtx cc_reg = gen_rtx_REG (mode, 24);

  emit_insn (gen_rtx_SET (VOIDmode, cc_reg,
			  gen_rtx_COMPARE (mode, x, y)));

  return cc_reg;
}

void
arm_reload_in_hi (operands)
     rtx * operands;
{
  rtx ref = operands[1];
  rtx base, scratch;
  HOST_WIDE_INT offset = 0;

  if (GET_CODE (ref) == SUBREG)
    {
      offset = SUBREG_WORD (ref) * UNITS_PER_WORD;
      if (BYTES_BIG_ENDIAN)
	offset -= (MIN (UNITS_PER_WORD, GET_MODE_SIZE (GET_MODE (ref)))
		   - MIN (UNITS_PER_WORD,
			  GET_MODE_SIZE (GET_MODE (SUBREG_REG (ref)))));
      ref = SUBREG_REG (ref);
    }

  if (GET_CODE (ref) == REG)
    {
      /* We have a pseudo which has been spilt onto the stack; there
	 are two cases here: the first where there is a simple
	 stack-slot replacement and a second where the stack-slot is
	 out of range, or is used as a subreg.  */
      if (reg_equiv_mem[REGNO (ref)])
	{
	  ref = reg_equiv_mem[REGNO (ref)];
	  base = find_replacement (&XEXP (ref, 0));
	}
      else
	/* The slot is out of range, or was dressed up in a SUBREG */
	base = reg_equiv_address[REGNO (ref)];
    }
  else
    base = find_replacement (&XEXP (ref, 0));

  /* Handle the case where the address is too complex to be offset by 1.  */
  if (GET_CODE (base) == MINUS
      || (GET_CODE (base) == PLUS && GET_CODE (XEXP (base, 1)) != CONST_INT))
    {
      rtx base_plus = gen_rtx_REG (SImode, REGNO (operands[2]) + 1);

      emit_insn (gen_rtx_SET (VOIDmode, base_plus, base));
      base = base_plus;
    }
  else if (GET_CODE (base) == PLUS)
    {
      /* The addend must be CONST_INT, or we would have dealt with it above */
      HOST_WIDE_INT hi, lo;

      offset += INTVAL (XEXP (base, 1));
      base = XEXP (base, 0);

      /* Rework the address into a legal sequence of insns */
      /* Valid range for lo is -4095 -> 4095 */
      lo = (offset >= 0
	    ? (offset & 0xfff)
	    : -((-offset) & 0xfff));

      /* Corner case, if lo is the max offset then we would be out of range
	 once we have added the additional 1 below, so bump the msb into the
	 pre-loading insn(s).  */
      if (lo == 4095)
	lo &= 0x7ff;

      hi = ((((offset - lo) & (HOST_WIDE_INT) 0xFFFFFFFFUL)
	     ^ (HOST_WIDE_INT) 0x80000000UL)
	    - (HOST_WIDE_INT) 0x80000000UL);

      if (hi + lo != offset)
	abort ();

      if (hi != 0)
	{
	  rtx base_plus = gen_rtx_REG (SImode, REGNO (operands[2]) + 1);

	  /* Get the base address; addsi3 knows how to handle constants
	     that require more than one insn */
	  emit_insn (gen_addsi3 (base_plus, base, GEN_INT (hi)));
	  base = base_plus;
	  offset = lo;
	}
    }

  scratch = gen_rtx_REG (SImode, REGNO (operands[2]));
  emit_insn (gen_zero_extendqisi2 (scratch,
				   gen_rtx_MEM (QImode,
						plus_constant (base,
							       offset))));
  emit_insn (gen_zero_extendqisi2 (gen_rtx_SUBREG (SImode, operands[0], 0),
				   gen_rtx_MEM (QImode, 
						plus_constant (base,
							       offset + 1))));
  if (! BYTES_BIG_ENDIAN)
    emit_insn (gen_rtx_SET (VOIDmode, gen_rtx_SUBREG (SImode, operands[0], 0),
			gen_rtx_IOR (SImode, 
				     gen_rtx_ASHIFT
				     (SImode,
				      gen_rtx_SUBREG (SImode, operands[0], 0),
				      GEN_INT (8)),
				     scratch)));
  else
    emit_insn (gen_rtx_SET (VOIDmode, gen_rtx_SUBREG (SImode, operands[0], 0),
			    gen_rtx_IOR (SImode, 
					 gen_rtx_ASHIFT (SImode, scratch,
							 GEN_INT (8)),
					 gen_rtx_SUBREG (SImode, operands[0],
							 0))));
}

/* Handle storing a half-word to memory during reload by synthesising as two
   byte stores.  Take care not to clobber the input values until after we
   have moved them somewhere safe.  This code assumes that if the DImode
   scratch in operands[2] overlaps either the input value or output address
   in some way, then that value must die in this insn (we absolutely need
   two scratch registers for some corner cases).  */
void
arm_reload_out_hi (operands)
     rtx * operands;
{
  rtx ref = operands[0];
  rtx outval = operands[1];
  rtx base, scratch;
  HOST_WIDE_INT offset = 0;

  if (GET_CODE (ref) == SUBREG)
    {
      offset = SUBREG_WORD (ref) * UNITS_PER_WORD;
      if (BYTES_BIG_ENDIAN)
	offset -= (MIN (UNITS_PER_WORD, GET_MODE_SIZE (GET_MODE (ref)))
		   - MIN (UNITS_PER_WORD,
			  GET_MODE_SIZE (GET_MODE (SUBREG_REG (ref)))));
      ref = SUBREG_REG (ref);
    }


  if (GET_CODE (ref) == REG)
    {
      /* We have a pseudo which has been spilt onto the stack; there
	 are two cases here: the first where there is a simple
	 stack-slot replacement and a second where the stack-slot is
	 out of range, or is used as a subreg.  */
      if (reg_equiv_mem[REGNO (ref)])
	{
	  ref = reg_equiv_mem[REGNO (ref)];
	  base = find_replacement (&XEXP (ref, 0));
	}
      else
	/* The slot is out of range, or was dressed up in a SUBREG */
	base = reg_equiv_address[REGNO (ref)];
    }
  else
    base = find_replacement (&XEXP (ref, 0));

  scratch = gen_rtx_REG (SImode, REGNO (operands[2]));

  /* Handle the case where the address is too complex to be offset by 1.  */
  if (GET_CODE (base) == MINUS
      || (GET_CODE (base) == PLUS && GET_CODE (XEXP (base, 1)) != CONST_INT))
    {
      rtx base_plus = gen_rtx_REG (SImode, REGNO (operands[2]) + 1);

      /* Be careful not to destroy OUTVAL.  */
      if (reg_overlap_mentioned_p (base_plus, outval))
	{
	  /* Updating base_plus might destroy outval, see if we can
	     swap the scratch and base_plus.  */
	  if (! reg_overlap_mentioned_p (scratch, outval))
	    {
	      rtx tmp = scratch;
	      scratch = base_plus;
	      base_plus = tmp;
	    }
	  else
	    {
	      rtx scratch_hi = gen_rtx_REG (HImode, REGNO (operands[2]));

	      /* Be conservative and copy OUTVAL into the scratch now,
		 this should only be necessary if outval is a subreg
		 of something larger than a word.  */
	      /* XXX Might this clobber base?  I can't see how it can,
		 since scratch is known to overlap with OUTVAL, and
		 must be wider than a word.  */
	      emit_insn (gen_movhi (scratch_hi, outval));
	      outval = scratch_hi;
	    }
	}

      emit_insn (gen_rtx_SET (VOIDmode, base_plus, base));
      base = base_plus;
    }
  else if (GET_CODE (base) == PLUS)
    {
      /* The addend must be CONST_INT, or we would have dealt with it above */
      HOST_WIDE_INT hi, lo;

      offset += INTVAL (XEXP (base, 1));
      base = XEXP (base, 0);

      /* Rework the address into a legal sequence of insns */
      /* Valid range for lo is -4095 -> 4095 */
      lo = (offset >= 0
	    ? (offset & 0xfff)
	    : -((-offset) & 0xfff));

      /* Corner case, if lo is the max offset then we would be out of range
	 once we have added the additional 1 below, so bump the msb into the
	 pre-loading insn(s).  */
      if (lo == 4095)
	lo &= 0x7ff;

      hi = ((((offset - lo) & (HOST_WIDE_INT) 0xFFFFFFFFUL)
	     ^ (HOST_WIDE_INT) 0x80000000UL)
	    - (HOST_WIDE_INT) 0x80000000UL);

      if (hi + lo != offset)
	abort ();

      if (hi != 0)
	{
	  rtx base_plus = gen_rtx_REG (SImode, REGNO (operands[2]) + 1);

	  /* Be careful not to destroy OUTVAL.  */
	  if (reg_overlap_mentioned_p (base_plus, outval))
	    {
	      /* Updating base_plus might destroy outval, see if we
		 can swap the scratch and base_plus.  */
	      if (! reg_overlap_mentioned_p (scratch, outval))
		{
		  rtx tmp = scratch;
		  scratch = base_plus;
		  base_plus = tmp;
		}
	      else
		{
		  rtx scratch_hi = gen_rtx_REG (HImode, REGNO (operands[2]));

		  /* Be conservative and copy outval into scratch now,
		     this should only be necessary if outval is a
		     subreg of something larger than a word.  */
		  /* XXX Might this clobber base?  I can't see how it
		     can, since scratch is known to overlap with
		     outval.  */
		  emit_insn (gen_movhi (scratch_hi, outval));
		  outval = scratch_hi;
		}
	    }

	  /* Get the base address; addsi3 knows how to handle constants
	     that require more than one insn */
	  emit_insn (gen_addsi3 (base_plus, base, GEN_INT (hi)));
	  base = base_plus;
	  offset = lo;
	}
    }

  if (BYTES_BIG_ENDIAN)
    {
      emit_insn (gen_movqi (gen_rtx_MEM (QImode, 
					 plus_constant (base, offset + 1)),
			    gen_rtx_SUBREG (QImode, outval, 0)));
      emit_insn (gen_lshrsi3 (scratch,
			      gen_rtx_SUBREG (SImode, outval, 0),
			      GEN_INT (8)));
      emit_insn (gen_movqi (gen_rtx_MEM (QImode, plus_constant (base, offset)),
			    gen_rtx_SUBREG (QImode, scratch, 0)));
    }
  else
    {
      emit_insn (gen_movqi (gen_rtx_MEM (QImode, plus_constant (base, offset)),
			    gen_rtx_SUBREG (QImode, outval, 0)));
      emit_insn (gen_lshrsi3 (scratch,
			      gen_rtx_SUBREG (SImode, outval, 0),
			      GEN_INT (8)));
      emit_insn (gen_movqi (gen_rtx_MEM (QImode,
					 plus_constant (base, offset + 1)),
			    gen_rtx_SUBREG (QImode, scratch, 0)));
    }
}

/* Routines for manipulation of the constant pool.  */

/* Arm instructions cannot load a large constant directly into a
   register; they have to come from a pc relative load.  The constant
   must therefore be placed in the addressable range of the pc
   relative load.  Depending on the precise pc relative load
   instruction the range is somewhere between 256 bytes and 4k.  This
   means that we often have to dump a constant inside a function, and
   generate code to branch around it.

   It is important to minimize this, since the branches will slow
   things down and make the code larger.

   Normally we can hide the table after an existing unconditional
   branch so that there is no interruption of the flow, but in the
   worst case the code looks like this:

	ldr	rn, L1
	...
	b	L2
	align
	L1:	.long value
	L2:
	...

	ldr	rn, L3
	...
	b	L4
	align
	L3:	.long value
	L4:
	...

   We fix this by performing a scan after scheduling, which notices
   which instructions need to have their operands fetched from the
   constant table and builds the table.

   The algorithm starts by building a table of all the constants that
   need fixing up and all the natural barriers in the function (places
   where a constant table can be dropped without breaking the flow).
   For each fixup we note how far the pc-relative replacement will be
   able to reach and the offset of the instruction into the function.

   Having built the table we then group the fixes together to form
   tables that are as large as possible (subject to addressing
   constraints) and emit each table of constants after the last
   barrier that is within range of all the instructions in the group.
   If a group does not contain a barrier, then we forcibly create one
   by inserting a jump instruction into the flow.  Once the table has
   been inserted, the insns are then modified to reference the
   relevant entry in the pool.

   Possible enhancements to the alogorithm (not implemented) are:

   1) ARM instructions (but not thumb) can use negative offsets, so we
   could reference back to a previous pool rather than forwards to a
   new one.  For large functions this may reduce the number of pools
   required.

   2) For some processors and object formats, there may be benefit in
   aligning the pools to the start of cache lines; this alignment
   would need to be taken into account when calculating addressability
   of a pool.

 */

typedef struct
{
  rtx value;                    /* Value in table */
  HOST_WIDE_INT next_offset;
  enum machine_mode mode;       /* Mode of value */
} minipool_node;

/* The maximum number of constants that can fit into one pool, since
   the pc relative range is 0...4092 bytes and constants are at least 4
   bytes long.  */

#define MAX_MINIPOOL_SIZE (4092/4)
static minipool_node minipool_vector[MAX_MINIPOOL_SIZE];
static int minipool_size;
static rtx minipool_vector_label;

/* Add a constant to the pool and return its offset within the current
   pool.

   X is the rtx we want to replace. MODE is its mode.  On return,
   ADDRESS_ONLY will be non-zero if we really want the address of such
   a constant, not the constant itself.  */
static HOST_WIDE_INT
add_minipool_constant (x, mode)
     rtx x;
     enum machine_mode mode;
{
  int i;
  HOST_WIDE_INT offset;
  
  /* First, see if we've already got it.  */
  for (i = 0; i < minipool_size; i++)
    {
      if (GET_CODE (x) == minipool_vector[i].value->code
	  && mode == minipool_vector[i].mode)
	{
	  if (GET_CODE (x) == CODE_LABEL)
	    {
	      if (XINT (x, 3) != XINT (minipool_vector[i].value, 3))
		continue;
	    }
	  if (rtx_equal_p (x, minipool_vector[i].value))
	    return minipool_vector[i].next_offset - GET_MODE_SIZE (mode);
	}
    }

  /* Need a new one */
  minipool_vector[minipool_size].next_offset = GET_MODE_SIZE (mode);
  offset = 0;
  if (minipool_size == 0)
    minipool_vector_label = gen_label_rtx ();
  else
    minipool_vector[minipool_size].next_offset
      += (offset = minipool_vector[minipool_size - 1].next_offset);

  minipool_vector[minipool_size].value = x;
  minipool_vector[minipool_size].mode = mode;
  minipool_size++;
  return offset;
}

/* Output the literal table */
static void
dump_minipool (scan)
     rtx scan;
{
  int i;

  scan = emit_label_after (gen_label_rtx (), scan);
  scan = emit_insn_after (gen_align_4 (), scan);
  scan = emit_label_after (minipool_vector_label, scan);

  for (i = 0; i < minipool_size; i++)
    {
      minipool_node *p = minipool_vector + i;

      switch (GET_MODE_SIZE (p->mode))
	{
	case 4:
	  scan = emit_insn_after (gen_consttable_4 (p->value), scan);
	  break;

	case 8:
	  scan = emit_insn_after (gen_consttable_8 (p->value), scan);
	  break;

	default:
	  abort ();
	  break;
	}
    }

  scan = emit_insn_after (gen_consttable_end (), scan);
  scan = emit_barrier_after (scan);
  minipool_size = 0;
}

/* Find the last barrier less than MAX_COUNT bytes from FROM, or
   create one.  */
static rtx
find_barrier (from, max_count)
     rtx from;
     int max_count;
{
  int count = 0;
  rtx found_barrier = 0;
  rtx last = from;

  while (from && count < max_count)
    {
      rtx tmp;
      
      if (GET_CODE (from) == BARRIER)
	found_barrier = from;

      /* Count the length of this insn */
      if (GET_CODE (from) == JUMP_INSN
	  && JUMP_LABEL (from) != 0
	  && ((tmp = next_real_insn (JUMP_LABEL (from)))
	      == next_real_insn (from))
	  && tmp != NULL
	  && GET_CODE (tmp) == JUMP_INSN
	  && (GET_CODE (PATTERN (tmp)) == ADDR_VEC
	      || GET_CODE (PATTERN (tmp)) == ADDR_DIFF_VEC))
	{
	  int elt = GET_CODE (PATTERN (tmp)) == ADDR_DIFF_VEC ? 1 : 0;
	  count += (get_attr_length (from)
		    + GET_MODE_SIZE (SImode) * XVECLEN (PATTERN (tmp), elt));
	  /* Continue after the dispatch table.  */
	  last = from;
	  from = NEXT_INSN (tmp);
	  continue;
	}
      else
	count += get_attr_length (from);

      last = from;
      from = NEXT_INSN (from);
    }

  if (! found_barrier)
    {
      /* We didn't find a barrier in time to
	 dump our stuff, so we'll make one.  */
      rtx label = gen_label_rtx ();
      
      if (from)
	from = PREV_INSN (last);
      else
	from = get_last_insn ();
      
      /* Walk back to be just before any jump.  */
      while (GET_CODE (from) == JUMP_INSN
	     || GET_CODE (from) == NOTE
	     || GET_CODE (from) == CODE_LABEL)
	from = PREV_INSN (from);
      
      from = emit_jump_insn_after (gen_jump (label), from);
      JUMP_LABEL (from) = label;
      found_barrier = emit_barrier_after (from);
      emit_label_after (label, found_barrier);
    }

  return found_barrier;
}

struct minipool_fixup
{
  struct minipool_fixup *next;
  rtx insn;
  int address;
  rtx *loc;
  enum machine_mode mode;
  rtx value;
  int range;
};
  
struct minipool_fixup *minipool_fix_head;
struct minipool_fixup *minipool_fix_tail;

static void
push_minipool_barrier (insn, address)
     rtx insn;
     int address;
{
  struct minipool_fixup *fix
    = (struct minipool_fixup *) oballoc (sizeof (struct minipool_fixup));

  fix->insn = insn;
  fix->address = address;

  fix->next = NULL;
  if (minipool_fix_head != NULL)
    minipool_fix_tail->next = fix;
  else
    minipool_fix_head = fix;

  minipool_fix_tail = fix;
}

static void
push_minipool_fix (insn, address, loc, mode, value)
     rtx insn;
     int address;
     rtx *loc;
     enum machine_mode mode;
     rtx value;
{
  struct minipool_fixup *fix
    = (struct minipool_fixup *) oballoc (sizeof (struct minipool_fixup));

#ifdef AOF_ASSEMBLER
  /* PIC symbol refereneces need to be converted into offsets into the
     based area.  */
  if (flag_pic && GET_MODE == SYMBOL_REF)
    value = aof_pic_entry (value);
#endif /* AOF_ASSEMBLER */

  fix->insn = insn;
  fix->address = address;
  fix->loc = loc;
  fix->mode = mode;
  fix->value = value;
  fix->range = get_attr_pool_range (insn);

  /* If an insn doesn't have a range defined for it, then it isn't
     expecting to be reworked by this code.  Better to abort now than
     to generate duff assembly code.  */
  if (fix->range == 0)
    abort ();

  /* Add it to the chain of fixes */
  fix->next = NULL;
  if (minipool_fix_head != NULL)
    minipool_fix_tail->next = fix;
  else
    minipool_fix_head = fix;

  minipool_fix_tail = fix;
}

static void
note_invalid_constants (insn, address)
     rtx insn;
     int address;
{
  int opno;

  /* Extract the operands of the insn */
  extract_insn(insn);

  /* Find the alternative selected */
  if (! constrain_operands (1))
    fatal_insn_not_found (insn);

  /* Preprocess the constraints, to extract some useful information.  */
  preprocess_constraints ();

  for (opno = 0; opno < recog_data.n_operands; opno++)
    {
      /* Things we need to fix can only occur in inputs */
      if (recog_data.operand_type[opno] != OP_IN)
	continue;

      /* If this alternative is a memory reference, then any mention
	 of constants in this alternative is really to fool reload
	 into allowing us to accept one there.  We need to fix them up
	 now so that we output the right code.  */
      if (recog_op_alt[opno][which_alternative].memory_ok)
	{
	  rtx op = recog_data.operand[opno];

	  if (CONSTANT_P (op))
	    push_minipool_fix (insn, address, recog_data.operand_loc[opno],
			       recog_data.operand_mode[opno], op);
#ifndef AOF_ASSEMBLER
	  else if (GET_CODE (op) == UNSPEC && XINT (op, 1) == 3)
	    push_minipool_fix (insn, address, recog_data.operand_loc[opno],
			       recog_data.operand_mode[opno],
			       XVECEXP (op, 0, 0));
#endif
	  else if (recog_data.operand_mode[opno] == SImode
		   && GET_CODE (op) == MEM
		   && GET_CODE (XEXP (op, 0)) == SYMBOL_REF
		   && CONSTANT_POOL_ADDRESS_P (XEXP (op, 0)))
	    push_minipool_fix (insn, address, recog_data.operand_loc[opno],
			       recog_data.operand_mode[opno],
			       get_pool_constant (XEXP (op, 0)));
	}
    }
}

void
arm_reorg (first)
     rtx first;
{
  rtx insn;
  int address = 0;
  struct minipool_fixup *fix;

  minipool_fix_head = minipool_fix_tail = NULL;

  /* The first insn must always be a note, or the code below won't
     scan it properly.  */
  if (GET_CODE (first) != NOTE)
    abort ();

  /* Scan all the insns and record the operands that will need fixing.  */
  for (insn = next_nonnote_insn (first); insn; insn = next_nonnote_insn (insn))
    {

      if (GET_CODE (insn) == BARRIER)
	push_minipool_barrier(insn, address);
      else if (GET_CODE (insn) == INSN || GET_CODE (insn) == CALL_INSN
	       || GET_CODE (insn) == JUMP_INSN)
	{
	  rtx table;

	  note_invalid_constants (insn, address);
	  address += get_attr_length (insn);
	  /* If the insn is a vector jump, add the size of the table
	     and skip the table.  */
	  if (GET_CODE (insn) == JUMP_INSN
	      && JUMP_LABEL (insn) != NULL
	      && ((table = next_real_insn (JUMP_LABEL (insn)))
		  == next_real_insn (insn))
	      && table != NULL
	      && GET_CODE (table) == JUMP_INSN
	      && (GET_CODE (PATTERN (table)) == ADDR_VEC
		  || GET_CODE (PATTERN (table)) == ADDR_DIFF_VEC))
	    {
	      int elt = GET_CODE (PATTERN (table)) == ADDR_DIFF_VEC ? 1 : 0;

	      address += GET_MODE_SIZE (SImode) * XVECLEN (PATTERN (table), 
							   elt);
	      insn = table;
	    }
	}
    }

  /* Now scan the fixups and perform the required changes.  */
  for (fix = minipool_fix_head; fix; fix = fix->next)
    {
      struct minipool_fixup *ftmp;
      struct minipool_fixup *last_barrier = NULL;
      int max_range;
      rtx barrier;
      struct minipool_fixup *this_fix;
      int new_minipool_size = 0;

      /* Skip any further barriers before the next fix.  */
      while (fix && GET_CODE (fix->insn) == BARRIER)
	fix = fix->next;

      if (fix == NULL)
	break;

      ftmp = fix;
      max_range = fix->address + fix->range;

      /* Find all the other fixes that can live in the same pool.  */
      while (ftmp->next && ftmp->next->address < max_range
	     && (GET_CODE (ftmp->next->insn) == BARRIER
		 /* Ensure we can reach the constant inside the pool.  */
		 || ftmp->next->range > new_minipool_size))
	{
	  ftmp = ftmp->next;
	  if (GET_CODE (ftmp->insn) == BARRIER)
	    last_barrier = ftmp;
	  else
	    {
	      /* Does this fix constrain the range we can search?  */
	      if (ftmp->address + ftmp->range - new_minipool_size < max_range)
		max_range = ftmp->address + ftmp->range - new_minipool_size;

	      new_minipool_size += GET_MODE_SIZE (ftmp->mode);
	    }
	}

      /* If we found a barrier, drop back to that; any fixes that we could
	 have reached but come after the barrier will now go in the next
	 mini-pool.  */
      if (last_barrier != NULL)
	{
	  barrier = last_barrier->insn;
	  ftmp = last_barrier;
	}
      /* ftmp is last fix that we can fit into this pool and we
	 failed to find a barrier that we could use.  Insert a new
	 barrier in the code and arrange to jump around it.  */
      else
        {
	  /* Check that there isn't another fix that is in range that
	     we couldn't fit into this pool because the pool was
	     already too large: we need to put the pool before such an
	     instruction.  */
	  if (ftmp->next && ftmp->next->address < max_range)
	    max_range = ftmp->address;

	  barrier = find_barrier (ftmp->insn, max_range - ftmp->address);
	}

      /* Scan over the fixes we have identified for this pool, fixing them
	 up and adding the constants to the pool itself.  */
      for (this_fix = fix; this_fix && ftmp->next != this_fix;
	   this_fix = this_fix->next)
	if (GET_CODE (this_fix->insn) != BARRIER)
	  {
	    int offset = add_minipool_constant (this_fix->value,
						this_fix->mode);
	    rtx addr
	      = plus_constant (gen_rtx_LABEL_REF (VOIDmode, 
						  minipool_vector_label),
			       offset);
	    *this_fix->loc = gen_rtx_MEM (this_fix->mode, addr);
	  }

      dump_minipool (barrier);
      fix = ftmp;
    }

  /* From now on we must synthesize any constants that we can't handle
     directly.  This can happen if the RTL gets split during final
     instruction generation.  */
  after_arm_reorg = 1;
}


/* Routines to output assembly language.  */

/* If the rtx is the correct value then return the string of the number.
   In this way we can ensure that valid double constants are generated even
   when cross compiling. */
char *
fp_immediate_constant (x)
     rtx x;
{
  REAL_VALUE_TYPE r;
  int i;
  
  if (!fpa_consts_inited)
    init_fpa_table ();
  
  REAL_VALUE_FROM_CONST_DOUBLE (r, x);
  for (i = 0; i < 8; i++)
    if (REAL_VALUES_EQUAL (r, values_fpa[i]))
      return strings_fpa[i];

  abort ();
}

/* As for fp_immediate_constant, but value is passed directly, not in rtx.  */
static char *
fp_const_from_val (r)
     REAL_VALUE_TYPE * r;
{
  int i;

  if (! fpa_consts_inited)
    init_fpa_table ();

  for (i = 0; i < 8; i++)
    if (REAL_VALUES_EQUAL (*r, values_fpa[i]))
      return strings_fpa[i];

  abort ();
}

/* Output the operands of a LDM/STM instruction to STREAM.
   MASK is the ARM register set mask of which only bits 0-15 are important.
   INSTR is the possibly suffixed base register.  HAT unequals zero if a hat
   must follow the register list.  */

void
print_multi_reg (stream, instr, reg, mask, hat)
     FILE * stream;
     char * instr;
     int reg;
     int mask;
     int hat;
{
  int i;
  int not_first = FALSE;

  fputc ('\t', stream);
  asm_fprintf (stream, instr, reg);
  fputs (", {", stream);
  
  for (i = 0; i < 16; i++)
    if (mask & (1 << i))
      {
	if (not_first)
	  fprintf (stream, ", ");
	
	asm_fprintf (stream, "%r", i);
	not_first = TRUE;
      }

  fprintf (stream, "}%s\n", hat ? "^" : "");
}

/* Output a 'call' insn. */

char *
output_call (operands)
     rtx * operands;
{
  /* Handle calls to lr using ip (which may be clobbered in subr anyway). */

  if (REGNO (operands[0]) == LR_REGNUM)
    {
      operands[0] = gen_rtx_REG (SImode, IP_REGNUM);
      output_asm_insn ("mov%?\t%0, %|lr", operands);
    }
  
  output_asm_insn ("mov%?\t%|lr, %|pc", operands);
  
  if (TARGET_INTERWORK)
    output_asm_insn ("bx%?\t%0", operands);
  else
    output_asm_insn ("mov%?\t%|pc, %0", operands);
  
  return "";
}

static int
eliminate_lr2ip (x)
     rtx * x;
{
  int something_changed = 0;
  rtx x0 = * x;
  int code = GET_CODE (x0);
  register int i, j;
  register const char * fmt;
  
  switch (code)
    {
    case REG:
      if (REGNO (x0) == LR_REGNUM)
        {
	  *x = gen_rtx_REG (SImode, IP_REGNUM);
	  return 1;
        }
      return 0;
    default:
      /* Scan through the sub-elements and change any references there */
      fmt = GET_RTX_FORMAT (code);
      
      for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
	if (fmt[i] == 'e')
	  something_changed |= eliminate_lr2ip (&XEXP (x0, i));
	else if (fmt[i] == 'E')
	  for (j = 0; j < XVECLEN (x0, i); j++)
	    something_changed |= eliminate_lr2ip (&XVECEXP (x0, i, j));
      
      return something_changed;
    }
}
  
/* Output a 'call' insn that is a reference in memory. */

char *
output_call_mem (operands)
     rtx * operands;
{
  operands[0] = copy_rtx (operands[0]); /* Be ultra careful */
  /* Handle calls using lr by using ip (which may be clobbered in subr anyway).
   */
  if (eliminate_lr2ip (&operands[0]))
    output_asm_insn ("mov%?\t%|ip, %|lr", operands);

  if (TARGET_INTERWORK)
    {
      output_asm_insn ("ldr%?\t%|ip, %0", operands);
      output_asm_insn ("mov%?\t%|lr, %|pc", operands);
      output_asm_insn ("bx%?\t%|ip", operands);
    }
  else
    {
      output_asm_insn ("mov%?\t%|lr, %|pc", operands);
      output_asm_insn ("ldr%?\t%|pc, %0", operands);
    }

  return "";
}


/* Output a move from arm registers to an fpu registers.
   OPERANDS[0] is an fpu register.
   OPERANDS[1] is the first registers of an arm register pair.  */

char *
output_mov_long_double_fpu_from_arm (operands)
     rtx * operands;
{
  int arm_reg0 = REGNO (operands[1]);
  rtx ops[3];

  if (arm_reg0 == IP_REGNUM)
    abort ();

  ops[0] = gen_rtx_REG (SImode, arm_reg0);
  ops[1] = gen_rtx_REG (SImode, 1 + arm_reg0);
  ops[2] = gen_rtx_REG (SImode, 2 + arm_reg0);
  
  output_asm_insn ("stm%?fd\t%|sp!, {%0, %1, %2}", ops);
  output_asm_insn ("ldf%?e\t%0, [%|sp], #12", operands);
  
  return "";
}

/* Output a move from an fpu register to arm registers.
   OPERANDS[0] is the first registers of an arm register pair.
   OPERANDS[1] is an fpu register.  */

char *
output_mov_long_double_arm_from_fpu (operands)
     rtx * operands;
{
  int arm_reg0 = REGNO (operands[0]);
  rtx ops[3];

  if (arm_reg0 == IP_REGNUM)
    abort ();

  ops[0] = gen_rtx_REG (SImode, arm_reg0);
  ops[1] = gen_rtx_REG (SImode, 1 + arm_reg0);
  ops[2] = gen_rtx_REG (SImode, 2 + arm_reg0);

  output_asm_insn ("stf%?e\t%1, [%|sp, #-12]!", operands);
  output_asm_insn ("ldm%?fd\t%|sp!, {%0, %1, %2}", ops);
  return "";
}

/* Output a move from arm registers to arm registers of a long double
   OPERANDS[0] is the destination.
   OPERANDS[1] is the source.  */
char *
output_mov_long_double_arm_from_arm (operands)
     rtx * operands;
{
  /* We have to be careful here because the two might overlap */
  int dest_start = REGNO (operands[0]);
  int src_start = REGNO (operands[1]);
  rtx ops[2];
  int i;

  if (dest_start < src_start)
    {
      for (i = 0; i < 3; i++)
	{
	  ops[0] = gen_rtx_REG (SImode, dest_start + i);
	  ops[1] = gen_rtx_REG (SImode, src_start + i);
	  output_asm_insn ("mov%?\t%0, %1", ops);
	}
    }
  else
    {
      for (i = 2; i >= 0; i--)
	{
	  ops[0] = gen_rtx_REG (SImode, dest_start + i);
	  ops[1] = gen_rtx_REG (SImode, src_start + i);
	  output_asm_insn ("mov%?\t%0, %1", ops);
	}
    }

  return "";
}


/* Output a move from arm registers to an fpu registers.
   OPERANDS[0] is an fpu register.
   OPERANDS[1] is the first registers of an arm register pair.  */

char *
output_mov_double_fpu_from_arm (operands)
     rtx * operands;
{
  int arm_reg0 = REGNO (operands[1]);
  rtx ops[2];

  if (arm_reg0 == IP_REGNUM)
    abort ();
  
  ops[0] = gen_rtx_REG (SImode, arm_reg0);
  ops[1] = gen_rtx_REG (SImode, 1 + arm_reg0);
  output_asm_insn ("stm%?fd\t%|sp!, {%0, %1}", ops);
  output_asm_insn ("ldf%?d\t%0, [%|sp], #8", operands);
  return "";
}

/* Output a move from an fpu register to arm registers.
   OPERANDS[0] is the first registers of an arm register pair.
   OPERANDS[1] is an fpu register.  */

char *
output_mov_double_arm_from_fpu (operands)
     rtx * operands;
{
  int arm_reg0 = REGNO (operands[0]);
  rtx ops[2];

  if (arm_reg0 == IP_REGNUM)
    abort ();

  ops[0] = gen_rtx_REG (SImode, arm_reg0);
  ops[1] = gen_rtx_REG (SImode, 1 + arm_reg0);
  output_asm_insn ("stf%?d\t%1, [%|sp, #-8]!", operands);
  output_asm_insn ("ldm%?fd\t%|sp!, {%0, %1}", ops);
  return "";
}

/* Output a move between double words.
   It must be REG<-REG, REG<-CONST_DOUBLE, REG<-CONST_INT, REG<-MEM
   or MEM<-REG and all MEMs must be offsettable addresses.  */

char *
output_move_double (operands)
     rtx * operands;
{
  enum rtx_code code0 = GET_CODE (operands[0]);
  enum rtx_code code1 = GET_CODE (operands[1]);
  rtx otherops[3];

  if (code0 == REG)
    {
      int reg0 = REGNO (operands[0]);

      otherops[0] = gen_rtx_REG (SImode, 1 + reg0);
      
      if (code1 == REG)
	{
	  int reg1 = REGNO (operands[1]);
	  if (reg1 == IP_REGNUM)
	    abort ();

	  /* Ensure the second source is not overwritten */
	  if (reg1 == reg0 + (WORDS_BIG_ENDIAN ? -1 : 1))
	    output_asm_insn ("mov%?\t%Q0, %Q1\n\tmov%?\t%R0, %R1", operands);
	  else
	    output_asm_insn ("mov%?\t%R0, %R1\n\tmov%?\t%Q0, %Q1", operands);
	}
      else if (code1 == CONST_DOUBLE)
	{
	  if (GET_MODE (operands[1]) == DFmode)
	    {
	      long l[2];
	      union real_extract u;

	      bcopy ((char *) &CONST_DOUBLE_LOW (operands[1]), (char *) &u,
		     sizeof (u));
	      REAL_VALUE_TO_TARGET_DOUBLE (u.d, l);
	      otherops[1] = GEN_INT(l[1]);
	      operands[1] = GEN_INT(l[0]);
	    }
	  else if (GET_MODE (operands[1]) != VOIDmode)
	    abort ();
	  else if (WORDS_BIG_ENDIAN)
	    {
	      
	      otherops[1] = GEN_INT (CONST_DOUBLE_LOW (operands[1]));
	      operands[1] = GEN_INT (CONST_DOUBLE_HIGH (operands[1]));
	    }
	  else
	    {
	      
	      otherops[1] = GEN_INT (CONST_DOUBLE_HIGH (operands[1]));
	      operands[1] = GEN_INT (CONST_DOUBLE_LOW (operands[1]));
	    }
	  
	  output_mov_immediate (operands);
	  output_mov_immediate (otherops);
	}
      else if (code1 == CONST_INT)
	{
#if HOST_BITS_PER_WIDE_INT > 32
	  /* If HOST_WIDE_INT is more than 32 bits, the intval tells us
	     what the upper word is.  */
	  if (WORDS_BIG_ENDIAN)
	    {
	      otherops[1] = GEN_INT (ARM_SIGN_EXTEND (INTVAL (operands[1])));
	      operands[1] = GEN_INT (INTVAL (operands[1]) >> 32);
	    }
	  else
	    {
	      otherops[1] = GEN_INT (INTVAL (operands[1]) >> 32);
	      operands[1] = GEN_INT (ARM_SIGN_EXTEND (INTVAL (operands[1])));
	    }
#else
	  /* Sign extend the intval into the high-order word */
	  if (WORDS_BIG_ENDIAN)
	    {
	      otherops[1] = operands[1];
	      operands[1] = (INTVAL (operands[1]) < 0
			     ? constm1_rtx : const0_rtx);
	    }
	  else
	    otherops[1] = INTVAL (operands[1]) < 0 ? constm1_rtx : const0_rtx;
#endif
	  output_mov_immediate (otherops);
	  output_mov_immediate (operands);
	}
      else if (code1 == MEM)
	{
	  switch (GET_CODE (XEXP (operands[1], 0)))
	    {
	    case REG:
	      output_asm_insn ("ldm%?ia\t%m1, %M0", operands);
	      break;

  	    case PRE_INC:
	      abort (); /* Should never happen now */
	      break;

	    case PRE_DEC:
	      output_asm_insn ("ldm%?db\t%m1!, %M0", operands);
	      break;

	    case POST_INC:
	      output_asm_insn ("ldm%?ia\t%m1!, %M0", operands);
	      break;

	    case POST_DEC:
	      abort (); /* Should never happen now */
	      break;

	    case LABEL_REF:
	    case CONST:
	      output_asm_insn ("adr%?\t%0, %1", operands);
	      output_asm_insn ("ldm%?ia\t%0, %M0", operands);
	      break;

	    default:
	      if (arm_add_operand (XEXP (XEXP (operands[1], 0), 1),
				   GET_MODE (XEXP (XEXP (operands[1], 0), 1))))
		{
		  otherops[0] = operands[0];
		  otherops[1] = XEXP (XEXP (operands[1], 0), 0);
		  otherops[2] = XEXP (XEXP (operands[1], 0), 1);
		  if (GET_CODE (XEXP (operands[1], 0)) == PLUS)
		    {
		      if (GET_CODE (otherops[2]) == CONST_INT)
			{
			  switch (INTVAL (otherops[2]))
			    {
			    case -8:
			      output_asm_insn ("ldm%?db\t%1, %M0", otherops);
			      return "";
			    case -4:
			      output_asm_insn ("ldm%?da\t%1, %M0", otherops);
			      return "";
			    case 4:
			      output_asm_insn ("ldm%?ib\t%1, %M0", otherops);
			      return "";
			    }
			  if (!(const_ok_for_arm (INTVAL (otherops[2]))))
			    output_asm_insn ("sub%?\t%0, %1, #%n2", otherops);
			  else
			    output_asm_insn ("add%?\t%0, %1, %2", otherops);
			}
		      else
			output_asm_insn ("add%?\t%0, %1, %2", otherops);
		    }
		  else
		    output_asm_insn ("sub%?\t%0, %1, %2", otherops);
		  
		  return "ldm%?ia\t%0, %M0";
                }
              else
                {
		  otherops[1] = adj_offsettable_operand (operands[1], 4);
		  /* Take care of overlapping base/data reg.  */
		  if (reg_mentioned_p (operands[0], operands[1]))
		    {
		      output_asm_insn ("ldr%?\t%0, %1", otherops);
		      output_asm_insn ("ldr%?\t%0, %1", operands);
		    }
		  else
		    {
		      output_asm_insn ("ldr%?\t%0, %1", operands);
		      output_asm_insn ("ldr%?\t%0, %1", otherops);
		    }
		}
	    }
	}
      else
	abort ();  /* Constraints should prevent this */
    }
  else if (code0 == MEM && code1 == REG)
    {
      if (REGNO (operands[1]) == IP_REGNUM)
	abort ();

      switch (GET_CODE (XEXP (operands[0], 0)))
        {
	case REG:
	  output_asm_insn ("stm%?ia\t%m0, %M1", operands);
	  break;

        case PRE_INC:
	  abort (); /* Should never happen now */
	  break;

        case PRE_DEC:
	  output_asm_insn ("stm%?db\t%m0!, %M1", operands);
	  break;

        case POST_INC:
	  output_asm_insn ("stm%?ia\t%m0!, %M1", operands);
	  break;

        case POST_DEC:
	  abort (); /* Should never happen now */
	  break;

	case PLUS:
	  if (GET_CODE (XEXP (XEXP (operands[0], 0), 1)) == CONST_INT)
	    {
	      switch (INTVAL (XEXP (XEXP (operands[0], 0), 1)))
		{
		case -8:
		  output_asm_insn ("stm%?db\t%m0, %M1", operands);
		  return "";

		case -4:
		  output_asm_insn ("stm%?da\t%m0, %M1", operands);
		  return "";

		case 4:
		  output_asm_insn ("stm%?ib\t%m0, %M1", operands);
		  return "";
		}
	    }
	  /* Fall through */

        default:
	  otherops[0] = adj_offsettable_operand (operands[0], 4);
	  otherops[1] = gen_rtx_REG (SImode, 1 + REGNO (operands[1]));
	  output_asm_insn ("str%?\t%1, %0", operands);
	  output_asm_insn ("str%?\t%1, %0", otherops);
	}
    }
  else
    abort ();  /* Constraints should prevent this */

  return "";
}


/* Output an arbitrary MOV reg, #n.
   OPERANDS[0] is a register.  OPERANDS[1] is a const_int.  */

char *
output_mov_immediate (operands)
     rtx * operands;
{
  HOST_WIDE_INT n = INTVAL (operands[1]);
  int n_ones = 0;
  int i;

  /* Try to use one MOV */
  if (const_ok_for_arm (n))
    {
      output_asm_insn ("mov%?\t%0, %1", operands);
      return "";
    }

  /* Try to use one MVN */
  if (const_ok_for_arm (~n))
    {
      operands[1] = GEN_INT (~n);
      output_asm_insn ("mvn%?\t%0, %1", operands);
      return "";
    }

  /* If all else fails, make it out of ORRs or BICs as appropriate. */

  for (i=0; i < 32; i++)
    if (n & 1 << i)
      n_ones++;

  if (n_ones > 16)  /* Shorter to use MVN with BIC in this case. */
    output_multi_immediate(operands, "mvn%?\t%0, %1", "bic%?\t%0, %0, %1", 1,
			   ~n);
  else
    output_multi_immediate(operands, "mov%?\t%0, %1", "orr%?\t%0, %0, %1", 1,
			   n);

  return "";
}


/* Output an ADD r, s, #n where n may be too big for one instruction.  If
   adding zero to one register, output nothing.  */

char *
output_add_immediate (operands)
     rtx * operands;
{
  HOST_WIDE_INT n = INTVAL (operands[2]);

  if (n != 0 || REGNO (operands[0]) != REGNO (operands[1]))
    {
      if (n < 0)
	output_multi_immediate (operands,
				"sub%?\t%0, %1, %2", "sub%?\t%0, %0, %2", 2,
				-n);
      else
	output_multi_immediate (operands,
				"add%?\t%0, %1, %2", "add%?\t%0, %0, %2", 2,
				n);
    }

  return "";
}

/* Output a multiple immediate operation.
   OPERANDS is the vector of operands referred to in the output patterns.
   INSTR1 is the output pattern to use for the first constant.
   INSTR2 is the output pattern to use for subsequent constants.
   IMMED_OP is the index of the constant slot in OPERANDS.
   N is the constant value.  */

static char *
output_multi_immediate (operands, instr1, instr2, immed_op, n)
     rtx * operands;
     char * instr1, * instr2;
     int immed_op;
     HOST_WIDE_INT n;
{
#if HOST_BITS_PER_WIDE_INT > 32
  n &= 0xffffffff;
#endif

  if (n == 0)
    {
      operands[immed_op] = const0_rtx;
      output_asm_insn (instr1, operands); /* Quick and easy output */
    }
  else
    {
      int i;
      char *instr = instr1;

      /* Note that n is never zero here (which would give no output) */
      for (i = 0; i < 32; i += 2)
	{
	  if (n & (3 << i))
	    {
	      operands[immed_op] = GEN_INT (n & (255 << i));
	      output_asm_insn (instr, operands);
	      instr = instr2;
	      i += 6;
	    }
	}
    }
  return "";
}


/* Return the appropriate ARM instruction for the operation code.
   The returned result should not be overwritten.  OP is the rtx of the
   operation.  SHIFT_FIRST_ARG is TRUE if the first argument of the operator
   was shifted.  */

char *
arithmetic_instr (op, shift_first_arg)
     rtx op;
     int shift_first_arg;
{
  switch (GET_CODE (op))
    {
    case PLUS:
      return "add";

    case MINUS:
      return shift_first_arg ? "rsb" : "sub";

    case IOR:
      return "orr";

    case XOR:
      return "eor";

    case AND:
      return "and";

    default:
      abort ();
    }
}


/* Ensure valid constant shifts and return the appropriate shift mnemonic
   for the operation code.  The returned result should not be overwritten.
   OP is the rtx code of the shift.
   On exit, *AMOUNTP will be -1 if the shift is by a register, or a constant
   shift. */

static char *
shift_op (op, amountp)
     rtx op;
     HOST_WIDE_INT *amountp;
{
  char * mnem;
  enum rtx_code code = GET_CODE (op);

  if (GET_CODE (XEXP (op, 1)) == REG || GET_CODE (XEXP (op, 1)) == SUBREG)
    *amountp = -1;
  else if (GET_CODE (XEXP (op, 1)) == CONST_INT)
    *amountp = INTVAL (XEXP (op, 1));
  else
    abort ();

  switch (code)
    {
    case ASHIFT:
      mnem = "asl";
      break;

    case ASHIFTRT:
      mnem = "asr";
      break;

    case LSHIFTRT:
      mnem = "lsr";
      break;

    case ROTATERT:
      mnem = "ror";
      break;

    case MULT:
      /* We never have to worry about the amount being other than a
	 power of 2, since this case can never be reloaded from a reg.  */
      if (*amountp != -1)
	*amountp = int_log2 (*amountp);
      else
	abort ();
      return "asl";

    default:
      abort ();
    }

  if (*amountp != -1)
    {
      /* This is not 100% correct, but follows from the desire to merge
	 multiplication by a power of 2 with the recognizer for a
	 shift.  >=32 is not a valid shift for "asl", so we must try and
	 output a shift that produces the correct arithmetical result.
	 Using lsr #32 is identical except for the fact that the carry bit
	 is not set correctly if we set the flags; but we never use the 
	 carry bit from such an operation, so we can ignore that.  */
      if (code == ROTATERT)
	*amountp &= 31;		/* Rotate is just modulo 32 */
      else if (*amountp != (*amountp & 31))
	{
	  if (code == ASHIFT)
	    mnem = "lsr";
	  *amountp = 32;
	}

      /* Shifts of 0 are no-ops.  */
      if (*amountp == 0)
	return NULL;
    }	  

  return mnem;
}


/* Obtain the shift from the POWER of two. */

static HOST_WIDE_INT
int_log2 (power)
     HOST_WIDE_INT power;
{
  HOST_WIDE_INT shift = 0;

  while (((((HOST_WIDE_INT) 1) << shift) & power) == 0)
    {
      if (shift > 31)
	abort ();
      shift++;
    }

  return shift;
}

/* Output a .ascii pseudo-op, keeping track of lengths.  This is because
   /bin/as is horribly restrictive.  */
#define MAX_ASCII_LEN 51

void
output_ascii_pseudo_op (stream, p, len)
     FILE * stream;
     unsigned char * p;
     int len;
{
  int i;
  int len_so_far = 0;

  fputs ("\t.ascii\t\"", stream);
  
  for (i = 0; i < len; i++)
    {
      register int c = p[i];

      if (len_so_far >= MAX_ASCII_LEN)
	{
	  fputs ("\"\n\t.ascii\t\"", stream);
	  len_so_far = 0;
	}

      switch (c)
	{
	case TARGET_TAB:		
	  fputs ("\\t", stream);
	  len_so_far += 2;			
	  break;
	  
	case TARGET_FF:
	  fputs ("\\f", stream);
	  len_so_far += 2;
	  break;
	  
	case TARGET_BS:
	  fputs ("\\b", stream);
	  len_so_far += 2;
	  break;
	  
	case TARGET_CR:
	  fputs ("\\r", stream);
	  len_so_far += 2;
	  break;
	  
	case TARGET_NEWLINE:
	  fputs ("\\n", stream);
	  c = p [i + 1];
	  if ((c >= ' ' && c <= '~')
	      || c == TARGET_TAB)
	    /* This is a good place for a line break.  */
	    len_so_far = MAX_ASCII_LEN;
	  else
	    len_so_far += 2;
	  break;
	  
	case '\"':
	case '\\':
	  putc ('\\', stream);
	  len_so_far ++;
	  /* drop through.  */

	default:
	  if (c >= ' ' && c <= '~')
	    {
	      putc (c, stream);
	      len_so_far ++;
	    }
	  else
	    {
	      fprintf (stream, "\\%03o", c);
	      len_so_far += 4;
	    }
	  break;
	}
    }

  fputs ("\"\n", stream);
}


/* Try to determine whether a pattern really clobbers the link register.
   This information is useful when peepholing, so that lr need not be pushed
   if we combine a call followed by a return.
   NOTE: This code does not check for side-effect expressions in a SET_SRC:
   such a check should not be needed because these only update an existing
   value within a register; the register must still be set elsewhere within
   the function. */

static int
pattern_really_clobbers_lr (x)
     rtx x;
{
  int i;
  
  switch (GET_CODE (x))
    {
    case SET:
      switch (GET_CODE (SET_DEST (x)))
	{
	case REG:
	  return REGNO (SET_DEST (x)) == LR_REGNUM;

        case SUBREG:
	  if (GET_CODE (XEXP (SET_DEST (x), 0)) == REG)
	    return REGNO (XEXP (SET_DEST (x), 0)) == LR_REGNUM;

	  if (GET_CODE (XEXP (SET_DEST (x), 0)) == MEM)
	    return 0;
	  abort ();

        default:
	  return 0;
        }

    case PARALLEL:
      for (i = 0; i < XVECLEN (x, 0); i++)
	if (pattern_really_clobbers_lr (XVECEXP (x, 0, i)))
	  return 1;
      return 0;

    case CLOBBER:
      switch (GET_CODE (XEXP (x, 0)))
        {
	case REG:
	  return REGNO (XEXP (x, 0)) == LR_REGNUM;

        case SUBREG:
	  if (GET_CODE (XEXP (XEXP (x, 0), 0)) == REG)
	    return REGNO (XEXP (XEXP (x, 0), 0)) == LR_REGNUM;
	  abort ();

        default:
	  return 0;
        }

    case UNSPEC:
      return 1;

    default:
      return 0;
    }
}

static int
function_really_clobbers_lr (first)
     rtx first;
{
  rtx insn, next;
  
  for (insn = first; insn; insn = next_nonnote_insn (insn))
    {
      switch (GET_CODE (insn))
        {
	case BARRIER:
	case NOTE:
	case CODE_LABEL:
	case JUMP_INSN:		/* Jump insns only change the PC (and conds) */
	  break;

        case INSN:
	  if (pattern_really_clobbers_lr (PATTERN (insn)))
	    return 1;
	  break;

        case CALL_INSN:
	  /* Don't yet know how to handle those calls that are not to a 
	     SYMBOL_REF */
	  if (GET_CODE (PATTERN (insn)) != PARALLEL)
	    abort ();

	  switch (GET_CODE (XVECEXP (PATTERN (insn), 0, 0)))
	    {
	    case CALL:
	      if (GET_CODE (XEXP (XEXP (XVECEXP (PATTERN (insn), 0, 0), 0), 0))
		  != SYMBOL_REF)
		return 1;
	      break;

	    case SET:
	      if (GET_CODE (XEXP (XEXP (SET_SRC (XVECEXP (PATTERN (insn),
							  0, 0)), 0), 0))
		  != SYMBOL_REF)
		return 1;
	      break;

	    default:	/* Don't recognize it, be safe */
	      return 1;
	    }

	  /* A call can be made (by peepholing) not to clobber lr iff it is
	     followed by a return.  There may, however, be a use insn iff
	     we are returning the result of the call. 
	     If we run off the end of the insn chain, then that means the
	     call was at the end of the function.  Unfortunately we don't
	     have a return insn for the peephole to recognize, so we
	     must reject this.  (Can this be fixed by adding our own insn?) */
	  if ((next = next_nonnote_insn (insn)) == NULL)
	    return 1;

	  /* No need to worry about lr if the call never returns */
	  if (GET_CODE (next) == BARRIER)
	    break;

	  if (GET_CODE (next) == INSN && GET_CODE (PATTERN (next)) == USE
	      && (GET_CODE (XVECEXP (PATTERN (insn), 0, 0)) == SET)
	      && (REGNO (SET_DEST (XVECEXP (PATTERN (insn), 0, 0)))
		  == REGNO (XEXP (PATTERN (next), 0))))
	    if ((next = next_nonnote_insn (next)) == NULL)
	      return 1;

	  if (GET_CODE (next) == JUMP_INSN
	      && GET_CODE (PATTERN (next)) == RETURN)
	    break;
	  return 1;

        default:
	  abort ();
        }
    }

  /* We have reached the end of the chain so lr was _not_ clobbered */
  return 0;
}

char *
output_return_instruction (operand, really_return, reverse)
     rtx operand;
     int really_return;
     int reverse;
{
  char instr[100];
  int reg, live_regs = 0;
  int volatile_func = (optimize > 0 
		       && TREE_THIS_VOLATILE (current_function_decl));

  return_used_this_function = 1;

  if (TARGET_ABORT_NORETURN && volatile_func)
    {
      /* If this function was declared non-returning, and we have found a tail 
	 call, then we have to trust that the called function won't return.  */
      if (really_return)
	{
	  rtx ops[2];
      
	  /* Otherwise, trap an attempted return by aborting.  */
	  ops[0] = operand;
	  ops[1] = gen_rtx_SYMBOL_REF (Pmode, NEED_PLT_RELOC ? "abort(PLT)" 
				       : "abort");
	  assemble_external_libcall (ops[1]);
	  output_asm_insn (reverse ? "bl%D0\t%a1" : "bl%d0\t%a1", ops);
	}
      
      return "";
    }
      
  if (current_function_calls_alloca && ! really_return)
    abort ();
    
  for (reg = 0; reg <= 10; reg++)
    if (regs_ever_live[reg] && ! call_used_regs[reg])
      live_regs++;

  if (flag_pic && ! TARGET_SINGLE_PIC_BASE
      && regs_ever_live[PIC_OFFSET_TABLE_REGNUM])
    live_regs++;

  if (live_regs || (regs_ever_live[LR_REGNUM] && ! lr_save_eliminated))
    live_regs++;

  if (frame_pointer_needed)
    live_regs += 4;

  /* On some ARM architectures it is faster to use LDR rather than LDM to
     load a single register.  On other architectures, the cost is the same.  */
  if (live_regs == 1
      && regs_ever_live[LR_REGNUM]
      && ! lr_save_eliminated
      /* FIXME: We ought to handle the case TARGET_APCS_32 is true,
	 really_return is true, and only the PC needs restoring.  */
      && ! really_return)
    {
      output_asm_insn (reverse ? "ldr%?%D0\t%|lr, [%|sp], #4" 
		       : "ldr%?%d0\t%|lr, [%|sp], #4", &operand);
    }
  else if (live_regs)
    {
      if (lr_save_eliminated || ! regs_ever_live[LR_REGNUM])
        live_regs++;

      if (frame_pointer_needed)
        strcpy (instr,
		reverse ? "ldm%?%D0ea\t%|fp, {" : "ldm%?%d0ea\t%|fp, {");
      else
        strcpy (instr, 
		reverse ? "ldm%?%D0fd\t%|sp!, {" : "ldm%?%d0fd\t%|sp!, {");

      for (reg = 0; reg <= 10; reg++)
        if (regs_ever_live[reg]
	    && (! call_used_regs[reg]
		|| (flag_pic && ! TARGET_SINGLE_PIC_BASE
		    && reg == PIC_OFFSET_TABLE_REGNUM)))
          {
	    strcat (instr, "%|");
            strcat (instr, reg_names[reg]);
	    if (--live_regs)
              strcat (instr, ", ");
          }

      if (frame_pointer_needed)
        {
	  strcat (instr, "%|");
          strcat (instr, reg_names[11]);
          strcat (instr, ", ");
	  strcat (instr, "%|");
          strcat (instr, reg_names[13]);
          strcat (instr, ", ");
	  strcat (instr, "%|");
	  strcat (instr, TARGET_INTERWORK || (! really_return)
		  ? reg_names[LR_REGNUM] : reg_names[PC_REGNUM] );
        }
      else
	{
	  strcat (instr, "%|");
	  if (TARGET_INTERWORK && really_return)
	    strcat (instr, reg_names[IP_REGNUM]);
	  else
	    strcat (instr, really_return ? reg_names[PC_REGNUM] : reg_names[LR_REGNUM]);
	}
      strcat (instr, (TARGET_APCS_32 || !really_return) ? "}" : "}^");
      output_asm_insn (instr, &operand);

      if (TARGET_INTERWORK && really_return)
	{
	  strcpy (instr, "bx%?");
	  strcat (instr, reverse ? "%D0" : "%d0");
	  strcat (instr, "\t%|");
	  strcat (instr, frame_pointer_needed ? "lr" : "ip");

	  output_asm_insn (instr, & operand);
	}
    }
  else if (really_return)
    {
      if (TARGET_INTERWORK)
	sprintf (instr, "bx%%?%%%s0\t%%|lr", reverse ? "D" : "d");
      else
	sprintf (instr, "mov%%?%%%s0%s\t%%|pc, %%|lr",
		 reverse ? "D" : "d", TARGET_APCS_32 ? "" : "s");
      
      output_asm_insn (instr, & operand);
    }

  return "";
}

/* Return nonzero if optimizing and the current function is volatile.
   Such functions never return, and many memory cycles can be saved
   by not storing register values that will never be needed again.
   This optimization was added to speed up context switching in a
   kernel application. */

int
arm_volatile_func ()
{
  return (optimize > 0 && TREE_THIS_VOLATILE (current_function_decl));
}

/* Write the function name into the code section, directly preceding
   the function prologue.

   Code will be output similar to this:
     t0
	 .ascii "arm_poke_function_name", 0
	 .align
     t1
	 .word 0xff000000 + (t1 - t0)
     arm_poke_function_name
	 mov     ip, sp
	 stmfd   sp!, {fp, ip, lr, pc}
	 sub     fp, ip, #4

   When performing a stack backtrace, code can inspect the value
   of 'pc' stored at 'fp' + 0.  If the trace function then looks
   at location pc - 12 and the top 8 bits are set, then we know
   that there is a function name embedded immediately preceding this
   location and has length ((pc[-3]) & 0xff000000).

   We assume that pc is declared as a pointer to an unsigned long.

   It is of no benefit to output the function name if we are assembling
   a leaf function.  These function types will not contain a stack
   backtrace structure, therefore it is not possible to determine the
   function name.  */

void
arm_poke_function_name (stream, name)
   FILE * stream;
   char * name;
{
  unsigned long alignlength;
  unsigned long length;
  rtx           x;

  length = strlen (name) + 1;
  alignlength = (length + 3) & ~3;
  
  ASM_OUTPUT_ASCII (stream, name, length);
  ASM_OUTPUT_ALIGN (stream, 2);
  x = GEN_INT (0xff000000UL + alignlength);
  ASM_OUTPUT_INT (stream, x);
}

/* The amount of stack adjustment that happens here, in output_return and in
   output_epilogue must be exactly the same as was calculated during reload,
   or things will point to the wrong place.  The only time we can safely
   ignore this constraint is when a function has no arguments on the stack,
   no stack frame requirement and no live registers execpt for `lr'.  If we
   can guarantee that by making all function calls into tail calls and that
   lr is not clobbered in any other way, then there is no need to push lr
   onto the stack. */
   
void
output_func_prologue (f, frame_size)
     FILE * f;
     int frame_size;
{
  int reg, live_regs_mask = 0;
  int volatile_func = (optimize > 0
		       && TREE_THIS_VOLATILE (current_function_decl));

  /* Nonzero if we must stuff some register arguments onto the stack as if
     they were passed there.  */
  int store_arg_regs = 0;

  if (arm_ccfsm_state || arm_target_insn)
    abort ();					/* Sanity check */

  if (arm_naked_function_p (current_function_decl))
    return;

  return_used_this_function = 0;
  lr_save_eliminated = 0;
  
  asm_fprintf (f, "\t%@ args = %d, pretend = %d, frame = %d\n",
	       current_function_args_size,
	       current_function_pretend_args_size, frame_size);
  asm_fprintf (f, "\t%@ frame_needed = %d, current_function_anonymous_args = %d\n",
	       frame_pointer_needed,
	       current_function_anonymous_args);

  if (volatile_func)
    asm_fprintf (f, "\t%@ Volatile function.\n");

  if (current_function_anonymous_args && current_function_pretend_args_size)
    store_arg_regs = 1;

  for (reg = 0; reg <= 10; reg++)
    if (regs_ever_live[reg] && ! call_used_regs[reg])
      live_regs_mask |= (1 << reg);

  if (flag_pic && ! TARGET_SINGLE_PIC_BASE
      && regs_ever_live[PIC_OFFSET_TABLE_REGNUM])
    live_regs_mask |= (1 << PIC_OFFSET_TABLE_REGNUM);

  if (frame_pointer_needed)
    live_regs_mask |= 0xD800;
  else if (regs_ever_live[LR_REGNUM])
    {
      if (! current_function_args_size
	  && ! function_really_clobbers_lr (get_insns ()))
	lr_save_eliminated = 1;
      else
        live_regs_mask |= 1 << LR_REGNUM;
    }

  if (live_regs_mask)
    {
      /* if a di mode load/store multiple is used, and the base register
	 is r3, then r4 can become an ever live register without lr
	 doing so,  in this case we need to push lr as well, or we
	 will fail to get a proper return. */

      live_regs_mask |= 1 << LR_REGNUM;
      lr_save_eliminated = 0;

    }

  if (lr_save_eliminated)
    asm_fprintf (f,"\t%@ I don't think this function clobbers lr\n");

#ifdef AOF_ASSEMBLER
  if (flag_pic)
    asm_fprintf (f, "\tmov\t%r, %r\n", IP_REGNUM, PIC_OFFSET_TABLE_REGNUM);
#endif
}

char *
arm_output_epilogue ()
{
  int reg;
  int live_regs_mask = 0;
  /* If we need this, then it will always be at least this much */
  int floats_offset = 12;
  rtx operands[3];
  int frame_size = get_frame_size ();
  FILE *f = asm_out_file;
  int volatile_func = (optimize > 0
		       && TREE_THIS_VOLATILE (current_function_decl));

  if (use_return_insn (FALSE) && return_used_this_function)
    return "";

  /* Naked functions don't have epilogues.  */
  if (arm_naked_function_p (current_function_decl))
    return "";

  /* A volatile function should never return.  Call abort.  */
  if (TARGET_ABORT_NORETURN && volatile_func)
    {
      rtx op;
      op = gen_rtx_SYMBOL_REF (Pmode, NEED_PLT_RELOC ? "abort(PLT)" : "abort");
      assemble_external_libcall (op);
      output_asm_insn ("bl\t%a0", &op);
      return "";
    }

  for (reg = 0; reg <= 10; reg++)
    if (regs_ever_live[reg] && ! call_used_regs[reg])
      {
        live_regs_mask |= (1 << reg);
	floats_offset += 4;
      }

  /* If we aren't loading the PIC register, don't stack it even though it may
     be live.  */
  if (flag_pic && ! TARGET_SINGLE_PIC_BASE 
      && regs_ever_live[PIC_OFFSET_TABLE_REGNUM])
    {
      live_regs_mask |= (1 << PIC_OFFSET_TABLE_REGNUM);
      floats_offset += 4;
    }

  if (frame_pointer_needed)
    {
      if (arm_fpu_arch == FP_SOFT2)
	{
	  for (reg = 23; reg > 15; reg--)
	    if (regs_ever_live[reg] && ! call_used_regs[reg])
	      {
		floats_offset += 12;
		asm_fprintf (f, "\tldfe\t%r, [%r, #-%d]\n", 
			     reg, FP_REGNUM, floats_offset);
	      }
	}
      else
	{
	  int start_reg = 23;

	  for (reg = 23; reg > 15; reg--)
	    {
	      if (regs_ever_live[reg] && ! call_used_regs[reg])
		{
		  floats_offset += 12;
		  
		  /* We can't unstack more than four registers at once */
		  if (start_reg - reg == 3)
		    {
		      asm_fprintf (f, "\tlfm\t%r, 4, [%r, #-%d]\n",
			           reg, FP_REGNUM, floats_offset);
		      start_reg = reg - 1;
		    }
		}
	      else
		{
		  if (reg != start_reg)
		    asm_fprintf (f, "\tlfm\t%r, %d, [%r, #-%d]\n",
				 reg + 1, start_reg - reg,
				 FP_REGNUM, floats_offset);
		  start_reg = reg - 1;
		}
	    }

	  /* Just in case the last register checked also needs unstacking.  */
	  if (reg != start_reg)
	    asm_fprintf (f, "\tlfm\t%r, %d, [%r, #-%d]\n",
			 reg + 1, start_reg - reg,
			 FP_REGNUM, floats_offset);
	}
      
      if (TARGET_INTERWORK)
	{
	  live_regs_mask |= 0x6800;
	  print_multi_reg (f, "ldmea\t%r", FP_REGNUM, live_regs_mask, FALSE);
	  asm_fprintf (f, "\tbx\t%r\n", LR_REGNUM);
	}
      else
	{
	  live_regs_mask |= 0xA800;
	  print_multi_reg (f, "ldmea\t%r", FP_REGNUM, live_regs_mask,
			   TARGET_APCS_32 ? FALSE : TRUE);
	}
    }
  else
    {
      /* Restore stack pointer if necessary.  */
      if (frame_size + current_function_outgoing_args_size != 0)
	{
	  operands[0] = operands[1] = stack_pointer_rtx;
	  operands[2] = GEN_INT (frame_size
				 + current_function_outgoing_args_size);
	  output_add_immediate (operands);
	}

      if (arm_fpu_arch == FP_SOFT2)
	{
	  for (reg = 16; reg < 24; reg++)
	    if (regs_ever_live[reg] && ! call_used_regs[reg])
	      asm_fprintf (f, "\tldfe\t%r, [%r], #12\n",
			   reg, SP_REGNUM);
	}
      else
	{
	  int start_reg = 16;

	  for (reg = 16; reg < 24; reg++)
	    {
	      if (regs_ever_live[reg] && ! call_used_regs[reg])
		{
		  if (reg - start_reg == 3)
		    {
		      asm_fprintf (f, "\tlfmfd\t%r, 4, [%r]!\n",
				   start_reg, SP_REGNUM);
		      start_reg = reg + 1;
		    }
		}
	      else
		{
		  if (reg != start_reg)
		    asm_fprintf (f, "\tlfmfd\t%r, %d, [%r]!\n",
				 start_reg, reg - start_reg,
				 SP_REGNUM);
		  
		  start_reg = reg + 1;
		}
	    }

	  /* Just in case the last register checked also needs unstacking.  */
	  if (reg != start_reg)
	    asm_fprintf (f, "\tlfmfd\t%r, %d, [%r]!\n",
			 start_reg, reg - start_reg, SP_REGNUM);
	}

      if (current_function_pretend_args_size == 0 && regs_ever_live[LR_REGNUM])
	{
	  if (TARGET_INTERWORK)
	    {
	      if (! lr_save_eliminated)
		live_regs_mask |= 1 << LR_REGNUM;

	      if (live_regs_mask != 0)
		print_multi_reg (f, "ldmfd\t%r!", SP_REGNUM, live_regs_mask, FALSE);
	      
	      asm_fprintf (f, "\tbx\t%r\n", LR_REGNUM);
	    }
	  else if (lr_save_eliminated)
	    asm_fprintf (f, "\tmov%c\t%r, %r\n",
			 TARGET_APCS_32 ? ' ' : 's',
			 PC_REGNUM, LR_REGNUM);
	  else
	    print_multi_reg (f, "ldmfd\t%r!", SP_REGNUM, live_regs_mask | 0x8000,
			     TARGET_APCS_32 ? FALSE : TRUE);
	}
      else
	{
	  if (live_regs_mask || regs_ever_live[LR_REGNUM])
	    {
	      /* Restore the integer regs, and the return address into lr */
	      if (! lr_save_eliminated)
		live_regs_mask |= 1 << LR_REGNUM;

	      if (live_regs_mask != 0)
		print_multi_reg (f, "ldmfd\t%r!", SP_REGNUM, live_regs_mask, FALSE);
	    }

	  if (current_function_pretend_args_size)
	    {
	      /* Unwind the pre-pushed regs */
	      operands[0] = operands[1] = stack_pointer_rtx;
	      operands[2] = GEN_INT (current_function_pretend_args_size);
	      output_add_immediate (operands);
	    }
	  /* And finally, go home */
	  if (TARGET_INTERWORK)
	    asm_fprintf (f, "\tbx\t%r\n", LR_REGNUM);
	  else if (TARGET_APCS_32)
	    asm_fprintf (f, "\tmov\t%r, %r\n", PC_REGNUM, LR_REGNUM);
	  else
	    asm_fprintf (f, "\tmovs\t%r, %r\n", PC_REGNUM, LR_REGNUM);
	}
    }

  return "";
}

void
output_func_epilogue (frame_size)
     int frame_size;
{
  if (use_return_insn (FALSE) && return_used_this_function
      && (frame_size + current_function_outgoing_args_size) != 0
      && ! (frame_pointer_needed && TARGET_APCS))
    abort ();

  /* Reset the ARM-specific per-function variables.  */
  current_function_anonymous_args = 0;
  after_arm_reorg = 0;
}

static void
emit_multi_reg_push (mask)
     int mask;
{
  int num_regs = 0;
  int i, j;
  rtx par;

  for (i = 0; i < 16; i++)
    if (mask & (1 << i))
      num_regs++;

  if (num_regs == 0 || num_regs > 16)
    abort ();

  par = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (num_regs));

  for (i = 0; i < 16; i++)
    {
      if (mask & (1 << i))
	{
	  XVECEXP (par, 0, 0)
	    = gen_rtx_SET (VOIDmode,
			   gen_rtx_MEM (BLKmode,
					gen_rtx_PRE_DEC (BLKmode,
							 stack_pointer_rtx)),
			   gen_rtx_UNSPEC (BLKmode,
					   gen_rtvec (1,
						      gen_rtx_REG (SImode, i)),
					   2));
	  break;
	}
    }

  for (j = 1, i++; j < num_regs; i++)
    {
      if (mask & (1 << i))
	{
	  XVECEXP (par, 0, j)
	    = gen_rtx_USE (VOIDmode, gen_rtx_REG (SImode, i));
	  j++;
	}
    }

  emit_insn (par);
}

static void
emit_sfm (base_reg, count)
     int base_reg;
     int count;
{
  rtx par;
  int i;

  par = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (count));

  XVECEXP (par, 0, 0)
    = gen_rtx_SET (VOIDmode, 
		   gen_rtx_MEM (BLKmode,
				gen_rtx_PRE_DEC (BLKmode, stack_pointer_rtx)),
		   gen_rtx_UNSPEC (BLKmode,
				   gen_rtvec (1, gen_rtx_REG (XFmode, 
							      base_reg++)),
				   2));
  for (i = 1; i < count; i++)
    XVECEXP (par, 0, i) = gen_rtx_USE (VOIDmode, 
				       gen_rtx_REG (XFmode, base_reg++));

  emit_insn (par);
}

void
arm_expand_prologue ()
{
  int reg;
  rtx amount = GEN_INT (-(get_frame_size ()
			  + current_function_outgoing_args_size));
  int live_regs_mask = 0;
  int store_arg_regs = 0;
  /* If this function doesn't return, then there is no need to push
     the call-saved regs.  */
  int volatile_func = (optimize > 0
		       && TREE_THIS_VOLATILE (current_function_decl));

  /* Naked functions don't have prologues.  */
  if (arm_naked_function_p (current_function_decl))
    return;

  if (current_function_anonymous_args && current_function_pretend_args_size)
    store_arg_regs = 1;

  if (! volatile_func)
    {
      for (reg = 0; reg <= 10; reg++)
	if (regs_ever_live[reg] && ! call_used_regs[reg])
	  live_regs_mask |= 1 << reg;

      if (flag_pic && regs_ever_live[PIC_OFFSET_TABLE_REGNUM])
	live_regs_mask |= 1 << PIC_OFFSET_TABLE_REGNUM;

      if (regs_ever_live[LR_REGNUM])
	live_regs_mask |= 1 << LR_REGNUM;
    }

  if (frame_pointer_needed)
    {
      live_regs_mask |= 0xD800;
      emit_insn (gen_movsi (gen_rtx_REG (SImode, IP_REGNUM),
			    stack_pointer_rtx));
    }

  if (current_function_pretend_args_size)
    {
      if (store_arg_regs)
	emit_multi_reg_push ((0xf0 >> (current_function_pretend_args_size / 4))
			     & 0xf);
      else
	emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx, 
			       GEN_INT (-current_function_pretend_args_size)));
    }

  if (live_regs_mask)
    {
      /* If we have to push any regs, then we must push lr as well, or
	 we won't get a proper return.  */
      live_regs_mask |= 1 << LR_REGNUM;
      emit_multi_reg_push (live_regs_mask);
    }
      
  /* For now the integer regs are still pushed in output_func_epilogue ().  */

  if (! volatile_func)
    {
      if (arm_fpu_arch == FP_SOFT2)
	{
	  for (reg = 23; reg > 15; reg--)
	    if (regs_ever_live[reg] && ! call_used_regs[reg])
	      emit_insn (gen_rtx_SET
			 (VOIDmode, 
			  gen_rtx_MEM (XFmode, 
				       gen_rtx_PRE_DEC (XFmode,
							stack_pointer_rtx)),
			  gen_rtx_REG (XFmode, reg)));
	}
      else
	{
	  int start_reg = 23;

	  for (reg = 23; reg > 15; reg--)
	    {
	      if (regs_ever_live[reg] && ! call_used_regs[reg])
		{
		  if (start_reg - reg == 3)
		    {
		      emit_sfm (reg, 4);
		      start_reg = reg - 1;
		    }
		}
	      else
		{
		  if (start_reg != reg)
		    emit_sfm (reg + 1, start_reg - reg);
		  start_reg = reg - 1;
		}
	    }

	  if (start_reg != reg)
	    emit_sfm (reg + 1, start_reg - reg);
	}
    }

  if (frame_pointer_needed)
    emit_insn (gen_addsi3 (hard_frame_pointer_rtx, gen_rtx_REG (SImode, IP_REGNUM),
			   (GEN_INT
			    (-(4 + current_function_pretend_args_size)))));

  if (amount != const0_rtx)
    {
      emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx, amount));
      emit_insn (gen_rtx_CLOBBER (VOIDmode, 
				  gen_rtx_MEM (BLKmode, stack_pointer_rtx)));
    }

  /* If we are profiling, make sure no instructions are scheduled before
     the call to mcount.  Similarly if the user has requested no
     scheduling in the prolog.  */
  if (profile_flag || profile_block_flag || TARGET_NO_SCHED_PRO)
    emit_insn (gen_blockage ());
}
  

/* If CODE is 'd', then the X is a condition operand and the instruction
   should only be executed if the condition is true.
   if CODE is 'D', then the X is a condition operand and the instruction
   should only be executed if the condition is false: however, if the mode
   of the comparison is CCFPEmode, then always execute the instruction -- we
   do this because in these circumstances !GE does not necessarily imply LT;
   in these cases the instruction pattern will take care to make sure that
   an instruction containing %d will follow, thereby undoing the effects of
   doing this instruction unconditionally.
   If CODE is 'N' then X is a floating point operand that must be negated
   before output.
   If CODE is 'B' then output a bitwise inverted value of X (a const int).
   If X is a REG and CODE is `M', output a ldm/stm style multi-reg.  */

void
arm_print_operand (stream, x, code)
     FILE * stream;
     rtx x;
     int code;
{
  switch (code)
    {
    case '@':
      fputs (ASM_COMMENT_START, stream);
      return;

    case '|':
      fputs (REGISTER_PREFIX, stream);
      return;

    case '?':
      if (arm_ccfsm_state == 3 || arm_ccfsm_state == 4)
	fputs (arm_condition_codes[arm_current_cc], stream);
      return;

    case 'N':
      {
	REAL_VALUE_TYPE r;
	REAL_VALUE_FROM_CONST_DOUBLE (r, x);
	r = REAL_VALUE_NEGATE (r);
	fprintf (stream, "%s", fp_const_from_val (&r));
      }
      return;

    case 'B':
      if (GET_CODE (x) == CONST_INT)
	{
	  HOST_WIDE_INT val;
	  val = ARM_SIGN_EXTEND (~ INTVAL (x));
	  fprintf (stream, HOST_WIDE_INT_PRINT_DEC, val);
	}
      else
	{
	  putc ('~', stream);
	  output_addr_const (stream, x);
	}
      return;

    case 'i':
      fprintf (stream, "%s", arithmetic_instr (x, 1));
      return;

    case 'I':
      fprintf (stream, "%s", arithmetic_instr (x, 0));
      return;

    case 'S':
      {
	HOST_WIDE_INT val;
	char * shift = shift_op (x, & val);

	if (shift)
	  {
	    fprintf (stream, ", %s ", shift_op (x, & val));
	    if (val == -1)
	      arm_print_operand (stream, XEXP (x, 1), 0);
	    else
	      {
		fputc ('#', stream);
		fprintf (stream, HOST_WIDE_INT_PRINT_DEC, val);
	      }
	  }
      }
      return;

    case 'Q':
      if (REGNO (x) > 15)
	abort ();
      fputs (REGISTER_PREFIX, stream);
      fputs (reg_names[REGNO (x) + (WORDS_BIG_ENDIAN ? 1 : 0)], stream);
      return;

    case 'R':
      if (REGNO (x) > 15)
	abort ();
      fputs (REGISTER_PREFIX, stream);
      fputs (reg_names[REGNO (x) + (WORDS_BIG_ENDIAN ? 0 : 1)], stream);
      return;

    case 'm':
      fputs (REGISTER_PREFIX, stream);
      if (GET_CODE (XEXP (x, 0)) == REG)
	fputs (reg_names[REGNO (XEXP (x, 0))], stream);
      else
	fputs (reg_names[REGNO (XEXP (XEXP (x, 0), 0))], stream);
      return;

    case 'M':
      asm_fprintf (stream, "{%r-%r}",
		   REGNO (x), REGNO (x) + NUM_REGS (GET_MODE (x)) - 1);
      return;

    case 'd':
      if (x)
        fputs (arm_condition_codes[get_arm_condition_code (x)],
	       stream);
      return;

    case 'D':
      if (x)
        fputs (arm_condition_codes[ARM_INVERSE_CONDITION_CODE
				   (get_arm_condition_code (x))],
	       stream);
      return;

    default:
      if (x == 0)
	abort ();

      if (GET_CODE (x) == REG)
	{
	  fputs (REGISTER_PREFIX, stream);
	  fputs (reg_names[REGNO (x)], stream);
	}
      else if (GET_CODE (x) == MEM)
	{
	  output_memory_reference_mode = GET_MODE (x);
	  output_address (XEXP (x, 0));
	}
      else if (GET_CODE (x) == CONST_DOUBLE)
	fprintf (stream, "#%s", fp_immediate_constant (x));
      else if (GET_CODE (x) == NEG)
	abort (); /* This should never happen now. */
      else
	{
	  fputc ('#', stream);
	  output_addr_const (stream, x);
	}
    }
}

/* A finite state machine takes care of noticing whether or not instructions
   can be conditionally executed, and thus decrease execution time and code
   size by deleting branch instructions.  The fsm is controlled by
   final_prescan_insn, and controls the actions of ASM_OUTPUT_OPCODE.  */

/* The state of the fsm controlling condition codes are:
   0: normal, do nothing special
   1: make ASM_OUTPUT_OPCODE not output this instruction
   2: make ASM_OUTPUT_OPCODE not output this instruction
   3: make instructions conditional
   4: make instructions conditional

   State transitions (state->state by whom under condition):
   0 -> 1 final_prescan_insn if the `target' is a label
   0 -> 2 final_prescan_insn if the `target' is an unconditional branch
   1 -> 3 ASM_OUTPUT_OPCODE after not having output the conditional branch
   2 -> 4 ASM_OUTPUT_OPCODE after not having output the conditional branch
   3 -> 0 ASM_OUTPUT_INTERNAL_LABEL if the `target' label is reached
          (the target label has CODE_LABEL_NUMBER equal to arm_target_label).
   4 -> 0 final_prescan_insn if the `target' unconditional branch is reached
          (the target insn is arm_target_insn).

   If the jump clobbers the conditions then we use states 2 and 4.

   A similar thing can be done with conditional return insns.

   XXX In case the `target' is an unconditional branch, this conditionalising
   of the instructions always reduces code size, but not always execution
   time.  But then, I want to reduce the code size to somewhere near what
   /bin/cc produces.  */

/* Returns the index of the ARM condition code string in
   `arm_condition_codes'.  COMPARISON should be an rtx like
   `(eq (...) (...))'.  */

static enum arm_cond_code
get_arm_condition_code (comparison)
     rtx comparison;
{
  enum machine_mode mode = GET_MODE (XEXP (comparison, 0));
  register int code;
  register enum rtx_code comp_code = GET_CODE (comparison);

  if (GET_MODE_CLASS (mode) != MODE_CC)
    mode = SELECT_CC_MODE (comp_code, XEXP (comparison, 0),
			   XEXP (comparison, 1));

  switch (mode)
    {
    case CC_DNEmode: code = ARM_NE; goto dominance;
    case CC_DEQmode: code = ARM_EQ; goto dominance;
    case CC_DGEmode: code = ARM_GE; goto dominance;
    case CC_DGTmode: code = ARM_GT; goto dominance;
    case CC_DLEmode: code = ARM_LE; goto dominance;
    case CC_DLTmode: code = ARM_LT; goto dominance;
    case CC_DGEUmode: code = ARM_CS; goto dominance;
    case CC_DGTUmode: code = ARM_HI; goto dominance;
    case CC_DLEUmode: code = ARM_LS; goto dominance;
    case CC_DLTUmode: code = ARM_CC;

    dominance:
      if (comp_code != EQ && comp_code != NE)
	abort ();

      if (comp_code == EQ)
	return ARM_INVERSE_CONDITION_CODE (code);
      return code;

    case CC_NOOVmode:
      switch (comp_code)
	{
	case NE: return ARM_NE;
	case EQ: return ARM_EQ;
	case GE: return ARM_PL;
	case LT: return ARM_MI;
	default: abort ();
	}

    case CC_Zmode:
    case CCFPmode:
      switch (comp_code)
	{
	case NE: return ARM_NE;
	case EQ: return ARM_EQ;
	default: abort ();
	}

    case CCFPEmode:
      switch (comp_code)
	{
	case GE: return ARM_GE;
	case GT: return ARM_GT;
	case LE: return ARM_LS;
	case LT: return ARM_MI;
	default: abort ();
	}

    case CC_SWPmode:
      switch (comp_code)
	{
	case NE: return ARM_NE;
	case EQ: return ARM_EQ;
	case GE: return ARM_LE;
	case GT: return ARM_LT;
	case LE: return ARM_GE;
	case LT: return ARM_GT;
	case GEU: return ARM_LS;
	case GTU: return ARM_CC;
	case LEU: return ARM_CS;
	case LTU: return ARM_HI;
	default: abort ();
	}

    case CC_Cmode:
      switch (comp_code)
      {
      case LTU: return ARM_CS;
      case GEU: return ARM_CC;
      default: abort ();
      }
      
    case CCmode:
      switch (comp_code)
	{
	case NE: return ARM_NE;
	case EQ: return ARM_EQ;
	case GE: return ARM_GE;
	case GT: return ARM_GT;
	case LE: return ARM_LE;
	case LT: return ARM_LT;
	case GEU: return ARM_CS;
	case GTU: return ARM_HI;
	case LEU: return ARM_LS;
	case LTU: return ARM_CC;
	default: abort ();
	}

    default: abort ();
    }

  abort ();
}


void
arm_final_prescan_insn (insn)
     rtx insn;
{
  /* BODY will hold the body of INSN.  */
  register rtx body = PATTERN (insn);

  /* This will be 1 if trying to repeat the trick, and things need to be
     reversed if it appears to fail.  */
  int reverse = 0;

  /* JUMP_CLOBBERS will be one implies that the conditions if a branch is
     taken are clobbered, even if the rtl suggests otherwise.  It also
     means that we have to grub around within the jump expression to find
     out what the conditions are when the jump isn't taken.  */
  int jump_clobbers = 0;
  
  /* If we start with a return insn, we only succeed if we find another one. */
  int seeking_return = 0;
  
  /* START_INSN will hold the insn from where we start looking.  This is the
     first insn after the following code_label if REVERSE is true.  */
  rtx start_insn = insn;

  /* If in state 4, check if the target branch is reached, in order to
     change back to state 0.  */
  if (arm_ccfsm_state == 4)
    {
      if (insn == arm_target_insn)
	{
	  arm_target_insn = NULL;
	  arm_ccfsm_state = 0;
	}
      return;
    }

  /* If in state 3, it is possible to repeat the trick, if this insn is an
     unconditional branch to a label, and immediately following this branch
     is the previous target label which is only used once, and the label this
     branch jumps to is not too far off.  */
  if (arm_ccfsm_state == 3)
    {
      if (simplejump_p (insn))
	{
	  start_insn = next_nonnote_insn (start_insn);
	  if (GET_CODE (start_insn) == BARRIER)
	    {
	      /* XXX Isn't this always a barrier?  */
	      start_insn = next_nonnote_insn (start_insn);
	    }
	  if (GET_CODE (start_insn) == CODE_LABEL
	      && CODE_LABEL_NUMBER (start_insn) == arm_target_label
	      && LABEL_NUSES (start_insn) == 1)
	    reverse = TRUE;
	  else
	    return;
	}
      else if (GET_CODE (body) == RETURN)
        {
	  start_insn = next_nonnote_insn (start_insn);
	  if (GET_CODE (start_insn) == BARRIER)
	    start_insn = next_nonnote_insn (start_insn);
	  if (GET_CODE (start_insn) == CODE_LABEL
	      && CODE_LABEL_NUMBER (start_insn) == arm_target_label
	      && LABEL_NUSES (start_insn) == 1)
	    {
	      reverse = TRUE;
	      seeking_return = 1;
	    }
	  else
	    return;
        }
      else
	return;
    }

  if (arm_ccfsm_state != 0 && !reverse)
    abort ();
  if (GET_CODE (insn) != JUMP_INSN)
    return;

  /* This jump might be paralleled with a clobber of the condition codes 
     the jump should always come first */
  if (GET_CODE (body) == PARALLEL && XVECLEN (body, 0) > 0)
    body = XVECEXP (body, 0, 0);

#if 0  
  /* If this is a conditional return then we don't want to know */
  if (GET_CODE (body) == SET && GET_CODE (SET_DEST (body)) == PC
      && GET_CODE (SET_SRC (body)) == IF_THEN_ELSE
      && (GET_CODE (XEXP (SET_SRC (body), 1)) == RETURN
          || GET_CODE (XEXP (SET_SRC (body), 2)) == RETURN))
    return;
#endif

  if (reverse
      || (GET_CODE (body) == SET && GET_CODE (SET_DEST (body)) == PC
	  && GET_CODE (SET_SRC (body)) == IF_THEN_ELSE))
    {
      int insns_skipped;
      int fail = FALSE, succeed = FALSE;
      /* Flag which part of the IF_THEN_ELSE is the LABEL_REF.  */
      int then_not_else = TRUE;
      rtx this_insn = start_insn, label = 0;

      if (get_attr_conds (insn) == CONDS_JUMP_CLOB)
	{
	  /* The code below is wrong for these, and I haven't time to
	     fix it now.  So we just do the safe thing and return.  This
	     whole function needs re-writing anyway.  */
	  jump_clobbers = 1;
	  return;
	}
      
      /* Register the insn jumped to.  */
      if (reverse)
        {
	  if (!seeking_return)
	    label = XEXP (SET_SRC (body), 0);
        }
      else if (GET_CODE (XEXP (SET_SRC (body), 1)) == LABEL_REF)
	label = XEXP (XEXP (SET_SRC (body), 1), 0);
      else if (GET_CODE (XEXP (SET_SRC (body), 2)) == LABEL_REF)
	{
	  label = XEXP (XEXP (SET_SRC (body), 2), 0);
	  then_not_else = FALSE;
	}
      else if (GET_CODE (XEXP (SET_SRC (body), 1)) == RETURN)
	seeking_return = 1;
      else if (GET_CODE (XEXP (SET_SRC (body), 2)) == RETURN)
        {
	  seeking_return = 1;
	  then_not_else = FALSE;
        }
      else
	abort ();

      /* See how many insns this branch skips, and what kind of insns.  If all
	 insns are okay, and the label or unconditional branch to the same
	 label is not too far away, succeed.  */
      for (insns_skipped = 0;
	   !fail && !succeed && insns_skipped++ < max_insns_skipped;)
	{
	  rtx scanbody;

	  this_insn = next_nonnote_insn (this_insn);
	  if (!this_insn)
	    break;

	  switch (GET_CODE (this_insn))
	    {
	    case CODE_LABEL:
	      /* Succeed if it is the target label, otherwise fail since
		 control falls in from somewhere else.  */
	      if (this_insn == label)
		{
		  if (jump_clobbers)
		    {
		      arm_ccfsm_state = 2;
		      this_insn = next_nonnote_insn (this_insn);
		    }
		  else
		    arm_ccfsm_state = 1;
		  succeed = TRUE;
		}
	      else
		fail = TRUE;
	      break;

	    case BARRIER:
	      /* Succeed if the following insn is the target label.
		 Otherwise fail.  
		 If return insns are used then the last insn in a function 
		 will be a barrier. */
	      this_insn = next_nonnote_insn (this_insn);
	      if (this_insn && this_insn == label)
		{
		  if (jump_clobbers)
		    {
		      arm_ccfsm_state = 2;
		      this_insn = next_nonnote_insn (this_insn);
		    }
		  else
		    arm_ccfsm_state = 1;
		  succeed = TRUE;
		}
	      else
		fail = TRUE;
	      break;

	    case CALL_INSN:
	      /* If using 32-bit addresses the cc is not preserved over
		 calls.  */
	      if (TARGET_APCS_32)
		{
		  /* Succeed if the following insn is the target label,
		     or if the following two insns are a barrier and
		     the target label.  */
		  this_insn = next_nonnote_insn (this_insn);
		  if (this_insn && GET_CODE (this_insn) == BARRIER)
		    this_insn = next_nonnote_insn (this_insn);

		  if (this_insn && this_insn == label
		      && insns_skipped < max_insns_skipped)
		    {
		      if (jump_clobbers)
			{
			  arm_ccfsm_state = 2;
			  this_insn = next_nonnote_insn (this_insn);
			}
		      else
			arm_ccfsm_state = 1;
		      succeed = TRUE;
		    }
		  else
		    fail = TRUE;
		}
	      break;

	    case JUMP_INSN:
      	      /* If this is an unconditional branch to the same label, succeed.
		 If it is to another label, do nothing.  If it is conditional,
		 fail.  */
	      /* XXX Probably, the tests for SET and the PC are unnecessary.  */

	      scanbody = PATTERN (this_insn);
	      if (GET_CODE (scanbody) == SET
		  && GET_CODE (SET_DEST (scanbody)) == PC)
		{
		  if (GET_CODE (SET_SRC (scanbody)) == LABEL_REF
		      && XEXP (SET_SRC (scanbody), 0) == label && !reverse)
		    {
		      arm_ccfsm_state = 2;
		      succeed = TRUE;
		    }
		  else if (GET_CODE (SET_SRC (scanbody)) == IF_THEN_ELSE)
		    fail = TRUE;
		}
	      /* Fail if a conditional return is undesirable (eg on a
		 StrongARM), but still allow this if optimizing for size.  */
	      else if (GET_CODE (scanbody) == RETURN
		       && ! use_return_insn (TRUE)
		       && ! optimize_size)
		fail = TRUE;
	      else if (GET_CODE (scanbody) == RETURN
		       && seeking_return)
	        {
		  arm_ccfsm_state = 2;
		  succeed = TRUE;
	        }
	      else if (GET_CODE (scanbody) == PARALLEL)
	        {
		  switch (get_attr_conds (this_insn))
		    {
		    case CONDS_NOCOND:
		      break;
		    default:
		      fail = TRUE;
		      break;
		    }
		}
	      break;

	    case INSN:
	      /* Instructions using or affecting the condition codes make it
		 fail.  */
	      scanbody = PATTERN (this_insn);
	      if (! (GET_CODE (scanbody) == SET
		     || GET_CODE (scanbody) == PARALLEL)
		  || get_attr_conds (this_insn) != CONDS_NOCOND)
		fail = TRUE;
	      break;

	    default:
	      break;
	    }
	}
      if (succeed)
	{
	  if ((!seeking_return) && (arm_ccfsm_state == 1 || reverse))
	    arm_target_label = CODE_LABEL_NUMBER (label);
	  else if (seeking_return || arm_ccfsm_state == 2)
	    {
	      while (this_insn && GET_CODE (PATTERN (this_insn)) == USE)
	        {
		  this_insn = next_nonnote_insn (this_insn);
		  if (this_insn && (GET_CODE (this_insn) == BARRIER
				    || GET_CODE (this_insn) == CODE_LABEL))
		    abort ();
	        }
	      if (!this_insn)
	        {
		  /* Oh, dear! we ran off the end.. give up */
		  recog (PATTERN (insn), insn, NULL_PTR);
		  arm_ccfsm_state = 0;
		  arm_target_insn = NULL;
		  return;
	        }
	      arm_target_insn = this_insn;
	    }
	  else
	    abort ();
	  if (jump_clobbers)
	    {
	      if (reverse)
		abort ();
	      arm_current_cc = 
		  get_arm_condition_code (XEXP (XEXP (XEXP (SET_SRC (body),
							    0), 0), 1));
	      if (GET_CODE (XEXP (XEXP (SET_SRC (body), 0), 0)) == AND)
		arm_current_cc = ARM_INVERSE_CONDITION_CODE (arm_current_cc);
	      if (GET_CODE (XEXP (SET_SRC (body), 0)) == NE)
		arm_current_cc = ARM_INVERSE_CONDITION_CODE (arm_current_cc);
	    }
	  else
	    {
	      /* If REVERSE is true, ARM_CURRENT_CC needs to be inverted from
		 what it was.  */
	      if (!reverse)
		arm_current_cc = get_arm_condition_code (XEXP (SET_SRC (body),
							       0));
	    }

	  if (reverse || then_not_else)
	    arm_current_cc = ARM_INVERSE_CONDITION_CODE (arm_current_cc);
	}

      /* Restore recog_data (getting the attributes of other insns can
	 destroy this array, but final.c assumes that it remains intact
	 across this call; since the insn has been recognized already we
	 call recog direct).  */
      recog (PATTERN (insn), insn, NULL_PTR);
    }
}

#ifdef AOF_ASSEMBLER
/* Special functions only needed when producing AOF syntax assembler. */

rtx aof_pic_label = NULL_RTX;
struct pic_chain
{
  struct pic_chain * next;
  char * symname;
};

static struct pic_chain * aof_pic_chain = NULL;

rtx
aof_pic_entry (x)
     rtx x;
{
  struct pic_chain ** chainp;
  int offset;

  if (aof_pic_label == NULL_RTX)
    {
      /* We mark this here and not in arm_add_gc_roots() to avoid
	 polluting even more code with ifdefs, and because it never
	 contains anything useful until we assign to it here.  */
      ggc_add_rtx_root (&aof_pic_label, 1);
      /* This needs to persist throughout the compilation.  */
      end_temporary_allocation ();
      aof_pic_label = gen_rtx_SYMBOL_REF (Pmode, "x$adcons");
      resume_temporary_allocation ();
    }

  for (offset = 0, chainp = &aof_pic_chain; *chainp;
       offset += 4, chainp = &(*chainp)->next)
    if ((*chainp)->symname == XSTR (x, 0))
      return plus_constant (aof_pic_label, offset);

  *chainp = (struct pic_chain *) xmalloc (sizeof (struct pic_chain));
  (*chainp)->next = NULL;
  (*chainp)->symname = XSTR (x, 0);
  return plus_constant (aof_pic_label, offset);
}

void
aof_dump_pic_table (f)
     FILE * f;
{
  struct pic_chain * chain;

  if (aof_pic_chain == NULL)
    return;

  asm_fprintf (f, "\tAREA |%r$$adcons|, BASED %r\n",
	       PIC_OFFSET_TABLE_REGNUM,
	       PIC_OFFSET_TABLE_REGNUM);
  fputs ("|x$adcons|\n", f);
  
  for (chain = aof_pic_chain; chain; chain = chain->next)
    {
      fputs ("\tDCD\t", f);
      assemble_name (f, chain->symname);
      fputs ("\n", f);
    }
}

int arm_text_section_count = 1;

char *
aof_text_section ()
{
  static char buf[100];
  sprintf (buf, "\tAREA |C$$code%d|, CODE, READONLY",
	   arm_text_section_count++);
  if (flag_pic)
    strcat (buf, ", PIC, REENTRANT");
  return buf;
}

static int arm_data_section_count = 1;

char *
aof_data_section ()
{
  static char buf[100];
  sprintf (buf, "\tAREA |C$$data%d|, DATA", arm_data_section_count++);
  return buf;
}

/* The AOF assembler is religiously strict about declarations of
   imported and exported symbols, so that it is impossible to declare
   a function as imported near the beginning of the file, and then to
   export it later on.  It is, however, possible to delay the decision
   until all the functions in the file have been compiled.  To get
   around this, we maintain a list of the imports and exports, and
   delete from it any that are subsequently defined.  At the end of
   compilation we spit the remainder of the list out before the END
   directive.  */

struct import
{
  struct import * next;
  char * name;
};

static struct import * imports_list = NULL;

void
aof_add_import (name)
     char * name;
{
  struct import * new;

  for (new = imports_list; new; new = new->next)
    if (new->name == name)
      return;

  new = (struct import *) xmalloc (sizeof (struct import));
  new->next = imports_list;
  imports_list = new;
  new->name = name;
}

void
aof_delete_import (name)
     char * name;
{
  struct import ** old;

  for (old = &imports_list; *old; old = & (*old)->next)
    {
      if ((*old)->name == name)
	{
	  *old = (*old)->next;
	  return;
	}
    }
}

int arm_main_function = 0;

void
aof_dump_imports (f)
     FILE * f;
{
  /* The AOF assembler needs this to cause the startup code to be extracted
     from the library.  Brining in __main causes the whole thing to work
     automagically.  */
  if (arm_main_function)
    {
      text_section ();
      fputs ("\tIMPORT __main\n", f);
      fputs ("\tDCD __main\n", f);
    }

  /* Now dump the remaining imports.  */
  while (imports_list)
    {
      fprintf (f, "\tIMPORT\t");
      assemble_name (f, imports_list->name);
      fputc ('\n', f);
      imports_list = imports_list->next;
    }
}
#endif /* AOF_ASSEMBLER */
