/* Output routines for GCC for ARM.
   Copyright (C) 1991, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002
   Free Software Foundation, Inc.
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
#include "tree.h"
#include "obstack.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "reload.h"
#include "function.h"
#include "expr.h"
#include "optabs.h"
#include "toplev.h"
#include "recog.h"
#include "ggc.h"
#include "except.h"
#include "c-pragma.h"
#include "integrate.h"
#include "tm_p.h"
#include "target.h"
#include "target-def.h"

/* Forward definitions of types.  */
typedef struct minipool_node    Mnode;
typedef struct minipool_fixup   Mfix;

/* In order to improve the layout of the prototypes below
   some short type abbreviations are defined here.  */
#define Hint     HOST_WIDE_INT
#define Mmode    enum machine_mode
#define Ulong    unsigned long
#define Ccstar   const char *

const struct attribute_spec arm_attribute_table[];

/* Forward function declarations.  */
static void      arm_add_gc_roots 		PARAMS ((void));
static int       arm_gen_constant		PARAMS ((enum rtx_code, Mmode, Hint, rtx, rtx, int, int));
static Ulong     bit_count 			PARAMS ((signed int));
static int       const_ok_for_op 		PARAMS ((Hint, enum rtx_code));
static int       eliminate_lr2ip		PARAMS ((rtx *));
static rtx	 emit_multi_reg_push		PARAMS ((int));
static rtx	 emit_sfm			PARAMS ((int, int));
#ifndef AOF_ASSEMBLER
static bool	 arm_assemble_integer		PARAMS ((rtx, unsigned int, int));
#endif
static Ccstar    fp_const_from_val		PARAMS ((REAL_VALUE_TYPE *));
static arm_cc    get_arm_condition_code		PARAMS ((rtx));
static void      init_fpa_table			PARAMS ((void));
static Hint      int_log2			PARAMS ((Hint));
static rtx       is_jump_table 			PARAMS ((rtx));
static Ccstar    output_multi_immediate		PARAMS ((rtx *, Ccstar, Ccstar, int, Hint));
static void      print_multi_reg		PARAMS ((FILE *, Ccstar, int, int));
static Mmode     select_dominance_cc_mode	PARAMS ((rtx, rtx, Hint));
static Ccstar    shift_op			PARAMS ((rtx, Hint *));
static void      arm_init_machine_status	PARAMS ((struct function *));
static void      arm_mark_machine_status        PARAMS ((struct function *));
static void      arm_free_machine_status        PARAMS ((struct function *));
static int       number_of_first_bit_set        PARAMS ((int));
static void      replace_symbols_in_block       PARAMS ((tree, rtx, rtx));
static void      thumb_exit                     PARAMS ((FILE *, int, rtx));
static void      thumb_pushpop                  PARAMS ((FILE *, int, int));
static Ccstar    thumb_condition_code           PARAMS ((rtx, int));
static rtx	 is_jump_table		        PARAMS ((rtx));
static Hint	 get_jump_table_size	        PARAMS ((rtx));
static Mnode *   move_minipool_fix_forward_ref  PARAMS ((Mnode *, Mnode *, Hint));
static Mnode *   add_minipool_forward_ref	PARAMS ((Mfix *));
static Mnode *   move_minipool_fix_backward_ref PARAMS ((Mnode *, Mnode *, Hint));
static Mnode *   add_minipool_backward_ref      PARAMS ((Mfix *));
static void	 assign_minipool_offsets	PARAMS ((Mfix *));
static void	 arm_print_value		PARAMS ((FILE *, rtx));
static void	 dump_minipool		        PARAMS ((rtx));
static int	 arm_barrier_cost		PARAMS ((rtx));
static Mfix *    create_fix_barrier		PARAMS ((Mfix *, Hint));
static void	 push_minipool_barrier	        PARAMS ((rtx, Hint));
static void	 push_minipool_fix		PARAMS ((rtx, Hint, rtx *, Mmode, rtx));
static void	 note_invalid_constants	        PARAMS ((rtx, Hint));
static int       current_file_function_operand	PARAMS ((rtx));
static Ulong	 arm_compute_save_reg0_reg12_mask  PARAMS ((void));
static Ulong     arm_compute_save_reg_mask	PARAMS ((void));
static Ulong     arm_isr_value 			PARAMS ((tree));
static Ulong     arm_compute_func_type		PARAMS ((void));
static tree      arm_handle_fndecl_attribute    PARAMS ((tree *, tree, tree, int, bool *));
static tree      arm_handle_isr_attribute       PARAMS ((tree *, tree, tree, int, bool *));
static void	 arm_output_function_epilogue	PARAMS ((FILE *, Hint));
static void	 arm_output_function_prologue	PARAMS ((FILE *, Hint));
static void	 thumb_output_function_prologue PARAMS ((FILE *, Hint));
static int	 arm_comp_type_attributes	PARAMS ((tree, tree));
static void	 arm_set_default_type_attributes  PARAMS ((tree));
static int	 arm_adjust_cost		PARAMS ((rtx, rtx, rtx, int));
#ifdef OBJECT_FORMAT_ELF
static void	 arm_elf_asm_named_section	PARAMS ((const char *, unsigned int));
#endif

#undef Hint
#undef Mmode
#undef Ulong
#undef Ccstar

/* Initialize the GCC target structure.  */
#ifdef TARGET_DLLIMPORT_DECL_ATTRIBUTES
#undef  TARGET_MERGE_DECL_ATTRIBUTES
#define TARGET_MERGE_DECL_ATTRIBUTES merge_dllimport_decl_attributes
#endif

#undef  TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE arm_attribute_table

#ifdef AOF_ASSEMBLER
#undef  TARGET_ASM_BYTE_OP
#define TARGET_ASM_BYTE_OP "\tDCB\t"
#undef  TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\tDCW\t"
#undef  TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\tDCD\t"
#else
#undef  TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP NULL
#undef  TARGET_ASM_INTEGER
#define TARGET_ASM_INTEGER arm_assemble_integer
#endif

#undef  TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE arm_output_function_prologue

#undef  TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE arm_output_function_epilogue

#undef  TARGET_COMP_TYPE_ATTRIBUTES
#define TARGET_COMP_TYPE_ATTRIBUTES arm_comp_type_attributes

#undef  TARGET_SET_DEFAULT_TYPE_ATTRIBUTES
#define TARGET_SET_DEFAULT_TYPE_ATTRIBUTES arm_set_default_type_attributes

#undef  TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS arm_init_builtins

#undef  TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN arm_expand_builtin

#undef  TARGET_SCHED_ADJUST_COST
#define TARGET_SCHED_ADJUST_COST arm_adjust_cost

struct gcc_target targetm = TARGET_INITIALIZER;

/* Obstack for minipool constant handling.  */
static struct obstack minipool_obstack;
static char *         minipool_startobj;

#define obstack_chunk_alloc   xmalloc
#define obstack_chunk_free    free

/* The maximum number of insns skipped which
   will be conditionalised if possible.  */
static int max_insns_skipped = 5;

extern FILE * asm_out_file;

/* True if we are currently building a constant table.  */
int making_const_table;

/* Define the information needed to generate branch insns.  This is
   stored from the compare operation.  */
rtx arm_compare_op0, arm_compare_op1;

/* What type of floating point are we tuning for?  */
enum floating_point_type arm_fpu;

/* What type of floating point instructions are available?  */
enum floating_point_type arm_fpu_arch;

/* What program mode is the cpu running in? 26-bit mode or 32-bit mode.  */
enum prog_mode_type arm_prgmode;

/* Set by the -mfp=... option.  */
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
#define FL_ARCH5E     (1 << 9)        /* DSP extenstions to v5 */
#define FL_XSCALE     (1 << 10)	      /* XScale */

/* The bits in this mask specify which
   instructions we are allowed to generate.  */
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

/* Nonzero if this chip supports the ARM Architecture 4 extensions.  */
int arm_arch4 = 0;

/* Nonzero if this chip supports the ARM Architecture 5 extensions.  */
int arm_arch5 = 0;

/* Nonzero if this chip supports the ARM Architecture 5E extensions.  */
int arm_arch5e = 0;

/* Nonzero if this chip can benefit from load scheduling.  */
int arm_ld_sched = 0;

/* Nonzero if this chip is a StrongARM.  */
int arm_is_strong = 0;

/* Nonzero if this chip is an XScale.  */
int arm_is_xscale = 0;

/* Nonzero if this chip is an ARM6 or an ARM7.  */
int arm_is_6_or_7 = 0;

/* Nonzero if generating Thumb instructions.  */
int thumb_code = 0;

/* In case of a PRE_INC, POST_INC, PRE_DEC, POST_DEC memory reference, we
   must report the mode of the memory reference from PRINT_OPERAND to
   PRINT_OPERAND_ADDRESS.  */
enum machine_mode output_memory_reference_mode;

/* The register number to be used for the PIC offset register.  */
const char * arm_pic_register_string = NULL;
int arm_pic_register = INVALID_REGNUM;

/* Set to 1 when a return insn is output, this means that the epilogue
   is not needed.  */
int return_used_this_function;

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
static const char * const arm_condition_codes[] =
{
  "eq", "ne", "cs", "cc", "mi", "pl", "vs", "vc",
  "hi", "ls", "ge", "lt", "gt", "le", "al", "nv"
};

#define streq(string1, string2) (strcmp (string1, string2) == 0)

/* Initialization code.  */

struct processors
{
  const char *const name;
  const unsigned int flags;
};

/* Not all of these give usefully different compilation alternatives,
   but there is no simple way of generalizing them.  */
static const struct processors all_cores[] =
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
  {"arm710t",	             FL_MODE26 | FL_MODE32                           | FL_THUMB },
  {"arm720",	             FL_MODE26 | FL_MODE32 },
  {"arm720t",	             FL_MODE26 | FL_MODE32                           | FL_THUMB },
  {"arm740t",	             FL_MODE26 | FL_MODE32                           | FL_THUMB },
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
  {"arm940t",	                         FL_MODE32 | FL_FAST_MULT | FL_ARCH4 | FL_THUMB | FL_LDSCHED },
  {"arm9tdmi",	                         FL_MODE32 | FL_FAST_MULT | FL_ARCH4 | FL_THUMB | FL_LDSCHED },
  {"arm9e",	       	      		 FL_MODE32 | FL_FAST_MULT | FL_ARCH4 |            FL_LDSCHED },
  {"strongarm",	             FL_MODE26 | FL_MODE32 | FL_FAST_MULT | FL_ARCH4 |            FL_LDSCHED | FL_STRONG },
  {"strongarm110",           FL_MODE26 | FL_MODE32 | FL_FAST_MULT | FL_ARCH4 |            FL_LDSCHED | FL_STRONG },
  {"strongarm1100",          FL_MODE26 | FL_MODE32 | FL_FAST_MULT | FL_ARCH4 |            FL_LDSCHED | FL_STRONG },
  {"strongarm1110",          FL_MODE26 | FL_MODE32 | FL_FAST_MULT | FL_ARCH4 |            FL_LDSCHED | FL_STRONG },
  {"arm10tdmi",	                         FL_MODE32 | FL_FAST_MULT | FL_ARCH4 | FL_THUMB | FL_LDSCHED             | FL_ARCH5 },
  {"arm1020t",	                         FL_MODE32 | FL_FAST_MULT | FL_ARCH4 | FL_THUMB | FL_LDSCHED             | FL_ARCH5 },
  {"xscale",                             FL_MODE32 | FL_FAST_MULT | FL_ARCH4 | FL_THUMB | FL_LDSCHED | FL_STRONG | FL_ARCH5 | FL_ARCH5E | FL_XSCALE },
  
  {NULL, 0}
};

static const struct processors all_architectures[] =
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
  { "armv5t",    FL_CO_PROC |             FL_MODE32 | FL_FAST_MULT | FL_ARCH4 | FL_THUMB | FL_ARCH5 },
  { "armv5te",   FL_CO_PROC |             FL_MODE32 | FL_FAST_MULT | FL_ARCH4 | FL_THUMB | FL_ARCH5 | FL_ARCH5E },
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
static unsigned long
bit_count (value)
     signed int value;
{
  unsigned long count = 0;
  
  while (value)
    {
      value &= ~(value & -value);
      ++count;
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
  for (i = ARRAY_SIZE (arm_select); i--;)
    {
      struct arm_cpu_select * ptr = arm_select + i;
      
      if (ptr->string != NULL && ptr->string[0] != '\0')
        {
	  const struct processors * sel;

          for (sel = ptr->processors; sel->name != NULL; sel++)
            if (streq (ptr->string, sel->name))
              {
		if (i == 2)
		  tune_flags = sel->flags;
		else
		  {
		    /* If we have been given an architecture and a processor
		       make sure that they are compatible.  We only generate
		       a warning though, and we prefer the CPU over the
		       architecture.  */
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
      const struct processors * sel;
      unsigned int        sought;
      static const struct cpu_default
      {
	const int cpu;
	const char *const name;
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
	{ TARGET_CPU_xscale,    "xscale" },
	{ TARGET_CPU_generic,   "arm" },
	{ 0, 0 }
      };
      const struct cpu_default * def;
	  
      /* Find the default.  */
      for (def = cpu_defaults; def->name; def++)
	if (def->cpu == TARGET_CPU_DEFAULT)
	  break;

      /* Make sure we found the default CPU.  */
      if (def->name == NULL)
	abort ();
      
      /* Find the default CPU's flags.  */
      for (sel = all_cores; sel->name != NULL; sel++)
	if (streq (def->name, sel->name))
	  break;
      
      if (sel->name == NULL)
	abort ();

      insn_flags = sel->flags;
      
      /* Now check to see if the user has specified some command line
	 switch that require certain abilities from the cpu.  */
      sought = 0;
      
      if (TARGET_INTERWORK || TARGET_THUMB)
	{
	  sought |= (FL_THUMB | FL_MODE32);
	  
	  /* Force apcs-32 to be used for interworking.  */
	  target_flags |= ARM_FLAG_APCS_32;

	  /* There are no ARM processors that support both APCS-26 and
	     interworking.  Therefore we force FL_MODE26 to be removed
	     from insn_flags here (if it was set), so that the search
	     below will always be able to find a compatible processor.  */
	  insn_flags &= ~FL_MODE26;
	}
      else if (!TARGET_APCS_32)
	sought |= FL_MODE26;
      
      if (sought != 0 && ((sought & insn_flags) != sought))
	{
	  /* Try to locate a CPU type that supports all of the abilities
	     of the default CPU, plus the extra abilities requested by
	     the user.  */
	  for (sel = all_cores; sel->name != NULL; sel++)
	    if ((sel->flags & sought) == (sought | insn_flags))
	      break;

	  if (sel->name == NULL)
	    {
	      unsigned int        current_bit_count = 0;
	      const struct processors * best_fit = NULL;
	      
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
	      for (sel = all_cores; sel->name != NULL; sel++)
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
      target_flags &= ~ARM_FLAG_APCS_32;
    }
  else if (!TARGET_APCS_32 && !(insn_flags & FL_MODE26))
    {
      warning ("target CPU does not support APCS-26" );
      target_flags |= ARM_FLAG_APCS_32;
    }
  
  if (TARGET_INTERWORK && !(insn_flags & FL_THUMB))
    {
      warning ("target CPU does not support interworking" );
      target_flags &= ~ARM_FLAG_INTERWORK;
    }
  
  if (TARGET_THUMB && !(insn_flags & FL_THUMB))
    {
      warning ("target CPU does not support THUMB instructions");
      target_flags &= ~ARM_FLAG_THUMB;
    }

  if (TARGET_APCS_FRAME && TARGET_THUMB)
    {
      /* warning ("ignoring -mapcs-frame because -mthumb was used"); */
      target_flags &= ~ARM_FLAG_APCS_FRAME;
    }

  /* TARGET_BACKTRACE calls leaf_function_p, which causes a crash if done
     from here where no function is being compiled currently.  */
  if ((target_flags & (THUMB_FLAG_LEAF_BACKTRACE | THUMB_FLAG_BACKTRACE))
      && TARGET_ARM)
    warning ("enabling backtrace support is only meaningful when compiling for the Thumb");

  if (TARGET_ARM && TARGET_CALLEE_INTERWORKING)
    warning ("enabling callee interworking support is only meaningful when compiling for the Thumb");

  if (TARGET_ARM && TARGET_CALLER_INTERWORKING)
    warning ("enabling caller interworking support is only meaningful when compiling for the Thumb");

  /* If interworking is enabled then APCS-32 must be selected as well.  */
  if (TARGET_INTERWORK)
    {
      if (!TARGET_APCS_32)
	warning ("interworking forces APCS-32 to be used" );
      target_flags |= ARM_FLAG_APCS_32;
    }
  
  if (TARGET_APCS_STACK && !TARGET_APCS_FRAME)
    {
      warning ("-mapcs-stack-check incompatible with -mno-apcs-frame");
      target_flags |= ARM_FLAG_APCS_FRAME;
    }
  
  if (TARGET_POKE_FUNCTION_NAME)
    target_flags |= ARM_FLAG_APCS_FRAME;
  
  if (TARGET_APCS_REENT && flag_pic)
    error ("-fpic and -mapcs-reent are incompatible");
  
  if (TARGET_APCS_REENT)
    warning ("APCS reentrant code not supported.  Ignored");
  
  /* If this target is normally configured to use APCS frames, warn if they
     are turned off and debugging is turned on.  */
  if (TARGET_ARM
      && write_symbols != NO_DEBUG
      && !TARGET_APCS_FRAME
      && (TARGET_DEFAULT & ARM_FLAG_APCS_FRAME))
    warning ("-g with -mno-apcs-frame may not give sensible debugging");
  
  /* If stack checking is disabled, we can use r10 as the PIC register,
     which keeps r9 available.  */
  if (flag_pic)
    arm_pic_register = TARGET_APCS_STACK ? 9 : 10;
  
  if (TARGET_APCS_FLOAT)
    warning ("passing floating point arguments in fp regs not yet supported");
  
  /* Initialise boolean versions of the flags, for use in the arm.md file.  */
  arm_fast_multiply = (insn_flags & FL_FAST_MULT) != 0;
  arm_arch4         = (insn_flags & FL_ARCH4) != 0;
  arm_arch5         = (insn_flags & FL_ARCH5) != 0;
  arm_arch5e        = (insn_flags & FL_ARCH5E) != 0;
  arm_is_xscale     = (insn_flags & FL_XSCALE) != 0;

  arm_ld_sched      = (tune_flags & FL_LDSCHED) != 0;
  arm_is_strong     = (tune_flags & FL_STRONG) != 0;
  thumb_code	    = (TARGET_ARM == 0);
  arm_is_6_or_7     = (((tune_flags & (FL_MODE26 | FL_MODE32))
		       && !(tune_flags & FL_ARCH4))) != 0;

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
	error ("invalid floating point emulation option: -mfpe-%s",
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
  
  arm_prgmode = TARGET_APCS_32 ? PROG_MODE_PROG32 : PROG_MODE_PROG26;
  
  if (structure_size_string != NULL)
    {
      int size = strtol (structure_size_string, NULL, 0);
      
      if (size == 8 || size == 32)
	arm_structure_size_boundary = size;
      else
	warning ("structure size boundary can only be set to 8 or 32");
    }

  if (arm_pic_register_string != NULL)
    {
      int pic_register = decode_reg_name (arm_pic_register_string);
      
      if (!flag_pic)
	warning ("-mpic-register= is useless without -fpic");

      /* Prevent the user from choosing an obviously stupid PIC register.  */
      else if (pic_register < 0 || call_used_regs[pic_register]
	       || pic_register == HARD_FRAME_POINTER_REGNUM
	       || pic_register == STACK_POINTER_REGNUM
	       || pic_register >= PC_REGNUM)
	error ("unable to use '%s' for PIC register", arm_pic_register_string);
      else
	arm_pic_register = pic_register;
    }

  if (TARGET_THUMB && flag_schedule_insns)
    {
      /* Don't warn since it's on by default in -O2.  */
      flag_schedule_insns = 0;
    }

  /* If optimizing for space, don't synthesize constants.
     For processors with load scheduling, it never costs more than 2 cycles
     to load a constant, and the load scheduler may well reduce that to 1.  */
  if (optimize_size || (tune_flags & FL_LDSCHED))
    arm_constant_limit = 1;
  
  if (arm_is_xscale)
    arm_constant_limit = 2;

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
  ggc_add_rtx_root (&arm_target_insn, 1); /* Not sure this is really a root.  */

  gcc_obstack_init(&minipool_obstack);
  minipool_startobj = (char *) obstack_alloc (&minipool_obstack, 0);
}

/* A table of known ARM exception types.
   For use with the interrupt function attribute.  */

typedef struct
{
  const char *const arg;
  const unsigned long return_value;
}
isr_attribute_arg;

static const isr_attribute_arg isr_attribute_args [] =
{
  { "IRQ",   ARM_FT_ISR },
  { "irq",   ARM_FT_ISR },
  { "FIQ",   ARM_FT_FIQ },
  { "fiq",   ARM_FT_FIQ },
  { "ABORT", ARM_FT_ISR },
  { "abort", ARM_FT_ISR },
  { "ABORT", ARM_FT_ISR },
  { "abort", ARM_FT_ISR },
  { "UNDEF", ARM_FT_EXCEPTION },
  { "undef", ARM_FT_EXCEPTION },
  { "SWI",   ARM_FT_EXCEPTION },
  { "swi",   ARM_FT_EXCEPTION },
  { NULL,    ARM_FT_NORMAL }
};

/* Returns the (interrupt) function type of the current
   function, or ARM_FT_UNKNOWN if the type cannot be determined.  */

static unsigned long
arm_isr_value (argument)
     tree argument;
{
  const isr_attribute_arg * ptr;
  const char *              arg;

  /* No argument - default to IRQ.  */
  if (argument == NULL_TREE)
    return ARM_FT_ISR;

  /* Get the value of the argument.  */
  if (TREE_VALUE (argument) == NULL_TREE
      || TREE_CODE (TREE_VALUE (argument)) != STRING_CST)
    return ARM_FT_UNKNOWN;

  arg = TREE_STRING_POINTER (TREE_VALUE (argument));

  /* Check it against the list of known arguments.  */
  for (ptr = isr_attribute_args; ptr->arg != NULL; ptr ++)
    if (streq (arg, ptr->arg))
      return ptr->return_value;

  /* An unrecognised interrupt type.  */
  return ARM_FT_UNKNOWN;
}

/* Computes the type of the current function.  */

static unsigned long
arm_compute_func_type ()
{
  unsigned long type = ARM_FT_UNKNOWN;
  tree a;
  tree attr;
  
  if (TREE_CODE (current_function_decl) != FUNCTION_DECL)
    abort ();

  /* Decide if the current function is volatile.  Such functions
     never return, and many memory cycles can be saved by not storing
     register values that will never be needed again.  This optimization
     was added to speed up context switching in a kernel application.  */
  if (optimize > 0
      && current_function_nothrow
      && TREE_THIS_VOLATILE (current_function_decl))
    type |= ARM_FT_VOLATILE;
  
  if (current_function_needs_context)
    type |= ARM_FT_NESTED;

  attr = DECL_ATTRIBUTES (current_function_decl);
  
  a = lookup_attribute ("naked", attr);
  if (a != NULL_TREE)
    type |= ARM_FT_NAKED;

  if (cfun->machine->eh_epilogue_sp_ofs != NULL_RTX)
    type |= ARM_FT_EXCEPTION_HANDLER;
  else
    {
      a = lookup_attribute ("isr", attr);
      if (a == NULL_TREE)
	a = lookup_attribute ("interrupt", attr);
      
      if (a == NULL_TREE)
	type |= TARGET_INTERWORK ? ARM_FT_INTERWORKED : ARM_FT_NORMAL;
      else
	type |= arm_isr_value (TREE_VALUE (a));
    }
  
  return type;
}

/* Returns the type of the current function.  */

unsigned long
arm_current_func_type ()
{
  if (ARM_FUNC_TYPE (cfun->machine->func_type) == ARM_FT_UNKNOWN)
    cfun->machine->func_type = arm_compute_func_type ();

  return cfun->machine->func_type;
}

/* Return 1 if it is possible to return using a single instruction.  */

int
use_return_insn (iscond)
     int iscond;
{
  int regno;
  unsigned int func_type;
  unsigned long saved_int_regs;

  /* Never use a return instruction before reload has run.  */
  if (!reload_completed)
    return 0;
      
  func_type = arm_current_func_type ();

  /* Naked functions and volatile functions need special
     consideration.  */
  if (func_type & (ARM_FT_VOLATILE | ARM_FT_NAKED))
    return 0;
  
  /* As do variadic functions.  */
  if (current_function_pretend_args_size
      || cfun->machine->uses_anonymous_args
      /* Of if the function calls __builtin_eh_return () */
      || ARM_FUNC_TYPE (func_type) == ARM_FT_EXCEPTION_HANDLER
      /* Or if there is no frame pointer and there is a stack adjustment.  */
      || ((get_frame_size () + current_function_outgoing_args_size != 0)
	  && !frame_pointer_needed))
    return 0;

  saved_int_regs = arm_compute_save_reg_mask ();

  /* Can't be done if interworking with Thumb, and any registers have been
     stacked.  */
  if (TARGET_INTERWORK && saved_int_regs != 0)
    return 0;

  /* On StrongARM, conditional returns are expensive if they aren't
     taken and multiple registers have been stacked.  */
  if (iscond && arm_is_strong)
    {
      /* Conditional return when just the LR is stored is a simple 
	 conditional-load instruction, that's not expensive.  */
      if (saved_int_regs != 0 && saved_int_regs != (1 << LR_REGNUM))
	return 0;

      if (flag_pic && regs_ever_live[PIC_OFFSET_TABLE_REGNUM])
	return 0;
    }

  /* If there are saved registers but the LR isn't saved, then we need
     two instructions for the return.  */
  if (saved_int_regs && !(saved_int_regs & (1 << LR_REGNUM)))
    return 0;

  /* Can't be done if any of the FPU regs are pushed,
     since this also requires an insn.  */
  if (TARGET_HARD_FLOAT)
    for (regno = FIRST_ARM_FP_REGNUM; regno <= LAST_ARM_FP_REGNUM; regno++)
      if (regs_ever_live[regno] && !call_used_regs[regno])
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
  if ((i & ~(unsigned HOST_WIDE_INT) 0xffffffff) != 0
      && ((i & ~(unsigned HOST_WIDE_INT) 0xffffffff)
	  != ((~(unsigned HOST_WIDE_INT) 0)
	      & ~(unsigned HOST_WIDE_INT) 0xffffffff)))
    return FALSE;
  
  /* Fast return for 0 and powers of 2 */
  if ((i & (i - 1)) == 0)
    return TRUE;

  do
    {
      if ((i & mask & (unsigned HOST_WIDE_INT) 0xffffffff) == 0)
        return TRUE;
      mask =
	  (mask << 2) | ((mask & (unsigned HOST_WIDE_INT) 0xffffffff)
			  >> (32 - 2)) | ~(unsigned HOST_WIDE_INT) 0xffffffff;
    }
  while (mask != ~(unsigned HOST_WIDE_INT) 0xFF);

  return FALSE;
}

/* Return true if I is a valid constant for the operation CODE.  */
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
      if (!after_arm_reorg
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
					gen_rtx_MINUS (mode, temp, source)));
	      else
		emit_insn (gen_rtx_SET (VOIDmode, target,
					gen_rtx (code, mode, source, temp)));
	      return 2;
	    }
	}
    }

  return arm_gen_constant (code, mode, val, target, source, subtargets, 1);
}

static int
count_insns_for_constant (HOST_WIDE_INT remainder, int i)
{
  HOST_WIDE_INT temp1;
  int num_insns = 0;
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
	  num_insns++;
	  i -= 6;
	}
      i -= 2;
    } while (remainder);
  return num_insns;
}

/* As above, but extra parameter GENERATE which, if clear, suppresses
   RTL generation.  */

static int
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
  unsigned HOST_WIDE_INT remainder = val & 0xffffffff;

  /* Find out which operations are safe for a given CODE.  Also do a quick
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
      if (remainder == 0xffffffff)
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
      if (remainder == 0xffffffff)
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
      if (remainder == 0xffffffff)
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

  /* If we can do it in one insn get out quickly.  */
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
     optimizations.  */
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
      if (val & 0xffff0000)
	{
	  temp1 = remainder & 0xffff0000;
	  temp2 = remainder & 0x0000ffff;

	  /* Overlaps outside this range are best done using other methods.  */
	  for (i = 9; i < 24; i++)
	    {
	      if ((((temp2 | (temp2 << i)) & 0xffffffff) == remainder)
		  && !const_ok_for_arm (temp2))
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

	  /* Don't duplicate cases already considered.  */
	  for (i = 17; i < 24; i++)
	    {
	      if (((temp1 | (temp1 >> i)) == remainder)
		  && !const_ok_for_arm (temp1))
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
	  || (reload_completed && !reg_mentioned_p (target, source)))
	{
	  if (const_ok_for_arm (ARM_SIGN_EXTEND (~val)))
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

      if (const_ok_for_arm (temp1 = ARM_SIGN_EXTEND (~val)))
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
	  HOST_WIDE_INT shift_mask = ((0xffffffff
				       << (32 - clear_sign_bit_copies))
				      & 0xffffffff);

	  if ((remainder | shift_mask) != 0xffffffff)
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
	  
	  if ((remainder | shift_mask) != 0xffffffff)
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
    remainder = (~remainder) & 0xffffffff;
  else if (code == PLUS && num_bits_set > 16)
    remainder = (-remainder) & 0xffffffff;
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
     In the worst case this code should produce no more than four insns.  */
  {
    int best_start = 0;
    int best_consecutive_zeros = 0;

    for (i = 0; i < 32; i += 2)
      {
	int consecutive_zeros = 0;

	if (!(remainder & (3 << i)))
	  {
	    while ((i < 32) && !(remainder & (3 << i)))
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

    /* So long as it won't require any more insns to do so, it's
       desirable to emit a small constant (in bits 0...9) in the last
       insn.  This way there is more chance that it can be combined with
       a later addressing insn to form a pre-indexed load or store
       operation.  Consider:

	       *((volatile int *)0xe0000100) = 1;
	       *((volatile int *)0xe0000110) = 2;

       We want this to wind up as:

		mov rA, #0xe0000000
		mov rB, #1
		str rB, [rA, #0x100]
		mov rB, #2
		str rB, [rA, #0x110]

       rather than having to synthesize both large constants from scratch.

       Therefore, we calculate how many insns would be required to emit
       the constant starting from `best_start', and also starting from 
       zero (ie with bit 31 first to be output).  If `best_start' doesn't 
       yield a shorter sequence, we may as well use zero.  */
    if (best_start != 0
	&& ((((unsigned HOST_WIDE_INT) 1) << best_start) < remainder)
	&& (count_insns_for_constant (remainder, 0) <= 
	    count_insns_for_constant (remainder, best_start)))
      best_start = 0;

    /* Now start emitting the insns.  */
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
		rtx new_src, temp1_rtx;

		if (code == SET || code == MINUS)
		  {
		    new_src = (subtargets ? gen_reg_rtx (mode) : target);
		    if (can_invert && code != MINUS)
		      temp1 = ~temp1;
		  }
		else
		  {
		    if (remainder && subtargets)
		      new_src = gen_reg_rtx (mode);
		    else
		      new_src = target;
		    if (can_invert)
		      temp1 = ~temp1;
		    else if (can_negate)
		      temp1 = -temp1;
		  }

		temp1 = trunc_int_for_mode (temp1, mode);
		temp1_rtx = GEN_INT (temp1);

		if (code == SET)
		  ;
		else if (code == MINUS)
		  temp1_rtx = gen_rtx_MINUS (mode, temp1_rtx, source);
		else
		  temp1_rtx = gen_rtx_fmt_ee (code, mode, source, temp1_rtx);

		emit_insn (gen_rtx_SET (VOIDmode, new_src, temp1_rtx));
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
      }
    while (remainder);
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
      if (i != ((((unsigned HOST_WIDE_INT) 1) << (HOST_BITS_PER_WIDE_INT - 1)) - 1)
	  && (const_ok_for_arm (i + 1) || const_ok_for_arm (-(i + 1))))
	{
	  *op1 = GEN_INT (i + 1);
	  return code == GT ? GE : LT;
	}
      break;

    case GE:
    case LT:
      if (i != (((unsigned HOST_WIDE_INT) 1) << (HOST_BITS_PER_WIDE_INT - 1))
	  && (const_ok_for_arm (i - 1) || const_ok_for_arm (-(i - 1))))
	{
	  *op1 = GEN_INT (i - 1);
	  return code == GE ? GT : LE;
	}
      break;

    case GTU:
    case LEU:
      if (i != ~((unsigned HOST_WIDE_INT) 0)
	  && (const_ok_for_arm (i + 1) || const_ok_for_arm (-(i + 1))))
	{
	  *op1 = GEN_INT (i + 1);
	  return code == GTU ? GEU : LTU;
	}
      break;

    case GEU:
    case LTU:
      if (i != 0
	  && (const_ok_for_arm (i - 1) || const_ok_for_arm (-(i - 1))))
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
  if (!AGGREGATE_TYPE_P (type))
    /* All simple types are returned in registers.  */
    return 0;
  
  /* For the arm-wince targets we choose to be compitable with Microsoft's
     ARM and Thumb compilers, which always return aggregates in memory.  */
#ifndef ARM_WINCE
  /* All structures/unions bigger than one word are returned in memory.
     Also catch the case where int_size_in_bytes returns -1.  In this case
     the aggregate is either huge or of varaible size, and in either case
     we will want to return it via memory and not in a register.  */
  if (((unsigned int) int_size_in_bytes (type)) > UNITS_PER_WORD)
    return 1;
  
  if (TREE_CODE (type) == RECORD_TYPE)
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
	return 0; /* An empty structure.  Allowed by an extension to ANSI C.  */

      /* Check that the first field is valid for returning in a register.  */

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
	  
	  if (!DECL_BIT_FIELD_TYPE (field))
	    return 1;
	}

      return 0;
    }
  
  if (TREE_CODE (type) == UNION_TYPE)
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
#endif /* not ARM_WINCE */  
  
  /* Return all other types in memory.  */
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
  pcum->nregs = ((fntype && aggregate_value_p (TREE_TYPE (fntype))) ? 1 : 0);
  
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
  
  if (!named || pcum->nregs >= NUM_ARG_REGS)
    return NULL_RTX;
  
  return gen_rtx_REG (mode, pcum->nregs);
}

/* Encode the current state of the #pragma [no_]long_calls.  */
typedef enum
{
  OFF,		/* No #pramgma [no_]long_calls is in effect.  */
  LONG,		/* #pragma long_calls is in effect.  */
  SHORT		/* #pragma no_long_calls is in effect.  */
} arm_pragma_enum;

static arm_pragma_enum arm_pragma_long_calls = OFF;

void
arm_pr_long_calls (pfile)
     cpp_reader * pfile ATTRIBUTE_UNUSED;
{
  arm_pragma_long_calls = LONG;
}

void
arm_pr_no_long_calls (pfile)
     cpp_reader * pfile ATTRIBUTE_UNUSED;
{
  arm_pragma_long_calls = SHORT;
}

void
arm_pr_long_calls_off (pfile)
     cpp_reader * pfile ATTRIBUTE_UNUSED;
{
  arm_pragma_long_calls = OFF;
}

/* Table of machine attributes.  */
const struct attribute_spec arm_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler } */
  /* Function calls made to this symbol must be done indirectly, because
     it may lie outside of the 26 bit addressing range of a normal function
     call.  */
  { "long_call",    0, 0, false, true,  true,  NULL },
  /* Whereas these functions are always known to reside within the 26 bit
     addressing range.  */
  { "short_call",   0, 0, false, true,  true,  NULL },
  /* Interrupt Service Routines have special prologue and epilogue requirements.  */ 
  { "isr",          0, 1, false, false, false, arm_handle_isr_attribute },
  { "interrupt",    0, 1, false, false, false, arm_handle_isr_attribute },
  { "naked",        0, 0, true,  false, false, arm_handle_fndecl_attribute },
#ifdef ARM_PE
  /* ARM/PE has three new attributes:
     interfacearm - ?
     dllexport - for exporting a function/variable that will live in a dll
     dllimport - for importing a function/variable from a dll

     Microsoft allows multiple declspecs in one __declspec, separating
     them with spaces.  We do NOT support this.  Instead, use __declspec
     multiple times.
  */
  { "dllimport",    0, 0, true,  false, false, NULL },
  { "dllexport",    0, 0, true,  false, false, NULL },
  { "interfacearm", 0, 0, true,  false, false, arm_handle_fndecl_attribute },
#endif
  { NULL,           0, 0, false, false, false, NULL }
};

/* Handle an attribute requiring a FUNCTION_DECL;
   arguments as in struct attribute_spec.handler.  */

static tree
arm_handle_fndecl_attribute (node, name, args, flags, no_add_attrs)
     tree * node;
     tree   name;
     tree   args ATTRIBUTE_UNUSED;
     int    flags ATTRIBUTE_UNUSED;
     bool * no_add_attrs;
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning ("`%s' attribute only applies to functions",
	       IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle an "interrupt" or "isr" attribute;
   arguments as in struct attribute_spec.handler.  */

static tree
arm_handle_isr_attribute (node, name, args, flags, no_add_attrs)
     tree * node;
     tree   name;
     tree   args;
     int    flags;
     bool * no_add_attrs;
{
  if (DECL_P (*node))
    {
      if (TREE_CODE (*node) != FUNCTION_DECL)
	{
	  warning ("`%s' attribute only applies to functions",
		   IDENTIFIER_POINTER (name));
	  *no_add_attrs = true;
	}
      /* FIXME: the argument if any is checked for type attributes;
	 should it be checked for decl ones?  */
    }
  else
    {
      if (TREE_CODE (*node) == FUNCTION_TYPE
	  || TREE_CODE (*node) == METHOD_TYPE)
	{
	  if (arm_isr_value (args) == ARM_FT_UNKNOWN)
	    {
	      warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
	      *no_add_attrs = true;
	    }
	}
      else if (TREE_CODE (*node) == POINTER_TYPE
	       && (TREE_CODE (TREE_TYPE (*node)) == FUNCTION_TYPE
		   || TREE_CODE (TREE_TYPE (*node)) == METHOD_TYPE)
	       && arm_isr_value (args) != ARM_FT_UNKNOWN)
	{
	  *node = build_type_copy (*node);
	  TREE_TYPE (*node) = build_type_attribute_variant
	    (TREE_TYPE (*node),
	     tree_cons (name, args, TYPE_ATTRIBUTES (TREE_TYPE (*node))));
	  *no_add_attrs = true;
	}
      else
	{
	  /* Possibly pass this attribute on from the type to a decl.  */
	  if (flags & ((int) ATTR_FLAG_DECL_NEXT
		       | (int) ATTR_FLAG_FUNCTION_NEXT
		       | (int) ATTR_FLAG_ARRAY_NEXT))
	    {
	      *no_add_attrs = true;
	      return tree_cons (name, args, NULL_TREE);
	    }
	  else
	    {
	      warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
	    }
	}
    }

  return NULL_TREE;
}

/* Return 0 if the attributes for two types are incompatible, 1 if they
   are compatible, and 2 if they are nearly compatible (which causes a
   warning to be generated).  */

static int
arm_comp_type_attributes (type1, type2)
     tree type1;
     tree type2;
{
  int l1, l2, s1, s2;
  
  /* Check for mismatch of non-default calling convention.  */
  if (TREE_CODE (type1) != FUNCTION_TYPE)
    return 1;

  /* Check for mismatched call attributes.  */
  l1 = lookup_attribute ("long_call", TYPE_ATTRIBUTES (type1)) != NULL;
  l2 = lookup_attribute ("long_call", TYPE_ATTRIBUTES (type2)) != NULL;
  s1 = lookup_attribute ("short_call", TYPE_ATTRIBUTES (type1)) != NULL;
  s2 = lookup_attribute ("short_call", TYPE_ATTRIBUTES (type2)) != NULL;

  /* Only bother to check if an attribute is defined.  */
  if (l1 | l2 | s1 | s2)
    {
      /* If one type has an attribute, the other must have the same attribute.  */
      if ((l1 != l2) || (s1 != s2))
	return 0;

      /* Disallow mixed attributes.  */
      if ((l1 & s2) || (l2 & s1))
	return 0;
    }
  
  /* Check for mismatched ISR attribute.  */
  l1 = lookup_attribute ("isr", TYPE_ATTRIBUTES (type1)) != NULL;
  if (! l1)
    l1 = lookup_attribute ("interrupt", TYPE_ATTRIBUTES (type1)) != NULL;
  l2 = lookup_attribute ("isr", TYPE_ATTRIBUTES (type2)) != NULL;
  if (! l2)
    l1 = lookup_attribute ("interrupt", TYPE_ATTRIBUTES (type2)) != NULL;
  if (l1 != l2)
    return 0;

  return 1;
}

/*  Encode long_call or short_call attribute by prefixing
    symbol name in DECL with a special character FLAG.  */

void
arm_encode_call_attribute (decl, flag)
  tree decl;
  int flag;
{
  const char * str = XSTR (XEXP (DECL_RTL (decl), 0), 0);
  int          len = strlen (str);
  char *       newstr;

  /* Do not allow weak functions to be treated as short call.  */
  if (DECL_WEAK (decl) && flag == SHORT_CALL_FLAG_CHAR)
    return;

  newstr = alloca (len + 2);
  newstr[0] = flag;
  strcpy (newstr + 1, str);

  newstr = (char *) ggc_alloc_string (newstr, len + 1);
  XSTR (XEXP (DECL_RTL (decl), 0), 0) = newstr;
}

/*  Assigns default attributes to newly defined type.  This is used to
    set short_call/long_call attributes for function types of
    functions defined inside corresponding #pragma scopes.  */

static void
arm_set_default_type_attributes (type)
  tree type;
{
  /* Add __attribute__ ((long_call)) to all functions, when
     inside #pragma long_calls or __attribute__ ((short_call)),
     when inside #pragma no_long_calls.  */
  if (TREE_CODE (type) == FUNCTION_TYPE || TREE_CODE (type) == METHOD_TYPE)
    {
      tree type_attr_list, attr_name;
      type_attr_list = TYPE_ATTRIBUTES (type);

      if (arm_pragma_long_calls == LONG)
 	attr_name = get_identifier ("long_call");
      else if (arm_pragma_long_calls == SHORT)
 	attr_name = get_identifier ("short_call");
      else
 	return;

      type_attr_list = tree_cons (attr_name, NULL_TREE, type_attr_list);
      TYPE_ATTRIBUTES (type) = type_attr_list;
    }
}

/* Return 1 if the operand is a SYMBOL_REF for a function known to be
   defined within the current compilation unit.  If this caanot be
   determined, then 0 is returned.  */

static int
current_file_function_operand (sym_ref)
  rtx sym_ref;
{
  /* This is a bit of a fib.  A function will have a short call flag
     applied to its name if it has the short call attribute, or it has
     already been defined within the current compilation unit.  */
  if (ENCODED_SHORT_CALL_ATTR_P (XSTR (sym_ref, 0)))
    return 1;

  /* The current function is always defined within the current compilation
     unit.  if it s a weak definition however, then this may not be the real
     definition of the function, and so we have to say no.  */
  if (sym_ref == XEXP (DECL_RTL (current_function_decl), 0)
      && !DECL_WEAK (current_function_decl))
    return 1;

  /* We cannot make the determination - default to returning 0.  */
  return 0;
}

/* Return non-zero if a 32 bit "long_call" should be generated for
   this call.  We generate a long_call if the function:

        a.  has an __attribute__((long call))
     or b.  is within the scope of a #pragma long_calls
     or c.  the -mlong-calls command line switch has been specified

   However we do not generate a long call if the function:
   
        d.  has an __attribute__ ((short_call))
     or e.  is inside the scope of a #pragma no_long_calls
     or f.  has an __attribute__ ((section))
     or g.  is defined within the current compilation unit.
   
   This function will be called by C fragments contained in the machine
   description file.  CALL_REF and CALL_COOKIE correspond to the matched
   rtl operands.  CALL_SYMBOL is used to distinguish between
   two different callers of the function.  It is set to 1 in the
   "call_symbol" and "call_symbol_value" patterns and to 0 in the "call"
   and "call_value" patterns.  This is because of the difference in the
   SYM_REFs passed by these patterns.  */

int
arm_is_longcall_p (sym_ref, call_cookie, call_symbol)
  rtx sym_ref;
  int call_cookie;
  int call_symbol;
{
  if (!call_symbol)
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
  
  if (current_file_function_operand (sym_ref))
    return 0;
  
  return (call_cookie & CALL_LONG)
    || ENCODED_LONG_CALL_ATTR_P (XSTR (sym_ref, 0))
    || TARGET_LONG_CALLS;
}

/* Return non-zero if it is ok to make a tail-call to DECL.  */

int
arm_function_ok_for_sibcall (decl)
     tree decl;
{
  int call_type = TARGET_LONG_CALLS ? CALL_LONG : CALL_NORMAL;

  /* Never tailcall something for which we have no decl, or if we
     are in Thumb mode.  */
  if (decl == NULL || TARGET_THUMB)
    return 0;

  /* Get the calling method.  */
  if (lookup_attribute ("short_call", TYPE_ATTRIBUTES (TREE_TYPE (decl))))
    call_type = CALL_SHORT;
  else if (lookup_attribute ("long_call", TYPE_ATTRIBUTES (TREE_TYPE (decl))))
    call_type = CALL_LONG;

  /* Cannot tail-call to long calls, since these are out of range of
     a branch instruction.  However, if not compiling PIC, we know
     we can reach the symbol if it is in this compilation unit.  */
  if (call_type == CALL_LONG && (flag_pic || !TREE_ASM_WRITTEN (decl)))
    return 0;

  /* If we are interworking and the function is not declared static
     then we can't tail-call it unless we know that it exists in this 
     compilation unit (since it might be a Thumb routine).  */
  if (TARGET_INTERWORK && TREE_PUBLIC (decl) && !TREE_ASM_WRITTEN (decl))
    return 0;

  /* Never tailcall from an ISR routine - it needs a special exit sequence.  */
  if (IS_INTERRUPT (arm_current_func_type ()))
    return 0;

  /* Everything else is ok.  */
  return 1;
}


int
legitimate_pic_operand_p (x)
     rtx x;
{
  if (CONSTANT_P (x)
      && flag_pic
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
  if (GET_CODE (orig) == SYMBOL_REF
      || GET_CODE (orig) == LABEL_REF)
    {
#ifndef AOF_ASSEMBLER
      rtx pic_ref, address;
#endif
      rtx insn;
      int subregs = 0;

      if (reg == 0)
	{
	  if (no_new_pseudos)
	    abort ();
	  else
	    reg = gen_reg_rtx (Pmode);

	  subregs = 1;
	}

#ifdef AOF_ASSEMBLER
      /* The AOF assembler can generate relocations for these directly, and
	 understands that the PIC register has to be added into the offset.  */
      insn = emit_insn (gen_pic_load_addr_based (reg, orig));
#else
      if (subregs)
	address = gen_reg_rtx (Pmode);
      else
	address = reg;

      if (TARGET_ARM)
	emit_insn (gen_pic_load_addr_arm (address, orig));
      else
	emit_insn (gen_pic_load_addr_thumb (address, orig));

      if ((GET_CODE (orig) == LABEL_REF
	   || (GET_CODE (orig) == SYMBOL_REF && 
	       ENCODED_SHORT_CALL_ATTR_P (XSTR (orig, 0))))
	  && NEED_GOT_RELOC)
	pic_ref = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, address);
      else
	{
	  pic_ref = gen_rtx_MEM (Pmode,
				 gen_rtx_PLUS (Pmode, pic_offset_table_rtx,
					       address));
	  RTX_UNCHANGING_P (pic_ref) = 1;
	}

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
	  if (no_new_pseudos)
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
	  ARM_GO_IF_LEGITIMATE_INDEX (mode, 0, offset, win);

	  if (!no_new_pseudos)
	    offset = force_reg (Pmode, offset);
	  else
	    abort ();

	win:
	  if (GET_CODE (offset) == CONST_INT)
	    return plus_constant (base, INTVAL (offset));
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

  return orig;
}

/* Generate code to load the PIC register.  PROLOGUE is true if
   called from arm_expand_prologue (in which case we want the 
   generated insns at the start of the function);  false if called
   by an exception receiver that needs the PIC register reloaded
   (in which case the insns are just dumped at the current location).  */

void
arm_finalize_pic (prologue)
     int prologue ATTRIBUTE_UNUSED;
{
#ifndef AOF_ASSEMBLER
  rtx l1, pic_tmp, pic_tmp2, seq, pic_rtx;
  rtx global_offset_table;

  if (current_function_uses_pic_offset_table == 0 || TARGET_SINGLE_PIC_BASE)
    return;

  if (!flag_pic)
    abort ();

  start_sequence ();
  l1 = gen_label_rtx ();

  global_offset_table = gen_rtx_SYMBOL_REF (Pmode, "_GLOBAL_OFFSET_TABLE_");
  /* On the ARM the PC register contains 'dot + 8' at the time of the
     addition, on the Thumb it is 'dot + 4'.  */
  pic_tmp = plus_constant (gen_rtx_LABEL_REF (Pmode, l1), TARGET_ARM ? 8 : 4);
  if (GOT_PCREL)
    pic_tmp2 = gen_rtx_CONST (VOIDmode,
			    gen_rtx_PLUS (Pmode, global_offset_table, pc_rtx));
  else
    pic_tmp2 = gen_rtx_CONST (VOIDmode, global_offset_table);

  pic_rtx = gen_rtx_CONST (Pmode, gen_rtx_MINUS (Pmode, pic_tmp2, pic_tmp));
  
  if (TARGET_ARM)
    {
      emit_insn (gen_pic_load_addr_arm (pic_offset_table_rtx, pic_rtx));
      emit_insn (gen_pic_add_dot_plus_eight (pic_offset_table_rtx, l1));
    }
  else
    {
      emit_insn (gen_pic_load_addr_thumb (pic_offset_table_rtx, pic_rtx));
      emit_insn (gen_pic_add_dot_plus_four (pic_offset_table_rtx, l1));
    }

  seq = gen_sequence ();
  end_sequence ();
  if (prologue)
    emit_insn_after (seq, get_insns ());
  else
    emit_insn (seq);

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

#ifndef COSTS_N_INSNS
#define COSTS_N_INSNS(N) ((N) * 4 - 2)
#endif

int
arm_rtx_costs (x, code, outer)
     rtx x;
     enum rtx_code code;
     enum rtx_code outer;
{
  enum machine_mode mode = GET_MODE (x);
  enum rtx_code subcode;
  int extra_cost;

  if (TARGET_THUMB)
    {
      switch (code)
	{
	case ASHIFT:
	case ASHIFTRT:
	case LSHIFTRT:
	case ROTATERT:	
	case PLUS:
	case MINUS:
	case COMPARE:
	case NEG:
	case NOT:	
	  return COSTS_N_INSNS (1);
	  
	case MULT:							
	  if (GET_CODE (XEXP (x, 1)) == CONST_INT)			
	    {								
	      int cycles = 0;						
	      unsigned HOST_WIDE_INT i = INTVAL (XEXP (x, 1));
	      
	      while (i)						
		{							
		  i >>= 2;						
		  cycles++;						
		}							
	      return COSTS_N_INSNS (2) + cycles;			
	    }
	  return COSTS_N_INSNS (1) + 16;
	  
	case SET:							
	  return (COSTS_N_INSNS (1)					
		  + 4 * ((GET_CODE (SET_SRC (x)) == MEM)		
			 + GET_CODE (SET_DEST (x)) == MEM));
	  
	case CONST_INT:						
	  if (outer == SET)						
	    {							
	      if ((unsigned HOST_WIDE_INT) INTVAL (x) < 256)		
		return 0;						
	      if (thumb_shiftable_const (INTVAL (x)))			
		return COSTS_N_INSNS (2);				
	      return COSTS_N_INSNS (3);				
	    }								
	  else if (outer == PLUS					
		   && INTVAL (x) < 256 && INTVAL (x) > -256)		
	    return 0;							
	  else if (outer == COMPARE					
		   && (unsigned HOST_WIDE_INT) INTVAL (x) < 256)	
	    return 0;							
	  else if (outer == ASHIFT || outer == ASHIFTRT		
		   || outer == LSHIFTRT)				
	    return 0;							
	  return COSTS_N_INSNS (2);
	  
	case CONST:							
	case CONST_DOUBLE:						
	case LABEL_REF:						
	case SYMBOL_REF:						
	  return COSTS_N_INSNS (3);
	  
	case UDIV:
	case UMOD:
	case DIV:
	case MOD:
	  return 100;

	case TRUNCATE:
	  return 99;

	case AND:
	case XOR:
	case IOR: 
	  /* XXX guess. */
	  return 8;

	case ADDRESSOF:
	case MEM:
	  /* XXX another guess.  */
	  /* Memory costs quite a lot for the first word, but subsequent words
	     load at the equivalent of a single insn each.  */
	  return (10 + 4 * ((GET_MODE_SIZE (mode) - 1) / UNITS_PER_WORD)
		  + (CONSTANT_POOL_ADDRESS_P (x) ? 4 : 0));

	case IF_THEN_ELSE:
	  /* XXX a guess. */
	  if (GET_CODE (XEXP (x, 1)) == PC || GET_CODE (XEXP (x, 2)) == PC)
	    return 14;
	  return 2;

	case ZERO_EXTEND:
	  /* XXX still guessing.  */
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
	      return 99;
	    }
	  
	default:
	  return 99;
#if 0	  
	case FFS:
	case FLOAT:
	case FIX:
	case UNSIGNED_FIX:
	  /* XXX guess */
	  fprintf (stderr, "unexpected code for thumb in rtx_costs: %s\n",
		   rtx_name[code]);
	  abort ();
#endif
	}
    }
  
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
	 fast variant if it exists at all.  */
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
				      & (unsigned HOST_WIDE_INT) 0xffffffff);
	  int add_cost = const_ok_for_arm (i) ? 4 : 8;
	  int j;
	  
	  /* Tune as appropriate.  */ 
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

    case CONST_INT:						
      if (const_ok_for_arm (INTVAL (x)))			
	return outer == SET ? 2 : -1;	    		
      else if (outer == AND                  		
	       && const_ok_for_arm (~INTVAL (x)))		
	return -1;	                              		
      else if ((outer == COMPARE             		
		|| outer == PLUS || outer == MINUS)     
	       && const_ok_for_arm (-INTVAL (x)))		
	return -1;	                              		
      else                                        		
	return 5;
      
    case CONST: 							
    case LABEL_REF:						
    case SYMBOL_REF:						
      return 6;
      
    case CONST_DOUBLE:						
      if (const_double_rtx_ok_for_fpu (x))			
	return outer == SET ? 2 : -1;			
      else if ((outer == COMPARE || outer == PLUS)	
	       && neg_const_double_rtx_ok_for_fpu (x))		
	return -1;						
      return 7;
      
    default:
      return 99;
    }
}

static int
arm_adjust_cost (insn, link, dep, cost)
     rtx insn;
     rtx link;
     rtx dep;
     int cost;
{
  rtx i_pat, d_pat;

  /* Some true dependencies can have a higher cost depending
     on precisely how certain input operands are used.  */
  if (arm_is_xscale
      && REG_NOTE_KIND (link) == 0
      && recog_memoized (insn) < 0
      && recog_memoized (dep) < 0)
    {
      int shift_opnum = get_attr_shift (insn);
      enum attr_type attr_type = get_attr_type (dep);

      /* If nonzero, SHIFT_OPNUM contains the operand number of a shifted
	 operand for INSN.  If we have a shifted input operand and the
	 instruction we depend on is another ALU instruction, then we may
	 have to account for an additional stall.  */
      if (shift_opnum != 0 && attr_type == TYPE_NORMAL)
	{
	  rtx shifted_operand;
	  int opno;
	  
	  /* Get the shifted operand.  */
	  extract_insn (insn);
	  shifted_operand = recog_data.operand[shift_opnum];

	  /* Iterate over all the operands in DEP.  If we write an operand
	     that overlaps with SHIFTED_OPERAND, then we have increase the
	     cost of this dependency.  */
	  extract_insn (dep);
	  preprocess_constraints ();
	  for (opno = 0; opno < recog_data.n_operands; opno++)
	    {
	      /* We can ignore strict inputs.  */
	      if (recog_data.operand_type[opno] == OP_IN)
		continue;

	      if (reg_overlap_mentioned_p (recog_data.operand[opno],
					   shifted_operand))
		return 2;
	    }
	}
    }

  /* XXX This is not strictly true for the FPA.  */
  if (REG_NOTE_KIND (link) == REG_DEP_ANTI
      || REG_NOTE_KIND (link) == REG_DEP_OUTPUT)
    return 0;

  /* Call insns don't incur a stall, even if they follow a load.  */
  if (REG_NOTE_KIND (link) == 0
      && GET_CODE (insn) == CALL_INSN)
    return 1;

  if ((i_pat = single_set (insn)) != NULL
      && GET_CODE (SET_SRC (i_pat)) == MEM
      && (d_pat = single_set (dep)) != NULL
      && GET_CODE (SET_DEST (d_pat)) == MEM)
    {
      /* This is a load after a store, there is no conflict if the load reads
	 from a cached area.  Assume that loads from the stack, and from the
	 constant pool are cached, and that others will miss.  This is a 
	 hack.  */
      
      if (CONSTANT_POOL_ADDRESS_P (XEXP (SET_SRC (i_pat), 0))
	  || reg_mentioned_p (stack_pointer_rtx, XEXP (SET_SRC (i_pat), 0))
	  || reg_mentioned_p (frame_pointer_rtx, XEXP (SET_SRC (i_pat), 0))
	  || reg_mentioned_p (hard_frame_pointer_rtx, 
			      XEXP (SET_SRC (i_pat), 0)))
	return 1;
    }

  return cost;
}

/* This code has been fixed for cross compilation.  */

static int fpa_consts_inited = 0;

static const char * const strings_fpa[8] =
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

/* Return TRUE if rtx X is a valid immediate FPU constant.  */

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

/* Return TRUE if rtx X is a valid immediate FPU constant.  */

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
     rtx op;
     enum machine_mode mode;
{
  if (GET_MODE (op) != mode && mode != VOIDmode)
    return 0;

  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  /* We don't consider registers whose class is NO_REGS
     to be a register operand.  */
  /* XXX might have to check for lo regs only for thumb ??? */
  return (GET_CODE (op) == REG
	  && (REGNO (op) >= FIRST_PSEUDO_REGISTER
	      || REGNO_REG_CLASS (REGNO (op)) != NO_REGS));
}

/* A hard register operand (even before reload.  */

int
arm_hard_register_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_MODE (op) != mode && mode != VOIDmode)
    return 0;

  return (GET_CODE (op) == REG
	  && REGNO (op) < FIRST_PSEUDO_REGISTER);
}
    
/* Only accept reg, subreg(reg), const_int.  */

int
reg_or_int_operand (op, mode)
     rtx op;
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
arm_reload_memory_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  int regno = true_regnum (op);

  return (!CONSTANT_P (op)
	  && (regno == -1
	      || (GET_CODE (op) == REG
		  && REGNO (op) >= FIRST_PSEUDO_REGISTER)));
}

/* Return 1 if OP is a valid memory address, but not valid for a signed byte
   memory access (architecture V4).
   MODE is QImode if called when computing constraints, or VOIDmode when
   emitting patterns.  In this latter case we cannot use memory_operand()
   because it will fail on badly formed MEMs, which is precisly what we are
   trying to catch.  */

int
bad_signed_byte_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
#if 0
  if ((mode == QImode && !memory_operand (op, mode)) || GET_CODE (op) != MEM)
    return 0;
#endif
  if (GET_CODE (op) != MEM)
    return 0;

  op = XEXP (op, 0);

  /* A sum of anything more complex than reg + reg or reg + const is bad.  */
  if ((GET_CODE (op) == PLUS || GET_CODE (op) == MINUS)
      && (!s_register_operand (XEXP (op, 0), VOIDmode)
	  || (!s_register_operand (XEXP (op, 1), VOIDmode)
	      && GET_CODE (XEXP (op, 1)) != CONST_INT)))
    return 1;

  /* Big constants are also bad.  */
  if (GET_CODE (op) == PLUS && GET_CODE (XEXP (op, 1)) == CONST_INT
      && (INTVAL (XEXP (op, 1)) > 0xff
	  || -INTVAL (XEXP (op, 1)) > 0xff))
    return 1;

  /* Everything else is good, or can will automatically be made so.  */
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

/* Return TRUE for valid operands for the
   rhs of an ARM instruction, or a load.  */

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
  if (TARGET_THUMB)
    return thumb_cmp_operand (op, mode);
  
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
     rtx op;
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
     rtx op;
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
	  && REGNO_POINTER_ALIGN (REGNO (reg)) >= 32);
}

/* Similar to s_register_operand, but does not allow hard integer 
   registers.  */

int
f_register_operand (op, mode)
     rtx op;
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
      HOST_WIDE_INT value = INTVAL (op);

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

/* Like di_operand, but don't accept constants.  */

int
nonimmediate_di_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (s_register_operand (op, mode))
    return TRUE;

  if (mode != VOIDmode && GET_MODE (op) != VOIDmode && GET_MODE (op) != DImode)
    return FALSE;

  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  if (GET_CODE (op) == MEM)
    return memory_address_p (DImode, XEXP (op, 0));

  return FALSE;
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

/* Like soft_df_operand, but don't accept constants.  */

int
nonimmediate_soft_df_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (s_register_operand (op, mode))
    return TRUE;

  if (mode != VOIDmode && GET_MODE (op) != mode)
    return FALSE;

  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  if (GET_CODE (op) == MEM)
    return memory_address_p (DFmode, XEXP (op, 0));
  return FALSE;
}

/* Return TRUE for valid index operands.  */

int
index_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (s_register_operand (op, mode)
	  || (immediate_operand (op, mode)
	      && (GET_CODE (op) != CONST_INT
		  || (INTVAL (op) < 4096 && INTVAL (op) > -4096))));
}

/* Return TRUE for valid shifts by a constant. This also accepts any
   power of two on the (somewhat overly relaxed) assumption that the
   shift operator in this case was a mult.  */

int
const_shift_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (power_of_two_operand (op, mode)
	  || (immediate_operand (op, mode)
	      && (GET_CODE (op) != CONST_INT
		  || (INTVAL (op) < 32 && INTVAL (op) > 0))));
}

/* Return TRUE for arithmetic operators which can be combined with a multiply
   (shift).  */

int
shiftable_operator (x, mode)
     rtx x;
     enum machine_mode mode;
{
  enum rtx_code code;

  if (GET_MODE (x) != mode)
    return FALSE;

  code = GET_CODE (x);

  return (code == PLUS || code == MINUS
	  || code == IOR || code == XOR || code == AND);
}

/* Return TRUE for binary logical operators.  */

int
logical_binary_operator (x, mode)
     rtx x;
     enum machine_mode mode;
{
  enum rtx_code code;

  if (GET_MODE (x) != mode)
    return FALSE;

  code = GET_CODE (x);

  return (code == IOR || code == XOR || code == AND);
}

/* Return TRUE for shift operators.  */

int
shift_operator (x, mode)
     rtx x;
     enum machine_mode mode;
{
  enum rtx_code code;

  if (GET_MODE (x) != mode)
    return FALSE;

  code = GET_CODE (x);

  if (code == MULT)
    return power_of_two_operand (XEXP (x, 1), mode);

  return (code == ASHIFT || code == ASHIFTRT || code == LSHIFTRT
	  || code == ROTATERT);
}

/* Return TRUE if x is EQ or NE.  */

int
equality_operator (x, mode)
     rtx x;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return GET_CODE (x) == EQ || GET_CODE (x) == NE;
}

/* Return TRUE if x is a comparison operator other than LTGT or UNEQ.  */

int
arm_comparison_operator (x, mode)
     rtx x;
     enum machine_mode mode;
{
  return (comparison_operator (x, mode)
	  && GET_CODE (x) != LTGT
	  && GET_CODE (x) != UNEQ);
}

/* Return TRUE for SMIN SMAX UMIN UMAX operators.  */

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

/* Return TRUE if this is the condition code register, if we aren't given
   a mode, accept any class CCmode register.  */

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

  if (   GET_MODE (x) == mode
      && GET_CODE (x) == REG
      && REGNO    (x) == CC_REGNUM)
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

  if (   mode != CC_DNEmode && mode != CC_DEQmode
      && mode != CC_DLEmode && mode != CC_DLTmode
      && mode != CC_DGEmode && mode != CC_DGTmode
      && mode != CC_DLEUmode && mode != CC_DLTUmode
      && mode != CC_DGEUmode && mode != CC_DGTUmode)
    return FALSE;

  return cc_register (x, mode);
}

/* Return TRUE if X references a SYMBOL_REF.  */

int
symbol_mentioned_p (x)
     rtx x;
{
  const char * fmt;
  int i;

  if (GET_CODE (x) == SYMBOL_REF)
    return 1;

  fmt = GET_RTX_FORMAT (GET_CODE (x));
  
  for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  int j;

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
  const char * fmt;
  int i;

  if (GET_CODE (x) == LABEL_REF)
    return 1;

  fmt = GET_RTX_FORMAT (GET_CODE (x));
  for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  int j;

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

/* Return 1 if memory locations are adjacent.  */

int
adjacent_mem_locations (a, b)
     rtx a, b;
{
  if ((GET_CODE (XEXP (a, 0)) == REG
       || (GET_CODE (XEXP (a, 0)) == PLUS
	   && GET_CODE (XEXP (XEXP (a, 0), 1)) == CONST_INT))
      && (GET_CODE (XEXP (b, 0)) == REG
	  || (GET_CODE (XEXP (b, 0)) == PLUS
	      && GET_CODE (XEXP (XEXP (b, 0), 1)) == CONST_INT)))
    {
      int val0 = 0, val1 = 0;
      int reg0, reg1;
  
      if (GET_CODE (XEXP (a, 0)) == PLUS)
        {
	  reg0 = REGNO  (XEXP (XEXP (a, 0), 0));
	  val0 = INTVAL (XEXP (XEXP (a, 0), 1));
        }
      else
	reg0 = REGNO (XEXP (a, 0));

      if (GET_CODE (XEXP (b, 0)) == PLUS)
        {
	  reg1 = REGNO  (XEXP (XEXP (b, 0), 0));
	  val1 = INTVAL (XEXP (XEXP (b, 0), 1));
        }
      else
	reg1 = REGNO (XEXP (b, 0));

      return (reg0 == reg1) && ((val1 - val0) == 4 || (val0 - val1) == 4);
    }
  return 0;
}

/* Return 1 if OP is a load multiple operation.  It is known to be
   parallel and the first section will be tested.  */

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

  /* Check to see if this might be a write-back.  */
  if (GET_CODE (SET_SRC (elt = XVECEXP (op, 0, 0))) == PLUS)
    {
      i++;
      base = 1;

      /* Now check it more carefully.  */
      if (GET_CODE (SET_DEST (elt)) != REG
          || GET_CODE (XEXP (SET_SRC (elt), 0)) != REG
          || REGNO (XEXP (SET_SRC (elt), 0)) != REGNO (SET_DEST (elt))
          || GET_CODE (XEXP (SET_SRC (elt), 1)) != CONST_INT
          || INTVAL (XEXP (SET_SRC (elt), 1)) != (count - 1) * 4)
        return 0;
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
          || REGNO (SET_DEST (elt)) != (unsigned int)(dest_regno + i - base)
          || GET_CODE (SET_SRC (elt)) != MEM
          || GET_MODE (SET_SRC (elt)) != SImode
          || GET_CODE (XEXP (SET_SRC (elt), 0)) != PLUS
          || !rtx_equal_p (XEXP (XEXP (SET_SRC (elt), 0), 0), src_addr)
          || GET_CODE (XEXP (XEXP (SET_SRC (elt), 0), 1)) != CONST_INT
          || INTVAL (XEXP (XEXP (SET_SRC (elt), 0), 1)) != (i - base) * 4)
        return 0;
    }

  return 1;
}

/* Return 1 if OP is a store multiple operation.  It is known to be
   parallel and the first section will be tested.  */

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

  /* Check to see if this might be a write-back.  */
  if (GET_CODE (SET_SRC (elt = XVECEXP (op, 0, 0))) == PLUS)
    {
      i++;
      base = 1;

      /* Now check it more carefully.  */
      if (GET_CODE (SET_DEST (elt)) != REG
          || GET_CODE (XEXP (SET_SRC (elt), 0)) != REG
          || REGNO (XEXP (SET_SRC (elt), 0)) != REGNO (SET_DEST (elt))
          || GET_CODE (XEXP (SET_SRC (elt), 1)) != CONST_INT
          || INTVAL (XEXP (SET_SRC (elt), 1)) != (count - 1) * 4)
        return 0;
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
          || REGNO (SET_SRC (elt)) != (unsigned int)(src_regno + i - base)
          || GET_CODE (SET_DEST (elt)) != MEM
          || GET_MODE (SET_DEST (elt)) != SImode
          || GET_CODE (XEXP (SET_DEST (elt), 0)) != PLUS
          || !rtx_equal_p (XEXP (XEXP (SET_DEST (elt), 0), 0), dest_addr)
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

  /* Can only handle 2, 3, or 4 insns at present,
     though could be easily extended if required.  */
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
	operands[nops + i] = alter_subreg (operands + (nops + i));

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
	      if (base_reg != (int) REGNO (reg))
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

const char *
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
	operands[nops + i] = alter_subreg (operands + (nops + i));

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
	      if (base_reg != (int) REGNO (reg))
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

const char *
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
      || (XINT (SET_SRC (XVECEXP (op, 0, 0)), 1) != UNSPEC_PUSH_MULT))
    return 0;

  return 1;
}

/* Routines for use in generating RTL.  */

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

  /* XScale has load-store double instructions, but they have stricter
     alignment requirements than load-store multiple, so we can not
     use them.

     For XScale ldm requires 2 + NREGS cycles to complete and blocks
     the pipeline until completion.

	NREGS		CYCLES
	  1		  3
	  2		  4
	  3		  5
	  4		  6

     An ldr instruction takes 1-3 cycles, but does not block the
     pipeline.

	NREGS		CYCLES
	  1		 1-3
	  2		 2-6
	  3		 3-9
	  4		 4-12

     Best case ldr will always win.  However, the more ldr instructions
     we issue, the less likely we are to be able to schedule them well.
     Using ldr instructions also increases code size.

     As a compromise, we use ldr for counts of 1 or 2 regs, and ldm
     for counts of 3 or 4 regs.  */
  if (arm_is_xscale && count <= 2 && ! optimize_size)
    {
      rtx seq;
      
      start_sequence ();
      
      for (i = 0; i < count; i++)
	{
	  mem = gen_rtx_MEM (SImode, plus_constant (from, i * 4 * sign));
	  RTX_UNCHANGING_P (mem) = unchanging_p;
	  MEM_IN_STRUCT_P (mem) = in_struct_p;
	  MEM_SCALAR_P (mem) = scalar_p;
	  emit_move_insn (gen_rtx_REG (SImode, base_regno + i), mem);
	}

      if (write_back)
	emit_move_insn (from, plus_constant (from, count * 4 * sign));

      seq = gen_sequence ();
      end_sequence ();
      
      return seq;
    }

  result = gen_rtx_PARALLEL (VOIDmode,
			     rtvec_alloc (count + (write_back ? 1 : 0)));
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

  /* See arm_gen_load_multiple for discussion of
     the pros/cons of ldm/stm usage for XScale.  */
  if (arm_is_xscale && count <= 2 && ! optimize_size)
    {
      rtx seq;
      
      start_sequence ();
      
      for (i = 0; i < count; i++)
	{
	  mem = gen_rtx_MEM (SImode, plus_constant (to, i * 4 * sign));
	  RTX_UNCHANGING_P (mem) = unchanging_p;
	  MEM_IN_STRUCT_P (mem) = in_struct_p;
	  MEM_SCALAR_P (mem) = scalar_p;
	  emit_move_insn (mem, gen_rtx_REG (SImode, base_regno + i));
	}

      if (write_back)
	emit_move_insn (to, plus_constant (to, count * 4 * sign));

      seq = gen_sequence ();
      end_sequence ();
      
      return seq;
    }

  result = gen_rtx_PARALLEL (VOIDmode,
			     rtvec_alloc (count + (write_back ? 1 : 0)));
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

  in_words_to_go = NUM_INTS (INTVAL (operands[2]));
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

  if (last_bytes && part_bytes_reg == NULL)
    abort ();

  if (BYTES_BIG_ENDIAN && last_bytes)
    {
      rtx tmp = gen_reg_rtx (SImode);

      /* The bytes we want are in the top end of the word.  */
      emit_insn (gen_lshrsi3 (tmp, part_bytes_reg,
			      GEN_INT (8 * (4 - last_bytes))));
      part_bytes_reg = tmp;
      
      while (last_bytes)
	{
	  mem = gen_rtx_MEM (QImode, plus_constant (dst, last_bytes - 1));
	  RTX_UNCHANGING_P (mem) = dst_unchanging_p;
	  MEM_IN_STRUCT_P (mem) = dst_in_struct_p;
	  MEM_SCALAR_P (mem) = dst_scalar_p;
	  emit_move_insn (mem, gen_lowpart (QImode, part_bytes_reg));

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
      if (last_bytes > 1)
	{
	  mem = gen_rtx_MEM (HImode, dst);
	  RTX_UNCHANGING_P (mem) = dst_unchanging_p;
	  MEM_IN_STRUCT_P (mem) = dst_in_struct_p;
	  MEM_SCALAR_P (mem) = dst_scalar_p;
	  emit_move_insn (mem, gen_lowpart (HImode, part_bytes_reg));
	  last_bytes -= 2;
	  if (last_bytes)
	    {
	      rtx tmp = gen_reg_rtx (SImode);

	      emit_insn (gen_addsi3 (dst, dst, GEN_INT (2)));
	      emit_insn (gen_lshrsi3 (tmp, part_bytes_reg, GEN_INT (16)));
	      part_bytes_reg = tmp;
	    }
	}
      
      if (last_bytes)
	{
	  mem = gen_rtx_MEM (QImode, dst);
	  RTX_UNCHANGING_P (mem) = dst_unchanging_p;
	  MEM_IN_STRUCT_P (mem) = dst_in_struct_p;
	  MEM_SCALAR_P (mem) = dst_scalar_p;
	  emit_move_insn (mem, gen_lowpart (QImode, part_bytes_reg));
	}
    }

  return 1;
}

/* Generate a memory reference for a half word, such that it will be loaded
   into the top 16 bits of the word.  We can assume that the address is
   known to be alignable and of the form reg, or plus (reg, const).  */

rtx
arm_gen_rotated_half_load (memref)
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

/* Select a dominance comparison mode if possible.  We support three forms.
   COND_OR == 0 => (X && Y) 
   COND_OR == 1 => ((! X( || Y)
   COND_OR == 2 => (X || Y) 
   If we are unable to support a dominance comparsison we return CC mode.  
   This will then fail to match for the RTL expressions that generate this
   call.  */

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

  /* The if_then_else variant of this tests the second condition if the
     first passes, but is true if the first fails.  Reverse the first
     condition to get a true "inclusive-or" expression.  */
  if (cond_or == 1)
    cond1 = reverse_condition (cond1);

  /* If the comparisons are not equal, and one doesn't dominate the other,
     then we can't do this.  */
  if (cond1 != cond2 
      && !comparison_dominates_p (cond1, cond2)
      && (swapped = 1, !comparison_dominates_p (cond2, cond1)))
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
      if (cond2 == EQ || !cond_or)
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
      if (cond2 == LT || !cond_or)
	return CC_DLTmode;
      if (cond2 == LE)
	return CC_DLEmode;
      if (cond2 == NE)
	return CC_DNEmode;
      break;

    case GT:
      if (cond2 == GT || !cond_or)
	return CC_DGTmode;
      if (cond2 == GE)
	return CC_DGEmode;
      if (cond2 == NE)
	return CC_DNEmode;
      break;
      
    case LTU:
      if (cond2 == LTU || !cond_or)
	return CC_DLTUmode;
      if (cond2 == LEU)
	return CC_DLEUmode;
      if (cond2 == NE)
	return CC_DNEmode;
      break;

    case GTU:
      if (cond2 == GTU || !cond_or)
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
    {
      switch (op)
	{
	case EQ:
	case NE:
	case UNORDERED:
	case ORDERED:
	case UNLT:
	case UNLE:
	case UNGT:
	case UNGE:
	case UNEQ:
	case LTGT:
	  return CCFPmode;

	case LT:
	case LE:
	case GT:
	case GE:
	  return CCFPEmode;

	default:
	  abort ();
	}
    }
  
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

  /* Alternate canonicalizations of the above.  These are somewhat cleaner.  */
  if (GET_CODE (x) == AND
      && GET_RTX_CLASS (GET_CODE (XEXP (x, 0))) == '<'
      && GET_RTX_CLASS (GET_CODE (XEXP (x, 1))) == '<')
    return select_dominance_cc_mode (XEXP (x, 0), XEXP (x, 1), 0);

  if (GET_CODE (x) == IOR
      && GET_RTX_CLASS (GET_CODE (XEXP (x, 0))) == '<'
      && GET_RTX_CLASS (GET_CODE (XEXP (x, 1))) == '<')
    return select_dominance_cc_mode (XEXP (x, 0), XEXP (x, 1), 2);

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
arm_gen_compare_reg (code, x, y)
     enum rtx_code code;
     rtx x, y;
{
  enum machine_mode mode = SELECT_CC_MODE (code, x, y);
  rtx cc_reg = gen_rtx_REG (mode, CC_REGNUM);

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
      offset = SUBREG_BYTE (ref);
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
	/* The slot is out of range, or was dressed up in a SUBREG.  */
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
      /* The addend must be CONST_INT, or we would have dealt with it above.  */
      HOST_WIDE_INT hi, lo;

      offset += INTVAL (XEXP (base, 1));
      base = XEXP (base, 0);

      /* Rework the address into a legal sequence of insns.  */
      /* Valid range for lo is -4095 -> 4095 */
      lo = (offset >= 0
	    ? (offset & 0xfff)
	    : -((-offset) & 0xfff));

      /* Corner case, if lo is the max offset then we would be out of range
	 once we have added the additional 1 below, so bump the msb into the
	 pre-loading insn(s).  */
      if (lo == 4095)
	lo &= 0x7ff;

      hi = ((((offset - lo) & (HOST_WIDE_INT) 0xffffffff)
	     ^ (HOST_WIDE_INT) 0x80000000)
	    - (HOST_WIDE_INT) 0x80000000);

      if (hi + lo != offset)
	abort ();

      if (hi != 0)
	{
	  rtx base_plus = gen_rtx_REG (SImode, REGNO (operands[2]) + 1);

	  /* Get the base address; addsi3 knows how to handle constants
	     that require more than one insn.  */
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
  if (!BYTES_BIG_ENDIAN)
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
      offset = SUBREG_BYTE (ref);
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
	/* The slot is out of range, or was dressed up in a SUBREG.  */
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
	  if (!reg_overlap_mentioned_p (scratch, outval))
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
      /* The addend must be CONST_INT, or we would have dealt with it above.  */
      HOST_WIDE_INT hi, lo;

      offset += INTVAL (XEXP (base, 1));
      base = XEXP (base, 0);

      /* Rework the address into a legal sequence of insns.  */
      /* Valid range for lo is -4095 -> 4095 */
      lo = (offset >= 0
	    ? (offset & 0xfff)
	    : -((-offset) & 0xfff));

      /* Corner case, if lo is the max offset then we would be out of range
	 once we have added the additional 1 below, so bump the msb into the
	 pre-loading insn(s).  */
      if (lo == 4095)
	lo &= 0x7ff;

      hi = ((((offset - lo) & (HOST_WIDE_INT) 0xffffffff)
	     ^ (HOST_WIDE_INT) 0x80000000)
	    - (HOST_WIDE_INT) 0x80000000);

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
	      if (!reg_overlap_mentioned_p (scratch, outval))
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
	     that require more than one insn.  */
	  emit_insn (gen_addsi3 (base_plus, base, GEN_INT (hi)));
	  base = base_plus;
	  offset = lo;
	}
    }

  if (BYTES_BIG_ENDIAN)
    {
      emit_insn (gen_movqi (gen_rtx_MEM (QImode, 
					 plus_constant (base, offset + 1)),
			    gen_lowpart (QImode, outval)));
      emit_insn (gen_lshrsi3 (scratch,
			      gen_rtx_SUBREG (SImode, outval, 0),
			      GEN_INT (8)));
      emit_insn (gen_movqi (gen_rtx_MEM (QImode, plus_constant (base, offset)),
			    gen_lowpart (QImode, scratch)));
    }
  else
    {
      emit_insn (gen_movqi (gen_rtx_MEM (QImode, plus_constant (base, offset)),
			    gen_lowpart (QImode, outval)));
      emit_insn (gen_lshrsi3 (scratch,
			      gen_rtx_SUBREG (SImode, outval, 0),
			      GEN_INT (8)));
      emit_insn (gen_movqi (gen_rtx_MEM (QImode,
					 plus_constant (base, offset + 1)),
			    gen_lowpart (QImode, scratch)));
    }
}

/* Print a symbolic form of X to the debug file, F.  */

static void
arm_print_value (f, x)
     FILE * f;
     rtx x;
{
  switch (GET_CODE (x))
    {
    case CONST_INT:
      fprintf (f, HOST_WIDE_INT_PRINT_HEX, INTVAL (x));
      return;

    case CONST_DOUBLE:
      fprintf (f, "<0x%lx,0x%lx>", (long)XWINT (x, 2), (long)XWINT (x, 3));
      return;

    case CONST_STRING:
      fprintf (f, "\"%s\"", XSTR (x, 0));
      return;

    case SYMBOL_REF:
      fprintf (f, "`%s'", XSTR (x, 0));
      return;

    case LABEL_REF:
      fprintf (f, "L%d", INSN_UID (XEXP (x, 0)));
      return;

    case CONST:
      arm_print_value (f, XEXP (x, 0));
      return;

    case PLUS:
      arm_print_value (f, XEXP (x, 0));
      fprintf (f, "+");
      arm_print_value (f, XEXP (x, 1));
      return;

    case PC:
      fprintf (f, "pc");
      return;

    default:
      fprintf (f, "????");
      return;
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

   Possible enhancements to the algorithm (not implemented) are:

   1) For some processors and object formats, there may be benefit in
   aligning the pools to the start of cache lines; this alignment
   would need to be taken into account when calculating addressability
   of a pool.  */

/* These typedefs are located at the start of this file, so that
   they can be used in the prototypes there.  This comment is to
   remind readers of that fact so that the following structures
   can be understood more easily.

     typedef struct minipool_node    Mnode;
     typedef struct minipool_fixup   Mfix;  */

struct minipool_node
{
  /* Doubly linked chain of entries.  */
  Mnode * next;
  Mnode * prev;
  /* The maximum offset into the code that this entry can be placed.  While
     pushing fixes for forward references, all entries are sorted in order
     of increasing max_address.  */
  HOST_WIDE_INT max_address;
  /* Similarly for an entry inserted for a backwards ref.  */
  HOST_WIDE_INT min_address;
  /* The number of fixes referencing this entry.  This can become zero
     if we "unpush" an entry.  In this case we ignore the entry when we
     come to emit the code.  */
  int refcount;
  /* The offset from the start of the minipool.  */
  HOST_WIDE_INT offset;
  /* The value in table.  */
  rtx value;
  /* The mode of value.  */
  enum machine_mode mode;
  int fix_size;
};

struct minipool_fixup
{
  Mfix *            next;
  rtx               insn;
  HOST_WIDE_INT     address;
  rtx *             loc;
  enum machine_mode mode;
  int               fix_size;
  rtx               value;
  Mnode *           minipool;
  HOST_WIDE_INT     forwards;
  HOST_WIDE_INT     backwards;
};

/* Fixes less than a word need padding out to a word boundary.  */
#define MINIPOOL_FIX_SIZE(mode) \
  (GET_MODE_SIZE ((mode)) >= 4 ? GET_MODE_SIZE ((mode)) : 4)

static Mnode *	minipool_vector_head;
static Mnode *	minipool_vector_tail;
static rtx	minipool_vector_label;

/* The linked list of all minipool fixes required for this function.  */
Mfix * 		minipool_fix_head;
Mfix * 		minipool_fix_tail;
/* The fix entry for the current minipool, once it has been placed.  */
Mfix *		minipool_barrier;

/* Determines if INSN is the start of a jump table.  Returns the end
   of the TABLE or NULL_RTX.  */

static rtx
is_jump_table (insn)
     rtx insn;
{
  rtx table;
  
  if (GET_CODE (insn) == JUMP_INSN
      && JUMP_LABEL (insn) != NULL
      && ((table = next_real_insn (JUMP_LABEL (insn)))
	  == next_real_insn (insn))
      && table != NULL
      && GET_CODE (table) == JUMP_INSN
      && (GET_CODE (PATTERN (table)) == ADDR_VEC
	  || GET_CODE (PATTERN (table)) == ADDR_DIFF_VEC))
    return table;

  return NULL_RTX;
}

#ifndef JUMP_TABLES_IN_TEXT_SECTION
#define JUMP_TABLES_IN_TEXT_SECTION 0
#endif

static HOST_WIDE_INT
get_jump_table_size (insn)
     rtx insn;
{
  /* ADDR_VECs only take room if read-only data does into the text
     section.  */
  if (JUMP_TABLES_IN_TEXT_SECTION
#if !defined(READONLY_DATA_SECTION)
      || 1
#endif
      )
    {
      rtx body = PATTERN (insn);
      int elt = GET_CODE (body) == ADDR_DIFF_VEC ? 1 : 0;

      return GET_MODE_SIZE (GET_MODE (body)) * XVECLEN (body, elt);
    }

  return 0;
}

/* Move a minipool fix MP from its current location to before MAX_MP.
   If MAX_MP is NULL, then MP doesn't need moving, but the addressing
   contrains may need updating.  */

static Mnode *
move_minipool_fix_forward_ref (mp, max_mp, max_address)
     Mnode *       mp;
     Mnode *       max_mp;
     HOST_WIDE_INT max_address;
{
  /* This should never be true and the code below assumes these are
     different.  */
  if (mp == max_mp)
    abort ();

  if (max_mp == NULL)
    {
      if (max_address < mp->max_address)
	mp->max_address = max_address;
    }
  else
    {
      if (max_address > max_mp->max_address - mp->fix_size)
	mp->max_address = max_mp->max_address - mp->fix_size;
      else
	mp->max_address = max_address;

      /* Unlink MP from its current position.  Since max_mp is non-null,
       mp->prev must be non-null.  */
      mp->prev->next = mp->next;
      if (mp->next != NULL)
	mp->next->prev = mp->prev;
      else
	minipool_vector_tail = mp->prev;

      /* Re-insert it before MAX_MP.  */
      mp->next = max_mp;
      mp->prev = max_mp->prev;
      max_mp->prev = mp;
      
      if (mp->prev != NULL)
	mp->prev->next = mp;
      else
	minipool_vector_head = mp;
    }

  /* Save the new entry.  */
  max_mp = mp;

  /* Scan over the preceding entries and adjust their addresses as
     required.  */
  while (mp->prev != NULL
	 && mp->prev->max_address > mp->max_address - mp->prev->fix_size)
    {
      mp->prev->max_address = mp->max_address - mp->prev->fix_size;
      mp = mp->prev;
    }

  return max_mp;
}

/* Add a constant to the minipool for a forward reference.  Returns the
   node added or NULL if the constant will not fit in this pool.  */

static Mnode *
add_minipool_forward_ref (fix)
     Mfix * fix;
{
  /* If set, max_mp is the first pool_entry that has a lower
     constraint than the one we are trying to add.  */
  Mnode *       max_mp = NULL;
  HOST_WIDE_INT max_address = fix->address + fix->forwards;
  Mnode *       mp;
  
  /* If this fix's address is greater than the address of the first
     entry, then we can't put the fix in this pool.  We subtract the
     size of the current fix to ensure that if the table is fully
     packed we still have enough room to insert this value by suffling
     the other fixes forwards.  */
  if (minipool_vector_head &&
      fix->address >= minipool_vector_head->max_address - fix->fix_size)
    return NULL;

  /* Scan the pool to see if a constant with the same value has
     already been added.  While we are doing this, also note the
     location where we must insert the constant if it doesn't already
     exist.  */
  for (mp = minipool_vector_head; mp != NULL; mp = mp->next)
    {
      if (GET_CODE (fix->value) == GET_CODE (mp->value)
	  && fix->mode == mp->mode
	  && (GET_CODE (fix->value) != CODE_LABEL
	      || (CODE_LABEL_NUMBER (fix->value)
		  == CODE_LABEL_NUMBER (mp->value)))
	  && rtx_equal_p (fix->value, mp->value))
	{
	  /* More than one fix references this entry.  */
	  mp->refcount++;
	  return move_minipool_fix_forward_ref (mp, max_mp, max_address);
	}

      /* Note the insertion point if necessary.  */
      if (max_mp == NULL
	  && mp->max_address > max_address)
	max_mp = mp;
    }

  /* The value is not currently in the minipool, so we need to create
     a new entry for it.  If MAX_MP is NULL, the entry will be put on
     the end of the list since the placement is less constrained than
     any existing entry.  Otherwise, we insert the new fix before
     MAX_MP and, if neceesary, adjust the constraints on the other
     entries.  */
  mp = xmalloc (sizeof (* mp));
  mp->fix_size = fix->fix_size;
  mp->mode = fix->mode;
  mp->value = fix->value;
  mp->refcount = 1;
  /* Not yet required for a backwards ref.  */
  mp->min_address = -65536;

  if (max_mp == NULL)
    {
      mp->max_address = max_address;
      mp->next = NULL;
      mp->prev = minipool_vector_tail;

      if (mp->prev == NULL)
	{
	  minipool_vector_head = mp;
	  minipool_vector_label = gen_label_rtx ();
	}
      else
	mp->prev->next = mp;

      minipool_vector_tail = mp;
    }
  else
    {
      if (max_address > max_mp->max_address - mp->fix_size)
	mp->max_address = max_mp->max_address - mp->fix_size;
      else
	mp->max_address = max_address;

      mp->next = max_mp;
      mp->prev = max_mp->prev;
      max_mp->prev = mp;
      if (mp->prev != NULL)
	mp->prev->next = mp;
      else
	minipool_vector_head = mp;
    }

  /* Save the new entry.  */
  max_mp = mp;

  /* Scan over the preceding entries and adjust their addresses as
     required.  */
  while (mp->prev != NULL
	 && mp->prev->max_address > mp->max_address - mp->prev->fix_size)
    {
      mp->prev->max_address = mp->max_address - mp->prev->fix_size;
      mp = mp->prev;
    }

  return max_mp;
}

static Mnode *
move_minipool_fix_backward_ref (mp, min_mp, min_address)
     Mnode *        mp;
     Mnode *        min_mp;
     HOST_WIDE_INT  min_address;
{
  HOST_WIDE_INT offset;

  /* This should never be true, and the code below assumes these are
     different.  */
  if (mp == min_mp)
    abort ();

  if (min_mp == NULL)
    {
      if (min_address > mp->min_address)
	mp->min_address = min_address;
    }
  else
    {
      /* We will adjust this below if it is too loose.  */
      mp->min_address = min_address;

      /* Unlink MP from its current position.  Since min_mp is non-null,
	 mp->next must be non-null.  */
      mp->next->prev = mp->prev;
      if (mp->prev != NULL)
	mp->prev->next = mp->next;
      else
	minipool_vector_head = mp->next;

      /* Reinsert it after MIN_MP.  */
      mp->prev = min_mp;
      mp->next = min_mp->next;
      min_mp->next = mp;
      if (mp->next != NULL)
	mp->next->prev = mp;
      else
	minipool_vector_tail = mp;
    }

  min_mp = mp;

  offset = 0;
  for (mp = minipool_vector_head; mp != NULL; mp = mp->next)
    {
      mp->offset = offset;
      if (mp->refcount > 0)
	offset += mp->fix_size;

      if (mp->next && mp->next->min_address < mp->min_address + mp->fix_size)
	mp->next->min_address = mp->min_address + mp->fix_size;
    }

  return min_mp;
}      

/* Add a constant to the minipool for a backward reference.  Returns the
   node added or NULL if the constant will not fit in this pool.  

   Note that the code for insertion for a backwards reference can be
   somewhat confusing because the calculated offsets for each fix do
   not take into account the size of the pool (which is still under
   construction.  */

static Mnode *
add_minipool_backward_ref (fix)
     Mfix * fix;
{
  /* If set, min_mp is the last pool_entry that has a lower constraint
     than the one we are trying to add.  */
  Mnode *        min_mp = NULL;
  /* This can be negative, since it is only a constraint.  */
  HOST_WIDE_INT  min_address = fix->address - fix->backwards;
  Mnode *        mp;

  /* If we can't reach the current pool from this insn, or if we can't
     insert this entry at the end of the pool without pushing other
     fixes out of range, then we don't try.  This ensures that we
     can't fail later on.  */
  if (min_address >= minipool_barrier->address
      || (minipool_vector_tail->min_address + fix->fix_size
	  >= minipool_barrier->address))
    return NULL;

  /* Scan the pool to see if a constant with the same value has
     already been added.  While we are doing this, also note the
     location where we must insert the constant if it doesn't already
     exist.  */
  for (mp = minipool_vector_tail; mp != NULL; mp = mp->prev)
    {
      if (GET_CODE (fix->value) == GET_CODE (mp->value)
	  && fix->mode == mp->mode
	  && (GET_CODE (fix->value) != CODE_LABEL
	      || (CODE_LABEL_NUMBER (fix->value)
		  == CODE_LABEL_NUMBER (mp->value)))
	  && rtx_equal_p (fix->value, mp->value)
	  /* Check that there is enough slack to move this entry to the
	     end of the table (this is conservative).  */
	  && (mp->max_address 
	      > (minipool_barrier->address 
		 + minipool_vector_tail->offset
		 + minipool_vector_tail->fix_size)))
	{
	  mp->refcount++;
	  return move_minipool_fix_backward_ref (mp, min_mp, min_address);
	}

      if (min_mp != NULL)
	mp->min_address += fix->fix_size;
      else
	{
	  /* Note the insertion point if necessary.  */
	  if (mp->min_address < min_address)
	    min_mp = mp;
	  else if (mp->max_address
		   < minipool_barrier->address + mp->offset + fix->fix_size)
	    {
	      /* Inserting before this entry would push the fix beyond
		 its maximum address (which can happen if we have
		 re-located a forwards fix); force the new fix to come
		 after it.  */
	      min_mp = mp;
	      min_address = mp->min_address + fix->fix_size;
	    }
	}
    }

  /* We need to create a new entry.  */
  mp = xmalloc (sizeof (* mp));
  mp->fix_size = fix->fix_size;
  mp->mode = fix->mode;
  mp->value = fix->value;
  mp->refcount = 1;
  mp->max_address = minipool_barrier->address + 65536;

  mp->min_address = min_address;

  if (min_mp == NULL)
    {
      mp->prev = NULL;
      mp->next = minipool_vector_head;

      if (mp->next == NULL)
	{
	  minipool_vector_tail = mp;
	  minipool_vector_label = gen_label_rtx ();
	}
      else
	mp->next->prev = mp;

      minipool_vector_head = mp;
    }
  else
    {
      mp->next = min_mp->next;
      mp->prev = min_mp;
      min_mp->next = mp;
      
      if (mp->next != NULL)
	mp->next->prev = mp;
      else
	minipool_vector_tail = mp;
    }

  /* Save the new entry.  */
  min_mp = mp;

  if (mp->prev)
    mp = mp->prev;
  else
    mp->offset = 0;

  /* Scan over the following entries and adjust their offsets.  */
  while (mp->next != NULL)
    {
      if (mp->next->min_address < mp->min_address + mp->fix_size)
	mp->next->min_address = mp->min_address + mp->fix_size;

      if (mp->refcount)
	mp->next->offset = mp->offset + mp->fix_size;
      else
	mp->next->offset = mp->offset;

      mp = mp->next;
    }

  return min_mp;
}

static void
assign_minipool_offsets (barrier)
     Mfix * barrier;
{
  HOST_WIDE_INT offset = 0;
  Mnode * mp;

  minipool_barrier = barrier;

  for (mp = minipool_vector_head; mp != NULL; mp = mp->next)
    {
      mp->offset = offset;
      
      if (mp->refcount > 0)
	offset += mp->fix_size;
    }
}

/* Output the literal table */
static void
dump_minipool (scan)
     rtx scan;
{
  Mnode * mp;
  Mnode * nmp;

  if (rtl_dump_file)
    fprintf (rtl_dump_file,
	     ";; Emitting minipool after insn %u; address %ld\n",
	     INSN_UID (scan), (unsigned long) minipool_barrier->address);

  scan = emit_label_after (gen_label_rtx (), scan);
  scan = emit_insn_after (gen_align_4 (), scan);
  scan = emit_label_after (minipool_vector_label, scan);

  for (mp = minipool_vector_head; mp != NULL; mp = nmp)
    {
      if (mp->refcount > 0)
	{
	  if (rtl_dump_file)
	    {
	      fprintf (rtl_dump_file, 
		       ";;  Offset %u, min %ld, max %ld ",
		       (unsigned) mp->offset, (unsigned long) mp->min_address,
		       (unsigned long) mp->max_address);
	      arm_print_value (rtl_dump_file, mp->value);
	      fputc ('\n', rtl_dump_file);
	    }

	  switch (mp->fix_size)
	    {
#ifdef HAVE_consttable_1
	    case 1:
	      scan = emit_insn_after (gen_consttable_1 (mp->value), scan);
	      break;

#endif
#ifdef HAVE_consttable_2
	    case 2:
	      scan = emit_insn_after (gen_consttable_2 (mp->value), scan);
	      break;

#endif
#ifdef HAVE_consttable_4
	    case 4:
	      scan = emit_insn_after (gen_consttable_4 (mp->value), scan);
	      break;

#endif
#ifdef HAVE_consttable_8
	    case 8:
	      scan = emit_insn_after (gen_consttable_8 (mp->value), scan);
	      break;

#endif
	    default:
	      abort ();
	      break;
	    }
	}

      nmp = mp->next;
      free (mp);
    }

  minipool_vector_head = minipool_vector_tail = NULL;
  scan = emit_insn_after (gen_consttable_end (), scan);
  scan = emit_barrier_after (scan);
}

/* Return the cost of forcibly inserting a barrier after INSN.  */

static int
arm_barrier_cost (insn)
     rtx insn;
{
  /* Basing the location of the pool on the loop depth is preferable,
     but at the moment, the basic block information seems to be
     corrupt by this stage of the compilation.  */
  int base_cost = 50;
  rtx next = next_nonnote_insn (insn);

  if (next != NULL && GET_CODE (next) == CODE_LABEL)
    base_cost -= 20;

  switch (GET_CODE (insn))
    {
    case CODE_LABEL:
      /* It will always be better to place the table before the label, rather
	 than after it.  */
      return 50;  

    case INSN:
    case CALL_INSN:
      return base_cost;

    case JUMP_INSN:
      return base_cost - 10;

    default:
      return base_cost + 10;
    }
}

/* Find the best place in the insn stream in the range
   (FIX->address,MAX_ADDRESS) to forcibly insert a minipool barrier.
   Create the barrier by inserting a jump and add a new fix entry for
   it.  */

static Mfix *
create_fix_barrier (fix, max_address)
     Mfix * fix;
     HOST_WIDE_INT max_address;
{
  HOST_WIDE_INT count = 0;
  rtx barrier;
  rtx from = fix->insn;
  rtx selected = from;
  int selected_cost;
  HOST_WIDE_INT selected_address;
  Mfix * new_fix;
  HOST_WIDE_INT max_count = max_address - fix->address;
  rtx label = gen_label_rtx ();

  selected_cost = arm_barrier_cost (from);
  selected_address = fix->address;

  while (from && count < max_count)
    {
      rtx tmp;
      int new_cost;

      /* This code shouldn't have been called if there was a natural barrier
	 within range.  */
      if (GET_CODE (from) == BARRIER)
	abort ();

      /* Count the length of this insn.  */
      count += get_attr_length (from);

      /* If there is a jump table, add its length.  */
      tmp = is_jump_table (from);
      if (tmp != NULL)
	{
	  count += get_jump_table_size (tmp);

	  /* Jump tables aren't in a basic block, so base the cost on
	     the dispatch insn.  If we select this location, we will
	     still put the pool after the table.  */
	  new_cost = arm_barrier_cost (from);

	  if (count < max_count && new_cost <= selected_cost)
	    {
	      selected = tmp;
	      selected_cost = new_cost;
	      selected_address = fix->address + count;
	    }

	  /* Continue after the dispatch table.  */
	  from = NEXT_INSN (tmp);
	  continue;
	}

      new_cost = arm_barrier_cost (from);
      
      if (count < max_count && new_cost <= selected_cost)
	{
	  selected = from;
	  selected_cost = new_cost;
	  selected_address = fix->address + count;
	}

      from = NEXT_INSN (from);
    }

  /* Create a new JUMP_INSN that branches around a barrier.  */
  from = emit_jump_insn_after (gen_jump (label), selected);
  JUMP_LABEL (from) = label;
  barrier = emit_barrier_after (from);
  emit_label_after (label, barrier);

  /* Create a minipool barrier entry for the new barrier.  */
  new_fix = (Mfix *) obstack_alloc (&minipool_obstack, sizeof (* new_fix));
  new_fix->insn = barrier;
  new_fix->address = selected_address;
  new_fix->next = fix->next;
  fix->next = new_fix;

  return new_fix;
}

/* Record that there is a natural barrier in the insn stream at
   ADDRESS.  */
static void
push_minipool_barrier (insn, address)
     rtx insn;
     HOST_WIDE_INT address;
{
  Mfix * fix = (Mfix *) obstack_alloc (&minipool_obstack, sizeof (* fix));

  fix->insn = insn;
  fix->address = address;

  fix->next = NULL;
  if (minipool_fix_head != NULL)
    minipool_fix_tail->next = fix;
  else
    minipool_fix_head = fix;

  minipool_fix_tail = fix;
}

/* Record INSN, which will need fixing up to load a value from the
   minipool.  ADDRESS is the offset of the insn since the start of the
   function; LOC is a pointer to the part of the insn which requires
   fixing; VALUE is the constant that must be loaded, which is of type
   MODE.  */
static void
push_minipool_fix (insn, address, loc, mode, value)
     rtx insn;
     HOST_WIDE_INT address;
     rtx * loc;
     enum machine_mode mode;
     rtx value;
{
  Mfix * fix = (Mfix *) obstack_alloc (&minipool_obstack, sizeof (* fix));

#ifdef AOF_ASSEMBLER
  /* PIC symbol refereneces need to be converted into offsets into the
     based area.  */
  /* XXX This shouldn't be done here.  */
  if (flag_pic && GET_CODE (value) == SYMBOL_REF)
    value = aof_pic_entry (value);
#endif /* AOF_ASSEMBLER */

  fix->insn = insn;
  fix->address = address;
  fix->loc = loc;
  fix->mode = mode;
  fix->fix_size = MINIPOOL_FIX_SIZE (mode);
  fix->value = value;
  fix->forwards = get_attr_pool_range (insn);
  fix->backwards = get_attr_neg_pool_range (insn);
  fix->minipool = NULL;

  /* If an insn doesn't have a range defined for it, then it isn't
     expecting to be reworked by this code.  Better to abort now than
     to generate duff assembly code.  */
  if (fix->forwards == 0 && fix->backwards == 0)
    abort ();

  if (rtl_dump_file)
    {
      fprintf (rtl_dump_file,
	       ";; %smode fixup for i%d; addr %lu, range (%ld,%ld): ",
	       GET_MODE_NAME (mode),
	       INSN_UID (insn), (unsigned long) address, 
	       -1 * (long)fix->backwards, (long)fix->forwards);
      arm_print_value (rtl_dump_file, fix->value);
      fprintf (rtl_dump_file, "\n");
    }

  /* Add it to the chain of fixes.  */
  fix->next = NULL;
  
  if (minipool_fix_head != NULL)
    minipool_fix_tail->next = fix;
  else
    minipool_fix_head = fix;

  minipool_fix_tail = fix;
}

/* Scan INSN and note any of its operands that need fixing.  */

static void
note_invalid_constants (insn, address)
     rtx insn;
     HOST_WIDE_INT address;
{
  int opno;

  extract_insn (insn);

  if (!constrain_operands (1))
    fatal_insn_not_found (insn);

  /* Fill in recog_op_alt with information about the constraints of this
     insn.  */
  preprocess_constraints ();

  for (opno = 0; opno < recog_data.n_operands; opno++)
    {
      /* Things we need to fix can only occur in inputs.  */
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
#if 0
	  /* RWE: Now we look correctly at the operands for the insn,
	     this shouldn't be needed any more.  */
#ifndef AOF_ASSEMBLER
	  /* XXX Is this still needed?  */
	  else if (GET_CODE (op) == UNSPEC && XINT (op, 1) == UNSPEC_PIC_SYM)
	    push_minipool_fix (insn, address, recog_data.operand_loc[opno],
			       recog_data.operand_mode[opno],
			       XVECEXP (op, 0, 0));
#endif
#endif
	  else if (GET_CODE (op) == MEM
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
  HOST_WIDE_INT address = 0;
  Mfix * fix;

  minipool_fix_head = minipool_fix_tail = NULL;

  /* The first insn must always be a note, or the code below won't
     scan it properly.  */
  if (GET_CODE (first) != NOTE)
    abort ();

  /* Scan all the insns and record the operands that will need fixing.  */
  for (insn = next_nonnote_insn (first); insn; insn = next_nonnote_insn (insn))
    {
      if (GET_CODE (insn) == BARRIER)
	push_minipool_barrier (insn, address);
      else if (GET_CODE (insn) == INSN || GET_CODE (insn) == CALL_INSN
	       || GET_CODE (insn) == JUMP_INSN)
	{
	  rtx table;

	  note_invalid_constants (insn, address);
	  address += get_attr_length (insn);

	  /* If the insn is a vector jump, add the size of the table
	     and skip the table.  */
	  if ((table = is_jump_table (insn)) != NULL)
	    {
	      address += get_jump_table_size (table);
	      insn = table;
	    }
	}
    }

  fix = minipool_fix_head;
  
  /* Now scan the fixups and perform the required changes.  */
  while (fix)
    {
      Mfix * ftmp;
      Mfix * fdel;
      Mfix *  last_added_fix;
      Mfix * last_barrier = NULL;
      Mfix * this_fix;

      /* Skip any further barriers before the next fix.  */
      while (fix && GET_CODE (fix->insn) == BARRIER)
	fix = fix->next;

      /* No more fixes.  */
      if (fix == NULL)
	break;

      last_added_fix = NULL;

      for (ftmp = fix; ftmp; ftmp = ftmp->next)
	{
	  if (GET_CODE (ftmp->insn) == BARRIER)
	    {
	      if (ftmp->address >= minipool_vector_head->max_address)
		break;

	      last_barrier = ftmp;
	    }
	  else if ((ftmp->minipool = add_minipool_forward_ref (ftmp)) == NULL)
	    break;

	  last_added_fix = ftmp;  /* Keep track of the last fix added.  */
	}

      /* If we found a barrier, drop back to that; any fixes that we
	 could have reached but come after the barrier will now go in
	 the next mini-pool.  */
      if (last_barrier != NULL)
	{
	  /* Reduce the refcount for those fixes that won't go into this 
	     pool after all.  */
	  for (fdel = last_barrier->next;
	       fdel && fdel != ftmp;
	       fdel = fdel->next)
	    {
	      fdel->minipool->refcount--;
	      fdel->minipool = NULL;
	    }

	  ftmp = last_barrier;
	}
      else
        {
	  /* ftmp is first fix that we can't fit into this pool and
	     there no natural barriers that we could use.  Insert a
	     new barrier in the code somewhere between the previous
	     fix and this one, and arrange to jump around it.  */
	  HOST_WIDE_INT max_address;

	  /* The last item on the list of fixes must be a barrier, so
	     we can never run off the end of the list of fixes without
	     last_barrier being set.  */
	  if (ftmp == NULL)
	    abort ();

	  max_address = minipool_vector_head->max_address;
	  /* Check that there isn't another fix that is in range that
	     we couldn't fit into this pool because the pool was
	     already too large: we need to put the pool before such an
	     instruction.  */
	  if (ftmp->address < max_address)
	    max_address = ftmp->address;

	  last_barrier = create_fix_barrier (last_added_fix, max_address);
	}

      assign_minipool_offsets (last_barrier);

      while (ftmp)
	{
	  if (GET_CODE (ftmp->insn) != BARRIER
	      && ((ftmp->minipool = add_minipool_backward_ref (ftmp))
		  == NULL))
	    break;

	  ftmp = ftmp->next;
	}

      /* Scan over the fixes we have identified for this pool, fixing them
	 up and adding the constants to the pool itself.  */
      for (this_fix = fix; this_fix && ftmp != this_fix;
	   this_fix = this_fix->next)
	if (GET_CODE (this_fix->insn) != BARRIER)
	  {
	    rtx addr
	      = plus_constant (gen_rtx_LABEL_REF (VOIDmode, 
						  minipool_vector_label),
			       this_fix->minipool->offset);
	    *this_fix->loc = gen_rtx_MEM (this_fix->mode, addr);
	  }

      dump_minipool (last_barrier->insn);
      fix = ftmp;
    }

  /* From now on we must synthesize any constants that we can't handle
     directly.  This can happen if the RTL gets split during final
     instruction generation.  */
  after_arm_reorg = 1;

  /* Free the minipool memory.  */
  obstack_free (&minipool_obstack, minipool_startobj);
}

/* Routines to output assembly language.  */

/* If the rtx is the correct value then return the string of the number.
   In this way we can ensure that valid double constants are generated even
   when cross compiling.  */

const char *
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

static const char *
fp_const_from_val (r)
     REAL_VALUE_TYPE * r;
{
  int i;

  if (!fpa_consts_inited)
    init_fpa_table ();

  for (i = 0; i < 8; i++)
    if (REAL_VALUES_EQUAL (*r, values_fpa[i]))
      return strings_fpa[i];

  abort ();
}

/* Output the operands of a LDM/STM instruction to STREAM.
   MASK is the ARM register set mask of which only bits 0-15 are important.
   REG is the base register, either the frame pointer or the stack pointer,
   INSTR is the possibly suffixed load or store instruction.  */

static void
print_multi_reg (stream, instr, reg, mask)
     FILE * stream;
     const char * instr;
     int reg;
     int mask;
{
  int i;
  int not_first = FALSE;

  fputc ('\t', stream);
  asm_fprintf (stream, instr, reg);
  fputs (", {", stream);
  
  for (i = 0; i <= LAST_ARM_REGNUM; i++)
    if (mask & (1 << i))
      {
	if (not_first)
	  fprintf (stream, ", ");
	
	asm_fprintf (stream, "%r", i);
	not_first = TRUE;
      }

  fprintf (stream, "}%s\n", TARGET_APCS_32 ? "" : "^");
}

/* Output a 'call' insn.  */

const char *
output_call (operands)
     rtx * operands;
{
  /* Handle calls to lr using ip (which may be clobbered in subr anyway).  */

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
  int i, j;
  const char * fmt;
  
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
      /* Scan through the sub-elements and change any references there.  */
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
  
/* Output a 'call' insn that is a reference in memory.  */

const char *
output_call_mem (operands)
     rtx * operands;
{
  operands[0] = copy_rtx (operands[0]); /* Be ultra careful.  */
  /* Handle calls using lr by using ip (which may be clobbered in subr anyway).  */
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

const char *
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

const char *
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

const char *
output_mov_long_double_arm_from_arm (operands)
     rtx * operands;
{
  /* We have to be careful here because the two might overlap.  */
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

const char *
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

const char *
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

const char *
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

	  /* Ensure the second source is not overwritten.  */
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

	      memcpy (&u, &CONST_DOUBLE_LOW (operands[1]), sizeof (u));
	      REAL_VALUE_TO_TARGET_DOUBLE (u.d, l);
	      otherops[1] = GEN_INT (l[1]);
	      operands[1] = GEN_INT (l[0]);
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
	  /* Sign extend the intval into the high-order word.  */
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
	      abort (); /* Should never happen now.  */
	      break;

	    case PRE_DEC:
	      output_asm_insn ("ldm%?db\t%m1!, %M0", operands);
	      break;

	    case POST_INC:
	      output_asm_insn ("ldm%?ia\t%m1!, %M0", operands);
	      break;

	    case POST_DEC:
	      abort (); /* Should never happen now.  */
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
		  otherops[1] = adjust_address (operands[1], VOIDmode, 4);
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
	abort ();  /* Constraints should prevent this.  */
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
	  abort (); /* Should never happen now.  */
	  break;

        case PRE_DEC:
	  output_asm_insn ("stm%?db\t%m0!, %M1", operands);
	  break;

        case POST_INC:
	  output_asm_insn ("stm%?ia\t%m0!, %M1", operands);
	  break;

        case POST_DEC:
	  abort (); /* Should never happen now.  */
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
	  otherops[0] = adjust_address (operands[0], VOIDmode, 4);
	  otherops[1] = gen_rtx_REG (SImode, 1 + REGNO (operands[1]));
	  output_asm_insn ("str%?\t%1, %0", operands);
	  output_asm_insn ("str%?\t%1, %0", otherops);
	}
    }
  else
    /* Constraints should prevent this.  */
    abort ();

  return "";
}


/* Output an arbitrary MOV reg, #n.
   OPERANDS[0] is a register.  OPERANDS[1] is a const_int.  */

const char *
output_mov_immediate (operands)
     rtx * operands;
{
  HOST_WIDE_INT n = INTVAL (operands[1]);

  /* Try to use one MOV.  */
  if (const_ok_for_arm (n))
    output_asm_insn ("mov%?\t%0, %1", operands);

  /* Try to use one MVN.  */
  else if (const_ok_for_arm (~n))
    {
      operands[1] = GEN_INT (~n);
      output_asm_insn ("mvn%?\t%0, %1", operands);
    }
  else
    {
      int n_ones = 0;
      int i;

      /* If all else fails, make it out of ORRs or BICs as appropriate.  */
      for (i = 0; i < 32; i ++)
	if (n & 1 << i)
	  n_ones ++;

      if (n_ones > 16)  /* Shorter to use MVN with BIC in this case.  */
	output_multi_immediate (operands, "mvn%?\t%0, %1", "bic%?\t%0, %0, %1", 1, ~ n);
      else
	output_multi_immediate (operands, "mov%?\t%0, %1", "orr%?\t%0, %0, %1", 1, n);
    }

  return "";
}

/* Output an ADD r, s, #n where n may be too big for one instruction.
   If adding zero to one register, output nothing.  */

const char *
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

static const char *
output_multi_immediate (operands, instr1, instr2, immed_op, n)
     rtx * operands;
     const char * instr1;
     const char * instr2;
     int immed_op;
     HOST_WIDE_INT n;
{
#if HOST_BITS_PER_WIDE_INT > 32
  n &= 0xffffffff;
#endif

  if (n == 0)
    {
      /* Quick and easy output.  */
      operands[immed_op] = const0_rtx;
      output_asm_insn (instr1, operands);
    }
  else
    {
      int i;
      const char * instr = instr1;

      /* Note that n is never zero here (which would give no output).  */
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

const char *
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
   shift.  */

static const char *
shift_op (op, amountp)
     rtx op;
     HOST_WIDE_INT *amountp;
{
  const char * mnem;
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
	/* Rotate is just modulo 32.  */
	*amountp &= 31;
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

/* Obtain the shift from the POWER of two.  */

static HOST_WIDE_INT
int_log2 (power)
     HOST_WIDE_INT power;
{
  HOST_WIDE_INT shift = 0;

  while ((((HOST_WIDE_INT) 1 << shift) & power) == 0)
    {
      if (shift > 31)
	abort ();
      shift ++;
    }

  return shift;
}

/* Output a .ascii pseudo-op, keeping track of lengths.  This is because
   /bin/as is horribly restrictive.  */
#define MAX_ASCII_LEN 51

void
output_ascii_pseudo_op (stream, p, len)
     FILE * stream;
     const unsigned char * p;
     int len;
{
  int i;
  int len_so_far = 0;

  fputs ("\t.ascii\t\"", stream);
  
  for (i = 0; i < len; i++)
    {
      int c = p[i];

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
	  len_so_far++;
	  /* drop through.  */

	default:
	  if (c >= ' ' && c <= '~')
	    {
	      putc (c, stream);
	      len_so_far++;
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

/* Compute the register sabe mask for registers 0 through 12
   inclusive.  This code is used by both arm_compute_save_reg_mask
   and arm_compute_initial_elimination_offset.  */

static unsigned long
arm_compute_save_reg0_reg12_mask ()
{
  unsigned long func_type = arm_current_func_type ();
  unsigned int save_reg_mask = 0;
  unsigned int reg;

  if (IS_INTERRUPT (func_type))
    {
      unsigned int max_reg;
      /* Interrupt functions must not corrupt any registers,
	 even call clobbered ones.  If this is a leaf function
	 we can just examine the registers used by the RTL, but
	 otherwise we have to assume that whatever function is
	 called might clobber anything, and so we have to save
	 all the call-clobbered registers as well.  */
      if (ARM_FUNC_TYPE (func_type) == ARM_FT_FIQ)
	/* FIQ handlers have registers r8 - r12 banked, so
	   we only need to check r0 - r7, Normal ISRs only
	   bank r14 and r15, so we must check up to r12.
	   r13 is the stack pointer which is always preserved,
	   so we do not need to consider it here.  */
	max_reg = 7;
      else
	max_reg = 12;
	
      for (reg = 0; reg <= max_reg; reg++)
	if (regs_ever_live[reg]
	    || (! current_function_is_leaf && call_used_regs [reg]))
	  save_reg_mask |= (1 << reg);
    }
  else
    {
      /* In the normal case we only need to save those registers
	 which are call saved and which are used by this function.  */
      for (reg = 0; reg <= 10; reg++)
	if (regs_ever_live[reg] && ! call_used_regs [reg])
	  save_reg_mask |= (1 << reg);

      /* Handle the frame pointer as a special case.  */
      if (! TARGET_APCS_FRAME
	  && ! frame_pointer_needed
	  && regs_ever_live[HARD_FRAME_POINTER_REGNUM]
	  && ! call_used_regs[HARD_FRAME_POINTER_REGNUM])
	save_reg_mask |= 1 << HARD_FRAME_POINTER_REGNUM;

      /* If we aren't loading the PIC register,
	 don't stack it even though it may be live.  */
      if (flag_pic
	  && ! TARGET_SINGLE_PIC_BASE 
	  && regs_ever_live[PIC_OFFSET_TABLE_REGNUM])
	save_reg_mask |= 1 << PIC_OFFSET_TABLE_REGNUM;
    }

  return save_reg_mask;
}

/* Compute a bit mask of which registers need to be
   saved on the stack for the current function.  */

static unsigned long
arm_compute_save_reg_mask ()
{
  unsigned int save_reg_mask = 0;
  unsigned long func_type = arm_current_func_type ();

  if (IS_NAKED (func_type))
    /* This should never really happen.  */
    return 0;

  /* If we are creating a stack frame, then we must save the frame pointer,
     IP (which will hold the old stack pointer), LR and the PC.  */
  if (frame_pointer_needed)
    save_reg_mask |=
      (1 << ARM_HARD_FRAME_POINTER_REGNUM)
      | (1 << IP_REGNUM)
      | (1 << LR_REGNUM)
      | (1 << PC_REGNUM);

  /* Volatile functions do not return, so there
     is no need to save any other registers.  */
  if (IS_VOLATILE (func_type))
    return save_reg_mask;

  save_reg_mask |= arm_compute_save_reg0_reg12_mask ();

  /* Decide if we need to save the link register.
     Interrupt routines have their own banked link register,
     so they never need to save it.
     Otherwise if we do not use the link register we do not need to save
     it.  If we are pushing other registers onto the stack however, we
     can save an instruction in the epilogue by pushing the link register
     now and then popping it back into the PC.  This incurs extra memory
     accesses though, so we only do it when optimising for size, and only
     if we know that we will not need a fancy return sequence.  */
  if (regs_ever_live [LR_REGNUM]
	  || (save_reg_mask
	      && optimize_size
	      && ARM_FUNC_TYPE (func_type) == ARM_FT_NORMAL))
    save_reg_mask |= 1 << LR_REGNUM;

  if (cfun->machine->lr_save_eliminated)
    save_reg_mask &= ~ (1 << LR_REGNUM);

  return save_reg_mask;
}

/* Generate a function exit sequence.  If REALLY_RETURN is true, then do
   everything bar the final return instruction.  */

const char *
output_return_instruction (operand, really_return, reverse)
     rtx operand;
     int really_return;
     int reverse;
{
  char conditional[10];
  char instr[100];
  int reg;
  unsigned long live_regs_mask;
  unsigned long func_type;
  
  func_type = arm_current_func_type ();

  if (IS_NAKED (func_type))
    return "";

  if (IS_VOLATILE (func_type) && TARGET_ABORT_NORETURN)
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

  if (current_function_calls_alloca && !really_return)
    abort ();

  /* Construct the conditional part of the instruction(s) to be emitted.  */
  sprintf (conditional, "%%?%%%c0", reverse ? 'D' : 'd');

  return_used_this_function = 1;

  live_regs_mask = arm_compute_save_reg_mask ();

  if (live_regs_mask)
    {
      const char * return_reg;

      /* If we do not have any special requirements for function exit 
	 (eg interworking, or ISR) then we can load the return address 
	 directly into the PC.  Otherwise we must load it into LR.  */
      if (really_return
	  && ! TARGET_INTERWORK)
	return_reg = reg_names[PC_REGNUM];
      else
	return_reg = reg_names[LR_REGNUM];

      if ((live_regs_mask & (1 << IP_REGNUM)) == (1 << IP_REGNUM))
	/* There are two possible reasons for the IP register being saved.
	   Either a stack frame was created, in which case IP contains the
	   old stack pointer, or an ISR routine corrupted it.  If this in an
	   ISR routine then just restore IP, otherwise restore IP into SP.  */
	if (! IS_INTERRUPT (func_type))
	  {
	    live_regs_mask &= ~ (1 << IP_REGNUM);
	    live_regs_mask |=   (1 << SP_REGNUM);
	  }

      /* On some ARM architectures it is faster to use LDR rather than
	 LDM to load a single register.  On other architectures, the
	 cost is the same.  In 26 bit mode, or for exception handlers,
	 we have to use LDM to load the PC so that the CPSR is also
	 restored.  */
      for (reg = 0; reg <= LAST_ARM_REGNUM; reg++)
	{
	  if (live_regs_mask == (unsigned int)(1 << reg))
	    break;
	}
      if (reg <= LAST_ARM_REGNUM
	  && (reg != LR_REGNUM
	      || ! really_return 
	      || (TARGET_APCS_32 && ! IS_INTERRUPT (func_type))))
	{
	  sprintf (instr, "ldr%s\t%%|%s, [%%|sp], #4", conditional, 
		   (reg == LR_REGNUM) ? return_reg : reg_names[reg]);
	}
      else
	{
	  char *p;
	  int first = 1;

	  /* Generate the load multiple instruction to restore the registers.  */
	  if (frame_pointer_needed)
	    sprintf (instr, "ldm%sea\t%%|fp, {", conditional);
	  else
	    sprintf (instr, "ldm%sfd\t%%|sp!, {", conditional);

	  p = instr + strlen (instr);

	  for (reg = 0; reg <= SP_REGNUM; reg++)
	    if (live_regs_mask & (1 << reg))
	      {
		int l = strlen (reg_names[reg]);

		if (first)
		  first = 0;
		else
		  {
		    memcpy (p, ", ", 2);
		    p += 2;
		  }

		memcpy (p, "%|", 2);
		memcpy (p + 2, reg_names[reg], l);
		p += l + 2;
	      }
	  
	  if (live_regs_mask & (1 << LR_REGNUM))
	    {
	      int l = strlen (return_reg);

	      if (! first)
		{
		  memcpy (p, ", ", 2);
		  p += 2;
		}

	      memcpy (p, "%|", 2);
	      memcpy (p + 2, return_reg, l);
	      strcpy (p + 2 + l, ((TARGET_APCS_32 
				   && !IS_INTERRUPT (func_type)) 
				  || !really_return) 
		      ? "}" : "}^");
	    }
	  else
	    strcpy (p, "}");
	}

      output_asm_insn (instr, & operand);

      /* See if we need to generate an extra instruction to
	 perform the actual function return.  */
      if (really_return
	  && func_type != ARM_FT_INTERWORKED
	  && (live_regs_mask & (1 << LR_REGNUM)) != 0)
	{
	  /* The return has already been handled
	     by loading the LR into the PC.  */
	  really_return = 0;
	}
    }
  
  if (really_return)
    {
      switch ((int) ARM_FUNC_TYPE (func_type))
	{
	case ARM_FT_ISR:
	case ARM_FT_FIQ:
	  sprintf (instr, "sub%ss\t%%|pc, %%|lr, #4", conditional);
	  break;

	case ARM_FT_INTERWORKED:
	  sprintf (instr, "bx%s\t%%|lr", conditional);
	  break;

	case ARM_FT_EXCEPTION:
	  sprintf (instr, "mov%ss\t%%|pc, %%|lr", conditional);
	  break;

	default:
	  /* ARMv5 implementations always provide BX, so interworking
	     is the default unless APCS-26 is in use.  */
	  if ((insn_flags & FL_ARCH5) != 0 && TARGET_APCS_32)
	    sprintf (instr, "bx%s\t%%|lr", conditional);	    
	  else
	    sprintf (instr, "mov%s%s\t%%|pc, %%|lr",
		     conditional, TARGET_APCS_32 ? "" : "s");
	  break;
	}

      output_asm_insn (instr, & operand);
    }

  return "";
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
   const char * name;
{
  unsigned long alignlength;
  unsigned long length;
  rtx           x;

  length      = strlen (name) + 1;
  alignlength = ROUND_UP (length);
  
  ASM_OUTPUT_ASCII (stream, name, length);
  ASM_OUTPUT_ALIGN (stream, 2);
  x = GEN_INT ((unsigned HOST_WIDE_INT) 0xff000000 + alignlength);
  assemble_aligned_integer (UNITS_PER_WORD, x);
}

/* Place some comments into the assembler stream
   describing the current function.  */

static void
arm_output_function_prologue (f, frame_size)
     FILE * f;
     HOST_WIDE_INT frame_size;
{
  unsigned long func_type;

  if (!TARGET_ARM)
    {
      thumb_output_function_prologue (f, frame_size);
      return;
    }
  
  /* Sanity check.  */
  if (arm_ccfsm_state || arm_target_insn)
    abort ();

  func_type = arm_current_func_type ();
  
  switch ((int) ARM_FUNC_TYPE (func_type))
    {
    default:
    case ARM_FT_NORMAL:
      break;
    case ARM_FT_INTERWORKED:
      asm_fprintf (f, "\t%@ Function supports interworking.\n");
      break;
    case ARM_FT_EXCEPTION_HANDLER:
      asm_fprintf (f, "\t%@ C++ Exception Handler.\n");
      break;
    case ARM_FT_ISR:
      asm_fprintf (f, "\t%@ Interrupt Service Routine.\n");
      break;
    case ARM_FT_FIQ:
      asm_fprintf (f, "\t%@ Fast Interrupt Service Routine.\n");
      break;
    case ARM_FT_EXCEPTION:
      asm_fprintf (f, "\t%@ ARM Exception Handler.\n");
      break;
    }
  
  if (IS_NAKED (func_type))
    asm_fprintf (f, "\t%@ Naked Function: prologue and epilogue provided by programmer.\n");

  if (IS_VOLATILE (func_type))
    asm_fprintf (f, "\t%@ Volatile: function does not return.\n");

  if (IS_NESTED (func_type))
    asm_fprintf (f, "\t%@ Nested: function declared inside another function.\n");
    
  asm_fprintf (f, "\t%@ args = %d, pretend = %d, frame = %d\n",
	       current_function_args_size,
	       current_function_pretend_args_size, frame_size);

  asm_fprintf (f, "\t%@ frame_needed = %d, uses_anonymous_args = %d\n",
	       frame_pointer_needed,
	       cfun->machine->uses_anonymous_args);

  if (cfun->machine->lr_save_eliminated)
    asm_fprintf (f, "\t%@ link register save eliminated.\n");

#ifdef AOF_ASSEMBLER
  if (flag_pic)
    asm_fprintf (f, "\tmov\t%r, %r\n", IP_REGNUM, PIC_OFFSET_TABLE_REGNUM);
#endif

  return_used_this_function = 0;  
}

const char *
arm_output_epilogue (really_return)
     int really_return;
{
  int reg;
  unsigned long saved_regs_mask;
  unsigned long func_type;
  /* Floats_offset is the offset from the "virtual" frame.  In an APCS 
     frame that is $fp + 4 for a non-variadic function.  */
  int floats_offset = 0;
  rtx operands[3];
  int frame_size = get_frame_size ();
  FILE * f = asm_out_file;
  rtx eh_ofs = cfun->machine->eh_epilogue_sp_ofs;

  /* If we have already generated the return instruction
     then it is futile to generate anything else.  */
  if (use_return_insn (FALSE) && return_used_this_function)
    return "";

  func_type = arm_current_func_type ();

  if (IS_NAKED (func_type))
    /* Naked functions don't have epilogues.  */
    return "";

  if (IS_VOLATILE (func_type) && TARGET_ABORT_NORETURN)
    {
      rtx op;
	  
      /* A volatile function should never return.  Call abort.  */
      op = gen_rtx_SYMBOL_REF (Pmode, NEED_PLT_RELOC ? "abort(PLT)" : "abort");
      assemble_external_libcall (op);
      output_asm_insn ("bl\t%a0", &op);
      
      return "";
    }

  if (ARM_FUNC_TYPE (func_type) == ARM_FT_EXCEPTION_HANDLER
      && ! really_return)
    /* If we are throwing an exception, then we really must
       be doing a return,  so we can't tail-call.  */
    abort ();
  
  saved_regs_mask = arm_compute_save_reg_mask ();
  
  /* XXX We should adjust floats_offset for any anonymous args, and then
     re-adjust vfp_offset below to compensate.  */

  /* Compute how far away the floats will be.  */
  for (reg = 0; reg <= LAST_ARM_REGNUM; reg ++)
    if (saved_regs_mask & (1 << reg))
      floats_offset += 4;
  
  if (frame_pointer_needed)
    {
      int vfp_offset = 4;

      if (arm_fpu_arch == FP_SOFT2)
	{
	  for (reg = LAST_ARM_FP_REGNUM; reg >= FIRST_ARM_FP_REGNUM; reg--)
	    if (regs_ever_live[reg] && !call_used_regs[reg])
	      {
		floats_offset += 12;
		asm_fprintf (f, "\tldfe\t%r, [%r, #-%d]\n", 
			     reg, FP_REGNUM, floats_offset - vfp_offset);
	      }
	}
      else
	{
	  int start_reg = LAST_ARM_FP_REGNUM;

	  for (reg = LAST_ARM_FP_REGNUM; reg >= FIRST_ARM_FP_REGNUM; reg--)
	    {
	      if (regs_ever_live[reg] && !call_used_regs[reg])
		{
		  floats_offset += 12;
		  
		  /* We can't unstack more than four registers at once.  */
		  if (start_reg - reg == 3)
		    {
		      asm_fprintf (f, "\tlfm\t%r, 4, [%r, #-%d]\n",
			           reg, FP_REGNUM, floats_offset - vfp_offset);
		      start_reg = reg - 1;
		    }
		}
	      else
		{
		  if (reg != start_reg)
		    asm_fprintf (f, "\tlfm\t%r, %d, [%r, #-%d]\n",
				 reg + 1, start_reg - reg,
				 FP_REGNUM, floats_offset - vfp_offset);
		  start_reg = reg - 1;
		}
	    }

	  /* Just in case the last register checked also needs unstacking.  */
	  if (reg != start_reg)
	    asm_fprintf (f, "\tlfm\t%r, %d, [%r, #-%d]\n",
			 reg + 1, start_reg - reg,
			 FP_REGNUM, floats_offset - vfp_offset);
	}

      /* saved_regs_mask should contain the IP, which at the time of stack
	 frame generation actually contains the old stack pointer.  So a
	 quick way to unwind the stack is just pop the IP register directly
	 into the stack pointer.  */
      if ((saved_regs_mask & (1 << IP_REGNUM)) == 0)
	abort ();
      saved_regs_mask &= ~ (1 << IP_REGNUM);
      saved_regs_mask |=   (1 << SP_REGNUM);

      /* There are two registers left in saved_regs_mask - LR and PC.  We
	 only need to restore the LR register (the return address), but to
	 save time we can load it directly into the PC, unless we need a
	 special function exit sequence, or we are not really returning.  */
      if (really_return && ARM_FUNC_TYPE (func_type) == ARM_FT_NORMAL)
	/* Delete the LR from the register mask, so that the LR on
	   the stack is loaded into the PC in the register mask.  */
	saved_regs_mask &= ~ (1 << LR_REGNUM);
      else
	saved_regs_mask &= ~ (1 << PC_REGNUM);
      
      print_multi_reg (f, "ldmea\t%r", FP_REGNUM, saved_regs_mask);

      if (IS_INTERRUPT (func_type))
	/* Interrupt handlers will have pushed the
	   IP onto the stack, so restore it now.  */
	print_multi_reg (f, "ldmfd\t%r", SP_REGNUM, 1 << IP_REGNUM);
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
	  for (reg = FIRST_ARM_FP_REGNUM; reg <= LAST_ARM_FP_REGNUM; reg++)
	    if (regs_ever_live[reg] && !call_used_regs[reg])
	      asm_fprintf (f, "\tldfe\t%r, [%r], #12\n",
			   reg, SP_REGNUM);
	}
      else
	{
	  int start_reg = FIRST_ARM_FP_REGNUM;

	  for (reg = FIRST_ARM_FP_REGNUM; reg <= LAST_ARM_FP_REGNUM; reg++)
	    {
	      if (regs_ever_live[reg] && !call_used_regs[reg])
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

      /* If we can, restore the LR into the PC.  */
      if (ARM_FUNC_TYPE (func_type) == ARM_FT_NORMAL
	  && really_return
	  && current_function_pretend_args_size == 0
	  && saved_regs_mask & (1 << LR_REGNUM))
	{
	  saved_regs_mask &= ~ (1 << LR_REGNUM);
	  saved_regs_mask |=   (1 << PC_REGNUM);
	}

      /* Load the registers off the stack.  If we only have one register
	 to load use the LDR instruction - it is faster.  */
      if (saved_regs_mask == (1 << LR_REGNUM))
	{
	  /* The exception handler ignores the LR, so we do
	     not really need to load it off the stack.  */
	  if (eh_ofs)
	    asm_fprintf (f, "\tadd\t%r, %r, #4\n", SP_REGNUM, SP_REGNUM);
	  else
	    asm_fprintf (f, "\tldr\t%r, [%r], #4\n", LR_REGNUM, SP_REGNUM);
	}
      else if (saved_regs_mask)
	print_multi_reg (f, "ldmfd\t%r!", SP_REGNUM, saved_regs_mask);

      if (current_function_pretend_args_size)
	{
	  /* Unwind the pre-pushed regs.  */
	  operands[0] = operands[1] = stack_pointer_rtx;
	  operands[2] = GEN_INT (current_function_pretend_args_size);
	  output_add_immediate (operands);
	}
    }

#if 0
  if (ARM_FUNC_TYPE (func_type) == ARM_FT_EXCEPTION_HANDLER)
    /* Adjust the stack to remove the exception handler stuff.  */
    asm_fprintf (f, "\tadd\t%r, %r, %r\n", SP_REGNUM, SP_REGNUM,
		 REGNO (eh_ofs));
#endif

  if (! really_return
    || (ARM_FUNC_TYPE (func_type) == ARM_FT_NORMAL
	&& current_function_pretend_args_size == 0
	&& saved_regs_mask & (1 << PC_REGNUM)))
    return "";

  /* Generate the return instruction.  */
  switch ((int) ARM_FUNC_TYPE (func_type))
    {
    case ARM_FT_EXCEPTION_HANDLER:
      /* Even in 26-bit mode we do a mov (rather than a movs)
	 because we don't have the PSR bits set in the address.  */
      asm_fprintf (f, "\tmov\t%r, %r\n", PC_REGNUM, EXCEPTION_LR_REGNUM);
      break;

    case ARM_FT_ISR:
    case ARM_FT_FIQ:
      asm_fprintf (f, "\tsubs\t%r, %r, #4\n", PC_REGNUM, LR_REGNUM);
      break;

    case ARM_FT_EXCEPTION:
      asm_fprintf (f, "\tmovs\t%r, %r\n", PC_REGNUM, LR_REGNUM);
      break;

    case ARM_FT_INTERWORKED:
      asm_fprintf (f, "\tbx\t%r\n", LR_REGNUM);
      break;

    default:
      if (frame_pointer_needed)
	/* If we used the frame pointer then the return adddress
	   will have been loaded off the stack directly into the
	   PC, so there is no need to issue a MOV instruction
	   here.  */
	;
      else if (current_function_pretend_args_size == 0
	       && (saved_regs_mask & (1 << LR_REGNUM)))
	/* Similarly we may have been able to load LR into the PC
	   even if we did not create a stack frame.  */
	;
      else if (TARGET_APCS_32)
	asm_fprintf (f, "\tmov\t%r, %r\n", PC_REGNUM, LR_REGNUM);
      else
	asm_fprintf (f, "\tmovs\t%r, %r\n", PC_REGNUM, LR_REGNUM);
      break;
    }

  return "";
}

static void
arm_output_function_epilogue (file, frame_size)
     FILE *file ATTRIBUTE_UNUSED;
     HOST_WIDE_INT frame_size;
{
  if (TARGET_THUMB)
    {
      /* ??? Probably not safe to set this here, since it assumes that a
	 function will be emitted as assembly immediately after we generate
	 RTL for it.  This does not happen for inline functions.  */
      return_used_this_function = 0;
    }
  else
    {
      if (use_return_insn (FALSE)
	  && return_used_this_function
	  && (frame_size + current_function_outgoing_args_size) != 0
	  && !frame_pointer_needed)
	abort ();

      /* Reset the ARM-specific per-function variables.  */
      after_arm_reorg = 0;
    }
}

/* Generate and emit an insn that we will recognize as a push_multi.
   Unfortunately, since this insn does not reflect very well the actual
   semantics of the operation, we need to annotate the insn for the benefit
   of DWARF2 frame unwind information.  */

static rtx
emit_multi_reg_push (mask)
     int mask;
{
  int num_regs = 0;
  int num_dwarf_regs;
  int i, j;
  rtx par;
  rtx dwarf;
  int dwarf_par_index;
  rtx tmp, reg;

  for (i = 0; i <= LAST_ARM_REGNUM; i++)
    if (mask & (1 << i))
      num_regs++;

  if (num_regs == 0 || num_regs > 16)
    abort ();

  /* We don't record the PC in the dwarf frame information.  */
  num_dwarf_regs = num_regs;
  if (mask & (1 << PC_REGNUM))
    num_dwarf_regs--;

  /* For the body of the insn we are going to generate an UNSPEC in
     parallel with several USEs.  This allows the insn to be recognised
     by the push_multi pattern in the arm.md file.  The insn looks
     something like this:

       (parallel [ 
           (set (mem:BLK (pre_dec:BLK (reg:SI sp)))
	        (unspec:BLK [(reg:SI r4)] UNSPEC_PUSH_MULT))
           (use (reg:SI 11 fp))
           (use (reg:SI 12 ip))
           (use (reg:SI 14 lr))
           (use (reg:SI 15 pc))
        ])

     For the frame note however, we try to be more explicit and actually
     show each register being stored into the stack frame, plus a (single)
     decrement of the stack pointer.  We do it this way in order to be
     friendly to the stack unwinding code, which only wants to see a single
     stack decrement per instruction.  The RTL we generate for the note looks
     something like this:

      (sequence [ 
           (set (reg:SI sp) (plus:SI (reg:SI sp) (const_int -20)))
           (set (mem:SI (reg:SI sp)) (reg:SI r4))
           (set (mem:SI (plus:SI (reg:SI sp) (const_int 4))) (reg:SI fp))
           (set (mem:SI (plus:SI (reg:SI sp) (const_int 8))) (reg:SI ip))
           (set (mem:SI (plus:SI (reg:SI sp) (const_int 12))) (reg:SI lr))
        ])

      This sequence is used both by the code to support stack unwinding for
      exceptions handlers and the code to generate dwarf2 frame debugging.  */
  
  par = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (num_regs));
  dwarf = gen_rtx_SEQUENCE (VOIDmode, rtvec_alloc (num_dwarf_regs + 1));
  RTX_FRAME_RELATED_P (dwarf) = 1;
  dwarf_par_index = 1;

  for (i = 0; i <= LAST_ARM_REGNUM; i++)
    {
      if (mask & (1 << i))
	{
	  reg = gen_rtx_REG (SImode, i);

	  XVECEXP (par, 0, 0)
	    = gen_rtx_SET (VOIDmode,
			   gen_rtx_MEM (BLKmode,
					gen_rtx_PRE_DEC (BLKmode,
							 stack_pointer_rtx)),
			   gen_rtx_UNSPEC (BLKmode,
					   gen_rtvec (1, reg),
					   UNSPEC_PUSH_MULT));

	  if (i != PC_REGNUM)
	    {
	      tmp = gen_rtx_SET (VOIDmode,
				 gen_rtx_MEM (SImode, stack_pointer_rtx),
				 reg);
	      RTX_FRAME_RELATED_P (tmp) = 1;
	      XVECEXP (dwarf, 0, dwarf_par_index) = tmp;
	      dwarf_par_index++;
	    }

	  break;
	}
    }

  for (j = 1, i++; j < num_regs; i++)
    {
      if (mask & (1 << i))
	{
	  reg = gen_rtx_REG (SImode, i);

	  XVECEXP (par, 0, j) = gen_rtx_USE (VOIDmode, reg);

	  if (i != PC_REGNUM)
	    {
	      tmp = gen_rtx_SET (VOIDmode,
				 gen_rtx_MEM (SImode,
					      plus_constant (stack_pointer_rtx,
							     4 * j)),
				 reg);
	      RTX_FRAME_RELATED_P (tmp) = 1;
	      XVECEXP (dwarf, 0, dwarf_par_index++) = tmp;
	    }

	  j++;
	}
    }

  par = emit_insn (par);
  
  tmp = gen_rtx_SET (SImode,
		     stack_pointer_rtx,
		     gen_rtx_PLUS (SImode,
				   stack_pointer_rtx,
				   GEN_INT (-4 * num_regs)));
  RTX_FRAME_RELATED_P (tmp) = 1;
  XVECEXP (dwarf, 0, 0) = tmp;
  
  REG_NOTES (par) = gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR, dwarf,
				       REG_NOTES (par));
  return par;
}

static rtx
emit_sfm (base_reg, count)
     int base_reg;
     int count;
{
  rtx par;
  rtx dwarf;
  rtx tmp, reg;
  int i;

  par = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (count));
  dwarf = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (count));
  RTX_FRAME_RELATED_P (dwarf) = 1;

  reg = gen_rtx_REG (XFmode, base_reg++);

  XVECEXP (par, 0, 0)
    = gen_rtx_SET (VOIDmode, 
		   gen_rtx_MEM (BLKmode,
				gen_rtx_PRE_DEC (BLKmode, stack_pointer_rtx)),
		   gen_rtx_UNSPEC (BLKmode,
				   gen_rtvec (1, reg),
				   UNSPEC_PUSH_MULT));
  tmp
    = gen_rtx_SET (VOIDmode, 
		   gen_rtx_MEM (XFmode,
				gen_rtx_PRE_DEC (BLKmode, stack_pointer_rtx)),
		   reg);
  RTX_FRAME_RELATED_P (tmp) = 1;
  XVECEXP (dwarf, 0, count - 1) = tmp;	  
  
  for (i = 1; i < count; i++)
    {
      reg = gen_rtx_REG (XFmode, base_reg++);
      XVECEXP (par, 0, i) = gen_rtx_USE (VOIDmode, reg);

      tmp = gen_rtx_SET (VOIDmode, 
			 gen_rtx_MEM (XFmode,
				      gen_rtx_PRE_DEC (BLKmode,
						       stack_pointer_rtx)),
			 reg);
      RTX_FRAME_RELATED_P (tmp) = 1;
      XVECEXP (dwarf, 0, count - i - 1) = tmp;	  
    }

  par = emit_insn (par);
  REG_NOTES (par) = gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR, dwarf,
				       REG_NOTES (par));
  return par;
}

/* Compute the distance from register FROM to register TO.
   These can be the arg pointer (26), the soft frame pointer (25),
   the stack pointer (13) or the hard frame pointer (11).
   Typical stack layout looks like this:

       old stack pointer -> |    |
                             ----
                            |    | \
                            |    |   saved arguments for
                            |    |   vararg functions
			    |    | /
                              --
   hard FP & arg pointer -> |    | \
                            |    |   stack
                            |    |   frame
                            |    | /
                              --
                            |    | \
                            |    |   call saved
                            |    |   registers
      soft frame pointer -> |    | /
                              --
                            |    | \
                            |    |   local
                            |    |   variables
                            |    | /
                              --
                            |    | \
                            |    |   outgoing
                            |    |   arguments
   current stack pointer -> |    | /
                              --

  For a given funciton some or all of these stack compomnents
  may not be needed, giving rise to the possibility of
  eliminating some of the registers.

  The values returned by this function must reflect the behaviour
  of arm_expand_prologue() and arm_compute_save_reg_mask().

  The sign of the number returned reflects the direction of stack
  growth, so the values are positive for all eliminations except
  from the soft frame pointer to the hard frame pointer.  */
			    
unsigned int
arm_compute_initial_elimination_offset (from, to)
     unsigned int from;
     unsigned int to;
{
  unsigned int local_vars    = (get_frame_size () + 3) & ~3;
  unsigned int outgoing_args = current_function_outgoing_args_size;
  unsigned int stack_frame;
  unsigned int call_saved_registers;
  unsigned long func_type;
  
  func_type = arm_current_func_type ();

  /* Volatile functions never return, so there is
     no need to save call saved registers.  */
  call_saved_registers = 0;
  if (! IS_VOLATILE (func_type))
    {
      unsigned int reg_mask;
      unsigned int reg;

      /* Make sure that we compute which registers will be saved
	 on the stack using the same algorithm that is used by
	 arm_compute_save_reg_mask().  */
      reg_mask = arm_compute_save_reg0_reg12_mask ();

      /* Now count the number of bits set in save_reg_mask.
	 For each set bit we need 4 bytes of stack space.  */
      while (reg_mask)
	{
	  call_saved_registers += 4;
	  reg_mask = reg_mask & ~ (reg_mask & - reg_mask);
	}

      if (regs_ever_live[LR_REGNUM]
	  /* If a stack frame is going to be created, the LR will
	     be saved as part of that, so we do not need to allow
	     for it here.  */
	  && ! frame_pointer_needed)
	call_saved_registers += 4;

      /* If the hard floating point registers are going to be
	 used then they must be saved on the stack as well.
         Each register occupies 12 bytes of stack space.  */
      for (reg = FIRST_ARM_FP_REGNUM; reg <= LAST_ARM_FP_REGNUM; reg ++)
	if (regs_ever_live[reg] && ! call_used_regs[reg])
	  call_saved_registers += 12;
    }

  /* The stack frame contains 4 registers - the old frame pointer,
     the old stack pointer, the return address and PC of the start
     of the function.  */
  stack_frame = frame_pointer_needed ? 16 : 0;

  /* OK, now we have enough information to compute the distances.
     There must be an entry in these switch tables for each pair
     of registers in ELIMINABLE_REGS, even if some of the entries
     seem to be redundant or useless.  */
  switch (from)
    {
    case ARG_POINTER_REGNUM:
      switch (to)
	{
	case THUMB_HARD_FRAME_POINTER_REGNUM:
	  return 0;

	case FRAME_POINTER_REGNUM:
	  /* This is the reverse of the soft frame pointer
	     to hard frame pointer elimination below.  */
	  if (call_saved_registers == 0 && stack_frame == 0)
	    return 0;
	  return (call_saved_registers + stack_frame - 4);

	case ARM_HARD_FRAME_POINTER_REGNUM:
	  /* If there is no stack frame then the hard
	     frame pointer and the arg pointer coincide.  */
	  if (stack_frame == 0 && call_saved_registers != 0)
	    return 0;
	  /* FIXME:  Not sure about this.  Maybe we should always return 0 ?  */
	  return (frame_pointer_needed
		  && current_function_needs_context
		  && ! cfun->machine->uses_anonymous_args) ? 4 : 0;

	case STACK_POINTER_REGNUM:
	  /* If nothing has been pushed on the stack at all
	     then this will return -4.  This *is* correct!  */
	  return call_saved_registers + stack_frame + local_vars + outgoing_args - 4;

	default:
	  abort ();
	}
      break;

    case FRAME_POINTER_REGNUM:
      switch (to)
	{
	case THUMB_HARD_FRAME_POINTER_REGNUM:
	  return 0;

	case ARM_HARD_FRAME_POINTER_REGNUM:
	  /* The hard frame pointer points to the top entry in the
	     stack frame.  The soft frame pointer to the bottom entry
	     in the stack frame.  If there is no stack frame at all,
	     then they are identical.  */
	  if (call_saved_registers == 0 && stack_frame == 0)
	    return 0;
	  return - (call_saved_registers + stack_frame - 4);

	case STACK_POINTER_REGNUM:
	  return local_vars + outgoing_args;

	default:
	  abort ();
	}
      break;

    default:
      /* You cannot eliminate from the stack pointer.
	 In theory you could eliminate from the hard frame
	 pointer to the stack pointer, but this will never
	 happen, since if a stack frame is not needed the
	 hard frame pointer will never be used.  */
      abort ();
    }
}

/* Generate the prologue instructions for entry into an ARM function.  */

void
arm_expand_prologue ()
{
  int reg;
  rtx amount;
  rtx insn;
  rtx ip_rtx;
  unsigned long live_regs_mask;
  unsigned long func_type;
  int fp_offset = 0;
  int saved_pretend_args = 0;
  unsigned int args_to_push;

  func_type = arm_current_func_type ();

  /* Naked functions don't have prologues.  */
  if (IS_NAKED (func_type))
    return;

  /* Make a copy of c_f_p_a_s as we may need to modify it locally.  */
  args_to_push = current_function_pretend_args_size;
  
  /* Compute which register we will have to save onto the stack.  */
  live_regs_mask = arm_compute_save_reg_mask ();

  ip_rtx = gen_rtx_REG (SImode, IP_REGNUM);

  if (frame_pointer_needed)
    {
      if (IS_INTERRUPT (func_type))
	{
	  /* Interrupt functions must not corrupt any registers.
	     Creating a frame pointer however, corrupts the IP
	     register, so we must push it first.  */
	  insn = emit_multi_reg_push (1 << IP_REGNUM);

	  /* Do not set RTX_FRAME_RELATED_P on this insn.
	     The dwarf stack unwinding code only wants to see one
	     stack decrement per function, and this is not it.  If
	     this instruction is labeled as being part of the frame
	     creation sequence then dwarf2out_frame_debug_expr will
	     abort when it encounters the assignment of IP to FP
	     later on, since the use of SP here establishes SP as
	     the CFA register and not IP.

	     Anyway this instruction is not really part of the stack
	     frame creation although it is part of the prologue.  */
	}
      else if (IS_NESTED (func_type))
	{
	  /* The Static chain register is the same as the IP register
	     used as a scratch register during stack frame creation.
	     To get around this need to find somewhere to store IP
	     whilst the frame is being created.  We try the following
	     places in order:
	     
	       1. The last argument register.
	       2. A slot on the stack above the frame.  (This only
	          works if the function is not a varargs function).
	       3. Register r3, after pushing the argument registers
	          onto the stack.

	     Note - we only need to tell the dwarf2 backend about the SP
	     adjustment in the second variant; the static chain register
	     doesn't need to be unwound, as it doesn't contain a value
	     inherited from the caller.  */

	  if (regs_ever_live[3] == 0)
	    {
	      insn = gen_rtx_REG (SImode, 3);
	      insn = gen_rtx_SET (SImode, insn, ip_rtx);
	      insn = emit_insn (insn);
	    }
	  else if (args_to_push == 0)
	    {
	      rtx dwarf;
	      insn = gen_rtx_PRE_DEC (SImode, stack_pointer_rtx);
	      insn = gen_rtx_MEM (SImode, insn);
	      insn = gen_rtx_SET (VOIDmode, insn, ip_rtx);
	      insn = emit_insn (insn);

	      fp_offset = 4;

	      /* Just tell the dwarf backend that we adjusted SP.  */
	      dwarf = gen_rtx_SET (VOIDmode, stack_pointer_rtx,
				   gen_rtx_PLUS (SImode, stack_pointer_rtx,
						 GEN_INT (-fp_offset)));
	      RTX_FRAME_RELATED_P (insn) = 1;
	      REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR,
						    dwarf, REG_NOTES (insn));
	    }
	  else
	    {
	      /* Store the args on the stack.  */
	      if (cfun->machine->uses_anonymous_args)
		insn = emit_multi_reg_push
		  ((0xf0 >> (args_to_push / 4)) & 0xf);
	      else
		insn = emit_insn
		  (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx, 
			       GEN_INT (- args_to_push)));

	      RTX_FRAME_RELATED_P (insn) = 1;

	      saved_pretend_args = 1;
	      fp_offset = args_to_push;
	      args_to_push = 0;

	      /* Now reuse r3 to preserve IP.  */
	      insn = gen_rtx_REG (SImode, 3);
	      insn = gen_rtx_SET (SImode, insn, ip_rtx);
	      (void) emit_insn (insn);
	    }
	}

      if (fp_offset)
	{
	  insn = gen_rtx_PLUS (SImode, stack_pointer_rtx, GEN_INT (fp_offset));
	  insn = gen_rtx_SET  (SImode, ip_rtx, insn);
	}
      else
	insn = gen_movsi (ip_rtx, stack_pointer_rtx);
      
      insn = emit_insn (insn);
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  if (args_to_push)
    {
      /* Push the argument registers, or reserve space for them.  */
      if (cfun->machine->uses_anonymous_args)
	insn = emit_multi_reg_push
	  ((0xf0 >> (args_to_push / 4)) & 0xf);
      else
	insn = emit_insn
	  (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx, 
		       GEN_INT (- args_to_push)));
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  /* If this is an interrupt service routine, and the link register is
     going to be pushed, subtracting four now will mean that the
     function return can be done with a single instruction.  */
  if ((func_type == ARM_FT_ISR || func_type == ARM_FT_FIQ)
      && (live_regs_mask & (1 << LR_REGNUM)) != 0)
    {
      emit_insn (gen_rtx_SET (SImode, 
			      gen_rtx_REG (SImode, LR_REGNUM),
			      gen_rtx_PLUS (SImode,
				    gen_rtx_REG (SImode, LR_REGNUM),
				    GEN_INT (-4))));
    }

  if (live_regs_mask)
    {
      insn = emit_multi_reg_push (live_regs_mask);
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  if (! IS_VOLATILE (func_type))
    {
      /* Save any floating point call-saved registers used by this function.  */
      if (arm_fpu_arch == FP_SOFT2)
	{
	  for (reg = LAST_ARM_FP_REGNUM; reg >= FIRST_ARM_FP_REGNUM; reg --)
	    if (regs_ever_live[reg] && !call_used_regs[reg])
	      {
		insn = gen_rtx_PRE_DEC (XFmode, stack_pointer_rtx);
		insn = gen_rtx_MEM (XFmode, insn);
		insn = emit_insn (gen_rtx_SET (VOIDmode, insn,
					       gen_rtx_REG (XFmode, reg)));
		RTX_FRAME_RELATED_P (insn) = 1;
	      }
	}
      else
	{
	  int start_reg = LAST_ARM_FP_REGNUM;

	  for (reg = LAST_ARM_FP_REGNUM; reg >= FIRST_ARM_FP_REGNUM; reg --)
	    {
	      if (regs_ever_live[reg] && !call_used_regs[reg])
		{
		  if (start_reg - reg == 3)
		    {
		      insn = emit_sfm (reg, 4);
		      RTX_FRAME_RELATED_P (insn) = 1;
		      start_reg = reg - 1;
		    }
		}
	      else
		{
		  if (start_reg != reg)
		    {
		      insn = emit_sfm (reg + 1, start_reg - reg);
		      RTX_FRAME_RELATED_P (insn) = 1;
		    }
		  start_reg = reg - 1;
		}
	    }

	  if (start_reg != reg)
	    {
	      insn = emit_sfm (reg + 1, start_reg - reg);
	      RTX_FRAME_RELATED_P (insn) = 1;
	    }
	}
    }

  if (frame_pointer_needed)
    {
      /* Create the new frame pointer.  */
      insn = GEN_INT (-(4 + args_to_push + fp_offset));
      insn = emit_insn (gen_addsi3 (hard_frame_pointer_rtx, ip_rtx, insn));
      RTX_FRAME_RELATED_P (insn) = 1;
      
      if (IS_NESTED (func_type))
	{
	  /* Recover the static chain register.  */
	  if (regs_ever_live [3] == 0
	      || saved_pretend_args)
	    insn = gen_rtx_REG (SImode, 3);
	  else /* if (current_function_pretend_args_size == 0) */
	    {
	      insn = gen_rtx_PLUS (SImode, hard_frame_pointer_rtx, GEN_INT (4));
	      insn = gen_rtx_MEM (SImode, insn);
	    }

	  emit_insn (gen_rtx_SET (SImode, ip_rtx, insn));
	  /* Add a USE to stop propagate_one_insn() from barfing.  */
	  emit_insn (gen_prologue_use (ip_rtx));
	}
    }

  amount = GEN_INT (-(get_frame_size ()
		      + current_function_outgoing_args_size));

  if (amount != const0_rtx)
    {
      /* This add can produce multiple insns for a large constant, so we
	 need to get tricky.  */
      rtx last = get_last_insn ();
      insn = emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx,
				    amount));
      do
	{
	  last = last ? NEXT_INSN (last) : get_insns ();
	  RTX_FRAME_RELATED_P (last) = 1;
	}
      while (last != insn);

      /* If the frame pointer is needed, emit a special barrier that
	 will prevent the scheduler from moving stores to the frame
	 before the stack adjustment.  */
      if (frame_pointer_needed)
	{
	  rtx unspec = gen_rtx_UNSPEC (SImode,
				       gen_rtvec (2, stack_pointer_rtx,
						  hard_frame_pointer_rtx),
				       UNSPEC_PRLG_STK);

	  insn = emit_insn (gen_rtx_CLOBBER (VOIDmode,
				      gen_rtx_MEM (BLKmode, unspec)));
	}
    }

  /* If we are profiling, make sure no instructions are scheduled before
     the call to mcount.  Similarly if the user has requested no
     scheduling in the prolog.  */
  if (current_function_profile || TARGET_NO_SCHED_PRO)
    emit_insn (gen_blockage ());

  /* If the link register is being kept alive, with the return address in it,
     then make sure that it does not get reused by the ce2 pass.  */
  if ((live_regs_mask & (1 << LR_REGNUM)) == 0)
    {
      emit_insn (gen_prologue_use (gen_rtx_REG (SImode, LR_REGNUM)));
      cfun->machine->lr_save_eliminated = 1;
    }
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

    case '_':
      fputs (user_label_prefix, stream);
      return;
	  
    case '|':
      fputs (REGISTER_PREFIX, stream);
      return;

    case '?':
      if (arm_ccfsm_state == 3 || arm_ccfsm_state == 4)
	{
	  if (TARGET_THUMB || current_insn_predicate != NULL)
	    abort ();

	  fputs (arm_condition_codes[arm_current_cc], stream);
	}
      else if (current_insn_predicate)
	{
	  enum arm_cond_code code;

	  if (TARGET_THUMB)
	    abort ();

	  code = get_arm_condition_code (current_insn_predicate);
	  fputs (arm_condition_codes[code], stream);
	}
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
	  val = ARM_SIGN_EXTEND (~INTVAL (x));
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
	const char * shift = shift_op (x, &val);

	if (shift)
	  {
	    fprintf (stream, ", %s ", shift_op (x, &val));
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

      /* An explanation of the 'Q', 'R' and 'H' register operands:
	 
	 In a pair of registers containing a DI or DF value the 'Q'
	 operand returns the register number of the register containing
	 the least signficant part of the value.  The 'R' operand returns
	 the register number of the register containing the most
	 significant part of the value.
	 
	 The 'H' operand returns the higher of the two register numbers.
	 On a run where WORDS_BIG_ENDIAN is true the 'H' operand is the
	 same as the 'Q' operand, since the most signficant part of the
	 value is held in the lower number register.  The reverse is true
	 on systems where WORDS_BIG_ENDIAN is false.
	 
	 The purpose of these operands is to distinguish between cases
	 where the endian-ness of the values is important (for example
	 when they are added together), and cases where the endian-ness
	 is irrelevant, but the order of register operations is important.
	 For example when loading a value from memory into a register
	 pair, the endian-ness does not matter.  Provided that the value
	 from the lower memory address is put into the lower numbered
	 register, and the value from the higher address is put into the
	 higher numbered register, the load will work regardless of whether
	 the value being loaded is big-wordian or little-wordian.  The
	 order of the two register loads can matter however, if the address
	 of the memory location is actually held in one of the registers
	 being overwritten by the load.  */
    case 'Q':
      if (REGNO (x) > LAST_ARM_REGNUM)
	abort ();
      asm_fprintf (stream, "%r", REGNO (x) + (WORDS_BIG_ENDIAN ? 1 : 0));
      return;

    case 'R':
      if (REGNO (x) > LAST_ARM_REGNUM)
	abort ();
      asm_fprintf (stream, "%r", REGNO (x) + (WORDS_BIG_ENDIAN ? 0 : 1));
      return;

    case 'H':
      if (REGNO (x) > LAST_ARM_REGNUM)
	abort ();
      asm_fprintf (stream, "%r", REGNO (x) + 1);
      return;

    case 'm':
      asm_fprintf (stream, "%r", 
		   GET_CODE (XEXP (x, 0)) == REG
		   ? REGNO (XEXP (x, 0)) : REGNO (XEXP (XEXP (x, 0), 0)));
      return;

    case 'M':
      asm_fprintf (stream, "{%r-%r}",
		   REGNO (x),
		   REGNO (x) + NUM_REGS (GET_MODE (x)) - 1);
      return;

    case 'd':
      if (!x)
	return;
      
      if (TARGET_ARM)
        fputs (arm_condition_codes[get_arm_condition_code (x)],
	       stream);
      else
	fputs (thumb_condition_code (x, 0), stream);
      return;

    case 'D':
      if (!x)
	return;

      if (TARGET_ARM)
	fputs (arm_condition_codes[ARM_INVERSE_CONDITION_CODE
				  (get_arm_condition_code (x))],
	       stream);
      else
	fputs (thumb_condition_code (x, 1), stream);
      return;

    default:
      if (x == 0)
	abort ();

      if (GET_CODE (x) == REG)
	asm_fprintf (stream, "%r", REGNO (x));
      else if (GET_CODE (x) == MEM)
	{
	  output_memory_reference_mode = GET_MODE (x);
	  output_address (XEXP (x, 0));
	}
      else if (GET_CODE (x) == CONST_DOUBLE)
	fprintf (stream, "#%s", fp_immediate_constant (x));
      else if (GET_CODE (x) == NEG)
	abort (); /* This should never happen now.  */
      else
	{
	  fputc ('#', stream);
	  output_addr_const (stream, x);
	}
    }
}

#ifndef AOF_ASSEMBLER
/* Target hook for assembling integer objects.  The ARM version needs to
   handle word-sized values specially.  */

static bool
arm_assemble_integer (x, size, aligned_p)
     rtx x;
     unsigned int size;
     int aligned_p;
{
  if (size == UNITS_PER_WORD && aligned_p)
    {
      fputs ("\t.word\t", asm_out_file);
      output_addr_const (asm_out_file, x);

      /* Mark symbols as position independent.  We only do this in the
	 .text segment, not in the .data segment. */
      if (NEED_GOT_RELOC && flag_pic && making_const_table &&
	  (GET_CODE (x) == SYMBOL_REF || GET_CODE (x) == LABEL_REF))
	{
	  if (GET_CODE (x) == SYMBOL_REF 
	      && (CONSTANT_POOL_ADDRESS_P (x)
		  || ENCODED_SHORT_CALL_ATTR_P (XSTR (x, 0))))
	    fputs ("(GOTOFF)", asm_out_file);
	  else if (GET_CODE (x) == LABEL_REF)
	    fputs ("(GOTOFF)", asm_out_file);
	  else
	    fputs ("(GOT)", asm_out_file);
	}
      fputc ('\n', asm_out_file);
      return true;
    }

  return default_assemble_integer (x, size, aligned_p);
}
#endif

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
  int code;
  enum rtx_code comp_code = GET_CODE (comparison);

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
      switch (comp_code)
	{
	case NE: return ARM_NE;
	case EQ: return ARM_EQ;
	default: abort ();
	}

    case CCFPEmode:
    case CCFPmode:
      /* These encodings assume that AC=1 in the FPA system control
	 byte.  This allows us to handle all cases except UNEQ and
	 LTGT.  */
      switch (comp_code)
	{
	case GE: return ARM_GE;
	case GT: return ARM_GT;
	case LE: return ARM_LS;
	case LT: return ARM_MI;
	case NE: return ARM_NE;
	case EQ: return ARM_EQ;
	case ORDERED: return ARM_VC;
	case UNORDERED: return ARM_VS;
	case UNLT: return ARM_LT;
	case UNLE: return ARM_LE;
	case UNGT: return ARM_HI;
	case UNGE: return ARM_PL;
	  /* UNEQ and LTGT do not have a representation.  */
	case UNEQ: /* Fall through.  */
	case LTGT: /* Fall through.  */
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
  rtx body = PATTERN (insn);

  /* This will be 1 if trying to repeat the trick, and things need to be
     reversed if it appears to fail.  */
  int reverse = 0;

  /* JUMP_CLOBBERS will be one implies that the conditions if a branch is
     taken are clobbered, even if the rtl suggests otherwise.  It also
     means that we have to grub around within the jump expression to find
     out what the conditions are when the jump isn't taken.  */
  int jump_clobbers = 0;
  
  /* If we start with a return insn, we only succeed if we find another one.  */
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

      /* If the jump cannot be done with one instruction, we cannot 
	 conditionally execute the instruction in the inverse case.  */
      if (get_attr_conds (insn) == CONDS_JUMP_CLOB)
	{
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
		 will be a barrier.  */
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
		       && !use_return_insn (TRUE)
		       && !optimize_size)
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
	      else
		fail = TRUE;	/* Unrecognized jump (eg epilogue).  */

	      break;

	    case INSN:
	      /* Instructions using or affecting the condition codes make it
		 fail.  */
	      scanbody = PATTERN (this_insn);
	      if (!(GET_CODE (scanbody) == SET
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
		  recog (PATTERN (insn), insn, NULL);
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
      recog (PATTERN (insn), insn, NULL);
    }
}

/* Returns true if REGNO is a valid register
   for holding a quantity of tyoe MODE.  */

int
arm_hard_regno_mode_ok (regno, mode)
     unsigned int regno;
     enum machine_mode mode;
{
  if (GET_MODE_CLASS (mode) == MODE_CC)
    return regno == CC_REGNUM;
  
  if (TARGET_THUMB)
    /* For the Thumb we only allow values bigger than SImode in
       registers 0 - 6, so that there is always a second low
       register available to hold the upper part of the value.
       We probably we ought to ensure that the register is the
       start of an even numbered register pair.  */
    return (NUM_REGS (mode) < 2) || (regno < LAST_LO_REGNUM);

  if (regno <= LAST_ARM_REGNUM)
    /* We allow any value to be stored in the general regisetrs.  */
    return 1;

  if (   regno == FRAME_POINTER_REGNUM
      || regno == ARG_POINTER_REGNUM)
    /* We only allow integers in the fake hard registers.  */
    return GET_MODE_CLASS (mode) == MODE_INT;

  /* The only registers left are the FPU registers
     which we only allow to hold FP values.  */
  return GET_MODE_CLASS (mode) == MODE_FLOAT
    && regno >= FIRST_ARM_FP_REGNUM
    && regno <= LAST_ARM_FP_REGNUM;
}

int
arm_regno_class (regno)
     int regno;
{
  if (TARGET_THUMB)
    {
      if (regno == STACK_POINTER_REGNUM)
	return STACK_REG;
      if (regno == CC_REGNUM)
	return CC_REG;
      if (regno < 8)
	return LO_REGS;
      return HI_REGS;
    }

  if (   regno <= LAST_ARM_REGNUM
      || regno == FRAME_POINTER_REGNUM
      || regno == ARG_POINTER_REGNUM)
    return GENERAL_REGS;
  
  if (regno == CC_REGNUM)
    return NO_REGS;

  return FPU_REGS;
}

/* Handle a special case when computing the offset
   of an argument from the frame pointer.  */

int
arm_debugger_arg_offset (value, addr)
     int value;
     rtx addr;
{
  rtx insn;

  /* We are only interested if dbxout_parms() failed to compute the offset.  */
  if (value != 0)
    return 0;

  /* We can only cope with the case where the address is held in a register.  */
  if (GET_CODE (addr) != REG)
    return 0;

  /* If we are using the frame pointer to point at the argument, then
     an offset of 0 is correct.  */
  if (REGNO (addr) == (unsigned) HARD_FRAME_POINTER_REGNUM)
    return 0;
  
  /* If we are using the stack pointer to point at the
     argument, then an offset of 0 is correct.  */
  if ((TARGET_THUMB || !frame_pointer_needed)
      && REGNO (addr) == SP_REGNUM)
    return 0;
  
  /* Oh dear.  The argument is pointed to by a register rather
     than being held in a register, or being stored at a known
     offset from the frame pointer.  Since GDB only understands
     those two kinds of argument we must translate the address
     held in the register into an offset from the frame pointer.
     We do this by searching through the insns for the function
     looking to see where this register gets its value.  If the
     register is initialised from the frame pointer plus an offset
     then we are in luck and we can continue, otherwise we give up.
     
     This code is exercised by producing debugging information
     for a function with arguments like this:
     
           double func (double a, double b, int c, double d) {return d;}
     
     Without this code the stab for parameter 'd' will be set to
     an offset of 0 from the frame pointer, rather than 8.  */

  /* The if() statement says:

     If the insn is a normal instruction
     and if the insn is setting the value in a register
     and if the register being set is the register holding the address of the argument
     and if the address is computing by an addition
     that involves adding to a register
     which is the frame pointer
     a constant integer

     then... */
  
  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      if (   GET_CODE (insn) == INSN 
	  && GET_CODE (PATTERN (insn)) == SET
	  && REGNO    (XEXP (PATTERN (insn), 0)) == REGNO (addr)
	  && GET_CODE (XEXP (PATTERN (insn), 1)) == PLUS
	  && GET_CODE (XEXP (XEXP (PATTERN (insn), 1), 0)) == REG
	  && REGNO    (XEXP (XEXP (PATTERN (insn), 1), 0)) == (unsigned) HARD_FRAME_POINTER_REGNUM
	  && GET_CODE (XEXP (XEXP (PATTERN (insn), 1), 1)) == CONST_INT
	     )
	{
	  value = INTVAL (XEXP (XEXP (PATTERN (insn), 1), 1));
	  
	  break;
	}
    }
  
  if (value == 0)
    {
      debug_rtx (addr);
      warning ("unable to compute real location of stacked parameter");
      value = 8; /* XXX magic hack */
    }

  return value;
}

#define def_builtin(NAME, TYPE, CODE) \
  builtin_function ((NAME), (TYPE), (CODE), BUILT_IN_MD, NULL)

void
arm_init_builtins ()
{
  tree endlink = void_list_node;
  tree int_endlink = tree_cons (NULL_TREE, integer_type_node, endlink);
  tree pchar_type_node = build_pointer_type (char_type_node);

  tree int_ftype_int, void_ftype_pchar;

  /* void func (void *) */
  void_ftype_pchar
    = build_function_type (void_type_node,
			   tree_cons (NULL_TREE, pchar_type_node, endlink));

  /* int func (int) */
  int_ftype_int
    = build_function_type (integer_type_node, int_endlink);

  /* Initialize arm V5 builtins.  */
  if (arm_arch5)
    def_builtin ("__builtin_clz", int_ftype_int, ARM_BUILTIN_CLZ);
}

/* Expand an expression EXP that calls a built-in function,
   with result going to TARGET if that's convenient
   (and in mode MODE if that's convenient).
   SUBTARGET may be used as the target for computing one of EXP's operands.
   IGNORE is nonzero if the value is to be ignored.  */

rtx
arm_expand_builtin (exp, target, subtarget, mode, ignore)
     tree exp;
     rtx target;
     rtx subtarget ATTRIBUTE_UNUSED;
     enum machine_mode mode ATTRIBUTE_UNUSED;
     int ignore ATTRIBUTE_UNUSED;
{
  enum insn_code icode;
  tree fndecl = TREE_OPERAND (TREE_OPERAND (exp, 0), 0);
  tree arglist = TREE_OPERAND (exp, 1);
  tree arg0;
  rtx op0, pat;
  enum machine_mode tmode, mode0;
  int fcode = DECL_FUNCTION_CODE (fndecl);

  switch (fcode)
    {
    default:
      break;
      
    case ARM_BUILTIN_CLZ:
      icode = CODE_FOR_clz;
      arg0 = TREE_VALUE (arglist);
      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
      tmode = insn_data[icode].operand[0].mode;
      mode0 = insn_data[icode].operand[1].mode;

      if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);
      if (target == 0
	  || GET_MODE (target) != tmode
	  || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
	target = gen_reg_rtx (tmode);
      pat = GEN_FCN (icode) (target, op0);
      if (! pat)
	return 0;
      emit_insn (pat);
      return target;
    }
  
  /* @@@ Should really do something sensible here.  */
  return NULL_RTX;
}

/* Recursively search through all of the blocks in a function
   checking to see if any of the variables created in that
   function match the RTX called 'orig'.  If they do then
   replace them with the RTX called 'new'.  */

static void
replace_symbols_in_block (block, orig, new)
     tree block;
     rtx orig;
     rtx new;
{
  for (; block; block = BLOCK_CHAIN (block))
    {
      tree sym;
      
      if (!TREE_USED (block))
	continue;

      for (sym = BLOCK_VARS (block); sym; sym = TREE_CHAIN (sym))
	{
	  if (  (DECL_NAME (sym) == 0 && TREE_CODE (sym) != TYPE_DECL)
	      || DECL_IGNORED_P (sym)
	      || TREE_CODE (sym) != VAR_DECL
	      || DECL_EXTERNAL (sym)
	      || !rtx_equal_p (DECL_RTL (sym), orig)
	      )
	    continue;

	  SET_DECL_RTL (sym, new);
	}
      
      replace_symbols_in_block (BLOCK_SUBBLOCKS (block), orig, new);
    }
}

/* Return the number (counting from 0) of
   the least significant set bit in MASK.  */

#ifdef __GNUC__
inline
#endif
static int
number_of_first_bit_set (mask)
     int mask;
{
  int bit;

  for (bit = 0;
       (mask & (1 << bit)) == 0;
       ++bit)
    continue;

  return bit;
}

/* Generate code to return from a thumb function.
   If 'reg_containing_return_addr' is -1, then the return address is
   actually on the stack, at the stack pointer.  */
static void
thumb_exit (f, reg_containing_return_addr, eh_ofs)
     FILE * f;
     int    reg_containing_return_addr;
     rtx    eh_ofs;
{
  unsigned regs_available_for_popping;
  unsigned regs_to_pop;
  int pops_needed;
  unsigned available;
  unsigned required;
  int mode;
  int size;
  int restore_a4 = FALSE;

  /* Compute the registers we need to pop.  */
  regs_to_pop = 0;
  pops_needed = 0;

  /* There is an assumption here, that if eh_ofs is not NULL, the
     normal return address will have been pushed.  */
  if (reg_containing_return_addr == -1 || eh_ofs)
    {
      /* When we are generating a return for __builtin_eh_return, 
	 reg_containing_return_addr must specify the return regno.  */
      if (eh_ofs && reg_containing_return_addr == -1)
	abort ();

      regs_to_pop |= 1 << LR_REGNUM;
      ++pops_needed;
    }

  if (TARGET_BACKTRACE)
    {
      /* Restore the (ARM) frame pointer and stack pointer.  */
      regs_to_pop |= (1 << ARM_HARD_FRAME_POINTER_REGNUM) | (1 << SP_REGNUM);
      pops_needed += 2;
    }

  /* If there is nothing to pop then just emit the BX instruction and
     return.  */
  if (pops_needed == 0)
    {
      if (eh_ofs)
	asm_fprintf (f, "\tadd\t%r, %r\n", SP_REGNUM, REGNO (eh_ofs));

      asm_fprintf (f, "\tbx\t%r\n", reg_containing_return_addr);
      return;
    }
  /* Otherwise if we are not supporting interworking and we have not created
     a backtrace structure and the function was not entered in ARM mode then
     just pop the return address straight into the PC.  */
  else if (!TARGET_INTERWORK
	   && !TARGET_BACKTRACE
	   && !is_called_in_ARM_mode (current_function_decl))
    {
      if (eh_ofs)
	{
	  asm_fprintf (f, "\tadd\t%r, #4\n", SP_REGNUM);
	  asm_fprintf (f, "\tadd\t%r, %r\n", SP_REGNUM, REGNO (eh_ofs));
	  asm_fprintf (f, "\tbx\t%r\n", reg_containing_return_addr);
	}
      else
	asm_fprintf (f, "\tpop\t{%r}\n", PC_REGNUM);

      return;
    }

  /* Find out how many of the (return) argument registers we can corrupt.  */
  regs_available_for_popping = 0;

  /* If returning via __builtin_eh_return, the bottom three registers
     all contain information needed for the return.  */
  if (eh_ofs)
    size = 12;
  else
    {
#ifdef RTX_CODE
      /* If we can deduce the registers used from the function's
	 return value.  This is more reliable that examining
	 regs_ever_live[] because that will be set if the register is
	 ever used in the function, not just if the register is used
	 to hold a return value.  */

      if (current_function_return_rtx != 0)
	mode = GET_MODE (current_function_return_rtx);
      else
#endif
	mode = DECL_MODE (DECL_RESULT (current_function_decl));

      size = GET_MODE_SIZE (mode);

      if (size == 0)
	{
	  /* In a void function we can use any argument register.
	     In a function that returns a structure on the stack
	     we can use the second and third argument registers.  */
	  if (mode == VOIDmode)
	    regs_available_for_popping =
	      (1 << ARG_REGISTER (1))
	      | (1 << ARG_REGISTER (2))
	      | (1 << ARG_REGISTER (3));
	  else
	    regs_available_for_popping =
	      (1 << ARG_REGISTER (2))
	      | (1 << ARG_REGISTER (3));
	}
      else if (size <= 4)
	regs_available_for_popping =
	  (1 << ARG_REGISTER (2))
	  | (1 << ARG_REGISTER (3));
      else if (size <= 8)
	regs_available_for_popping =
	  (1 << ARG_REGISTER (3));
    }

  /* Match registers to be popped with registers into which we pop them.  */
  for (available = regs_available_for_popping,
       required  = regs_to_pop;
       required != 0 && available != 0;
       available &= ~(available & - available),
       required  &= ~(required  & - required))
    -- pops_needed;

  /* If we have any popping registers left over, remove them.  */
  if (available > 0)
    regs_available_for_popping &= ~available;
  
  /* Otherwise if we need another popping register we can use
     the fourth argument register.  */
  else if (pops_needed)
    {
      /* If we have not found any free argument registers and
	 reg a4 contains the return address, we must move it.  */
      if (regs_available_for_popping == 0
	  && reg_containing_return_addr == LAST_ARG_REGNUM)
	{
	  asm_fprintf (f, "\tmov\t%r, %r\n", LR_REGNUM, LAST_ARG_REGNUM);
	  reg_containing_return_addr = LR_REGNUM;
	}
      else if (size > 12)
	{
	  /* Register a4 is being used to hold part of the return value,
	     but we have dire need of a free, low register.  */
	  restore_a4 = TRUE;
	  
	  asm_fprintf (f, "\tmov\t%r, %r\n",IP_REGNUM, LAST_ARG_REGNUM);
	}
      
      if (reg_containing_return_addr != LAST_ARG_REGNUM)
	{
	  /* The fourth argument register is available.  */
	  regs_available_for_popping |= 1 << LAST_ARG_REGNUM;
	  
	  --pops_needed;
	}
    }

  /* Pop as many registers as we can.  */
  thumb_pushpop (f, regs_available_for_popping, FALSE);

  /* Process the registers we popped.  */
  if (reg_containing_return_addr == -1)
    {
      /* The return address was popped into the lowest numbered register.  */
      regs_to_pop &= ~(1 << LR_REGNUM);
      
      reg_containing_return_addr =
	number_of_first_bit_set (regs_available_for_popping);

      /* Remove this register for the mask of available registers, so that
         the return address will not be corrupted by futher pops.  */
      regs_available_for_popping &= ~(1 << reg_containing_return_addr);
    }

  /* If we popped other registers then handle them here.  */
  if (regs_available_for_popping)
    {
      int frame_pointer;
      
      /* Work out which register currently contains the frame pointer.  */
      frame_pointer = number_of_first_bit_set (regs_available_for_popping);

      /* Move it into the correct place.  */
      asm_fprintf (f, "\tmov\t%r, %r\n",
		   ARM_HARD_FRAME_POINTER_REGNUM, frame_pointer);

      /* (Temporarily) remove it from the mask of popped registers.  */
      regs_available_for_popping &= ~(1 << frame_pointer);
      regs_to_pop &= ~(1 << ARM_HARD_FRAME_POINTER_REGNUM);
      
      if (regs_available_for_popping)
	{
	  int stack_pointer;
	  
	  /* We popped the stack pointer as well,
	     find the register that contains it.  */
	  stack_pointer = number_of_first_bit_set (regs_available_for_popping);

	  /* Move it into the stack register.  */
	  asm_fprintf (f, "\tmov\t%r, %r\n", SP_REGNUM, stack_pointer);
	  
	  /* At this point we have popped all necessary registers, so
	     do not worry about restoring regs_available_for_popping
	     to its correct value:

	     assert (pops_needed == 0)
	     assert (regs_available_for_popping == (1 << frame_pointer))
	     assert (regs_to_pop == (1 << STACK_POINTER))  */
	}
      else
	{
	  /* Since we have just move the popped value into the frame
	     pointer, the popping register is available for reuse, and
	     we know that we still have the stack pointer left to pop.  */
	  regs_available_for_popping |= (1 << frame_pointer);
	}
    }
  
  /* If we still have registers left on the stack, but we no longer have
     any registers into which we can pop them, then we must move the return
     address into the link register and make available the register that
     contained it.  */
  if (regs_available_for_popping == 0 && pops_needed > 0)
    {
      regs_available_for_popping |= 1 << reg_containing_return_addr;
      
      asm_fprintf (f, "\tmov\t%r, %r\n", LR_REGNUM,
		   reg_containing_return_addr);
      
      reg_containing_return_addr = LR_REGNUM;
    }

  /* If we have registers left on the stack then pop some more.
     We know that at most we will want to pop FP and SP.  */
  if (pops_needed > 0)
    {
      int  popped_into;
      int  move_to;
      
      thumb_pushpop (f, regs_available_for_popping, FALSE);

      /* We have popped either FP or SP.
	 Move whichever one it is into the correct register.  */
      popped_into = number_of_first_bit_set (regs_available_for_popping);
      move_to     = number_of_first_bit_set (regs_to_pop);

      asm_fprintf (f, "\tmov\t%r, %r\n", move_to, popped_into);

      regs_to_pop &= ~(1 << move_to);

      --pops_needed;
    }
  
  /* If we still have not popped everything then we must have only
     had one register available to us and we are now popping the SP.  */
  if (pops_needed > 0)
    {
      int  popped_into;
      
      thumb_pushpop (f, regs_available_for_popping, FALSE);

      popped_into = number_of_first_bit_set (regs_available_for_popping);

      asm_fprintf (f, "\tmov\t%r, %r\n", SP_REGNUM, popped_into);
      /*
	assert (regs_to_pop == (1 << STACK_POINTER))
	assert (pops_needed == 1)
      */
    }

  /* If necessary restore the a4 register.  */
  if (restore_a4)
    {
      if (reg_containing_return_addr != LR_REGNUM)
	{
	  asm_fprintf (f, "\tmov\t%r, %r\n", LR_REGNUM, LAST_ARG_REGNUM);
	  reg_containing_return_addr = LR_REGNUM;
	}
    
      asm_fprintf (f, "\tmov\t%r, %r\n", LAST_ARG_REGNUM, IP_REGNUM);
    }

  if (eh_ofs)
    asm_fprintf (f, "\tadd\t%r, %r\n", SP_REGNUM, REGNO (eh_ofs));

  /* Return to caller.  */
  asm_fprintf (f, "\tbx\t%r\n", reg_containing_return_addr);
}

/* Emit code to push or pop registers to or from the stack.  */

static void
thumb_pushpop (f, mask, push)
     FILE * f;
     int mask;
     int push;
{
  int regno;
  int lo_mask = mask & 0xFF;

  if (lo_mask == 0 && !push && (mask & (1 << 15)))
    {
      /* Special case.  Do not generate a POP PC statement here, do it in
	 thumb_exit() */
      thumb_exit (f, -1, NULL_RTX);
      return;
    }
      
  fprintf (f, "\t%s\t{", push ? "push" : "pop");

  /* Look at the low registers first.  */
  for (regno = 0; regno <= LAST_LO_REGNUM; regno++, lo_mask >>= 1)
    {
      if (lo_mask & 1)
	{
	  asm_fprintf (f, "%r", regno);
	  
	  if ((lo_mask & ~1) != 0)
	    fprintf (f, ", ");
	}
    }
  
  if (push && (mask & (1 << LR_REGNUM)))
    {
      /* Catch pushing the LR.  */
      if (mask & 0xFF)
	fprintf (f, ", ");
      
      asm_fprintf (f, "%r", LR_REGNUM);
    }
  else if (!push && (mask & (1 << PC_REGNUM)))
    {
      /* Catch popping the PC.  */
      if (TARGET_INTERWORK || TARGET_BACKTRACE)
	{
	  /* The PC is never poped directly, instead
	     it is popped into r3 and then BX is used.  */
	  fprintf (f, "}\n");

	  thumb_exit (f, -1, NULL_RTX);

	  return;
	}
      else
	{
	  if (mask & 0xFF)
	    fprintf (f, ", ");
	  
	  asm_fprintf (f, "%r", PC_REGNUM);
	}
    }
       
  fprintf (f, "}\n");
}

void
thumb_final_prescan_insn (insn)
     rtx insn;
{
  if (flag_print_asm_name)
    asm_fprintf (asm_out_file, "%@ 0x%04x\n",
		 INSN_ADDRESSES (INSN_UID (insn)));
}

int
thumb_shiftable_const (val)
     unsigned HOST_WIDE_INT val;
{
  unsigned HOST_WIDE_INT mask = 0xff;
  int i;

  if (val == 0) /* XXX */
    return 0;
  
  for (i = 0; i < 25; i++)
    if ((val & (mask << i)) == val)
      return 1;

  return 0;
}

/* Returns non-zero if the current function contains,
   or might contain a far jump.  */

int
thumb_far_jump_used_p (int in_prologue)
{
  rtx insn;

  /* This test is only important for leaf functions.  */
  /* assert (!leaf_function_p ()); */
  
  /* If we have already decided that far jumps may be used,
     do not bother checking again, and always return true even if
     it turns out that they are not being used.  Once we have made
     the decision that far jumps are present (and that hence the link
     register will be pushed onto the stack) we cannot go back on it.  */
  if (cfun->machine->far_jump_used)
    return 1;

  /* If this function is not being called from the prologue/epilogue
     generation code then it must be being called from the
     INITIAL_ELIMINATION_OFFSET macro.  */
  if (!in_prologue)
    {
      /* In this case we know that we are being asked about the elimination
	 of the arg pointer register.  If that register is not being used,
	 then there are no arguments on the stack, and we do not have to
	 worry that a far jump might force the prologue to push the link
	 register, changing the stack offsets.  In this case we can just
	 return false, since the presence of far jumps in the function will
	 not affect stack offsets.

	 If the arg pointer is live (or if it was live, but has now been
	 eliminated and so set to dead) then we do have to test to see if
	 the function might contain a far jump.  This test can lead to some
	 false negatives, since before reload is completed, then length of
	 branch instructions is not known, so gcc defaults to returning their
	 longest length, which in turn sets the far jump attribute to true.

	 A false negative will not result in bad code being generated, but it
	 will result in a needless push and pop of the link register.  We
	 hope that this does not occur too often.  */
      if (regs_ever_live [ARG_POINTER_REGNUM])
	cfun->machine->arg_pointer_live = 1;
      else if (!cfun->machine->arg_pointer_live)
	return 0;
    }

  /* Check to see if the function contains a branch
     insn with the far jump attribute set.  */
  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      if (GET_CODE (insn) == JUMP_INSN
	  /* Ignore tablejump patterns.  */
	  && GET_CODE (PATTERN (insn)) != ADDR_VEC
	  && GET_CODE (PATTERN (insn)) != ADDR_DIFF_VEC
	  && get_attr_far_jump (insn) == FAR_JUMP_YES
	  )
	{
	  /* Record the fact that we have decied that
	     the function does use far jumps.  */
	  cfun->machine->far_jump_used = 1;
	  return 1;
	}
    }
  
  return 0;
}

/* Return non-zero if FUNC must be entered in ARM mode.  */

int
is_called_in_ARM_mode (func)
     tree func;
{
  if (TREE_CODE (func) != FUNCTION_DECL)
    abort ();

  /* Ignore the problem about functions whoes address is taken.  */
  if (TARGET_CALLEE_INTERWORKING && TREE_PUBLIC (func))
    return TRUE;

#ifdef ARM_PE 
  return lookup_attribute ("interfacearm", DECL_ATTRIBUTES (func)) != NULL_TREE;
#else
  return FALSE;
#endif
}

/* The bits which aren't usefully expanded as rtl. */

const char *
thumb_unexpanded_epilogue ()
{
  int regno;
  int live_regs_mask = 0;
  int high_regs_pushed = 0;
  int leaf_function = leaf_function_p ();
  int had_to_push_lr;
  rtx eh_ofs = cfun->machine->eh_epilogue_sp_ofs;

  if (return_used_this_function)
    return "";

  for (regno = 0; regno <= LAST_LO_REGNUM; regno++)
    if (regs_ever_live[regno] && !call_used_regs[regno]
	&& !(TARGET_SINGLE_PIC_BASE && (regno == arm_pic_register)))
      live_regs_mask |= 1 << regno;

  for (regno = 8; regno < 13; regno++)
    {
      if (regs_ever_live[regno] && !call_used_regs[regno]
	  && !(TARGET_SINGLE_PIC_BASE && (regno == arm_pic_register)))
	high_regs_pushed++;
    }

  /* The prolog may have pushed some high registers to use as
     work registers.  eg the testuite file:
     gcc/testsuite/gcc/gcc.c-torture/execute/complex-2.c
     compiles to produce:
	push	{r4, r5, r6, r7, lr}
	mov	r7, r9
	mov	r6, r8
	push	{r6, r7}
     as part of the prolog.  We have to undo that pushing here.  */
  
  if (high_regs_pushed)
    {
      int mask = live_regs_mask;
      int next_hi_reg;
      int size;
      int mode;
       
#ifdef RTX_CODE
      /* If we can deduce the registers used from the function's return value.
	 This is more reliable that examining regs_ever_live[] because that
	 will be set if the register is ever used in the function, not just if
	 the register is used to hold a return value.  */

      if (current_function_return_rtx != 0)
	mode = GET_MODE (current_function_return_rtx);
      else
#endif
	mode = DECL_MODE (DECL_RESULT (current_function_decl));

      size = GET_MODE_SIZE (mode);

      /* Unless we are returning a type of size > 12 register r3 is
         available.  */
      if (size < 13)
	mask |=  1 << 3;

      if (mask == 0)
	/* Oh dear!  We have no low registers into which we can pop
           high registers!  */
	internal_error
	  ("no low registers available for popping high registers");
      
      for (next_hi_reg = 8; next_hi_reg < 13; next_hi_reg++)
	if (regs_ever_live[next_hi_reg] && !call_used_regs[next_hi_reg]
	    && !(TARGET_SINGLE_PIC_BASE && (next_hi_reg == arm_pic_register)))
	  break;

      while (high_regs_pushed)
	{
	  /* Find lo register(s) into which the high register(s) can
             be popped.  */
	  for (regno = 0; regno <= LAST_LO_REGNUM; regno++)
	    {
	      if (mask & (1 << regno))
		high_regs_pushed--;
	      if (high_regs_pushed == 0)
		break;
	    }

	  mask &= (2 << regno) - 1;	/* A noop if regno == 8 */

	  /* Pop the values into the low register(s). */
	  thumb_pushpop (asm_out_file, mask, 0);

	  /* Move the value(s) into the high registers.  */
	  for (regno = 0; regno <= LAST_LO_REGNUM; regno++)
	    {
	      if (mask & (1 << regno))
		{
		  asm_fprintf (asm_out_file, "\tmov\t%r, %r\n", next_hi_reg,
			       regno);
		  
		  for (next_hi_reg++; next_hi_reg < 13; next_hi_reg++)
		    if (regs_ever_live[next_hi_reg]
			&& !call_used_regs[next_hi_reg]
			&& !(TARGET_SINGLE_PIC_BASE 
			     && (next_hi_reg == arm_pic_register)))
		      break;
		}
	    }
	}
    }

  had_to_push_lr = (live_regs_mask || !leaf_function
		    || thumb_far_jump_used_p (1));
  
  if (TARGET_BACKTRACE
      && ((live_regs_mask & 0xFF) == 0)
      && regs_ever_live [LAST_ARG_REGNUM] != 0)
    {
      /* The stack backtrace structure creation code had to
	 push R7 in order to get a work register, so we pop
	 it now.   */
      live_regs_mask |= (1 << LAST_LO_REGNUM);
    }
  
  if (current_function_pretend_args_size == 0 || TARGET_BACKTRACE)
    {
      if (had_to_push_lr
	  && !is_called_in_ARM_mode (current_function_decl)
	  && !eh_ofs)
	live_regs_mask |= 1 << PC_REGNUM;

      /* Either no argument registers were pushed or a backtrace
	 structure was created which includes an adjusted stack
	 pointer, so just pop everything.  */
      if (live_regs_mask)
	thumb_pushpop (asm_out_file, live_regs_mask, FALSE);
      
      if (eh_ofs)
	thumb_exit (asm_out_file, 2, eh_ofs);
      /* We have either just popped the return address into the
	 PC or it is was kept in LR for the entire function or
	 it is still on the stack because we do not want to
	 return by doing a pop {pc}.  */
      else if ((live_regs_mask & (1 << PC_REGNUM)) == 0)
	thumb_exit (asm_out_file,
		    (had_to_push_lr
		     && is_called_in_ARM_mode (current_function_decl)) ?
		    -1 : LR_REGNUM, NULL_RTX);
    }
  else
    {
      /* Pop everything but the return address.  */
      live_regs_mask &= ~(1 << PC_REGNUM);
      
      if (live_regs_mask)
	thumb_pushpop (asm_out_file, live_regs_mask, FALSE);

      if (had_to_push_lr)
	/* Get the return address into a temporary register.  */
	thumb_pushpop (asm_out_file, 1 << LAST_ARG_REGNUM, 0);
      
      /* Remove the argument registers that were pushed onto the stack.  */
      asm_fprintf (asm_out_file, "\tadd\t%r, %r, #%d\n",
		   SP_REGNUM, SP_REGNUM,
		   current_function_pretend_args_size);
      
      if (eh_ofs)
	thumb_exit (asm_out_file, 2, eh_ofs);
      else
	thumb_exit (asm_out_file,
		    had_to_push_lr ? LAST_ARG_REGNUM : LR_REGNUM, NULL_RTX);
    }

  return "";
}

/* Functions to save and restore machine-specific function data.  */

static void
arm_mark_machine_status (p)
     struct function * p;
{
  machine_function *machine = p->machine;

  if (machine)
    ggc_mark_rtx (machine->eh_epilogue_sp_ofs);
}

static void
arm_init_machine_status (p)
     struct function * p;
{
  p->machine =
    (machine_function *) xcalloc (1, sizeof (machine_function));

#if ARM_FT_UNKNOWWN != 0  
  ((machine_function *) p->machine)->func_type = ARM_FT_UNKNOWN;
#endif
}

static void
arm_free_machine_status (p)
     struct function * p;
{
  if (p->machine)
    {
      free (p->machine);
      p->machine = NULL;
    }
}

/* Return an RTX indicating where the return address to the
   calling function can be found.  */

rtx
arm_return_addr (count, frame)
     int count;
     rtx frame ATTRIBUTE_UNUSED;
{
  if (count != 0)
    return NULL_RTX;

  if (TARGET_APCS_32)
    return get_hard_reg_initial_val (Pmode, LR_REGNUM);
  else
    {
      rtx lr = gen_rtx_AND (Pmode, gen_rtx_REG (Pmode, LR_REGNUM),
			    GEN_INT (RETURN_ADDR_MASK26));
      return get_func_hard_reg_initial_val (cfun, lr);
    }
}

/* Do anything needed before RTL is emitted for each function.  */

void
arm_init_expanders ()
{
  /* Arrange to initialize and mark the machine per-function status.  */
  init_machine_status = arm_init_machine_status;
  mark_machine_status = arm_mark_machine_status;
  free_machine_status = arm_free_machine_status;
}

/* Generate the rest of a function's prologue.  */

void
thumb_expand_prologue ()
{
  HOST_WIDE_INT amount = (get_frame_size ()
			  + current_function_outgoing_args_size);
  unsigned long func_type;

  func_type = arm_current_func_type ();
  
  /* Naked functions don't have prologues.  */
  if (IS_NAKED (func_type))
    return;

  if (IS_INTERRUPT (func_type))
    {
      error ("interrupt Service Routines cannot be coded in Thumb mode");
      return;
    }

  if (frame_pointer_needed)
    emit_insn (gen_movsi (hard_frame_pointer_rtx, stack_pointer_rtx));

  if (amount)
    {
      amount = ROUND_UP (amount);
      
      if (amount < 512)
	emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx,
			       GEN_INT (- amount)));
      else
	{
	  int regno;
	  rtx reg;

	  /* The stack decrement is too big for an immediate value in a single
	     insn.  In theory we could issue multiple subtracts, but after
	     three of them it becomes more space efficient to place the full
	     value in the constant pool and load into a register.  (Also the
	     ARM debugger really likes to see only one stack decrement per
	     function).  So instead we look for a scratch register into which
	     we can load the decrement, and then we subtract this from the
	     stack pointer.  Unfortunately on the thumb the only available
	     scratch registers are the argument registers, and we cannot use
	     these as they may hold arguments to the function.  Instead we
	     attempt to locate a call preserved register which is used by this
	     function.  If we can find one, then we know that it will have
	     been pushed at the start of the prologue and so we can corrupt
	     it now.  */
	  for (regno = LAST_ARG_REGNUM + 1; regno <= LAST_LO_REGNUM; regno++)
	    if (regs_ever_live[regno]
		&& !call_used_regs[regno] /* Paranoia */
		&& !(TARGET_SINGLE_PIC_BASE && (regno == arm_pic_register))
		&& !(frame_pointer_needed
		     && (regno == THUMB_HARD_FRAME_POINTER_REGNUM)))
	      break;

	  if (regno > LAST_LO_REGNUM) /* Very unlikely */
	    {
	      rtx spare = gen_rtx (REG, SImode, IP_REGNUM);

	      /* Choose an arbitary, non-argument low register.  */
	      reg = gen_rtx (REG, SImode, LAST_LO_REGNUM);

	      /* Save it by copying it into a high, scratch register.  */
	      emit_insn (gen_movsi (spare, reg));
	      /* Add a USE to stop propagate_one_insn() from barfing.  */
	      emit_insn (gen_prologue_use (spare));

	      /* Decrement the stack.  */
	      emit_insn (gen_movsi (reg, GEN_INT (- amount)));
	      emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx,
				     reg));

	      /* Restore the low register's original value.  */
	      emit_insn (gen_movsi (reg, spare));
	      
	      /* Emit a USE of the restored scratch register, so that flow
		 analysis will not consider the restore redundant.  The
		 register won't be used again in this function and isn't
		 restored by the epilogue.  */
	      emit_insn (gen_prologue_use (reg));
	    }
	  else
	    {
	      reg = gen_rtx (REG, SImode, regno);

	      emit_insn (gen_movsi (reg, GEN_INT (- amount)));
	      emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx,
				     reg));
	    }
	}
    }
  
  if (current_function_profile || TARGET_NO_SCHED_PRO)
    emit_insn (gen_blockage ());
}

void
thumb_expand_epilogue ()
{
  HOST_WIDE_INT amount = (get_frame_size ()
			  + current_function_outgoing_args_size);
  
  /* Naked functions don't have prologues.  */
  if (IS_NAKED (arm_current_func_type ()))
    return;

  if (frame_pointer_needed)
    emit_insn (gen_movsi (stack_pointer_rtx, hard_frame_pointer_rtx));
  else if (amount)
    {
      amount = ROUND_UP (amount);
      
      if (amount < 512)
	emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx,
			       GEN_INT (amount)));
      else
	{
	  /* r3 is always free in the epilogue.  */
	  rtx reg = gen_rtx (REG, SImode, LAST_ARG_REGNUM);

	  emit_insn (gen_movsi (reg, GEN_INT (amount)));
	  emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx, reg));
	}
    }
      
  /* Emit a USE (stack_pointer_rtx), so that
     the stack adjustment will not be deleted.  */
  emit_insn (gen_prologue_use (stack_pointer_rtx));

  if (current_function_profile || TARGET_NO_SCHED_PRO)
    emit_insn (gen_blockage ());
}

static void
thumb_output_function_prologue (f, size)
     FILE * f;
     HOST_WIDE_INT size ATTRIBUTE_UNUSED;
{
  int live_regs_mask = 0;
  int high_regs_pushed = 0;
  int regno;

  if (IS_NAKED (arm_current_func_type ()))
    return;

  if (is_called_in_ARM_mode (current_function_decl))
    {
      const char * name;

      if (GET_CODE (DECL_RTL (current_function_decl)) != MEM)
	abort ();
      if (GET_CODE (XEXP (DECL_RTL (current_function_decl), 0)) != SYMBOL_REF)
	abort ();
      name = XSTR  (XEXP (DECL_RTL (current_function_decl), 0), 0);
      
      /* Generate code sequence to switch us into Thumb mode.  */
      /* The .code 32 directive has already been emitted by
	 ASM_DECLARE_FUNCTION_NAME.  */
      asm_fprintf (f, "\torr\t%r, %r, #1\n", IP_REGNUM, PC_REGNUM);
      asm_fprintf (f, "\tbx\t%r\n", IP_REGNUM);

      /* Generate a label, so that the debugger will notice the
	 change in instruction sets.  This label is also used by
	 the assembler to bypass the ARM code when this function
	 is called from a Thumb encoded function elsewhere in the
	 same file.  Hence the definition of STUB_NAME here must
	 agree with the definition in gas/config/tc-arm.c  */
      
#define STUB_NAME ".real_start_of"
      
      asm_fprintf (f, "\t.code\t16\n");
#ifdef ARM_PE
      if (arm_dllexport_name_p (name))
        name = arm_strip_name_encoding (name);
#endif        
      asm_fprintf (f, "\t.globl %s%U%s\n", STUB_NAME, name);
      asm_fprintf (f, "\t.thumb_func\n");
      asm_fprintf (f, "%s%U%s:\n", STUB_NAME, name);
    }
    
  if (current_function_pretend_args_size)
    {
      if (cfun->machine->uses_anonymous_args)
	{
	  int num_pushes;
	  
	  asm_fprintf (f, "\tpush\t{");

	  num_pushes = NUM_INTS (current_function_pretend_args_size);
	  
	  for (regno = LAST_ARG_REGNUM + 1 - num_pushes;
	       regno <= LAST_ARG_REGNUM;
	       regno++)
	    asm_fprintf (f, "%r%s", regno,
			 regno == LAST_ARG_REGNUM ? "" : ", ");

	  asm_fprintf (f, "}\n");
	}
      else
	asm_fprintf (f, "\tsub\t%r, %r, #%d\n", 
		     SP_REGNUM, SP_REGNUM,
		     current_function_pretend_args_size);
    }

  for (regno = 0; regno <= LAST_LO_REGNUM; regno++)
    if (regs_ever_live[regno] && !call_used_regs[regno]
	&& !(TARGET_SINGLE_PIC_BASE && (regno == arm_pic_register)))
      live_regs_mask |= 1 << regno;

  if (live_regs_mask || !leaf_function_p () || thumb_far_jump_used_p (1))
    live_regs_mask |= 1 << LR_REGNUM;

  if (TARGET_BACKTRACE)
    {
      int    offset;
      int    work_register = 0;
      int    wr;
      
      /* We have been asked to create a stack backtrace structure.
         The code looks like this:
	 
	 0   .align 2
	 0   func:
         0     sub   SP, #16         Reserve space for 4 registers.
	 2     push  {R7}            Get a work register.
         4     add   R7, SP, #20     Get the stack pointer before the push.
         6     str   R7, [SP, #8]    Store the stack pointer (before reserving the space).
         8     mov   R7, PC          Get hold of the start of this code plus 12.
        10     str   R7, [SP, #16]   Store it.
        12     mov   R7, FP          Get hold of the current frame pointer.
        14     str   R7, [SP, #4]    Store it.
        16     mov   R7, LR          Get hold of the current return address.
        18     str   R7, [SP, #12]   Store it.
        20     add   R7, SP, #16     Point at the start of the backtrace structure.
        22     mov   FP, R7          Put this value into the frame pointer.  */

      if ((live_regs_mask & 0xFF) == 0)
	{
	  /* See if the a4 register is free.  */

	  if (regs_ever_live [LAST_ARG_REGNUM] == 0)
	    work_register = LAST_ARG_REGNUM;
	  else	  /* We must push a register of our own */
	    live_regs_mask |= (1 << LAST_LO_REGNUM);
	}

      if (work_register == 0)
	{
	  /* Select a register from the list that will be pushed to
             use as our work register.  */
	  for (work_register = (LAST_LO_REGNUM + 1); work_register--;)
	    if ((1 << work_register) & live_regs_mask)
	      break;
	}
      
      asm_fprintf
	(f, "\tsub\t%r, %r, #16\t%@ Create stack backtrace structure\n",
	 SP_REGNUM, SP_REGNUM);
      
      if (live_regs_mask)
	thumb_pushpop (f, live_regs_mask, 1);
      
      for (offset = 0, wr = 1 << 15; wr != 0; wr >>= 1)
	if (wr & live_regs_mask)
	  offset += 4;
      
      asm_fprintf (f, "\tadd\t%r, %r, #%d\n", work_register, SP_REGNUM,
		   offset + 16 + current_function_pretend_args_size);
      
      asm_fprintf (f, "\tstr\t%r, [%r, #%d]\n", work_register, SP_REGNUM,
		   offset + 4);

      /* Make sure that the instruction fetching the PC is in the right place
	 to calculate "start of backtrace creation code + 12".  */
      if (live_regs_mask)
	{
	  asm_fprintf (f, "\tmov\t%r, %r\n", work_register, PC_REGNUM);
	  asm_fprintf (f, "\tstr\t%r, [%r, #%d]\n", work_register, SP_REGNUM,
		       offset + 12);
	  asm_fprintf (f, "\tmov\t%r, %r\n", work_register,
		       ARM_HARD_FRAME_POINTER_REGNUM);
	  asm_fprintf (f, "\tstr\t%r, [%r, #%d]\n", work_register, SP_REGNUM,
		       offset);
	}
      else
	{
	  asm_fprintf (f, "\tmov\t%r, %r\n", work_register,
		       ARM_HARD_FRAME_POINTER_REGNUM);
	  asm_fprintf (f, "\tstr\t%r, [%r, #%d]\n", work_register, SP_REGNUM,
		       offset);
	  asm_fprintf (f, "\tmov\t%r, %r\n", work_register, PC_REGNUM);
	  asm_fprintf (f, "\tstr\t%r, [%r, #%d]\n", work_register, SP_REGNUM,
		       offset + 12);
	}
      
      asm_fprintf (f, "\tmov\t%r, %r\n", work_register, LR_REGNUM);
      asm_fprintf (f, "\tstr\t%r, [%r, #%d]\n", work_register, SP_REGNUM,
		   offset + 8);
      asm_fprintf (f, "\tadd\t%r, %r, #%d\n", work_register, SP_REGNUM,
		   offset + 12);
      asm_fprintf (f, "\tmov\t%r, %r\t\t%@ Backtrace structure created\n",
		   ARM_HARD_FRAME_POINTER_REGNUM, work_register);
    }
  else if (live_regs_mask)
    thumb_pushpop (f, live_regs_mask, 1);

  for (regno = 8; regno < 13; regno++)
    {
      if (regs_ever_live[regno] && !call_used_regs[regno]
	  && !(TARGET_SINGLE_PIC_BASE && (regno == arm_pic_register)))
	high_regs_pushed++;
    }

  if (high_regs_pushed)
    {
      int pushable_regs = 0;
      int mask = live_regs_mask & 0xff;
      int next_hi_reg;

      for (next_hi_reg = 12; next_hi_reg > LAST_LO_REGNUM; next_hi_reg--)
	{
	  if (regs_ever_live[next_hi_reg] && !call_used_regs[next_hi_reg]
	      && !(TARGET_SINGLE_PIC_BASE
		   && (next_hi_reg == arm_pic_register)))
	    break;
	}

      pushable_regs = mask;

      if (pushable_regs == 0)
	{
	  /* Desperation time -- this probably will never happen.  */
	  if (regs_ever_live[LAST_ARG_REGNUM]
	      || !call_used_regs[LAST_ARG_REGNUM])
	    asm_fprintf (f, "\tmov\t%r, %r\n", IP_REGNUM, LAST_ARG_REGNUM);
	  mask = 1 << LAST_ARG_REGNUM;
	}

      while (high_regs_pushed > 0)
	{
	  for (regno = LAST_LO_REGNUM; regno >= 0; regno--)
	    {
	      if (mask & (1 << regno))
		{
		  asm_fprintf (f, "\tmov\t%r, %r\n", regno, next_hi_reg);
		  
		  high_regs_pushed--;
		  
		  if (high_regs_pushed)
		    for (next_hi_reg--; next_hi_reg > LAST_LO_REGNUM;
			 next_hi_reg--)
		      {
			if (regs_ever_live[next_hi_reg]
			    && !call_used_regs[next_hi_reg]
			    && !(TARGET_SINGLE_PIC_BASE 
				 && (next_hi_reg == arm_pic_register)))
			  break;
		      }
		  else
		    {
		      mask &= ~((1 << regno) - 1);
		      break;
		    }
		}
	    }
	  
	  thumb_pushpop (f, mask, 1);
	}

      if (pushable_regs == 0
	  && (regs_ever_live[LAST_ARG_REGNUM]
	      || !call_used_regs[LAST_ARG_REGNUM]))
	asm_fprintf (f, "\tmov\t%r, %r\n", LAST_ARG_REGNUM, IP_REGNUM);
    }
}

/* Handle the case of a double word load into a low register from
   a computed memory address.  The computed address may involve a
   register which is overwritten by the load.  */

const char *
thumb_load_double_from_address (operands)
     rtx *operands;
{
  rtx addr;
  rtx base;
  rtx offset;
  rtx arg1;
  rtx arg2;
  
  if (GET_CODE (operands[0]) != REG)
    abort ();
  
  if (GET_CODE (operands[1]) != MEM)
    abort ();

  /* Get the memory address.  */
  addr = XEXP (operands[1], 0);
      
  /* Work out how the memory address is computed.  */
  switch (GET_CODE (addr))
    {
    case REG:
      operands[2] = gen_rtx (MEM, SImode,
			     plus_constant (XEXP (operands[1], 0), 4));

      if (REGNO (operands[0]) == REGNO (addr))
	{
	  output_asm_insn ("ldr\t%H0, %2", operands);
	  output_asm_insn ("ldr\t%0, %1", operands);
	}
      else
	{
	  output_asm_insn ("ldr\t%0, %1", operands);
	  output_asm_insn ("ldr\t%H0, %2", operands);
	}
      break;
      
    case CONST:
      /* Compute <address> + 4 for the high order load.  */
      operands[2] = gen_rtx (MEM, SImode,
			     plus_constant (XEXP (operands[1], 0), 4));
      
      output_asm_insn ("ldr\t%0, %1", operands);
      output_asm_insn ("ldr\t%H0, %2", operands);
      break;
	  
    case PLUS:
      arg1   = XEXP (addr, 0);
      arg2   = XEXP (addr, 1);
	    
      if (CONSTANT_P (arg1))
	base = arg2, offset = arg1;
      else
	base = arg1, offset = arg2;
  
      if (GET_CODE (base) != REG)
	abort ();

      /* Catch the case of <address> = <reg> + <reg> */
      if (GET_CODE (offset) == REG)
	{
	  int reg_offset = REGNO (offset);
	  int reg_base   = REGNO (base);
	  int reg_dest   = REGNO (operands[0]);
	  
	  /* Add the base and offset registers together into the
             higher destination register.  */
	  asm_fprintf (asm_out_file, "\tadd\t%r, %r, %r",
		       reg_dest + 1, reg_base, reg_offset);
	  
	  /* Load the lower destination register from the address in
             the higher destination register.  */
	  asm_fprintf (asm_out_file, "\tldr\t%r, [%r, #0]",
		       reg_dest, reg_dest + 1);
	  
	  /* Load the higher destination register from its own address
             plus 4.  */
	  asm_fprintf (asm_out_file, "\tldr\t%r, [%r, #4]",
		       reg_dest + 1, reg_dest + 1);
	}
      else
	{
	  /* Compute <address> + 4 for the high order load.  */
	  operands[2] = gen_rtx (MEM, SImode,
				 plus_constant (XEXP (operands[1], 0), 4));
	  
	  /* If the computed address is held in the low order register
	     then load the high order register first, otherwise always
	     load the low order register first.  */
	  if (REGNO (operands[0]) == REGNO (base))
	    {
	      output_asm_insn ("ldr\t%H0, %2", operands);
	      output_asm_insn ("ldr\t%0, %1", operands);
	    }
	  else
	    {
	      output_asm_insn ("ldr\t%0, %1", operands);
	      output_asm_insn ("ldr\t%H0, %2", operands);
	    }
	}
      break;

    case LABEL_REF:
      /* With no registers to worry about we can just load the value
         directly.  */
      operands[2] = gen_rtx (MEM, SImode,
			     plus_constant (XEXP (operands[1], 0), 4));
	  
      output_asm_insn ("ldr\t%H0, %2", operands);
      output_asm_insn ("ldr\t%0, %1", operands);
      break;
      
    default:
      abort ();
      break;
    }
  
  return "";
}


const char *
thumb_output_move_mem_multiple (n, operands)
     int n;
     rtx * operands;
{
  rtx tmp;

  switch (n)
    {
    case 2:
      if (REGNO (operands[4]) > REGNO (operands[5]))
	{
	  tmp = operands[4];
	  operands[4] = operands[5];
	  operands[5] = tmp;
	}
      output_asm_insn ("ldmia\t%1!, {%4, %5}", operands);
      output_asm_insn ("stmia\t%0!, {%4, %5}", operands);
      break;

    case 3:
      if (REGNO (operands[4]) > REGNO (operands[5]))
	{
	  tmp = operands[4];
	  operands[4] = operands[5];
	  operands[5] = tmp;
	}
      if (REGNO (operands[5]) > REGNO (operands[6]))
	{
	  tmp = operands[5];
	  operands[5] = operands[6];
	  operands[6] = tmp;
	}
      if (REGNO (operands[4]) > REGNO (operands[5]))
	{
	  tmp = operands[4];
	  operands[4] = operands[5];
	  operands[5] = tmp;
	}
      
      output_asm_insn ("ldmia\t%1!, {%4, %5, %6}", operands);
      output_asm_insn ("stmia\t%0!, {%4, %5, %6}", operands);
      break;

    default:
      abort ();
    }

  return "";
}

/* Routines for generating rtl.  */

void
thumb_expand_movstrqi (operands)
     rtx * operands;
{
  rtx out = copy_to_mode_reg (SImode, XEXP (operands[0], 0));
  rtx in  = copy_to_mode_reg (SImode, XEXP (operands[1], 0));
  HOST_WIDE_INT len = INTVAL (operands[2]);
  HOST_WIDE_INT offset = 0;

  while (len >= 12)
    {
      emit_insn (gen_movmem12b (out, in, out, in));
      len -= 12;
    }
  
  if (len >= 8)
    {
      emit_insn (gen_movmem8b (out, in, out, in));
      len -= 8;
    }
  
  if (len >= 4)
    {
      rtx reg = gen_reg_rtx (SImode);
      emit_insn (gen_movsi (reg, gen_rtx (MEM, SImode, in)));
      emit_insn (gen_movsi (gen_rtx (MEM, SImode, out), reg));
      len -= 4;
      offset += 4;
    }
  
  if (len >= 2)
    {
      rtx reg = gen_reg_rtx (HImode);
      emit_insn (gen_movhi (reg, gen_rtx (MEM, HImode, 
					  plus_constant (in, offset))));
      emit_insn (gen_movhi (gen_rtx (MEM, HImode, plus_constant (out, offset)),
			    reg));
      len -= 2;
      offset += 2;
    }
  
  if (len)
    {
      rtx reg = gen_reg_rtx (QImode);
      emit_insn (gen_movqi (reg, gen_rtx (MEM, QImode,
					  plus_constant (in, offset))));
      emit_insn (gen_movqi (gen_rtx (MEM, QImode, plus_constant (out, offset)),
			    reg));
    }
}

int
thumb_cmp_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ((GET_CODE (op) == CONST_INT
	   && (unsigned HOST_WIDE_INT) (INTVAL (op)) < 256)
	  || register_operand (op, mode));
}

static const char *
thumb_condition_code (x, invert)
     rtx x;
     int invert;
{
  static const char * const conds[] =
  {
    "eq", "ne", "cs", "cc", "mi", "pl", "vs", "vc", 
    "hi", "ls", "ge", "lt", "gt", "le"
  };
  int val;

  switch (GET_CODE (x))
    {
    case EQ: val = 0; break;
    case NE: val = 1; break;
    case GEU: val = 2; break;
    case LTU: val = 3; break;
    case GTU: val = 8; break;
    case LEU: val = 9; break;
    case GE: val = 10; break;
    case LT: val = 11; break;
    case GT: val = 12; break;
    case LE: val = 13; break;
    default:
      abort ();
    }

  return conds[val ^ invert];
}

/* Handle storing a half-word to memory during reload.  */ 

void
thumb_reload_out_hi (operands)
     rtx * operands;
{
  emit_insn (gen_thumb_movhi_clobber (operands[0], operands[1], operands[2]));
}

/* Handle storing a half-word to memory during reload.  */ 

void
thumb_reload_in_hi (operands)
     rtx * operands ATTRIBUTE_UNUSED;
{
  abort ();
}

/* Return the length of a function name prefix
    that starts with the character 'c'.  */

static int
arm_get_strip_length (char c)
{
  switch (c)
    {
    ARM_NAME_ENCODING_LENGTHS
      default: return 0; 
    }
}

/* Return a pointer to a function's name with any
   and all prefix encodings stripped from it.  */

const char *
arm_strip_name_encoding (const char * name)
{
  int skip;
  
  while ((skip = arm_get_strip_length (* name)))
    name += skip;

  return name;
}

#ifdef AOF_ASSEMBLER
/* Special functions only needed when producing AOF syntax assembler.  */

rtx aof_pic_label = NULL_RTX;
struct pic_chain
{
  struct pic_chain * next;
  const char * symname;
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
      aof_pic_label = gen_rtx_SYMBOL_REF (Pmode, "x$adcons");
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
  const char * name;
};

static struct import * imports_list = NULL;

void
aof_add_import (name)
     const char * name;
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
     const char * name;
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

#ifdef OBJECT_FORMAT_ELF
/* Switch to an arbitrary section NAME with attributes as specified
   by FLAGS.  ALIGN specifies any known alignment requirements for
   the section; 0 if the default should be used.

   Differs from the default elf version only in the prefix character
   used before the section type.  */

static void
arm_elf_asm_named_section (name, flags)
     const char *name;
     unsigned int flags;
{
  char flagchars[8], *f = flagchars;
  const char *type;

  if (!(flags & SECTION_DEBUG))
    *f++ = 'a';
  if (flags & SECTION_WRITE)
    *f++ = 'w';
  if (flags & SECTION_CODE)
    *f++ = 'x';
  if (flags & SECTION_SMALL)
    *f++ = 's';
  if (flags & SECTION_MERGE)
    *f++ = 'M';
  if (flags & SECTION_STRINGS)
    *f++ = 'S';
  *f = '\0';

  if (flags & SECTION_BSS)
    type = "nobits";
  else
    type = "progbits";

  if (flags & SECTION_ENTSIZE)
    fprintf (asm_out_file, "\t.section\t%s,\"%s\",%%%s,%d\n",
	     name, flagchars, type, flags & SECTION_ENTSIZE);
  else
    fprintf (asm_out_file, "\t.section\t%s,\"%s\",%%%s\n",
	     name, flagchars, type);
}
#endif
