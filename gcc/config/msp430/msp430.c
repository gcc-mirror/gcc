/* Subroutines used for code generation on TI MSP430 processors.
   Copyright (C) 2012-2013 Free Software Foundation, Inc.
   Contributed by Red Hat.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "insn-config.h"
#include "conditions.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "function.h"
#include "expr.h"
#include "optabs.h"
#include "libfuncs.h"
#include "recog.h"
#include "diagnostic-core.h"
#include "toplev.h"
#include "reload.h"
#include "df.h"
#include "ggc.h"
#include "tm_p.h"
#include "debug.h"
#include "target.h"
#include "target-def.h"
#include "langhooks.h"
#include "msp430-protos.h"
#include "dumpfile.h"
#include "opts.h"



static void msp430_compute_frame_info (void);



/* Run-time Target Specification */

bool msp430x = false;

struct GTY(()) machine_function
{
  /* If set, the rest of the fields have been computed.  */
  int computed;
  /* Which registers need to be saved in the pro/epilogue.  */
  int need_to_save [FIRST_PSEUDO_REGISTER];

  /* These fields describe the frame layout...  */
  /* arg pointer */
  /* 2/4 bytes for saved PC */
  int framesize_regs;
  /* frame pointer */
  int framesize_locals;
  int framesize_outgoing;
  /* stack pointer */
  int framesize;

  /* How much we adjust the stack when returning from an exception
     handler.  */
  rtx eh_stack_adjust;
};

/* This is our init_machine_status, as set in
   msp_option_override.  */
static struct machine_function *
msp430_init_machine_status (void)
{
  struct machine_function *m;

  m = ggc_alloc_cleared_machine_function ();

  return m;
}

#undef  TARGET_HANDLE_OPTION
#define TARGET_HANDLE_OPTION msp430_handle_option

bool
msp430_handle_option (struct gcc_options *opts ATTRIBUTE_UNUSED,
		      struct gcc_options *opts_set ATTRIBUTE_UNUSED,
		      const struct cl_decoded_option *decoded ATTRIBUTE_UNUSED,
		      location_t loc ATTRIBUTE_UNUSED)
{
  return true;
}

#undef  TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE		msp430_option_override

static void
msp430_option_override (void)
{
  init_machine_status = msp430_init_machine_status;

  if (target_cpu
      && (strstr (target_cpu, "430x")
	  || strstr (target_cpu, "430X")))
    msp430x = true;

  if (TARGET_LARGE && !msp430x)
    error ("-mlarge requires a 430X-compatible -mmcu=");

  if (flag_exceptions || flag_non_call_exceptions
      || flag_unwind_tables || flag_asynchronous_unwind_tables)
    flag_omit_frame_pointer = false;
  else
    flag_omit_frame_pointer = true;

  /* This is a hack to work around a problem with the newlib build
     mechanism.  Newlib always appends CFLAGS to the end of the GCC
     command line and always sets -O2 in CFLAGS.  Thus it is not
     possible to build newlib with -Os enabled.  Until now...  */
  if (TARGET_OPT_SPACE && optimize < 3)
    optimize_size = 1;
}



/* Storage Layout */

#undef  TARGET_MS_BITFIELD_LAYOUT_P
#define TARGET_MS_BITFIELD_LAYOUT_P msp430_ms_bitfield_layout_p

bool
msp430_ms_bitfield_layout_p (const_tree record_type ATTRIBUTE_UNUSED)
{
  return false;
}



/* Register Usage */

/* Implements HARD_REGNO_NREGS.  MSP430X registers can hold a single
   PSImode value, but not an SImode value.  */
int
msp430_hard_regno_nregs (int regno ATTRIBUTE_UNUSED,
			 enum machine_mode mode)
{
  if (mode == PSImode && msp430x)
    return 1;
  return ((GET_MODE_SIZE (mode) + UNITS_PER_WORD - 1)
	  / UNITS_PER_WORD);
}

/* Implements HARD_REGNO_MODE_OK.  */
int
msp430_hard_regno_mode_ok (int regno ATTRIBUTE_UNUSED,
			   enum machine_mode mode)
{
  return regno <= (ARG_POINTER_REGNUM - msp430_hard_regno_nregs (regno, mode));
}

/* Implements MODES_TIEABLE_P.  */
bool
msp430_modes_tieable_p (enum machine_mode mode1, enum machine_mode mode2)
{
  if ((mode1 == PSImode || mode2 == SImode)
      || (mode1 == SImode || mode2 == PSImode))
    return false;

  return ((GET_MODE_CLASS (mode1) == MODE_FLOAT
	   || GET_MODE_CLASS (mode1) == MODE_COMPLEX_FLOAT)
	  == (GET_MODE_CLASS (mode2) == MODE_FLOAT
	      || GET_MODE_CLASS (mode2) == MODE_COMPLEX_FLOAT));
}

#undef  TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED msp430_frame_pointer_required

static bool
msp430_frame_pointer_required (void)
{
  return false;
}

#undef  TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE		msp430_can_eliminate

static bool
msp430_can_eliminate (const int from_reg ATTRIBUTE_UNUSED,
		      const int to_reg ATTRIBUTE_UNUSED)
{
  return true;
}

/* Implements INITIAL_ELIMINATION_OFFSET.  */
int
msp430_initial_elimination_offset (int from, int to)
{
  int rv = 0; /* As if arg to arg.  */

  msp430_compute_frame_info ();

  switch (to)
    {
    case STACK_POINTER_REGNUM:
      rv += cfun->machine->framesize_outgoing;
      rv += cfun->machine->framesize_locals;
      /* Fall through.  */
    case FRAME_POINTER_REGNUM:
      rv += cfun->machine->framesize_regs;
      /* Allow for the saved return address.  */
      rv += (TARGET_LARGE ? 4 : 2);
      /* NB/ No need to allow for crtl->args.pretend_args_size.
         GCC does that for us.  */
      break;
    default:
      gcc_unreachable ();
    }

  switch (from)
    {
    case FRAME_POINTER_REGNUM:
      /* Allow for the fall through above.  */
      rv -= (TARGET_LARGE ? 4 : 2);
      rv -= cfun->machine->framesize_regs;
    case ARG_POINTER_REGNUM:
      break;
    default:
      gcc_unreachable ();
    }

  return rv;
}

/* Named Address Space support */


/* Return the appropriate mode for a named address pointer.  */
#undef  TARGET_ADDR_SPACE_POINTER_MODE
#define TARGET_ADDR_SPACE_POINTER_MODE msp430_addr_space_pointer_mode
#undef  TARGET_ADDR_SPACE_ADDRESS_MODE
#define TARGET_ADDR_SPACE_ADDRESS_MODE msp430_addr_space_pointer_mode

static enum machine_mode
msp430_addr_space_pointer_mode (addr_space_t addrspace)
{
  switch (addrspace)
    {
    default:
    case ADDR_SPACE_GENERIC:
      return Pmode;
    case ADDR_SPACE_NEAR:
      return HImode;
    case ADDR_SPACE_FAR:
      return PSImode;
    }
}

/* Function pointers are stored in unwind_word sized
   variables, so make sure that unwind_word is big enough.  */
#undef  TARGET_UNWIND_WORD_MODE
#define TARGET_UNWIND_WORD_MODE msp430_unwind_word_mode

static enum machine_mode
msp430_unwind_word_mode (void)
{
  return TARGET_LARGE ? SImode : HImode;
}

/* Determine if one named address space is a subset of another.  */
#undef  TARGET_ADDR_SPACE_SUBSET_P
#define TARGET_ADDR_SPACE_SUBSET_P msp430_addr_space_subset_p
static bool
msp430_addr_space_subset_p (addr_space_t subset, addr_space_t superset)
{
  if (subset == superset)
    return true;
  else
    return (subset != ADDR_SPACE_FAR && superset == ADDR_SPACE_FAR);
}

#undef  TARGET_ADDR_SPACE_CONVERT
#define TARGET_ADDR_SPACE_CONVERT msp430_addr_space_convert
/* Convert from one address space to another.  */
static rtx
msp430_addr_space_convert (rtx op, tree from_type, tree to_type)
{
  addr_space_t from_as = TYPE_ADDR_SPACE (TREE_TYPE (from_type));
  addr_space_t to_as = TYPE_ADDR_SPACE (TREE_TYPE (to_type));
  rtx result;

  if (to_as != ADDR_SPACE_FAR && from_as == ADDR_SPACE_FAR)
    {
      /* This is unpredictable, as we're truncating off usable address
	 bits.  */

      if (CONSTANT_P (op))
	return gen_rtx_CONST (HImode, op);

      result = gen_reg_rtx (HImode);
      emit_insn (gen_truncpsihi2 (result, op));
      return result;
    }
  else if (to_as == ADDR_SPACE_FAR && from_as != ADDR_SPACE_FAR)
    {
      /* This always works.  */

      if (CONSTANT_P (op))
	return gen_rtx_CONST (PSImode, op);

      result = gen_reg_rtx (PSImode);
      emit_insn (gen_zero_extendhipsi2 (result, op));
      return result;
    }
  else
    gcc_unreachable ();
}

/* Stack Layout and Calling Conventions.  */

/* For each function, we list the gcc version and the TI version on
   each line, where we're converting the function names.  */
static char const * const special_convention_function_names [] =
{
  "__muldi3", "__mspabi_mpyll",
  "__udivdi3", "__mspabi_divull",
  "__umoddi3", "__mspabi_remull",
  "__divdi3", "__mspabi_divlli",
  "__moddi3", "__mspabi_remlli",
  "__mspabi_srall",
  "__mspabi_srlll",
  "__mspabi_sllll",
  "__adddf3", "__mspabi_addd",
  "__subdf3", "__mspabi_subd",
  "__muldf3", "__mspabi_mpyd",
  "__divdf3", "__mspabi_divd",
  "__mspabi_cmpd",
  NULL
};

/* TRUE if the function passed is a "speical" function.  Special
   functions pass two DImode parameters in registers.  */
static bool
msp430_special_register_convention_p (const char *name)
{
  int i;

  for (i = 0; special_convention_function_names [i]; i++)
    if (! strcmp (name, special_convention_function_names [i]))
      return true;

  return false;
}

#undef  TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P msp430_function_value_regno_p

bool
msp430_function_value_regno_p (unsigned int regno)
{
  return regno == 12;
}


#undef  TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE msp430_function_value

rtx
msp430_function_value (const_tree ret_type,
		       const_tree fn_decl_or_type ATTRIBUTE_UNUSED,
		       bool outgoing ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (TYPE_MODE (ret_type), 12);
}

#undef  TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE msp430_libcall_value

rtx
msp430_libcall_value (enum machine_mode mode, const_rtx fun ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (mode, 12);
}

/* Implements INIT_CUMULATIVE_ARGS.  */
void
msp430_init_cumulative_args (CUMULATIVE_ARGS *ca,
			     tree fntype ATTRIBUTE_UNUSED,
			     rtx libname ATTRIBUTE_UNUSED,
			     tree fndecl ATTRIBUTE_UNUSED,
			     int n_named_args ATTRIBUTE_UNUSED)
{
  const char *fname;
  memset (ca, 0, sizeof(*ca));

  ca->can_split = 1;

  if (fndecl)
    fname = IDENTIFIER_POINTER (DECL_NAME (fndecl));
  else if (libname)
    fname = XSTR (libname, 0);
  else
    fname = NULL;

  if (fname && msp430_special_register_convention_p (fname))
    ca->special_p = 1;
}

/* Helper function for argument passing; this function is the common
   code that determines where an argument will be passed.  */
static void
msp430_evaluate_arg (cumulative_args_t cap,
		     enum machine_mode mode,
		     const_tree type ATTRIBUTE_UNUSED,
		     bool named)
{
  CUMULATIVE_ARGS *ca = get_cumulative_args (cap);
  int nregs = GET_MODE_SIZE (mode);
  int i;

  ca->reg_count = 0;
  ca->mem_count = 0;

  if (!named)
    return;

  if (mode == PSImode)
    nregs = 1;
  else
    nregs = (nregs + 1) / 2;

  if (ca->special_p)
    {
      /* Function is passed two DImode operands, in R8:R11 and
	 R12:15.  */
      ca->start_reg = 8;
      ca->reg_count = 4;
      return;
    }

  switch (nregs)
    {
    case 1:
      for (i = 0; i < 4; i++)
	if (! ca->reg_used [i])
	  {
	    ca->reg_count = 1;
	    ca->start_reg = CA_FIRST_REG + i;
	    return;
	  }
      break;
    case 2:
      for (i = 0; i < 3; i++)
	if (! ca->reg_used [i] && ! ca->reg_used [i + 1])
	  {
	    ca->reg_count = 2;
	    ca->start_reg = CA_FIRST_REG + i;
	    return;
	  }
      if (! ca->reg_used [3] && ca->can_split)
	{
	  ca->reg_count = 1;
	  ca->mem_count = 2;
	  ca->start_reg = CA_FIRST_REG + 3;
	  return;
	}
      break;
    case 3:
    case 4:
      ca->can_split = 0;
      if (! ca->reg_used [0]
	  && ! ca->reg_used [1]
	  && ! ca->reg_used [2]
	  && ! ca->reg_used [3])
	{
	  ca->reg_count = 4;
	  ca->start_reg = CA_FIRST_REG;
	  return;
	}
      break;
    }
}

#undef  TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES msp430_promote_prototypes

bool
msp430_promote_prototypes (const_tree fntype ATTRIBUTE_UNUSED)
{
  return false;
}

#undef  TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG msp430_function_arg

rtx
msp430_function_arg (cumulative_args_t cap,
		     enum machine_mode mode,
		     const_tree type,
		     bool named)
{
  CUMULATIVE_ARGS *ca = get_cumulative_args (cap);

  msp430_evaluate_arg (cap, mode, type, named);

  if (ca->reg_count)
    return gen_rtx_REG (mode, ca->start_reg);

  return 0;
}

#undef  TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES msp430_arg_partial_bytes

int
msp430_arg_partial_bytes (cumulative_args_t cap,
			  enum machine_mode mode,
			  tree type,
			  bool named)
{
  CUMULATIVE_ARGS *ca = get_cumulative_args (cap);

  msp430_evaluate_arg (cap, mode, type, named);

  if (ca->reg_count && ca->mem_count)
    return ca->reg_count * UNITS_PER_WORD;

  return 0;
}

#undef  TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE msp430_pass_by_reference

static bool
msp430_pass_by_reference (cumulative_args_t cap ATTRIBUTE_UNUSED,
			  enum machine_mode mode,
			  const_tree type,
			  bool named ATTRIBUTE_UNUSED)
{
  return (mode == BLKmode
	  || (type && TREE_CODE (type) == RECORD_TYPE)
	  || (type && TREE_CODE (type) == UNION_TYPE));
}

#undef  TARGET_CALLEE_COPIES
#define TARGET_CALLEE_COPIES msp430_callee_copies

static bool
msp430_callee_copies (cumulative_args_t cap ATTRIBUTE_UNUSED,
                     enum machine_mode mode ATTRIBUTE_UNUSED,
                     const_tree type ATTRIBUTE_UNUSED,
                     bool named ATTRIBUTE_UNUSED)
{
  return true;
}

#undef  TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE msp430_function_arg_advance

void
msp430_function_arg_advance (cumulative_args_t cap,
			     enum machine_mode mode,
			     const_tree type,
			     bool named)
{
  CUMULATIVE_ARGS *ca = get_cumulative_args (cap);
  int i;

  msp430_evaluate_arg (cap, mode, type, named);

  if (ca->start_reg >= CA_FIRST_REG)
    for (i = 0; i < ca->reg_count; i ++)
      ca->reg_used [i + ca->start_reg - CA_FIRST_REG] = 1;

  ca->special_p = 0;
}

#undef  TARGET_FUNCTION_ARG_BOUNDARY
#define TARGET_FUNCTION_ARG_BOUNDARY msp430_function_arg_boundary

static unsigned int
msp430_function_arg_boundary (enum machine_mode mode, const_tree type)
{
  if (mode == BLKmode
      && int_size_in_bytes (type) > 1)
    return 16;
  if (GET_MODE_BITSIZE (mode) > 8)
    return 16;
  return 8;
}

#undef  TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY msp430_return_in_memory

static bool
msp430_return_in_memory (const_tree ret_type, const_tree fntype ATTRIBUTE_UNUSED)
{
  enum machine_mode mode = TYPE_MODE (ret_type);

  if (mode == BLKmode
      || (fntype && TREE_CODE (TREE_TYPE (fntype)) == RECORD_TYPE)
      || (fntype && TREE_CODE (TREE_TYPE (fntype)) == UNION_TYPE))
    return true;

  if (GET_MODE_SIZE (mode) > 8)
    return true;

  return false;
}

#undef  TARGET_GET_RAW_ARG_MODE
#define TARGET_GET_RAW_ARG_MODE msp430_get_raw_arg_mode

static enum machine_mode
msp430_get_raw_arg_mode (int regno)
{
  return (regno == ARG_POINTER_REGNUM) ? VOIDmode : Pmode;
}

#undef  TARGET_GET_RAW_RESULT_MODE
#define TARGET_GET_RAW_RESULT_MODE msp430_get_raw_result_mode

static enum machine_mode
msp430_get_raw_result_mode (int regno ATTRIBUTE_UNUSED)
{
  return Pmode;
}

/* Addressing Modes */

#undef  TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P msp430_legitimate_address_p

static bool
reg_ok_for_addr (rtx r, bool strict)
{
  int rn = REGNO (r);

  if (strict && rn >= FIRST_PSEUDO_REGISTER)
    rn = reg_renumber [rn];
  if (strict && 0 <= rn && rn < FIRST_PSEUDO_REGISTER)
    return true;
  if (!strict)
    return true;
  return false;
}

bool
msp430_legitimate_address_p (enum machine_mode mode ATTRIBUTE_UNUSED,
			     rtx x ATTRIBUTE_UNUSED,
			     bool strict ATTRIBUTE_UNUSED)
{
  switch (GET_CODE (x))
    {
    case MEM:
      return false;

    case PLUS:
      if (REG_P (XEXP (x, 0)))
	{
	  if (GET_MODE (x) != GET_MODE (XEXP (x, 0)))
	    return false;
	  if (!reg_ok_for_addr (XEXP (x, 0), strict))
	    return false;
	  switch (GET_CODE (XEXP (x, 1)))
	    {
	    case CONST:
	    case SYMBOL_REF:
	    case CONST_INT:
	      return true;
	    default:
	      return false;
	    }
	}
      return false;

    case REG:
      if (!reg_ok_for_addr (x, strict))
	return false;
      /* else... */
    case CONST:
    case SYMBOL_REF:
    case CONST_INT:
      return true;

    default:
      return false;
    }
}

#undef  TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P msp430_legitimate_constant

static bool
msp430_legitimate_constant (enum machine_mode mode, rtx x)
{
  return ! CONST_INT_P (x)
    || mode != PSImode
    /* GCC does not know the width of the PSImode, so make
       sure that it does not try to use a constant value that
       is out of range.  */
    || (INTVAL (x) < (1 << 20) && INTVAL (x) >= (-1 << 20));
}


#undef  TARGET_RTX_COSTS
#define TARGET_RTX_COSTS msp430_rtx_costs

static bool msp430_rtx_costs (rtx   x ATTRIBUTE_UNUSED,
			      int   code,
			      int   outer_code ATTRIBUTE_UNUSED,
			      int   opno ATTRIBUTE_UNUSED,
			      int * total,
			      bool  speed ATTRIBUTE_UNUSED)
{
  switch (code)
    {
    case SIGN_EXTEND:
      if (GET_MODE (x) == SImode && outer_code == SET)
	{
	  *total = COSTS_N_INSNS (4);
	  return true;
	}
      break;
    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
      if (!msp430x)
	{
	  *total = COSTS_N_INSNS (100);
	  return true;
	}
      break;
    }
  return false;
}

/* Function Entry and Exit */

/* The MSP430 call frame looks like this:

   <higher addresses>
   +--------------------+
   |                    |
   | Stack Arguments    |
   |                    |
   +--------------------+ <-- "arg pointer"
   |                    |
   | PC from call       |  (2 bytes for 430, 4 for TARGET_LARGE)
   |                    |
   +--------------------+
   | SR if this func has|
   | been called via an |
   | interrupt.         |
   +--------------------+  <-- SP before prologue, also AP
   |                    |
   | Saved Regs         |  (2 bytes per reg for 430, 4 per for TARGET_LARGE)
   |                    |
   +--------------------+  <-- "frame pointer"
   |                    |
   | Locals             |
   |                    |
   +--------------------+
   |                    |
   | Outgoing Args      |
   |                    |
   +--------------------+  <-- SP during function
   <lower addresses>

*/

/* We use this to wrap all emitted insns in the prologue, so they get
   the "frame-related" (/f) flag set.  */
static rtx
F (rtx x)
{
  RTX_FRAME_RELATED_P (x) = 1;
  return x;
}

/* This is the one spot that decides if a register is to be saved and
   restored in the prologue/epilogue.  */
static bool
msp430_preserve_reg_p (int regno)
{
  /* PC, SP, SR, and the constant generator.  */
  if (regno <= 3)
    return false;

  /* FIXME: add interrupt, EH, etc.  */
  if (crtl->calls_eh_return)
    return true;

  /* Shouldn't be more than the above, but just in case...  */
  if (fixed_regs [regno])
    return false;

  /* Interrupt handlers save all registers they use, even
     ones which are call saved.  If they call other functions
     then *every* register is saved.  */
  if (msp430_is_interrupt_func ())
    return ! crtl->is_leaf || df_regs_ever_live_p (regno);

  if (!call_used_regs [regno]
      && df_regs_ever_live_p (regno))
    return true;

  return false;
}

/* Compute all the frame-related fields in our machine_function
   structure.  */
static void
msp430_compute_frame_info (void)
{
  int i;

  cfun->machine->computed = 1;
  cfun->machine->framesize_regs = 0;
  cfun->machine->framesize_locals = get_frame_size ();
  cfun->machine->framesize_outgoing = crtl->outgoing_args_size;

  for (i = 0; i < ARG_POINTER_REGNUM; i ++)
    if (msp430_preserve_reg_p (i))
      {
	cfun->machine->need_to_save [i] = 1;
	cfun->machine->framesize_regs += (TARGET_LARGE ? 4 : 2);
      }
    else
      cfun->machine->need_to_save [i] = 0;

  if ((cfun->machine->framesize_locals + cfun->machine->framesize_outgoing) & 1)
    cfun->machine->framesize_locals ++;

  cfun->machine->framesize = (cfun->machine->framesize_regs
			      + cfun->machine->framesize_locals
			      + cfun->machine->framesize_outgoing);
}

static inline bool
is_attr_func (const char * attr)
{
  return lookup_attribute (attr, DECL_ATTRIBUTES (current_function_decl)) != NULL_TREE;
}

/* Returns true if the current function has the "interrupt" attribute.  */

bool
msp430_is_interrupt_func (void)
{
  return is_attr_func ("interrupt");
}

static inline bool
is_naked_func (void)
{
  return is_attr_func ("naked");
}

static inline bool
is_reentrant_func (void)
{
  return is_attr_func ("reentrant");
}

static inline bool
is_critical_func (void)
{
  return is_attr_func ("critical");
}

#undef  TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE	msp430_start_function

static void
msp430_start_function (FILE *outfile, HOST_WIDE_INT hwi_local ATTRIBUTE_UNUSED)
{
  int r, n;

  fprintf (outfile, "; start of function\n");

  if (DECL_ATTRIBUTES (current_function_decl) != NULL_TREE)
    {
      fprintf (outfile, "; attributes: ");
      if (is_naked_func ())
	fprintf (outfile, "naked ");
      if (msp430_is_interrupt_func ())
	fprintf (outfile, "interrupt ");
      if (is_reentrant_func ())
	fprintf (outfile, "reentrant ");
      if (is_critical_func ())
	fprintf (outfile, "critical ");
      fprintf (outfile, "\n");
    }

  fprintf (outfile, "; framesize_regs:     %d\n", cfun->machine->framesize_regs);
  fprintf (outfile, "; framesize_locals:   %d\n", cfun->machine->framesize_locals);
  fprintf (outfile, "; framesize_outgoing: %d\n", cfun->machine->framesize_outgoing);
  fprintf (outfile, "; framesize:          %d\n", cfun->machine->framesize);
  fprintf (outfile, "; elim ap -> fp       %d\n", msp430_initial_elimination_offset (ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM));
  fprintf (outfile, "; elim fp -> sp       %d\n", msp430_initial_elimination_offset (FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM));

  n = 0;
  fprintf (outfile, "; saved regs:");
  for (r = 0; r < ARG_POINTER_REGNUM; r++)
    if (cfun->machine->need_to_save [r])
      {
	fprintf (outfile, " %s", reg_names [r]);
	n = 1;
      }
  if (n == 0)
    fprintf (outfile, "(none)");
  fprintf (outfile, "\n");
}

/* Common code to change the stack pointer.  */
static void
increment_stack (HOST_WIDE_INT amount)
{
  rtx inc;
  rtx sp =  stack_pointer_rtx;

  if (amount == 0)
    return;

  if (amount < 0)
    {
      inc = GEN_INT (- amount);
      if (TARGET_LARGE)
	F (emit_insn (gen_subpsi3 (sp, sp, inc)));
      else
	F (emit_insn (gen_subhi3 (sp, sp, inc)));
    }
  else
    {
      inc = GEN_INT (amount);
      if (TARGET_LARGE)
	emit_insn (gen_addpsi3 (sp, sp, inc));
      else
	emit_insn (gen_addhi3 (sp, sp, inc));
    }
}

/* Verify MSP430 specific attributes.  */

static tree
msp430_attr (tree * node,
	     tree   name,
	     tree   args,
	     int    flags ATTRIBUTE_UNUSED,
	     bool * no_add_attrs)
{
  gcc_assert (DECL_P (* node));

  if (args != NULL)
    {
      tree value = TREE_VALUE (args);

      switch (TREE_CODE (value))
	{
	case STRING_CST:
	  if (   strcmp (TREE_STRING_POINTER (value), "reset")
	      && strcmp (TREE_STRING_POINTER (value), "nmi")
	      && strcmp (TREE_STRING_POINTER (value), "watchdog"))
	    /* Allow the attribute to be added - the linker script
	       being used may still recognise this name.  */
	    warning (OPT_Wattributes,
		     "unrecognised interrupt vector argument of %qE attribute",
		     name);
	  break;

	case INTEGER_CST:
	  if (TREE_INT_CST_LOW (value) > 31)
	    /* Allow the attribute to be added - the linker script
	       being used may still recognise this value.  */
	    warning (OPT_Wattributes,
		     "numeric argument of %qE attribute must be in range 0..31",
		     name);
	  break;

	default:
	  warning (OPT_Wattributes,
		   "argument of %qE attribute is not a string constant or number",
		   name);
	  *no_add_attrs = true;
	  break;
	}
    }

  if (TREE_CODE (* node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes,
	       "%qE attribute only applies to functions",
	       name);
      * no_add_attrs = true;
    }

  /* FIXME: We ought to check that the interrupt handler
     attribute has been applied to a void function.  */
  /* FIXME: We should check that reentrant and critical
     functions are not naked and that critical functions
     are not reentrant.  */

  return NULL_TREE;
}

#undef  TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE		msp430_attribute_table

/* Table of MSP430-specific attributes.  */
const struct attribute_spec msp430_attribute_table[] =
{
  /* Name          min_len  decl_req,    fn_type_req,    affects_type_identity
                       max_len,  type_req,        handler.  */
  { "interrupt",      0, 1, true,  false, false, msp430_attr, false },
  { "naked",          0, 0, true,  false, false, msp430_attr, false },
  { "reentrant",      0, 0, true,  false, false, msp430_attr, false },
  { "critical",       0, 0, true,  false, false, msp430_attr, false },
  { NULL,             0, 0, false, false, false, NULL,        false }
};

void
msp430_start_function (FILE *file, const char *name, tree decl)
{
  tree int_attr;

  int_attr = lookup_attribute ("interrupt", DECL_ATTRIBUTES (decl));
  if (int_attr != NULL_TREE)
    {
      tree intr_vector = TREE_VALUE (int_attr);

      if (intr_vector != NULL_TREE)
	{
	  char buf[101];

	  intr_vector = TREE_VALUE (intr_vector);

	  /* The interrupt attribute has a vector value.  Turn this into a
	     section name, switch to that section and put the address of
	     the current function into that vector slot.  Note msp430_attr()
	     has already verified the vector name for us.  */
	  if (TREE_CODE (intr_vector) == STRING_CST)
	    sprintf (buf, "__interrupt_vector_%.80s",
		     TREE_STRING_POINTER (intr_vector));
	  else /* TREE_CODE (intr_vector) == INTEGER_CST */
	    sprintf (buf, "__interrupt_vector_%u",
		     (unsigned int) TREE_INT_CST_LOW (intr_vector));

	  switch_to_section (get_section (buf, SECTION_CODE, decl));
	  fputs ("\t.word\t", file);
	  assemble_name (file, name);
	  fputc ('\n', file);
	  fputc ('\t', file);
	}
    }

  switch_to_section (function_section (decl));
  ASM_OUTPUT_FUNCTION_LABEL (file, name, decl);
}

static section *
msp430_function_section (tree decl, enum node_frequency freq, bool startup, bool exit)
{
  /* In large mode we must make sure that interrupt handlers are put into
     low memory as the vector table only accepts 16-bit addresses.  */
  if (TARGET_LARGE
      && lookup_attribute ("interrupt", DECL_ATTRIBUTES (decl)))
    return get_section (".lowtext", SECTION_CODE | SECTION_WRITE , decl);

  /* Otherwise, use the default function section.  */
  return default_function_section (decl, freq, startup, exit);
}

#undef  TARGET_ASM_FUNCTION_SECTION
#define TARGET_ASM_FUNCTION_SECTION msp430_function_section

enum msp430_builtin
{
  MSP430_BUILTIN_BIC_SR,
  MSP430_BUILTIN_BIS_SR,
  MSP430_BUILTIN_max
};

static GTY(()) tree msp430_builtins [(int) MSP430_BUILTIN_max];

static void
msp430_init_builtins (void)
{
  tree void_ftype_int = build_function_type_list (void_type_node, integer_type_node, NULL);

  msp430_builtins[MSP430_BUILTIN_BIC_SR] =
    add_builtin_function ( "__bic_SR_register_on_exit", void_ftype_int,
			   MSP430_BUILTIN_BIC_SR, BUILT_IN_MD, NULL, NULL_TREE);

  msp430_builtins[MSP430_BUILTIN_BIS_SR] =
    add_builtin_function ( "__bis_SR_register_on_exit", void_ftype_int,
			   MSP430_BUILTIN_BIS_SR, BUILT_IN_MD, NULL, NULL_TREE);
}

static tree
msp430_builtin_decl (unsigned code, bool initialize ATTRIBUTE_UNUSED)
{
  switch (code)
    {
    case MSP430_BUILTIN_BIC_SR:
    case MSP430_BUILTIN_BIS_SR:
      return msp430_builtins[code];
    default:
      return error_mark_node;
    }
}

static rtx
msp430_expand_builtin (tree exp,
		       rtx target ATTRIBUTE_UNUSED,
		       rtx subtarget ATTRIBUTE_UNUSED,
		       enum machine_mode mode ATTRIBUTE_UNUSED,
		       int ignore ATTRIBUTE_UNUSED)
{
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  unsigned int fcode = DECL_FUNCTION_CODE (fndecl);
  rtx arg1 = expand_normal (CALL_EXPR_ARG (exp, 0));

  if (! msp430_is_interrupt_func ())
    {
      error ("MSP430 builtin functions only work inside interrupt handlers");
      return NULL_RTX;
    }

  if (! REG_P (arg1) && ! CONSTANT_P (arg1))
    arg1 = force_reg (mode, arg1);

  switch (fcode)
    {
    case MSP430_BUILTIN_BIC_SR:  emit_insn (gen_bic_SR (arg1)); break;
    case MSP430_BUILTIN_BIS_SR:  emit_insn (gen_bis_SR (arg1)); break;
    default:
      internal_error ("bad builtin code");
      break;
    }
  return NULL_RTX;
}

#undef  TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS  msp430_init_builtins

#undef  TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN msp430_expand_builtin

#undef  TARGET_BUILTIN_DECL
#define TARGET_BUILTIN_DECL   msp430_builtin_decl

void
msp430_expand_prologue (void)
{
  int i, j;
  int fs;
  /* Always use stack_pointer_rtx instead of calling
     rtx_gen_REG ourselves.  Code elsewhere in GCC assumes
     that there is a single rtx representing the stack pointer,
     namely stack_pointer_rtx, and uses == to recognize it.  */
  rtx sp = stack_pointer_rtx;
  rtx p;

  if (is_naked_func ())
    return;

  emit_insn (gen_prologue_start_marker ());

  if (is_critical_func ())
    {
      emit_insn (gen_push_intr_state ());
      emit_insn (gen_disable_interrupts ());
    }
  else if (is_reentrant_func ())
    emit_insn (gen_disable_interrupts ());

  if (!cfun->machine->computed)
    msp430_compute_frame_info ();

  if (flag_stack_usage_info)
    current_function_static_stack_size = cfun->machine->framesize;
  
  if (crtl->args.pretend_args_size)
    {
      rtx note;

      gcc_assert (crtl->args.pretend_args_size == 2);
      
      p = emit_insn (gen_grow_and_swap ());

      /* Document the stack decrement...  */
      note = F (gen_rtx_SET (Pmode, stack_pointer_rtx,
			     gen_rtx_MINUS (Pmode, stack_pointer_rtx, GEN_INT (2))));
      add_reg_note (p, REG_FRAME_RELATED_EXPR, note);

      /* ...and the establishment of a new location for the return address.  */
      note = F (gen_rtx_SET (Pmode, gen_rtx_MEM (Pmode,
						 gen_rtx_PLUS (Pmode, stack_pointer_rtx, GEN_INT (-2))),
			     pc_rtx));
      add_reg_note (p, REG_CFA_OFFSET, note);
      F (p);
    }

  for (i = 15; i >= 4; i--)
    if (cfun->machine->need_to_save [i])
      {
	int seq, count;
	rtx note;

	for (seq = i - 1; seq >= 4 && cfun->machine->need_to_save[seq]; seq --)
	  ;
	count = i - seq;

	if (msp430x)
	  {
	    /* Note: with TARGET_LARGE we still use PUSHM as PUSHX.A is two bytes bigger.  */
	    p = F (emit_insn (gen_pushm (gen_rtx_REG (Pmode, i),
					 GEN_INT (count))));

	    note = gen_rtx_SEQUENCE (VOIDmode, rtvec_alloc (count + 1));

	    XVECEXP (note, 0, 0)
	      = F (gen_rtx_SET (VOIDmode,
			     stack_pointer_rtx,
			     gen_rtx_PLUS (Pmode,
					   stack_pointer_rtx,
					   GEN_INT (count * (TARGET_LARGE ? -4 : -2)))));

	    /* *sp-- = R[i-j] */
	    /* sp+N	R10
	       ...
	       sp	R4  */
	    for (j = 0; j < count; j ++)
	      {
		rtx addr;
		int ofs = (count - j - 1) * (TARGET_LARGE ? 4 : 2);

		if (ofs)
		  addr = gen_rtx_PLUS (Pmode, sp, GEN_INT (ofs));
		else
		  addr = stack_pointer_rtx;

		XVECEXP (note, 0, j + 1) = 
		  F (gen_rtx_SET (VOIDmode,
				  gen_rtx_MEM (Pmode, addr),
				  gen_rtx_REG (Pmode, i - j)) );
	      }

	    add_reg_note (p, REG_FRAME_RELATED_EXPR, note);
	    i -= count - 1;
	  }
	else
	  F (emit_insn (gen_push (gen_rtx_REG (Pmode, i))));
      }

  if (frame_pointer_needed)
    F (emit_move_insn (gen_rtx_REG (Pmode, FRAME_POINTER_REGNUM), sp));

  fs = cfun->machine->framesize_locals + cfun->machine->framesize_outgoing;

  increment_stack (- fs);

  emit_insn (gen_prologue_end_marker ());
}

void
msp430_expand_epilogue (int is_eh)
{
  int i;
  int fs;
  int helper_n = 0;

  if (is_naked_func ())
    return;

  if (cfun->machine->need_to_save [10])
    {
      /* Check for a helper function.  */
      helper_n = 7; /* for when the loop below never sees a match.  */
      for (i = 9; i >= 4; i--)
	if (!cfun->machine->need_to_save [i])
	  {
	    helper_n = 10 - i;
	    for (; i >= 4; i--)
	      if (cfun->machine->need_to_save [i])
		{
		  helper_n = 0;
		  break;
		}
	    break;
	  }
    }

  emit_insn (gen_epilogue_start_marker ());

  fs = cfun->machine->framesize_locals + cfun->machine->framesize_outgoing;

  increment_stack (fs);

  if (is_eh)
    {
      /* We need to add the right "SP" register save just after the
	 regular ones, so that when we pop it off we're in the EH
	 return frame, not this one.  This overwrites our own return
	 address, but we're not going to be returning anyway.  */
      rtx r12 = gen_rtx_REG (Pmode, 12);
      rtx (*addPmode)(rtx, rtx, rtx) = TARGET_LARGE ? gen_addpsi3 : gen_addhi3;

      /* R12 will hold the new SP.  */
      i = cfun->machine->framesize_regs;
      emit_move_insn (r12, stack_pointer_rtx);
      emit_insn (addPmode (r12, r12, EH_RETURN_STACKADJ_RTX));
      emit_insn (addPmode (r12, r12, GEN_INT (i)));
      emit_move_insn (gen_rtx_MEM (Pmode, plus_constant (Pmode, stack_pointer_rtx, i)), r12);
    }

  for (i = 4; i <= 15; i++)
    if (cfun->machine->need_to_save [i])
      {
	int seq, count;

	for (seq = i + 1; seq <= 15 && cfun->machine->need_to_save[seq]; seq ++)
	  ;
	count = seq - i;

	if (msp430x)
	  {
	    /* Note: With TARGET_LARGE we still use POPM as POPX.A is two
	       bytes bigger.
	       Note: See the popm pattern for the explanation of the strange
	       arguments.  */
	    emit_insn (gen_popm (stack_pointer_rtx, GEN_INT (~(seq - 1)),
				 GEN_INT (count)));
	    i += count - 1;
	  }
	else if (i == 11 - helper_n
		 && ! msp430_is_interrupt_func ()
		 && ! is_reentrant_func ()
		 && ! is_critical_func ()
		 && crtl->args.pretend_args_size == 0
		 /* Calling the helper takes as many bytes as the POP;RET sequence.  */
		 && helper_n != 1
		 && !is_eh)
	  {
	    emit_insn (gen_epilogue_helper (GEN_INT (helper_n)));
	    return;
	  }
	else
	  emit_insn (gen_pop (gen_rtx_REG (Pmode, i)));
      }

  if (is_eh)
    {
      /* Also pop SP, which puts us into the EH return frame.  Except
	 that you can't "pop" sp, you have to just load it off the
	 stack.  */
      emit_move_insn (stack_pointer_rtx, gen_rtx_MEM (Pmode, stack_pointer_rtx));
    }

  if (crtl->args.pretend_args_size)
    emit_insn (gen_swap_and_shrink ());

  if (is_critical_func ())
    emit_insn (gen_pop_intr_state ());
  else if (is_reentrant_func ())
    emit_insn (gen_enable_interrupts ());

  emit_jump_insn (gen_msp_return ());
}

/* Implements EH_RETURN_STACKADJ_RTX.  Saved and used later in
   m32c_emit_eh_epilogue.  */
rtx
msp430_eh_return_stackadj_rtx (void)
{
  if (!cfun->machine->eh_stack_adjust)
    {
      rtx sa;

      sa = gen_rtx_REG (Pmode, 15);
      cfun->machine->eh_stack_adjust = sa;
    }
  return cfun->machine->eh_stack_adjust;
}

/* This function is called before reload, to "fix" the stack in
   preparation for an EH return.  */
void
msp430_expand_eh_return (rtx eh_handler)
{
  /* These are all Pmode */
  rtx ap, sa, ra, tmp;

  ap = arg_pointer_rtx;
  sa = msp430_eh_return_stackadj_rtx ();
  ra = eh_handler;

  tmp = ap;
  tmp = gen_rtx_PLUS (Pmode, ap, sa);
  tmp = plus_constant (Pmode, tmp, TARGET_LARGE ? -4 : -2);
  tmp = gen_rtx_MEM (Pmode, tmp);
  emit_move_insn (tmp, ra);
}

/* This is a list of MD patterns that implement fixed-count shifts.  */
static struct
{
  const char *name;
  int count;
  int need_430x;
  rtx (*genfunc)(rtx,rtx);
}
  const_shift_helpers[] =
{
#define CSH(N,C,X,G) { "__mspabi_"N, C, X, gen_##G }

  CSH ("slli", 1, 1, slli_1),
  CSH ("slll", 1, 1, slll_1),
  CSH ("slll", 2, 1, slll_2),

  CSH ("srai", 1, 0, srai_1),
  CSH ("sral", 1, 0, sral_1),
  CSH ("sral", 2, 0, sral_2),

  CSH ("srll", 1, 0, srll_1),
  CSH ("srll", 2, 1, srll_2x),
  { 0, 0, 0, 0 }
#undef CSH
};

/* The MSP430 ABI defines a number of helper functions that should be
   used for, for example, 32-bit shifts.  This function is called to
   emit such a function, using the table above to optimize some
   cases.  */
void
msp430_expand_helper (rtx *operands, const char *helper_name, bool const_variants)
{
  rtx c, f;
  char *helper_const = NULL;
  int arg2 = 13;
  int arg1sz = 1;
  enum machine_mode arg0mode = GET_MODE (operands[0]);
  enum machine_mode arg1mode = GET_MODE (operands[1]);
  enum machine_mode arg2mode = GET_MODE (operands[2]);
  int have_430x = msp430x ? 1 : 0;

  if (CONST_INT_P (operands[2]))
    {
      int i;

      for (i=0; const_shift_helpers[i].name; i++)
	{
	  if (const_shift_helpers[i].need_430x <= have_430x
	      && strcmp (helper_name, const_shift_helpers[i].name) == 0
	      && INTVAL (operands[2]) == const_shift_helpers[i].count)
	    {
	      emit_insn (const_shift_helpers[i].genfunc (operands[0], operands[1]));
	      return;
	    }
	}
    }

  if (arg1mode == VOIDmode)
    arg1mode = arg0mode;
  if (arg2mode == VOIDmode)
    arg2mode = arg0mode;

  if (arg1mode == SImode)
    {
      arg2 = 14;
      arg1sz = 2;
    }

  if (const_variants
      && CONST_INT_P (operands[2])
      && INTVAL (operands[2]) >= 1
      && INTVAL (operands[2]) <= 15)
    {
      /* Note that the INTVAL is limited in value and length by the conditional above.  */
      int len = strlen (helper_name) + 4;
      helper_const = (char *) xmalloc (len);
      snprintf (helper_const, len, "%s_%ld", helper_name, (int) INTVAL (operands[2]));
    }

  emit_move_insn (gen_rtx_REG (arg1mode, 12),
		  operands[1]);
  if (!helper_const)
    emit_move_insn (gen_rtx_REG (arg2mode, arg2),
		    operands[2]);

  c = gen_call_value_internal (gen_rtx_REG (arg0mode, 12),
			       gen_rtx_SYMBOL_REF (VOIDmode, helper_const ? helper_const : helper_name),
			       GEN_INT (0));
  c = emit_call_insn (c);
  RTL_CONST_CALL_P (c) = 1;

  f = 0;
  use_regs (&f, 12, arg1sz);
  if (!helper_const)
    use_regs (&f, arg2, 1);
  add_function_usage_to (c, f);

  emit_move_insn (operands[0],
		  gen_rtx_REG (arg0mode, 12));
}

/* Called by cbranch<mode>4 to coerce operands into usable forms.  */
void
msp430_fixup_compare_operands (enum machine_mode my_mode, rtx * operands)
{
  /* constants we're looking for, not constants which are allowed.  */
  int const_op_idx = 1;

  if (msp430_reversible_cmp_operator (operands[0], VOIDmode))
    const_op_idx = 2;

  if (GET_CODE (operands[const_op_idx]) != REG
      && GET_CODE (operands[const_op_idx]) != MEM)
    operands[const_op_idx] = copy_to_mode_reg (my_mode, operands[const_op_idx]);
}

/* Simplify_gen_subreg() doesn't handle memory references the way we
   need it to below, so we use this function for when we must get a
   valid subreg in a "natural" state.  */
rtx
msp430_subreg (enum machine_mode mode, rtx r, enum machine_mode omode, int byte)
{
  rtx rv;

  if (GET_CODE (r) == SUBREG
      && SUBREG_BYTE (r) == 0)
    {
      rtx ireg = SUBREG_REG (r);
      enum machine_mode imode = GET_MODE (ireg);

      /* special case for (HI (SI (PSI ...), 0)) */
      if (imode == PSImode
	  && mode == HImode
	  && byte == 0)
	rv = gen_rtx_SUBREG (mode, ireg, byte);
      else
	rv = simplify_gen_subreg (mode, ireg, imode, byte);
    }
  else if (GET_CODE (r) == MEM)
    rv = adjust_address (r, mode, byte);
  else
    rv = simplify_gen_subreg (mode, r, omode, byte);

  if (!rv)
    gcc_unreachable ();

  return rv;
}

/* Called by movsi_x to generate the HImode operands.  */
void
msp430_split_movsi (rtx *operands)
{
  rtx op00, op02, op10, op12;

  op00 = msp430_subreg (HImode, operands[0], SImode, 0);
  op02 = msp430_subreg (HImode, operands[0], SImode, 2);

  if (GET_CODE (operands[1]) == CONST
      || GET_CODE (operands[1]) == SYMBOL_REF)
    {
      op10 = gen_rtx_ZERO_EXTRACT (HImode, operands[1], GEN_INT (16), GEN_INT (0));
      op10 = gen_rtx_CONST (HImode, op10);
      op12 = gen_rtx_ZERO_EXTRACT (HImode, operands[1], GEN_INT (16), GEN_INT (16));
      op12 = gen_rtx_CONST (HImode, op12);
    }
  else
    {
      op10 = msp430_subreg (HImode, operands[1], SImode, 0);
      op12 = msp430_subreg (HImode, operands[1], SImode, 2);
    }

  if (rtx_equal_p (operands[0], operands[1]))
    {
      operands[2] = op02;
      operands[4] = op12;
      operands[3] = op00;
      operands[5] = op10;
    }
  else if (rtx_equal_p (op00, op12)
	   /* Catch the case where we are loading (rN, rN+1) from mem (rN).  */
	   || (REG_P (op00) && reg_mentioned_p (op00, op10))
	   /* Or storing (rN) into mem (rN).  */
	   || (REG_P (op10) && reg_mentioned_p (op10, op00))
	   )
    {
      operands[2] = op02;
      operands[4] = op12;
      operands[3] = op00;
      operands[5] = op10;
    }
  else
    {
      operands[2] = op00;
      operands[4] = op10;
      operands[3] = op02;
      operands[5] = op12;
    }
}


/* The MSPABI specifies the names of various helper functions, many of
   which are compatible with GCC's helpers.  This table maps the GCC
   name to the MSPABI name.  */
static const struct
{
  char const * const gcc_name;
  char const * const ti_name;
}
  helper_function_name_mappings [] =
{
  /* Floating point to/from integer conversions.  */
  { "__truncdfsf2", "__mspabi_cvtdf" },
  { "__extendsfdf2", "__mspabi_cvtfd" },
  { "__fixdfhi", "__mspabi_fixdi" },
  { "__fixdfsi", "__mspabi_fixdli" },
  { "__fixdfdi", "__mspabi_fixdlli" },
  { "__fixunsdfhi", "__mspabi_fixdu" },
  { "__fixunsdfsi", "__mspabi_fixdul" },
  { "__fixunsdfdi", "__mspabi_fixdull" },
  { "__fixsfhi", "__mspabi_fixfi" },
  { "__fixsfsi", "__mspabi_fixfli" },
  { "__fixsfdi", "__mspabi_fixflli" },
  { "__fixunsfhi", "__mspabi_fixfu" },
  { "__fixunsfsi", "__mspabi_fixful" },
  { "__fixunsfdi", "__mspabi_fixfull" },
  { "__floathisf", "__mspabi_fltif" },
  { "__floatsisf", "__mspabi_fltlif" },
  { "__floatdisf", "__mspabi_fltllif" },
  { "__floathidf", "__mspabi_fltid" },
  { "__floatsidf", "__mspabi_fltlid" },
  { "__floatdidf", "__mspabi_fltllid" },
  { "__floatunhisf", "__mspabi_fltuf" },
  { "__floatunsisf", "__mspabi_fltulf" },
  { "__floatundisf", "__mspabi_fltullf" },
  { "__floatunhidf", "__mspabi_fltud" },
  { "__floatunsidf", "__mspabi_fltuld" },
  { "__floatundidf", "__mspabi_fltulld" },

  /* Floating point comparisons.  */
  /* GCC uses individual functions for each comparison, TI uses one
     compare <=> function.  */

  /* Floating point arithmatic */
  { "__adddf3", "__mspabi_addd" },
  { "__addsf3", "__mspabi_addf" },
  { "__divdf3", "__mspabi_divd" },
  { "__divsf3", "__mspabi_divf" },
  { "__muldf3", "__mspabi_mpyd" },
  { "__mulsf3", "__mspabi_mpyf" },
  { "__subdf3", "__mspabi_subd" },
  { "__subsf3", "__mspabi_subf" },
  /* GCC does not use helper functions for negation */

  /* Integer multiply, divide, remainder.  */
  /* Note: gcc doesn't know about hardware multiply options (yet?)  */
  { "__mulhi3", "__mspabi_mpyi" },
  { "__mulsi3", "__mspabi_mpyl" },
  { "__muldi3", "__mspabi_mpyll" },
#if 0
  /* Clarify signed vs unsigned first.  */
  { "__mulhisi3", "__mspabi_mpysl" }, /* gcc doesn't use widening multiply (yet?) */
  { "__mulsidi3", "__mspabi_mpysll" }, /* gcc doesn't use widening multiply (yet?) */
#endif

  { "__divhi3", "__mspabi_divi" },
  { "__divsi3", "__mspabi_divli" },
  { "__divdi3", "__mspabi_divlli" },
  { "__udivhi3", "__mspabi_divu" },
  { "__udivsi3", "__mspabi_divlu" },
  { "__udivdi3", "__mspabi_divllu" },
  { "__modhi3", "__mspabi_remi" },
  { "__modsi3", "__mspabi_remli" },
  { "__moddi3", "__mspabi_remlli" },
  { "__umodhi3", "__mspabi_remu" },
  { "__umodsi3", "__mspabi_remul" },
  { "__umoddi3", "__mspabi_remull" },

  /* Bitwise operations.  */
  /* Rotation - no rotation support yet.  */
  /* Logical left shift - gcc already does these itself.  */
  /* Arithmetic left shift - gcc already does these itself.  */
  /* Arithmetic right shift - gcc already does these itself.  */

  { NULL, NULL }
};

/* This function does the same as the default, but it will replace GCC
   function names with the MSPABI-specified ones.  */
void
msp430_output_labelref (FILE *file, const char *name)
{
  int i;

  for (i = 0; helper_function_name_mappings [i].gcc_name; i++)
    if (! strcmp (helper_function_name_mappings [i].gcc_name, name))
      {
	fputs (helper_function_name_mappings [i].ti_name, file);
	return;
      }

  fputs (name, file);
}

#undef  TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND		msp430_print_operand

/* Common code for msp430_print_operand().  */
static void
msp430_print_operand_raw (FILE * file, rtx op, int letter ATTRIBUTE_UNUSED)
{
  int i;

  switch (GET_CODE (op))
    {
    case REG:
      fprintf (file, "%s", reg_names [REGNO (op)]);
      break;

    case CONST_INT:
      i = INTVAL (op);
      if (TARGET_ASM_HEX)
	fprintf (file, "%#x", i);
      else
	fprintf (file, "%d", i);
      break;

    case CONST:
    case PLUS:
    case MINUS:
    case SYMBOL_REF:
    case LABEL_REF:
      output_addr_const (file, op);
      break;

    default:
      print_rtl (file, op);
      break;
    }
}

static void
msp430_print_operand (FILE * file, rtx op, int letter)
{
  rtx addr;

  /* We can't use c, n, a, or l.  */
  switch (letter)
    {
    case 'Z':
      gcc_assert (CONST_INT_P (op));
      /* Print the constant value, less one.  */
      fprintf (file, "#%ld", INTVAL (op) - 1);
      return;
    case 'Y':
      gcc_assert (CONST_INT_P (op));
      /* Print the constant value, less four.  */
      fprintf (file, "#%ld", INTVAL (op) - 4);
      return;
      /* case 'D': used for "decimal without '#'" */
    case 'I':
      if (GET_CODE (op) == CONST_INT)
	{
	  /* Inverse of constants */
	  int i = INTVAL (op);
	  fprintf (file, "%d", ~i);
	  return;
	}
      op = XEXP (op, 0);
      break;
    case 'r': /* Conditional jump where the condition is reversed.  */
      switch (GET_CODE (op))
	{
	case EQ: fprintf (file, "NE"); break;
	case NE: fprintf (file, "EQ"); break;
	case GEU: fprintf (file, "LO"); break;
	case LTU: fprintf (file, "HS"); break;
	case GE: fprintf (file, "L"); break;
	case LT: fprintf (file, "GE"); break;
	  /* Assume these have reversed operands.  */
	case GTU: fprintf (file, "HS"); break;
	case LEU: fprintf (file, "LO"); break;
	case GT: fprintf (file, "GE"); break;
	case LE: fprintf (file, "L"); break;
	default:
	  msp430_print_operand_raw (file, op, letter);
	  break;
	}
      return;
    case 'R': /* Conditional jump where the operands are reversed.  */
      switch (GET_CODE (op))
	{
	case GTU: fprintf (file, "LO"); break;
	case LEU: fprintf (file, "HS"); break;
	case GT: fprintf (file, "L"); break;
	case LE: fprintf (file, "GE"); break;
	default:
	  msp430_print_operand_raw (file, op, letter);
	  break;
	}
      return;
    case 'p': /* Bit position. 0 == 0x01, 3 = 0x08 etc.  */
      gcc_assert (CONST_INT_P (op));
      fprintf (file, "#%d", 1 << INTVAL (op));
      return;
    case 'B':
      switch (GET_MODE (op))
	{
	case QImode: fprintf (file, ".B"); return;
	case HImode: fprintf (file, ".W"); return;
	case PSImode: fprintf (file, ".A"); return;
	case SImode: fprintf (file, ".A"); return;
	default:
	  return;
	}
    case 'L': /* Low half.  */
      switch (GET_CODE (op))
	{
	case MEM:
	  op = adjust_address (op, Pmode, 0);
	  break;
	case REG:
	  break;
	case CONST_INT:
	  op = GEN_INT (INTVAL (op) & 0xffff);
	  letter = 0;
	  break;
	default:
	  /* If you get here, figure out a test case :-) */
	  gcc_unreachable ();
	}
      break;
    case 'H': /* high half */
      switch (GET_CODE (op))
	{
	case MEM:
	  op = adjust_address (op, Pmode, 2);
	  break;
	case REG:
	  op = gen_rtx_REG (Pmode, REGNO (op) + 1);
	  break;
	case CONST_INT:
	  op = GEN_INT (INTVAL (op) >> 16);
	  letter = 0;
	  break;
	default:
	  /* If you get here, figure out a test case :-) */
	  gcc_unreachable ();
	}
      break;

    case 'X':
      /* This is used to turn, for example, an ADD opcode into an ADDX
	 opcode when we're using 20-bit addresses.  */
      if (TARGET_LARGE)
	fprintf (file, "X");
      /* We don't care which operand we use, but we want 'X' in the MD
	 file, so we do it this way.  */
      return;

    case 'x':
      /* Similarly, but only for PSImodes.  BIC, for example, needs this.  */
      if (TARGET_LARGE && GET_MODE (op) == PSImode)
	fprintf (file, "X");
      return;

    case 'A':
      /* Likewise, for BR -> BRA.  */
      if (TARGET_LARGE)
	fprintf (file, "A");
      return;

    case 'O':
      /* Computes the offset to the top of the stack for the current frame.
	 This has to be done here rather than in, say, msp430_expand_builtin()
	 because builtins are expanded before the frame layout is determined.  */
      fprintf (file, "%d",
	       msp430_initial_elimination_offset (ARG_POINTER_REGNUM, STACK_POINTER_REGNUM)
	        - 2);
      return ;
    }

  switch (GET_CODE (op))
    {
    case REG:
      msp430_print_operand_raw (file, op, letter);
      break;

    case MEM:
      addr = XEXP (op, 0);
      switch (GET_CODE (addr))
	{
	case REG:
	  fprintf (file, "@%s", reg_names [REGNO (addr)]);
	  break;
	case PLUS:
	  msp430_print_operand_raw (file, XEXP (addr, 1), letter);
	  fprintf (file, "(%s)", reg_names [REGNO (XEXP (addr, 0))]);
	  break;
	case CONST:
	case CONST_INT:
	case SYMBOL_REF:
	case LABEL_REF:
	  fprintf (file, "&");
	  msp430_print_operand_raw (file, addr, letter);
	  break;

	default:
	  print_rtl (file, addr);
	  break;
	}
      break;

    case CONST_INT:
    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
      if (letter == 0)
	fprintf (file, "#");
      msp430_print_operand_raw (file, op, letter);
      break;

    case EQ: fprintf (file, "EQ"); break;
    case NE: fprintf (file, "NE"); break;
    case GEU: fprintf (file, "HS"); break;
    case LTU: fprintf (file, "LO"); break;
    case GE: fprintf (file, "GE"); break;
    case LT: fprintf (file, "L"); break;

    default:
      print_rtl (file, op);
      break;
    }

}


/* Frame stuff.  */

rtx
msp430_return_addr_rtx (int count)
{
  int ra_size;
  if (count)
    return NULL_RTX;

  ra_size = TARGET_LARGE ? 4 : 2;
  if (crtl->args.pretend_args_size)
    ra_size += 2;

  return gen_rtx_MEM (Pmode, gen_rtx_PLUS (Pmode, arg_pointer_rtx, GEN_INT (- ra_size)));
}

rtx
msp430_incoming_return_addr_rtx (void)
{
  return gen_rtx_MEM (Pmode, stack_pointer_rtx);
}

/* Instruction generation stuff.  */

/* Generate a sequence of instructions to sign-extend an HI
   value into an SI value.  Handles the tricky case where
   we are overwriting the destination.  */

const char *
msp430x_extendhisi (rtx * operands)
{
  if (REGNO (operands[0]) == REGNO (operands[1]))
    /* Low word of dest == source word.  */
    return "BIT.W #0x8000, %L0 { SUBC.W %H0, %H0 { INV.W %H0, %H0"; /* 8-bytes.  */

  if (! msp430x)
    /* Note: This sequence is approximately the same length as invoking a helper
       function to perform the sign-extension, as in:
       
         MOV.W  %1, %L0
	 MOV.W  %1, r12
	 CALL   __mspabi_srai_15
	 MOV.W  r12, %H0

       but this version does not involve any function calls or using argument
       registers, so it reduces register pressure.  */
    return "MOV.W %1, %L0 { BIT.W #0x8000, %L0 { SUBC.W %H0, %H0 { INV.W %H0, %H0"; /* 10-bytes.  */
  
  if (REGNO (operands[0]) + 1 == REGNO (operands[1]))
    /* High word of dest == source word.  */
    return "MOV.W %1, %L0 { RPT #15 { RRAX.W %H0"; /* 6-bytes.  */

  /* No overlap between dest and source.  */
  return "MOV.W %1, %L0 { MOV.W %1, %H0 { RPT #15 { RRAX.W %H0"; /* 8-bytes.  */
}

/* Likewise for logical right shifts.  */
const char *
msp430x_logical_shift_right (rtx amount)
{
  /* The MSP430X's logical right shift instruction - RRUM - does
     not use an extension word, so we cannot encode a repeat count.
     Try various alternatives to work around this.  If the count
     is in a register we are stuck, hence the assert.  */
  gcc_assert (CONST_INT_P (amount));

  if (INTVAL (amount) <= 0
      || INTVAL (amount) >= 16)
    return "# nop logical shift.";

  if (INTVAL (amount) > 0	    
      && INTVAL (amount) < 5)
    return "rrum.w\t%2, %0"; /* Two bytes.  */

  if (INTVAL (amount) > 4	    
      && INTVAL (amount) < 9)
    return "rrum.w\t#4, %0 { rrum.w\t%Y2, %0 "; /* Four bytes.  */

  /* First we logically shift right by one.  Now we know
     that the top bit is zero and we can use the arithmetic
     right shift instruction to perform the rest of the shift.  */
  return "rrum.w\t#1, %0 { rpt\t%Z2 { rrax.w\t%0"; /* Six bytes.  */
}

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-msp430.h"
