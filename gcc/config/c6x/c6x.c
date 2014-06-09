/* Target Code for TI C6X
   Copyright (C) 2010-2014 Free Software Foundation, Inc.
   Contributed by Andrew Jenner <andrew@codesourcery.com>
   Contributed by Bernd Schmidt <bernds@codesourcery.com>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tree.h"
#include "stor-layout.h"
#include "varasm.h"
#include "calls.h"
#include "stringpool.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"
#include "insn-codes.h"
#include "expr.h"
#include "regs.h"
#include "optabs.h"
#include "recog.h"
#include "ggc.h"
#include "sched-int.h"
#include "timevar.h"
#include "tm_p.h"
#include "tm-preds.h"
#include "tm-constrs.h"
#include "df.h"
#include "function.h"
#include "diagnostic-core.h"
#include "cgraph.h"
#include "langhooks.h"
#include "target.h"
#include "target-def.h"
#include "sel-sched.h"
#include "debug.h"
#include "opts.h"
#include "hw-doloop.h"
#include "regrename.h"
#include "dumpfile.h"
#include "gimple-expr.h"
#include "builtins.h"

/* Table of supported architecture variants.  */
typedef struct
{
  const char *arch;
  enum c6x_cpu_type type;
  unsigned short features;
} c6x_arch_table;

/* A list of all ISAs, mapping each one to a representative device.
   Used for -march selection.  */
static const c6x_arch_table all_isas[] =
{
#define C6X_ISA(NAME,DEVICE,FLAGS) \
  { NAME, DEVICE, FLAGS },
#include "c6x-isas.def"
#undef C6X_ISA
  { NULL, C6X_CPU_C62X, 0 }
};

/* This is the parsed result of the "-march=" option, if given.  */
enum c6x_cpu_type c6x_arch = C6X_DEFAULT_ARCH;

/* A mask of insn types that are allowed by the architecture selected by
   the -march option.  */
unsigned long c6x_insn_mask = C6X_DEFAULT_INSN_MASK;

/* The instruction that is being output (as obtained from FINAL_PRESCAN_INSN).
 */
static rtx c6x_current_insn = NULL_RTX;

/* A decl we build to access __c6xabi_DSBT_base.  */
static GTY(()) tree dsbt_decl;

/* Determines whether we run our final scheduling pass or not.  We always
   avoid the normal second scheduling pass.  */
static int c6x_flag_schedule_insns2;

/* Determines whether we run variable tracking in machine dependent
   reorganization.  */
static int c6x_flag_var_tracking;

/* Determines whether we use modulo scheduling.  */
static int c6x_flag_modulo_sched;

/* Record the state of flag_pic before we set it to 1 for DSBT.  */
int c6x_initial_flag_pic;

typedef struct
{
  /* We record the clock cycle for every insn during scheduling.  */
  int clock;
  /* After scheduling, we run assign_reservations to choose unit
     reservations for all insns.  These are recorded here.  */
  int reservation;
  /* Records the new condition for insns which must be made
     conditional after scheduling.  An entry of NULL_RTX means no such
     change is necessary.  */
  rtx new_cond;
  /* True for the first insn that was scheduled in an ebb.  */
  bool ebb_start;
  /* The scheduler state after the insn, transformed into a mask of UNIT_QID
     bits rather than storing the state.  Meaningful only for the last
     insn in a cycle.  */
  unsigned int unit_mask;
} c6x_sched_insn_info;


/* Record a c6x_sched_insn_info structure for every insn in the function.  */
static vec<c6x_sched_insn_info> insn_info;

#define INSN_INFO_LENGTH (insn_info).length ()
#define INSN_INFO_ENTRY(N) (insn_info[(N)])

static bool done_cfi_sections;

#define RESERVATION_FLAG_D 1
#define RESERVATION_FLAG_L 2
#define RESERVATION_FLAG_S 4
#define RESERVATION_FLAG_M 8
#define RESERVATION_FLAG_DL (RESERVATION_FLAG_D | RESERVATION_FLAG_L)
#define RESERVATION_FLAG_DS (RESERVATION_FLAG_D | RESERVATION_FLAG_S)
#define RESERVATION_FLAG_LS (RESERVATION_FLAG_L | RESERVATION_FLAG_S)
#define RESERVATION_FLAG_DLS (RESERVATION_FLAG_D | RESERVATION_FLAG_LS)

/* The DFA names of the units.  */
static const char *const c6x_unit_names[] =
{
  "d1", "l1", "s1", "m1", "fps1", "fpl1", "adddps1", "adddpl1",
  "d2", "l2", "s2", "m2", "fps2", "fpl2", "adddps2", "adddpl2"
};

/* The DFA unit number for each unit in c6x_unit_names[].  */
static int c6x_unit_codes[ARRAY_SIZE (c6x_unit_names)];

/* Unit query IDs.  */
#define UNIT_QID_D1 0
#define UNIT_QID_L1 1
#define UNIT_QID_S1 2
#define UNIT_QID_M1 3
#define UNIT_QID_FPS1 4
#define UNIT_QID_FPL1 5
#define UNIT_QID_ADDDPS1 6
#define UNIT_QID_ADDDPL1 7
#define UNIT_QID_SIDE_OFFSET 8

#define RESERVATION_S1 2
#define RESERVATION_S2 10

/* An enum for the unit requirements we count in the UNIT_REQS table.  */
enum unitreqs
{
  UNIT_REQ_D,
  UNIT_REQ_L,
  UNIT_REQ_S,
  UNIT_REQ_M,
  UNIT_REQ_DL,
  UNIT_REQ_DS,
  UNIT_REQ_LS,
  UNIT_REQ_DLS,
  UNIT_REQ_T,
  UNIT_REQ_X,
  UNIT_REQ_MAX
};

/* A table used to count unit requirements.  Used when computing minimum
   iteration intervals.  */
typedef int unit_req_table[2][UNIT_REQ_MAX];
static unit_req_table unit_reqs;

/* Register map for debugging.  */
unsigned const dbx_register_map[FIRST_PSEUDO_REGISTER] =
{
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,	/* A0 - A15.  */
  37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49,	/* A16 - A32.  */
  50, 51, 52,
  16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28,	/* B0 - B15.  */
  29, 30, 31,
  53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65,	/* B16 - B32.  */
  66, 67, 68,
  -1, -1, -1						/* FP, ARGP, ILC.  */
};

/* Allocate a new, cleared machine_function structure.  */

static struct machine_function *
c6x_init_machine_status (void)
{
  return ggc_cleared_alloc<machine_function> ();
}

/* Implement TARGET_OPTION_OVERRIDE.  */

static void
c6x_option_override (void)
{
  unsigned i;

  if (global_options_set.x_c6x_arch_option)
    {
      c6x_arch = all_isas[c6x_arch_option].type;
      c6x_insn_mask &= ~C6X_INSNS_ALL_CPU_BITS;
      c6x_insn_mask |= all_isas[c6x_arch_option].features;
    }

  c6x_flag_schedule_insns2 = flag_schedule_insns_after_reload;
  flag_schedule_insns_after_reload = 0;

  c6x_flag_modulo_sched = flag_modulo_sched;
  flag_modulo_sched = 0;

  init_machine_status = c6x_init_machine_status;

  for (i = 0; i < ARRAY_SIZE (c6x_unit_names); i++)
    c6x_unit_codes[i] = get_cpu_unit_code (c6x_unit_names[i]);

  if (flag_pic && !TARGET_DSBT)
    {
      error ("-fpic and -fPIC not supported without -mdsbt on this target");
      flag_pic = 0;
    }
  c6x_initial_flag_pic = flag_pic;
  if (TARGET_DSBT && !flag_pic)
    flag_pic = 1;
}


/* Implement the TARGET_CONDITIONAL_REGISTER_USAGE hook.  */

static void
c6x_conditional_register_usage (void)
{
  int i;
  if (c6x_arch == C6X_CPU_C62X || c6x_arch == C6X_CPU_C67X)
    for (i = 16; i < 32; i++)
      {
	fixed_regs[i] = 1;
	fixed_regs[32 + i] = 1;
      }
  if (TARGET_INSNS_64)
    {
      SET_HARD_REG_BIT (reg_class_contents[(int)PREDICATE_A_REGS],
			REG_A0);
      SET_HARD_REG_BIT (reg_class_contents[(int)PREDICATE_REGS],
			REG_A0);
      CLEAR_HARD_REG_BIT (reg_class_contents[(int)NONPREDICATE_A_REGS],
			  REG_A0);
      CLEAR_HARD_REG_BIT (reg_class_contents[(int)NONPREDICATE_REGS],
			  REG_A0);
    }
}

static GTY(()) rtx eqdf_libfunc;
static GTY(()) rtx nedf_libfunc;
static GTY(()) rtx ledf_libfunc;
static GTY(()) rtx ltdf_libfunc;
static GTY(()) rtx gedf_libfunc;
static GTY(()) rtx gtdf_libfunc;
static GTY(()) rtx eqsf_libfunc;
static GTY(()) rtx nesf_libfunc;
static GTY(()) rtx lesf_libfunc;
static GTY(()) rtx ltsf_libfunc;
static GTY(()) rtx gesf_libfunc;
static GTY(()) rtx gtsf_libfunc;
static GTY(()) rtx strasgi_libfunc;
static GTY(()) rtx strasgi64p_libfunc;

/* Implement the TARGET_INIT_LIBFUNCS macro.  We use this to rename library
   functions to match the C6x ABI.  */

static void
c6x_init_libfuncs (void)
{
  /* Double-precision floating-point arithmetic.  */
  set_optab_libfunc (add_optab, DFmode, "__c6xabi_addd");
  set_optab_libfunc (sdiv_optab, DFmode, "__c6xabi_divd");
  set_optab_libfunc (smul_optab, DFmode, "__c6xabi_mpyd");
  set_optab_libfunc (neg_optab, DFmode, "__c6xabi_negd");
  set_optab_libfunc (sub_optab, DFmode, "__c6xabi_subd");

  /* Single-precision floating-point arithmetic.  */
  set_optab_libfunc (add_optab, SFmode, "__c6xabi_addf");
  set_optab_libfunc (sdiv_optab, SFmode, "__c6xabi_divf");
  set_optab_libfunc (smul_optab, SFmode, "__c6xabi_mpyf");
  set_optab_libfunc (neg_optab, SFmode, "__c6xabi_negf");
  set_optab_libfunc (sub_optab, SFmode, "__c6xabi_subf");

  /* Floating-point comparisons.  */
  eqsf_libfunc = init_one_libfunc ("__c6xabi_eqf");
  nesf_libfunc = init_one_libfunc ("__c6xabi_neqf");
  lesf_libfunc = init_one_libfunc ("__c6xabi_lef");
  ltsf_libfunc = init_one_libfunc ("__c6xabi_ltf");
  gesf_libfunc = init_one_libfunc ("__c6xabi_gef");
  gtsf_libfunc = init_one_libfunc ("__c6xabi_gtf");
  eqdf_libfunc = init_one_libfunc ("__c6xabi_eqd");
  nedf_libfunc = init_one_libfunc ("__c6xabi_neqd");
  ledf_libfunc = init_one_libfunc ("__c6xabi_led");
  ltdf_libfunc = init_one_libfunc ("__c6xabi_ltd");
  gedf_libfunc = init_one_libfunc ("__c6xabi_ged");
  gtdf_libfunc = init_one_libfunc ("__c6xabi_gtd");

  set_optab_libfunc (eq_optab, SFmode, NULL);
  set_optab_libfunc (ne_optab, SFmode, "__c6xabi_neqf");
  set_optab_libfunc (gt_optab, SFmode, NULL);
  set_optab_libfunc (ge_optab, SFmode, NULL);
  set_optab_libfunc (lt_optab, SFmode, NULL);
  set_optab_libfunc (le_optab, SFmode, NULL);
  set_optab_libfunc (unord_optab, SFmode, "__c6xabi_unordf");
  set_optab_libfunc (eq_optab, DFmode, NULL);
  set_optab_libfunc (ne_optab, DFmode, "__c6xabi_neqd");
  set_optab_libfunc (gt_optab, DFmode, NULL);
  set_optab_libfunc (ge_optab, DFmode, NULL);
  set_optab_libfunc (lt_optab, DFmode, NULL);
  set_optab_libfunc (le_optab, DFmode, NULL);
  set_optab_libfunc (unord_optab, DFmode, "__c6xabi_unordd");

  /* Floating-point to integer conversions.  */
  set_conv_libfunc (sfix_optab, SImode, DFmode, "__c6xabi_fixdi");
  set_conv_libfunc (ufix_optab, SImode, DFmode, "__c6xabi_fixdu");
  set_conv_libfunc (sfix_optab, DImode, DFmode, "__c6xabi_fixdlli");
  set_conv_libfunc (ufix_optab, DImode, DFmode, "__c6xabi_fixdull");
  set_conv_libfunc (sfix_optab, SImode, SFmode, "__c6xabi_fixfi");
  set_conv_libfunc (ufix_optab, SImode, SFmode, "__c6xabi_fixfu");
  set_conv_libfunc (sfix_optab, DImode, SFmode, "__c6xabi_fixflli");
  set_conv_libfunc (ufix_optab, DImode, SFmode, "__c6xabi_fixfull");

  /* Conversions between floating types.  */
  set_conv_libfunc (trunc_optab, SFmode, DFmode, "__c6xabi_cvtdf");
  set_conv_libfunc (sext_optab, DFmode, SFmode, "__c6xabi_cvtfd");

  /* Integer to floating-point conversions.  */
  set_conv_libfunc (sfloat_optab, DFmode, SImode, "__c6xabi_fltid");
  set_conv_libfunc (ufloat_optab, DFmode, SImode, "__c6xabi_fltud");
  set_conv_libfunc (sfloat_optab, DFmode, DImode, "__c6xabi_fltllid");
  set_conv_libfunc (ufloat_optab, DFmode, DImode, "__c6xabi_fltulld");
  set_conv_libfunc (sfloat_optab, SFmode, SImode, "__c6xabi_fltif");
  set_conv_libfunc (ufloat_optab, SFmode, SImode, "__c6xabi_fltuf");
  set_conv_libfunc (sfloat_optab, SFmode, DImode, "__c6xabi_fltllif");
  set_conv_libfunc (ufloat_optab, SFmode, DImode, "__c6xabi_fltullf");

  /* Long long.  */
  set_optab_libfunc (smul_optab, DImode, "__c6xabi_mpyll");
  set_optab_libfunc (ashl_optab, DImode, "__c6xabi_llshl");
  set_optab_libfunc (lshr_optab, DImode, "__c6xabi_llshru");
  set_optab_libfunc (ashr_optab, DImode, "__c6xabi_llshr");

  set_optab_libfunc (sdiv_optab, SImode, "__c6xabi_divi");
  set_optab_libfunc (udiv_optab, SImode, "__c6xabi_divu");
  set_optab_libfunc (smod_optab, SImode, "__c6xabi_remi");
  set_optab_libfunc (umod_optab, SImode, "__c6xabi_remu");
  set_optab_libfunc (sdivmod_optab, SImode, "__c6xabi_divremi");
  set_optab_libfunc (udivmod_optab, SImode, "__c6xabi_divremu");
  set_optab_libfunc (sdiv_optab, DImode, "__c6xabi_divlli");
  set_optab_libfunc (udiv_optab, DImode, "__c6xabi_divull");
  set_optab_libfunc (smod_optab, DImode, "__c6xabi_remlli");
  set_optab_libfunc (umod_optab, DImode, "__c6xabi_remull");
  set_optab_libfunc (udivmod_optab, DImode, "__c6xabi_divremull");

  /* Block move.  */
  strasgi_libfunc = init_one_libfunc ("__c6xabi_strasgi");
  strasgi64p_libfunc = init_one_libfunc ("__c6xabi_strasgi_64plus");
}

/* Begin the assembly file.  */

static void
c6x_file_start (void)
{
  /* Variable tracking should be run after all optimizations which change order
     of insns.  It also needs a valid CFG.  This can't be done in
     c6x_override_options, because flag_var_tracking is finalized after
     that.  */
  c6x_flag_var_tracking = flag_var_tracking;
  flag_var_tracking = 0;

  done_cfi_sections = false;
  default_file_start ();

  /* Arrays are aligned to 8-byte boundaries.  */
  asm_fprintf (asm_out_file,
	       "\t.c6xabi_attribute Tag_ABI_array_object_alignment, 0\n");
  asm_fprintf (asm_out_file,
	       "\t.c6xabi_attribute Tag_ABI_array_object_align_expected, 0\n");

  /* Stack alignment is 8 bytes.  */
  asm_fprintf (asm_out_file,
	       "\t.c6xabi_attribute Tag_ABI_stack_align_needed, 0\n");
  asm_fprintf (asm_out_file,
	       "\t.c6xabi_attribute Tag_ABI_stack_align_preserved, 0\n");

#if 0 /* FIXME: Reenable when TI's tools are fixed.  */
  /* ??? Ideally we'd check flag_short_wchar somehow.  */
  asm_fprintf (asm_out_file, "\t.c6xabi_attribute Tag_ABI_wchar_t, %d\n", 2);
#endif

  /* We conform to version 1.0 of the ABI.  */
  asm_fprintf (asm_out_file,
	       "\t.c6xabi_attribute Tag_ABI_conformance, \"1.0\"\n");

}

/* The LTO frontend only enables exceptions when it sees a function that
   uses it.  This changes the return value of dwarf2out_do_frame, so we
   have to check before every function.  */

void
c6x_output_file_unwind (FILE * f)
{
  if (done_cfi_sections)
    return;

  /* Output a .cfi_sections directive.  */
  if (dwarf2out_do_frame ())
    {
      if (flag_unwind_tables || flag_exceptions)
	{
	  if (write_symbols == DWARF2_DEBUG
	      || write_symbols == VMS_AND_DWARF2_DEBUG)
	    asm_fprintf (f, "\t.cfi_sections .debug_frame, .c6xabi.exidx\n");
	  else
	    asm_fprintf (f, "\t.cfi_sections .c6xabi.exidx\n");
	}
      else
	asm_fprintf (f, "\t.cfi_sections .debug_frame\n");
      done_cfi_sections = true;
    }
}

/* Output unwind directives at the end of a function.  */

static void
c6x_output_fn_unwind (FILE * f)
{
  /* Return immediately if we are not generating unwinding tables.  */
  if (! (flag_unwind_tables || flag_exceptions))
    return;

  /* If this function will never be unwound, then mark it as such.  */
  if (!(flag_unwind_tables || crtl->uses_eh_lsda)
      && (TREE_NOTHROW (current_function_decl)
	  || crtl->all_throwers_are_sibcalls))
    fputs("\t.cantunwind\n", f);

  fputs ("\t.endp\n", f);
}


/* Stack and Calling.  */

int argument_registers[10] =
{
  REG_A4, REG_B4,
  REG_A6, REG_B6,
  REG_A8, REG_B8,
  REG_A10, REG_B10,
  REG_A12, REG_B12
};

/* Implements the macro INIT_CUMULATIVE_ARGS defined in c6x.h.  */

void
c6x_init_cumulative_args (CUMULATIVE_ARGS *cum, const_tree fntype, rtx libname,
			  int n_named_args ATTRIBUTE_UNUSED)
{
  cum->count = 0;
  cum->nregs = 10;
  if (!libname && fntype)
    {
      /* We need to find out the number of named arguments.  Unfortunately,
	 for incoming arguments, N_NAMED_ARGS is set to -1.  */
      if (stdarg_p (fntype))
	cum->nregs = type_num_arguments (fntype) - 1;
      if (cum->nregs > 10)
	cum->nregs = 10;
    }
}

/* Implements the macro FUNCTION_ARG defined in c6x.h.  */

static rtx
c6x_function_arg (cumulative_args_t cum_v, enum machine_mode mode,
		  const_tree type, bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  if (cum->count >= cum->nregs)
    return NULL_RTX;
  if (type)
    {
      HOST_WIDE_INT size = int_size_in_bytes (type);
      if (TARGET_BIG_ENDIAN && AGGREGATE_TYPE_P (type))
	{
	  if (size > 4)
	    {
	      rtx reg1 = gen_rtx_REG (SImode, argument_registers[cum->count] + 1);
	      rtx reg2 = gen_rtx_REG (SImode, argument_registers[cum->count]);
	      rtvec vec = gen_rtvec (2, gen_rtx_EXPR_LIST (VOIDmode, reg1, const0_rtx),
				     gen_rtx_EXPR_LIST (VOIDmode, reg2, GEN_INT (4)));
	      return gen_rtx_PARALLEL (mode, vec);
	    }
	}
    }
  return gen_rtx_REG (mode, argument_registers[cum->count]);
}

static void
c6x_function_arg_advance (cumulative_args_t cum_v,
			  enum machine_mode mode ATTRIBUTE_UNUSED,
			  const_tree type ATTRIBUTE_UNUSED,
			  bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  cum->count++;
}


/* Return true if BLOCK_REG_PADDING (MODE, TYPE, FIRST) should return
   upward rather than downward.  */

bool
c6x_block_reg_pad_upward (enum machine_mode mode ATTRIBUTE_UNUSED,
			  const_tree type, bool first)
{
  HOST_WIDE_INT size;

  if (!TARGET_BIG_ENDIAN)
    return true;
  if (!first)
    return true;
  if (!type)
    return true;
  size = int_size_in_bytes (type);
  return size == 3;
}

/* Implement TARGET_FUNCTION_ARG_BOUNDARY.  */

static unsigned int
c6x_function_arg_boundary (enum machine_mode mode, const_tree type)
{
  unsigned int boundary = type ? TYPE_ALIGN (type) : GET_MODE_BITSIZE (mode);

  if (boundary > BITS_PER_WORD)
    return 2 * BITS_PER_WORD;

  if (mode == BLKmode)
    {
      HOST_WIDE_INT size = int_size_in_bytes (type);
      if (size > 4)
	return 2 * BITS_PER_WORD;
      if (boundary < BITS_PER_WORD)
	{
	  if (size >= 3)
	    return BITS_PER_WORD;
	  if (size >= 2)
	    return 2 * BITS_PER_UNIT;
	}
    }
  return boundary;
}

/* Implement TARGET_FUNCTION_ARG_ROUND_BOUNDARY.  */
static unsigned int
c6x_function_arg_round_boundary (enum machine_mode mode, const_tree type)
{
  return c6x_function_arg_boundary (mode, type);
}

/* TARGET_FUNCTION_VALUE implementation.  Returns an RTX representing the place
   where function FUNC returns or receives a value of data type TYPE.  */

static rtx
c6x_function_value (const_tree type, const_tree func ATTRIBUTE_UNUSED,
		    bool outgoing ATTRIBUTE_UNUSED)
{
  /* Functions return values in register A4.  When returning aggregates, we may
     have to adjust for endianness.  */
  if (TARGET_BIG_ENDIAN && type && AGGREGATE_TYPE_P (type))
    {
      HOST_WIDE_INT size = int_size_in_bytes (type);
      if (size > 4)
	{

	  rtx reg1 = gen_rtx_REG (SImode, REG_A4 + 1);
	  rtx reg2 = gen_rtx_REG (SImode, REG_A4);
	  rtvec vec = gen_rtvec (2, gen_rtx_EXPR_LIST (VOIDmode, reg1, const0_rtx),
				 gen_rtx_EXPR_LIST (VOIDmode, reg2, GEN_INT (4)));
	  return gen_rtx_PARALLEL (TYPE_MODE (type), vec);
	}
    }
  return gen_rtx_REG (TYPE_MODE (type), REG_A4);
}

/* Implement TARGET_LIBCALL_VALUE.  */

static rtx
c6x_libcall_value (enum machine_mode mode, const_rtx fun ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (mode, REG_A4);
}

/* TARGET_STRUCT_VALUE_RTX implementation.  */

static rtx
c6x_struct_value_rtx (tree type ATTRIBUTE_UNUSED, int incoming ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (Pmode, REG_A3);
}

/* Implement TARGET_FUNCTION_VALUE_REGNO_P.  */

static bool
c6x_function_value_regno_p (const unsigned int regno)
{
  return regno == REG_A4;
}

/* Types larger than 64 bit, and variable sized types, are passed by
   reference.  The callee must copy them; see c6x_callee_copies.  */

static bool
c6x_pass_by_reference (cumulative_args_t cum_v ATTRIBUTE_UNUSED,
		       enum machine_mode mode, const_tree type,
		       bool named ATTRIBUTE_UNUSED)
{
  int size = -1;
  if (type)
    size = int_size_in_bytes (type);
  else if (mode != VOIDmode)
    size = GET_MODE_SIZE (mode);
  return size > 2 * UNITS_PER_WORD || size == -1;
}

/* Decide whether a type should be returned in memory (true)
   or in a register (false).  This is called by the macro
   TARGET_RETURN_IN_MEMORY.  */

static bool
c6x_return_in_memory (const_tree type, const_tree fntype ATTRIBUTE_UNUSED)
{
  int size = int_size_in_bytes (type);
  return size > 2 * UNITS_PER_WORD || size == -1;
}

/* Values which must be returned in the most-significant end of the return
   register.  */

static bool
c6x_return_in_msb (const_tree valtype)
{
  HOST_WIDE_INT size = int_size_in_bytes (valtype);
  return TARGET_BIG_ENDIAN && AGGREGATE_TYPE_P (valtype) && size == 3;
}

/* Implement TARGET_CALLEE_COPIES.  */

static bool
c6x_callee_copies (cumulative_args_t cum_v ATTRIBUTE_UNUSED,
		   enum machine_mode mode ATTRIBUTE_UNUSED,
		   const_tree type ATTRIBUTE_UNUSED,
		   bool named ATTRIBUTE_UNUSED)
{
  return true;
}

/* Return the type to use as __builtin_va_list.  */
static tree
c6x_build_builtin_va_list (void)
{
  return build_pointer_type (char_type_node);
}

static void
c6x_asm_trampoline_template (FILE *f)
{
  fprintf (f, "\t.long\t0x0000002b\n"); /* mvkl .s2 fnlow,B0 */
  fprintf (f, "\t.long\t0x01000028\n"); /* || mvkl .s1 sclow,A2 */
  fprintf (f, "\t.long\t0x0000006b\n"); /* mvkh .s2 fnhigh,B0 */
  fprintf (f, "\t.long\t0x01000068\n"); /* || mvkh .s1 schigh,A2 */
  fprintf (f, "\t.long\t0x00000362\n"); /* b .s2 B0 */
  fprintf (f, "\t.long\t0x00008000\n"); /* nop 5 */
  fprintf (f, "\t.long\t0x00000000\n"); /* nop */
  fprintf (f, "\t.long\t0x00000000\n"); /* nop */
}

/* Emit RTL insns to initialize the variable parts of a trampoline at
   TRAMP. FNADDR is an RTX for the address of the function's pure
   code.  CXT is an RTX for the static chain value for the function.  */

static void
c6x_initialize_trampoline (rtx tramp, tree fndecl, rtx cxt)
{
  rtx fnaddr = XEXP (DECL_RTL (fndecl), 0);
  rtx t1 = copy_to_reg (fnaddr);
  rtx t2 = copy_to_reg (cxt);
  rtx mask = gen_reg_rtx (SImode);
  int i;

  emit_block_move (tramp, assemble_trampoline_template (),
		   GEN_INT (TRAMPOLINE_SIZE), BLOCK_OP_NORMAL);

  emit_move_insn (mask, GEN_INT (0xffff << 7));

  for (i = 0; i < 4; i++)
    {
      rtx mem = adjust_address (tramp, SImode, i * 4);
      rtx t = (i & 1) ? t2 : t1;
      rtx v1 = gen_reg_rtx (SImode);
      rtx v2 = gen_reg_rtx (SImode);
      emit_move_insn (v1, mem);
      if (i < 2)
	emit_insn (gen_ashlsi3 (v2, t, GEN_INT (7)));
      else
	emit_insn (gen_lshrsi3 (v2, t, GEN_INT (9)));
      emit_insn (gen_andsi3 (v2, v2, mask));
      emit_insn (gen_iorsi3 (v2, v2, v1));
      emit_move_insn (mem, v2);
    }
#ifdef CLEAR_INSN_CACHE
  tramp = XEXP (tramp, 0);
  emit_library_call (gen_rtx_SYMBOL_REF (Pmode, "__gnu_clear_cache"),
		     LCT_NORMAL, VOIDmode, 2, tramp, Pmode,
		     plus_constant (Pmode, tramp, TRAMPOLINE_SIZE),
		     Pmode);
#endif
}

/* Determine whether c6x_output_mi_thunk can succeed.  */

static bool
c6x_can_output_mi_thunk (const_tree thunk ATTRIBUTE_UNUSED,
			 HOST_WIDE_INT delta ATTRIBUTE_UNUSED,
			 HOST_WIDE_INT vcall_offset ATTRIBUTE_UNUSED,
			 const_tree function ATTRIBUTE_UNUSED)
{
  return !TARGET_LONG_CALLS;
}

/* Output the assembler code for a thunk function.  THUNK is the
   declaration for the thunk function itself, FUNCTION is the decl for
   the target function.  DELTA is an immediate constant offset to be
   added to THIS.  If VCALL_OFFSET is nonzero, the word at
   *(*this + vcall_offset) should be added to THIS.  */

static void
c6x_output_mi_thunk (FILE *file ATTRIBUTE_UNUSED,
		     tree thunk ATTRIBUTE_UNUSED, HOST_WIDE_INT delta,
		     HOST_WIDE_INT vcall_offset, tree function)
{
  rtx xops[5];
  /* The this parameter is passed as the first argument.  */
  rtx this_rtx = gen_rtx_REG (Pmode, REG_A4);

  c6x_current_insn = NULL_RTX;

  xops[4] = XEXP (DECL_RTL (function), 0);
  if (!vcall_offset)
    {
      output_asm_insn ("b .s2 \t%4", xops);
      if (!delta)
	output_asm_insn ("nop 5", xops);
    }

  /* Adjust the this parameter by a fixed constant.  */
  if (delta)
    {
      xops[0] = GEN_INT (delta);
      xops[1] = this_rtx;
      if (delta >= -16 && delta <= 15)
	{
	  output_asm_insn ("add .s1 %0, %1, %1", xops);
	  if (!vcall_offset)
	    output_asm_insn ("nop 4", xops);
	}
      else if (delta >= 16 && delta < 32)
	{
	  output_asm_insn ("add .d1 %0, %1, %1", xops);
	  if (!vcall_offset)
	    output_asm_insn ("nop 4", xops);
	}
      else if (delta >= -32768 && delta < 32768)
	{
	  output_asm_insn ("mvk .s1 %0, A0", xops);
	  output_asm_insn ("add .d1 %1, A0, %1", xops);
	  if (!vcall_offset)
	    output_asm_insn ("nop 3", xops);
	}
      else
	{
	  output_asm_insn ("mvkl .s1 %0, A0", xops);
	  output_asm_insn ("mvkh .s1 %0, A0", xops);
	  output_asm_insn ("add .d1 %1, A0, %1", xops);
	  if (!vcall_offset)
	    output_asm_insn ("nop 3", xops);
	}
    }

  /* Adjust the this parameter by a value stored in the vtable.  */
  if (vcall_offset)
    {
      rtx a0tmp = gen_rtx_REG (Pmode, REG_A0);
      rtx a3tmp = gen_rtx_REG (Pmode, REG_A3);

      xops[1] = a3tmp;
      xops[2] = a0tmp;
      xops[3] = gen_rtx_MEM (Pmode, a0tmp);
      output_asm_insn ("mv .s1 a4, %2", xops);
      output_asm_insn ("ldw .d1t1 %3, %2", xops);

      /* Adjust the this parameter.  */
      xops[0] = gen_rtx_MEM (Pmode, plus_constant (Pmode, a0tmp,
						   vcall_offset));
      if (!memory_operand (xops[0], Pmode))
	{
	  rtx tmp2 = gen_rtx_REG (Pmode, REG_A1);
	  xops[0] = GEN_INT (vcall_offset);
	  xops[1] = tmp2;
	  output_asm_insn ("mvkl .s1 %0, %1", xops);
	  output_asm_insn ("mvkh .s1 %0, %1", xops);
	  output_asm_insn ("nop 2", xops);
	  output_asm_insn ("add .d1 %2, %1, %2", xops);
	  xops[0] = gen_rtx_MEM (Pmode, a0tmp);
	}
      else
	output_asm_insn ("nop 4", xops);
      xops[2] = this_rtx;
      output_asm_insn ("ldw .d1t1 %0, %1", xops);
      output_asm_insn ("|| b .s2 \t%4", xops);
      output_asm_insn ("nop 4", xops);
      output_asm_insn ("add .d1 %2, %1, %2", xops);
    }
}

/* Return true if EXP goes in small data/bss.  */

static bool
c6x_in_small_data_p (const_tree exp)
{
  /* We want to merge strings, so we never consider them small data.  */
  if (TREE_CODE (exp) == STRING_CST)
    return false;

  /* Functions are never small data.  */
  if (TREE_CODE (exp) == FUNCTION_DECL)
    return false;

  if (TREE_CODE (exp) == VAR_DECL && DECL_WEAK (exp))
    return false;

  if (TREE_CODE (exp) == VAR_DECL && DECL_SECTION_NAME (exp))
    {
      const char *section = TREE_STRING_POINTER (DECL_SECTION_NAME (exp));

      if (strcmp (section, ".neardata") == 0
	  || strncmp (section, ".neardata.", 10) == 0
	  || strncmp (section, ".gnu.linkonce.s.", 16) == 0
	  || strcmp (section, ".bss") == 0
	  || strncmp (section, ".bss.", 5) == 0
	  || strncmp (section, ".gnu.linkonce.sb.", 17) == 0
	  || strcmp (section, ".rodata") == 0
	  || strncmp (section, ".rodata.", 8) == 0
	  || strncmp (section, ".gnu.linkonce.s2.", 17) == 0)
	return true;
    }
  else
    return PLACE_IN_SDATA_P (exp);

  return false;
}

/* Return a section for X.  The only special thing we do here is to
   honor small data.  We don't have a tree type, so we can't use the
   PLACE_IN_SDATA_P macro we use everywhere else; we choose to place
   everything sized 8 bytes or smaller into small data.  */

static section *
c6x_select_rtx_section (enum machine_mode mode, rtx x,
			unsigned HOST_WIDE_INT align)
{
  if (c6x_sdata_mode == C6X_SDATA_ALL
      || (c6x_sdata_mode != C6X_SDATA_NONE && GET_MODE_SIZE (mode) <= 8))
    /* ??? Consider using mergeable sdata sections.  */
    return sdata_section;
  else
    return default_elf_select_rtx_section (mode, x, align);
}

static section *
c6x_elf_select_section (tree decl, int reloc,
			unsigned HOST_WIDE_INT align)
{
  const char *sname = NULL;
  unsigned int flags = SECTION_WRITE;
  if (c6x_in_small_data_p (decl))
    {
      switch (categorize_decl_for_section (decl, reloc))
	{
	case SECCAT_SRODATA:
	  sname = ".rodata";
	  flags = 0;
	  break;
	case SECCAT_SDATA:
	  sname = ".neardata";
	  break;
	case SECCAT_SBSS:
	  sname = ".bss";
	  flags |= SECTION_BSS;
	default:
	  break;
	}
    }
  else
    {
      switch (categorize_decl_for_section (decl, reloc))
	{
	case SECCAT_DATA:
	  sname = ".fardata";
	  break;
	case SECCAT_DATA_REL:
	  sname = ".fardata.rel";
	  break;
	case SECCAT_DATA_REL_LOCAL:
	  sname = ".fardata.rel.local";
	  break;
	case SECCAT_DATA_REL_RO:
	  sname = ".fardata.rel.ro";
	  break;
	case SECCAT_DATA_REL_RO_LOCAL:
	  sname = ".fardata.rel.ro.local";
	  break;
	case SECCAT_BSS:
	  sname = ".far";
	  flags |= SECTION_BSS;
	  break;
	case SECCAT_RODATA:
	  sname = ".const";
	  flags = 0;
	  break;
	case SECCAT_SRODATA:
	case SECCAT_SDATA:
	case SECCAT_SBSS:
	  gcc_unreachable ();
	default:
	  break;
	}
    }
  if (sname)
    {
      /* We might get called with string constants, but get_named_section
	 doesn't like them as they are not DECLs.  Also, we need to set
	 flags in that case.  */
      if (!DECL_P (decl))
	return get_section (sname, flags, NULL);
      return get_named_section (decl, sname, reloc);
    }

  return default_elf_select_section (decl, reloc, align);
}

/* Build up a unique section name, expressed as a
   STRING_CST node, and assign it to DECL_SECTION_NAME (decl).
   RELOC indicates whether the initial value of EXP requires
   link-time relocations.  */

static void ATTRIBUTE_UNUSED
c6x_elf_unique_section (tree decl, int reloc)
{
  const char *prefix = NULL;
  /* We only need to use .gnu.linkonce if we don't have COMDAT groups.  */
  bool one_only = DECL_COMDAT_GROUP (decl) && !HAVE_COMDAT_GROUP;

  if (c6x_in_small_data_p (decl))
    {
      switch (categorize_decl_for_section (decl, reloc))
	{
	case SECCAT_SDATA:
          prefix = one_only ? ".s" : ".neardata";
	  break;
	case SECCAT_SBSS:
          prefix = one_only ? ".sb" : ".bss";
	  break;
	case SECCAT_SRODATA:
          prefix = one_only ? ".s2" : ".rodata";
	  break;
	case SECCAT_RODATA_MERGE_STR:
	case SECCAT_RODATA_MERGE_STR_INIT:
	case SECCAT_RODATA_MERGE_CONST:
	case SECCAT_RODATA:
	case SECCAT_DATA:
	case SECCAT_DATA_REL:
	case SECCAT_DATA_REL_LOCAL:
	case SECCAT_DATA_REL_RO:
	case SECCAT_DATA_REL_RO_LOCAL:
	  gcc_unreachable ();
	default:
	  /* Everything else we place into default sections and hope for the
	     best.  */
	  break;
	}
    }
  else
    {
      switch (categorize_decl_for_section (decl, reloc))
	{
	case SECCAT_DATA:
	case SECCAT_DATA_REL:
	case SECCAT_DATA_REL_LOCAL:
	case SECCAT_DATA_REL_RO:
	case SECCAT_DATA_REL_RO_LOCAL:
          prefix = one_only ? ".fd" : ".fardata";
	  break;
	case SECCAT_BSS:
          prefix = one_only ? ".fb" : ".far";
	  break;
	case SECCAT_RODATA:
	case SECCAT_RODATA_MERGE_STR:
	case SECCAT_RODATA_MERGE_STR_INIT:
	case SECCAT_RODATA_MERGE_CONST:
          prefix = one_only ? ".fr" : ".const";
	  break;
	case SECCAT_SRODATA:
	case SECCAT_SDATA:
	case SECCAT_SBSS:
	  gcc_unreachable ();
	default:
	  break;
	}
    }

  if (prefix)
    {
      const char *name, *linkonce;
      char *string;

      name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
      name = targetm.strip_name_encoding (name);

      /* If we're using one_only, then there needs to be a .gnu.linkonce
	 prefix to the section name.  */
      linkonce = one_only ? ".gnu.linkonce" : "";

      string = ACONCAT ((linkonce, prefix, ".", name, NULL));

      set_decl_section_name (decl, build_string (strlen (string), string));
      return;
    }
  default_unique_section (decl, reloc);
}

static unsigned int
c6x_section_type_flags (tree decl, const char *name, int reloc)
{
  unsigned int flags = 0;

  if (strcmp (name, ".far") == 0
      || strncmp (name, ".far.", 5) == 0)
    flags |= SECTION_BSS;

  flags |= default_section_type_flags (decl, name, reloc);

  return flags;
}

/* Checks whether the given CALL_EXPR would use a caller saved
   register.  This is used to decide whether sibling call optimization
   could be performed on the respective function call.  */

static bool
c6x_call_saved_register_used (tree call_expr)
{
  CUMULATIVE_ARGS cum_v;
  cumulative_args_t cum;
  HARD_REG_SET call_saved_regset;
  tree parameter;
  enum machine_mode mode;
  tree type;
  rtx parm_rtx;
  int i;

  INIT_CUMULATIVE_ARGS (cum_v, NULL, NULL, 0, 0);
  cum = pack_cumulative_args (&cum_v);

  COMPL_HARD_REG_SET (call_saved_regset, call_used_reg_set);
  for (i = 0; i < call_expr_nargs (call_expr); i++)
    {
      parameter = CALL_EXPR_ARG (call_expr, i);
      gcc_assert (parameter);

      /* For an undeclared variable passed as parameter we will get
	 an ERROR_MARK node here.  */
      if (TREE_CODE (parameter) == ERROR_MARK)
	return true;

      type = TREE_TYPE (parameter);
      gcc_assert (type);

      mode = TYPE_MODE (type);
      gcc_assert (mode);

      if (pass_by_reference (&cum_v, mode, type, true))
 	{
 	  mode = Pmode;
 	  type = build_pointer_type (type);
 	}

       parm_rtx = c6x_function_arg (cum, mode, type, 0);

       c6x_function_arg_advance (cum, mode, type, 0);

       if (!parm_rtx)
	 continue;

       if (REG_P (parm_rtx)
	   && overlaps_hard_reg_set_p (call_saved_regset, GET_MODE (parm_rtx),
				       REGNO (parm_rtx)))
	 return true;
       if (GET_CODE (parm_rtx) == PARALLEL)
	 {
	   int n = XVECLEN (parm_rtx, 0);
	   while (n-- > 0)
	     {
	       rtx x = XEXP (XVECEXP (parm_rtx, 0, n), 0);
	       if (REG_P (x)
		   && overlaps_hard_reg_set_p (call_saved_regset,
					       GET_MODE (x), REGNO (x)))
		 return true;
	     }
	 }
    }
  return false;
}

/* Decide whether we can make a sibling call to a function.  DECL is the
   declaration of the function being targeted by the call and EXP is the
   CALL_EXPR representing the call.  */

static bool
c6x_function_ok_for_sibcall (tree decl, tree exp)
{
  /* Registers A10, A12, B10 and B12 are available as arguments
     register but unfortunately caller saved. This makes functions
     needing these registers for arguments not suitable for
     sibcalls.  */
  if (c6x_call_saved_register_used (exp))
    return false;

  if (!flag_pic)
    return true;

  if (TARGET_DSBT)
    {
      /* When compiling for DSBT, the calling function must be local,
	 so that when we reload B14 in the sibcall epilogue, it will
	 not change its value.  */
      struct cgraph_local_info *this_func;

      if (!decl)
	/* Not enough information.  */
	return false;

      this_func = cgraph_local_info (current_function_decl);
      return this_func->local;
    }

  return true;
}

/* Return true if DECL is known to be linked into section SECTION.  */

static bool
c6x_function_in_section_p (tree decl, section *section)
{
  /* We can only be certain about functions defined in the same
     compilation unit.  */
  if (!TREE_STATIC (decl))
    return false;

  /* Make sure that SYMBOL always binds to the definition in this
     compilation unit.  */
  if (!targetm.binds_local_p (decl))
    return false;

  /* If DECL_SECTION_NAME is set, assume it is trustworthy.  */
  if (!DECL_SECTION_NAME (decl))
    {
      /* Make sure that we will not create a unique section for DECL.  */
      if (flag_function_sections || DECL_COMDAT_GROUP (decl))
	return false;
    }

  return function_section (decl) == section;
}

/* Return true if a call to OP, which is a SYMBOL_REF, must be expanded
   as a long call.  */
bool
c6x_long_call_p (rtx op)
{
  tree decl;

  if (!TARGET_LONG_CALLS)
    return false;

  decl = SYMBOL_REF_DECL (op);

  /* Try to determine whether the symbol is in the same section as the current
     function.  Be conservative, and only cater for cases in which the
     whole of the current function is placed in the same section.  */
  if (decl != NULL_TREE
      && !flag_reorder_blocks_and_partition
      && TREE_CODE (decl) == FUNCTION_DECL
      && c6x_function_in_section_p (decl, current_function_section ()))
    return false;

  return true;
}

/* Emit the sequence for a call.  */
void
c6x_expand_call (rtx retval, rtx address, bool sibcall)
{
  rtx callee = XEXP (address, 0);
  rtx call_insn;

  if (!c6x_call_operand (callee, Pmode))
    {
      callee = force_reg (Pmode, callee);
      address = change_address (address, Pmode, callee);
    }
  call_insn = gen_rtx_CALL (VOIDmode, address, const0_rtx);
  if (sibcall)
    {
      call_insn = emit_call_insn (call_insn);
      use_reg (&CALL_INSN_FUNCTION_USAGE (call_insn),
	       gen_rtx_REG (Pmode, REG_B3));
    }
  else
    {
      if (retval == NULL_RTX)
	call_insn = emit_call_insn (call_insn);
      else
	call_insn = emit_call_insn (gen_rtx_SET (GET_MODE (retval), retval,
						 call_insn));
    }
  if (flag_pic)
    use_reg (&CALL_INSN_FUNCTION_USAGE (call_insn), pic_offset_table_rtx);
}

/* Legitimize PIC addresses.  If the address is already position-independent,
   we return ORIG.  Newly generated position-independent addresses go into a
   reg.  This is REG if nonzero, otherwise we allocate register(s) as
   necessary.  PICREG is the register holding the pointer to the PIC offset
   table.  */

static rtx
legitimize_pic_address (rtx orig, rtx reg, rtx picreg)
{
  rtx addr = orig;
  rtx new_rtx = orig;

  if (GET_CODE (addr) == SYMBOL_REF || GET_CODE (addr) == LABEL_REF)
    {
      int unspec = UNSPEC_LOAD_GOT;
      rtx tmp;

      if (reg == 0)
	{
	  gcc_assert (can_create_pseudo_p ());
	  reg = gen_reg_rtx (Pmode);
	}
      if (flag_pic == 2)
	{
	  if (can_create_pseudo_p ())
	    tmp = gen_reg_rtx (Pmode);
	  else
	    tmp = reg;
	  emit_insn (gen_movsi_gotoff_high (tmp, addr));
	  emit_insn (gen_movsi_gotoff_lo_sum (tmp, tmp, addr));
	  emit_insn (gen_load_got_gotoff (reg, picreg, tmp));
	}
      else
	{
	  tmp = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), unspec);
	  new_rtx = gen_const_mem (Pmode, gen_rtx_PLUS (Pmode, picreg, tmp));

	  emit_move_insn (reg, new_rtx);
	}
      if (picreg == pic_offset_table_rtx)
	crtl->uses_pic_offset_table = 1;
      return reg;
    }

  else if (GET_CODE (addr) == CONST || GET_CODE (addr) == PLUS)
    {
      rtx base;

      if (GET_CODE (addr) == CONST)
	{
	  addr = XEXP (addr, 0);
	  gcc_assert (GET_CODE (addr) == PLUS);
	}

      if (XEXP (addr, 0) == picreg)
	return orig;

      if (reg == 0)
	{
	  gcc_assert (can_create_pseudo_p ());
	  reg = gen_reg_rtx (Pmode);
	}

      base = legitimize_pic_address (XEXP (addr, 0), reg, picreg);
      addr = legitimize_pic_address (XEXP (addr, 1),
				     base == reg ? NULL_RTX : reg,
				     picreg);

      if (GET_CODE (addr) == CONST_INT)
	{
	  gcc_assert (! reload_in_progress && ! reload_completed);
	  addr = force_reg (Pmode, addr);
	}

      if (GET_CODE (addr) == PLUS && CONSTANT_P (XEXP (addr, 1)))
	{
	  base = gen_rtx_PLUS (Pmode, base, XEXP (addr, 0));
	  addr = XEXP (addr, 1);
	}

      return gen_rtx_PLUS (Pmode, base, addr);
    }

  return new_rtx;
}

/* Expand a move operation in mode MODE.  The operands are in OPERANDS.
   Returns true if no further code must be generated, false if the caller
   should generate an insn to move OPERANDS[1] to OPERANDS[0].  */

bool
expand_move (rtx *operands, enum machine_mode mode)
{
  rtx dest = operands[0];
  rtx op = operands[1];

  if ((reload_in_progress | reload_completed) == 0
      && GET_CODE (dest) == MEM && GET_CODE (op) != REG)
    operands[1] = force_reg (mode, op);
  else if (mode == SImode && symbolic_operand (op, SImode))
    {
      if (flag_pic)
	{
	  if (sdata_symbolic_operand (op, SImode))
	    {
	      emit_insn (gen_load_sdata_pic (dest, pic_offset_table_rtx, op));
	      crtl->uses_pic_offset_table = 1;
	      return true;
	    }
	  else
	    {
	      rtx temp = (reload_completed || reload_in_progress
			  ? dest : gen_reg_rtx (Pmode));

	      operands[1] = legitimize_pic_address (op, temp,
						    pic_offset_table_rtx);
	    }
	}
      else if (reload_completed
	       && !sdata_symbolic_operand (op, SImode))
	{
	  emit_insn (gen_movsi_high (dest, op));
	  emit_insn (gen_movsi_lo_sum (dest, dest, op));
	  return true;
	}
    }
  return false;
}

/* This function is called when we're about to expand an integer compare
   operation which performs COMPARISON.  It examines the second operand,
   and if it is an integer constant that cannot be used directly on the
   current machine in a comparison insn, it returns true.  */
bool
c6x_force_op_for_comparison_p (enum rtx_code code, rtx op)
{
  if (!CONST_INT_P (op) || satisfies_constraint_Iu4 (op))
    return false;

  if ((code == EQ || code == LT || code == GT)
       && !satisfies_constraint_Is5 (op))
    return true;
  if ((code == GTU || code == LTU)
      && (!TARGET_INSNS_64 || !satisfies_constraint_Iu5 (op)))
    return true;

  return false;
}

/* Emit comparison instruction if necessary, returning the expression
   that holds the compare result in the proper mode.  Return the comparison
   that should be used in the jump insn.  */

rtx
c6x_expand_compare (rtx comparison, enum machine_mode mode)
{
  enum rtx_code code = GET_CODE (comparison);
  rtx op0 = XEXP (comparison, 0);
  rtx op1 = XEXP (comparison, 1);
  rtx cmp;
  enum rtx_code jump_code = code;
  enum machine_mode op_mode = GET_MODE (op0);

  if (op_mode == DImode && (code == NE || code == EQ) && op1 == const0_rtx)
    {
      rtx t = gen_reg_rtx (SImode);
      emit_insn (gen_iorsi3 (t, gen_lowpart (SImode, op0),
			     gen_highpart (SImode, op0)));
      op_mode = SImode;
      cmp = t;
    }
  else if (op_mode == DImode)
    {
      rtx lo[2], high[2];
      rtx cmp1, cmp2;

      if (code == NE || code == GEU || code == LEU || code == GE || code == LE)
	{
	  code = reverse_condition (code);
	  jump_code = EQ;
	}
      else
	jump_code = NE;

      split_di (&op0, 1, lo, high);
      split_di (&op1, 1, lo + 1, high + 1);

      if (c6x_force_op_for_comparison_p (code, high[1])
	  || c6x_force_op_for_comparison_p (EQ, high[1]))
	high[1] = force_reg (SImode, high[1]);

      cmp1 = gen_reg_rtx (SImode);
      cmp2 = gen_reg_rtx (SImode);
      emit_insn (gen_rtx_SET (VOIDmode, cmp1,
			      gen_rtx_fmt_ee (code, SImode, high[0], high[1])));
      if (code == EQ)
	{
	  if (c6x_force_op_for_comparison_p (code, lo[1]))
	    lo[1] = force_reg (SImode, lo[1]);
	  emit_insn (gen_rtx_SET (VOIDmode, cmp2,
				  gen_rtx_fmt_ee (code, SImode, lo[0], lo[1])));
	  emit_insn (gen_andsi3 (cmp1, cmp1, cmp2));
	}
      else
	{
	  emit_insn (gen_rtx_SET (VOIDmode, cmp2,
				  gen_rtx_EQ (SImode, high[0], high[1])));
	  if (code == GT)
	    code = GTU;
	  else if (code == LT)
	    code = LTU;
	  if (c6x_force_op_for_comparison_p (code, lo[1]))
	    lo[1] = force_reg (SImode, lo[1]);
	  emit_insn (gen_cmpsi_and (cmp2, gen_rtx_fmt_ee (code, SImode,
							  lo[0], lo[1]),
				    lo[0], lo[1], cmp2));
	  emit_insn (gen_iorsi3 (cmp1, cmp1, cmp2));
	}
      cmp = cmp1;
    }
  else if (TARGET_FP && !flag_finite_math_only
	   && (op_mode == DFmode || op_mode == SFmode)
	   && code != EQ && code != NE && code != LT && code != GT
	   && code != UNLE && code != UNGE)
    {
      enum rtx_code code1, code2, code3;
      rtx (*fn) (rtx, rtx, rtx, rtx, rtx);

      jump_code = NE;
      code3 = UNKNOWN;
      switch (code)
	{
	case UNLT:
	case UNGT:
	  jump_code = EQ;
	  /* fall through */
	case LE:
	case GE:
	  code1 = code == LE || code == UNGT ? LT : GT;
	  code2 = EQ;
	  break;

	case UNORDERED:
	  jump_code = EQ;
	  /* fall through */
	case ORDERED:
	  code3 = EQ;
	  /* fall through */
	case LTGT:
	  code1 = LT;
	  code2 = GT;
	  break;

	case UNEQ:
	  code1 = LT;
	  code2 = GT;
	  jump_code = EQ;
	  break;

	default:
	  gcc_unreachable ();
	}

      cmp = gen_reg_rtx (SImode);
      emit_insn (gen_rtx_SET (VOIDmode, cmp,
			      gen_rtx_fmt_ee (code1, SImode, op0, op1)));
      fn = op_mode == DFmode ? gen_cmpdf_ior : gen_cmpsf_ior;
      emit_insn (fn (cmp, gen_rtx_fmt_ee (code2, SImode, op0, op1),
		     op0, op1, cmp));
      if (code3 != UNKNOWN)
	emit_insn (fn (cmp, gen_rtx_fmt_ee (code3, SImode, op0, op1),
		       op0, op1, cmp));
    }
  else if (op_mode == SImode && (code == NE || code == EQ) && op1 == const0_rtx)
    cmp = op0;
  else
    {
      bool is_fp_libfunc;
      is_fp_libfunc = !TARGET_FP && (op_mode == DFmode || op_mode == SFmode);

      if ((code == NE || code == GEU || code == LEU || code == GE || code == LE)
	  && !is_fp_libfunc)
	{
	  code = reverse_condition (code);
	  jump_code = EQ;
	}
      else if (code == UNGE)
	{
	  code = LT;
	  jump_code = EQ;
	}
      else if (code == UNLE)
	{
	  code = GT;
	  jump_code = EQ;
	}
      else
	jump_code = NE;

      if (is_fp_libfunc)
	{
	  rtx insns;
	  rtx libfunc;
	  switch (code)
	    {
	    case EQ:
	      libfunc = op_mode == DFmode ? eqdf_libfunc : eqsf_libfunc;
	      break;
	    case NE:
	      libfunc = op_mode == DFmode ? nedf_libfunc : nesf_libfunc;
	      break;
	    case GT:
	      libfunc = op_mode == DFmode ? gtdf_libfunc : gtsf_libfunc;
	      break;
	    case GE:
	      libfunc = op_mode == DFmode ? gedf_libfunc : gesf_libfunc;
	      break;
	    case LT:
	      libfunc = op_mode == DFmode ? ltdf_libfunc : ltsf_libfunc;
	      break;
	    case LE:
	      libfunc = op_mode == DFmode ? ledf_libfunc : lesf_libfunc;
	      break;
	    default:
	      gcc_unreachable ();
	    }
	  start_sequence ();

	  cmp = emit_library_call_value (libfunc, 0, LCT_CONST, SImode, 2,
					 op0, op_mode, op1, op_mode);
	  insns = get_insns ();
	  end_sequence ();

	  emit_libcall_block (insns, cmp, cmp,
			      gen_rtx_fmt_ee (code, SImode, op0, op1));
	}
      else
	{
	  cmp = gen_reg_rtx (SImode);
	  if (c6x_force_op_for_comparison_p (code, op1))
	    op1 = force_reg (SImode, op1);
	  emit_insn (gen_rtx_SET (VOIDmode, cmp,
				  gen_rtx_fmt_ee (code, SImode, op0, op1)));
	}
    }

  return gen_rtx_fmt_ee (jump_code, mode, cmp, const0_rtx);
}

/* Return one word of double-word value OP.  HIGH_P is true to select the
   high part, false to select the low part.  When encountering auto-increment
   addressing, we make the assumption that the low part is going to be accessed
   first.  */

rtx
c6x_subword (rtx op, bool high_p)
{
  unsigned int byte;
  enum machine_mode mode;

  mode = GET_MODE (op);
  if (mode == VOIDmode)
    mode = DImode;

  if (TARGET_BIG_ENDIAN ? !high_p : high_p)
    byte = UNITS_PER_WORD;
  else
    byte = 0;

  if (MEM_P (op))
    {
      rtx addr = XEXP (op, 0);
      if (GET_CODE (addr) == PLUS || REG_P (addr))
	return adjust_address (op, word_mode, byte);
      /* FIXME: should really support autoincrement addressing for
	 multi-word modes.  */
      gcc_unreachable ();
    }

  return simplify_gen_subreg (word_mode, op, mode, byte);
}

/* Split one or more DImode RTL references into pairs of SImode
   references.  The RTL can be REG, offsettable MEM, integer constant, or
   CONST_DOUBLE.  "operands" is a pointer to an array of DImode RTL to
   split and "num" is its length.  lo_half and hi_half are output arrays
   that parallel "operands".  */

void
split_di (rtx operands[], int num, rtx lo_half[], rtx hi_half[])
{
  while (num--)
    {
      rtx op = operands[num];

      lo_half[num] = c6x_subword (op, false);
      hi_half[num] = c6x_subword (op, true);
    }
}

/* Return true if VAL is a mask valid for a clr instruction.  */
bool
c6x_valid_mask_p (HOST_WIDE_INT val)
{
  int i;
  for (i = 0; i < 32; i++)
    if (!(val & ((unsigned HOST_WIDE_INT)1 << i)))
      break;
  for (; i < 32; i++)
    if (val & ((unsigned HOST_WIDE_INT)1 << i))
      break;
  for (; i < 32; i++)
    if (!(val & ((unsigned HOST_WIDE_INT)1 << i)))
      return false;
  return true;
}

/* Expand a block move for a movmemM pattern.  */

bool
c6x_expand_movmem (rtx dst, rtx src, rtx count_exp, rtx align_exp,
		   rtx expected_align_exp ATTRIBUTE_UNUSED,
		   rtx expected_size_exp ATTRIBUTE_UNUSED)
{
  unsigned HOST_WIDE_INT align = 1;
  unsigned HOST_WIDE_INT src_mem_align, dst_mem_align, min_mem_align;
  unsigned HOST_WIDE_INT count = 0, offset = 0;
  unsigned int biggest_move = TARGET_STDW ? 8 : 4;

  if (CONST_INT_P (align_exp))
    align = INTVAL (align_exp);

  src_mem_align = MEM_ALIGN (src) / BITS_PER_UNIT;
  dst_mem_align = MEM_ALIGN (dst) / BITS_PER_UNIT;
  min_mem_align = MIN (src_mem_align, dst_mem_align);

  if (min_mem_align > align)
    align = min_mem_align / BITS_PER_UNIT;
  if (src_mem_align < align)
    src_mem_align = align;
  if (dst_mem_align < align)
    dst_mem_align = align;

  if (CONST_INT_P (count_exp))
    count = INTVAL (count_exp);
  else
    return false;

  /* Make sure we don't need to care about overflow later on.  */
  if (count > ((unsigned HOST_WIDE_INT) 1 << 30))
    return false;

  if (count >= 28 && (count & 3) == 0 && align >= 4)
    {
      tree dst_expr = MEM_EXPR (dst);
      tree src_expr = MEM_EXPR (src);
      rtx fn = TARGET_INSNS_64PLUS ? strasgi64p_libfunc : strasgi_libfunc;
      rtx srcreg = force_reg (Pmode, XEXP (src, 0));
      rtx dstreg = force_reg (Pmode, XEXP (dst, 0));

      if (src_expr)
	mark_addressable (src_expr);
      if (dst_expr)
	mark_addressable (dst_expr);
      emit_library_call (fn, LCT_NORMAL, VOIDmode, 3,
			 dstreg, Pmode, srcreg, Pmode, count_exp, SImode);
      return true;
    }

  if (biggest_move > align && !TARGET_INSNS_64)
    biggest_move = align;

  if (count / biggest_move > 7)
    return false;

  while (count > 0)
    {
      rtx reg, reg_lowpart;
      enum machine_mode srcmode, dstmode;
      unsigned HOST_WIDE_INT src_size, dst_size, src_left;
      int shift;
      rtx srcmem, dstmem;

      while (biggest_move > count)
	biggest_move /= 2;

      src_size = dst_size = biggest_move;
      if (src_size > src_mem_align && src_size == 2)
	src_size = 1;
      if (dst_size > dst_mem_align && dst_size == 2)
	dst_size = 1;

      if (dst_size > src_size)
	dst_size = src_size;

      srcmode = mode_for_size (src_size * BITS_PER_UNIT, MODE_INT, 0);
      dstmode = mode_for_size (dst_size * BITS_PER_UNIT, MODE_INT, 0);
      if (src_size >= 4)
	reg_lowpart = reg = gen_reg_rtx (srcmode);
      else
	{
	  reg = gen_reg_rtx (SImode);
	  reg_lowpart = gen_lowpart (srcmode, reg);
	}

      srcmem = adjust_address (copy_rtx (src), srcmode, offset);

      if (src_size > src_mem_align)
	{
	  enum insn_code icode = (srcmode == SImode ? CODE_FOR_movmisalignsi
				  : CODE_FOR_movmisaligndi);
	  emit_insn (GEN_FCN (icode) (reg_lowpart, srcmem));
	}
      else
	emit_move_insn (reg_lowpart, srcmem);

      src_left = src_size;
      shift = TARGET_BIG_ENDIAN ? (src_size - dst_size) * BITS_PER_UNIT  : 0;
      while (src_left > 0)
	{
	  rtx dstreg = reg_lowpart;

	  if (src_size > dst_size)
	    {
	      rtx srcword = reg;
	      int shift_amount = shift & (BITS_PER_WORD - 1);
	      if (src_size > 4)
		srcword = operand_subword_force (srcword, src_left >= 4 ? 0 : 4,
						 SImode);
	      if (shift_amount > 0)
		{
		  dstreg = gen_reg_rtx (SImode);
		  emit_insn (gen_lshrsi3 (dstreg, srcword,
					  GEN_INT (shift_amount)));
		}
	      else
		dstreg = srcword;
	      dstreg = gen_lowpart (dstmode, dstreg);
	    }

	  dstmem = adjust_address (copy_rtx (dst), dstmode, offset);
	  if (dst_size > dst_mem_align)
	    {
	      enum insn_code icode = (dstmode == SImode ? CODE_FOR_movmisalignsi
				      : CODE_FOR_movmisaligndi);
	      emit_insn (GEN_FCN (icode) (dstmem, dstreg));
	    }
	  else
	    emit_move_insn (dstmem, dstreg);

	  if (TARGET_BIG_ENDIAN)
	    shift -= dst_size * BITS_PER_UNIT;
	  else
	    shift += dst_size * BITS_PER_UNIT;
	  offset += dst_size;
	  src_left -= dst_size;
	}
      count -= src_size;
    }
  return true;
}

/* Subroutine of print_address_operand, print a single address offset OFF for
   a memory access of mode MEM_MODE, choosing between normal form and scaled
   form depending on the type of the insn.  Misaligned memory references must
   use the scaled form.  */

static void
print_address_offset (FILE *file, rtx off, enum machine_mode mem_mode)
{
  rtx pat;

  if (c6x_current_insn != NULL_RTX)
    {
      pat = PATTERN (c6x_current_insn);
      if (GET_CODE (pat) == COND_EXEC)
	pat = COND_EXEC_CODE (pat);
      if (GET_CODE (pat) == PARALLEL)
	pat = XVECEXP (pat, 0, 0);

      if (GET_CODE (pat) == SET
	  && GET_CODE (SET_SRC (pat)) == UNSPEC
	  && XINT (SET_SRC (pat), 1) == UNSPEC_MISALIGNED_ACCESS)
	{
	  gcc_assert (CONST_INT_P (off)
		      && (INTVAL (off) & (GET_MODE_SIZE (mem_mode) - 1)) == 0);
	  fprintf (file, "[" HOST_WIDE_INT_PRINT_DEC "]",
		   INTVAL (off) / GET_MODE_SIZE (mem_mode));
	  return;
	}
    }
  fputs ("(", file);
  output_address (off);
  fputs (")", file);
}

static bool
c6x_print_operand_punct_valid_p (unsigned char c)
{
  return c == '$' || c == '.' || c == '|';
}

static void c6x_print_operand (FILE *, rtx, int);

/* Subroutine of c6x_print_operand; used to print a memory reference X to FILE.  */

static void
c6x_print_address_operand (FILE *file, rtx x, enum machine_mode mem_mode)
{
  rtx off;
  switch (GET_CODE (x))
    {
    case PRE_MODIFY:
    case POST_MODIFY:
      if (GET_CODE (x) == POST_MODIFY)
	output_address (XEXP (x, 0));
      off = XEXP (XEXP (x, 1), 1);
      if (XEXP (x, 0) == stack_pointer_rtx)
	{
	  if (GET_CODE (x) == PRE_MODIFY)
	    gcc_assert (INTVAL (off) > 0);
	  else
	    gcc_assert (INTVAL (off) < 0);
	}
      if (CONST_INT_P (off) && INTVAL (off) < 0)
	{
	  fprintf (file, "--");
	  off = GEN_INT (-INTVAL (off));
	}
      else
	fprintf (file, "++");
      if (GET_CODE (x) == PRE_MODIFY)
	output_address (XEXP (x, 0));
      print_address_offset (file, off, mem_mode);
      break;

    case PLUS:
      off = XEXP (x, 1);
      if (CONST_INT_P (off) && INTVAL (off) < 0)
	{
	  fprintf (file, "-");
	  off = GEN_INT (-INTVAL (off));
	}
      else
	fprintf (file, "+");
      output_address (XEXP (x, 0));
      print_address_offset (file, off, mem_mode);
      break;

    case PRE_DEC:
      gcc_assert (XEXP (x, 0) != stack_pointer_rtx);
      fprintf (file, "--");
      output_address (XEXP (x, 0));
      fprintf (file, "[1]");
      break;
    case PRE_INC:
      fprintf (file, "++");
      output_address (XEXP (x, 0));
      fprintf (file, "[1]");
      break;
    case POST_INC:
      gcc_assert (XEXP (x, 0) != stack_pointer_rtx);
      output_address (XEXP (x, 0));
      fprintf (file, "++[1]");
      break;
    case POST_DEC:
      output_address (XEXP (x, 0));
      fprintf (file, "--[1]");
      break;

    case SYMBOL_REF:
    case CONST:
    case LABEL_REF:
      gcc_assert (sdata_symbolic_operand (x, Pmode));
      fprintf (file, "+B14(");
      output_addr_const (file, x);
      fprintf (file, ")");
      break;

    case UNSPEC:
      switch (XINT (x, 1))
	{
	case UNSPEC_LOAD_GOT:
	  fputs ("$GOT(", file);
	  output_addr_const (file, XVECEXP (x, 0, 0));
	  fputs (")", file);
	  break;
	case UNSPEC_LOAD_SDATA:
	  output_addr_const (file, XVECEXP (x, 0, 0));
	  break;
	default:
	  gcc_unreachable ();
	}
      break;

    default:
      gcc_assert (GET_CODE (x) != MEM);
      c6x_print_operand (file, x, 0);
      break;
    }
}

/* Return a single character, which is either 'l', 's', 'd' or 'm', which
   specifies the functional unit used by INSN.  */

char
c6x_get_unit_specifier (rtx insn)
{
  enum attr_units units;

  if (insn_info.exists ())
    {
      int unit = INSN_INFO_ENTRY (INSN_UID (insn)).reservation;
      return c6x_unit_names[unit][0];
    }

  units = get_attr_units (insn);
  switch (units)
    {
    case UNITS_D:
    case UNITS_DL:
    case UNITS_DS:
    case UNITS_DLS:
    case UNITS_D_ADDR:
      return 'd';
      break;
    case UNITS_L:
    case UNITS_LS:
      return 'l';
      break;
    case UNITS_S:
      return 's';
      break;
    case UNITS_M:
      return 'm';
      break;
    default:
      gcc_unreachable ();
    }
}

/* Prints the unit specifier field.  */
static void
c6x_print_unit_specifier_field (FILE *file, rtx insn)
{
  enum attr_units units = get_attr_units (insn);
  enum attr_cross cross = get_attr_cross (insn);
  enum attr_dest_regfile rf = get_attr_dest_regfile (insn);
  int half;
  char unitspec;

  if (units == UNITS_D_ADDR)
    {
      enum attr_addr_regfile arf = get_attr_addr_regfile (insn);
      int t_half;
      gcc_assert (arf != ADDR_REGFILE_UNKNOWN);
      half = arf == ADDR_REGFILE_A ? 1 : 2;
      t_half = rf == DEST_REGFILE_A ? 1 : 2;
      fprintf (file, ".d%dt%d", half, t_half);
      return;
    }

  if (insn_info.exists ())
    {
      int unit = INSN_INFO_ENTRY (INSN_UID (insn)).reservation;
      fputs (".", file);
      fputs (c6x_unit_names[unit], file);
      if (cross == CROSS_Y)
	fputs ("x", file);
      return;
    }

  gcc_assert (rf != DEST_REGFILE_UNKNOWN);
  unitspec = c6x_get_unit_specifier (insn);
  half = rf == DEST_REGFILE_A ? 1 : 2;
  fprintf (file, ".%c%d%s", unitspec, half, cross == CROSS_Y ? "x" : "");
}

/* Output assembly language output for the address ADDR to FILE.  */
static void
c6x_print_operand_address (FILE *file, rtx addr)
{
  c6x_print_address_operand (file, addr, VOIDmode);
}

/* Print an operand, X, to FILE, with an optional modifier in CODE.

   Meaning of CODE:
   $ -- print the unit specifier field for the instruction.
   . -- print the predicate for the instruction or an emptry string for an
        unconditional one.
   | -- print "||" if the insn should be issued in parallel with the previous
        one.

   C -- print an opcode suffix for a reversed condition
   d -- H, W or D as a suffix for ADDA, based on the factor given by the
        operand
   D -- print either B, H, W or D as a suffix for ADDA, based on the size of
        the operand
   J -- print a predicate
   j -- like J, but use reverse predicate
   k -- treat a CONST_INT as a register number and print it as a register
   k -- like k, but print out a doubleword register
   n -- print an integer operand, negated
   p -- print the low part of a DImode register
   P -- print the high part of a DImode register
   r -- print the absolute value of an integer operand, shifted right by 1
   R -- print the absolute value of an integer operand, shifted right by 2
   f -- the first clear bit in an integer operand assumed to be a mask for
        a clr instruction
   F -- the last clear bit in such a mask
   s -- the first set bit in an integer operand assumed to be a mask for
        a set instruction
   S -- the last set bit in such a mask
   U -- print either 1 or 2, depending on the side of the machine used by
        the operand  */

static void
c6x_print_operand (FILE *file, rtx x, int code)
{
  int i;
  HOST_WIDE_INT v;
  tree t;
  enum machine_mode mode;

  if (code == '|')
    {
      if (GET_MODE (c6x_current_insn) != TImode)
	fputs ("||", file);
      return;
    }
  if (code == '$')
    {
      c6x_print_unit_specifier_field (file, c6x_current_insn);
      return;
    }

  if (code == '.')
    {
      x = current_insn_predicate;
      if (x)
	{
	  unsigned int regno = REGNO (XEXP (x, 0));
	  fputs ("[", file);
 	  if (GET_CODE (x) == EQ)
	    fputs ("!", file);
	  fputs (reg_names [regno], file);
	  fputs ("]", file);
	}
      return;
    }

  mode = GET_MODE (x);

  switch (code)
    {
    case 'C':
    case 'c':
      {
	enum rtx_code c = GET_CODE (x);
	if (code == 'C')
	  c = swap_condition (c);
	fputs (GET_RTX_NAME (c), file);
      }
      return;

    case 'J':
    case 'j':
      {
	unsigned int regno = REGNO (XEXP (x, 0));
	if ((GET_CODE (x) == EQ) == (code == 'J'))
	  fputs ("!", file);
        fputs (reg_names [regno], file);
      }
      return;

    case 'k':
      gcc_assert (GET_CODE (x) == CONST_INT);
      v = INTVAL (x);
      fprintf (file, "%s", reg_names[v]);
      return;
    case 'K':
      gcc_assert (GET_CODE (x) == CONST_INT);
      v = INTVAL (x);
      gcc_assert ((v & 1) == 0);
      fprintf (file, "%s:%s", reg_names[v + 1], reg_names[v]);
      return;

    case 's':
    case 'S':
    case 'f':
    case 'F':
      gcc_assert (GET_CODE (x) == CONST_INT);
      v = INTVAL (x);
      for (i = 0; i < 32; i++)
	{
	  HOST_WIDE_INT tst = v & 1;
	  if (((code == 'f' || code == 'F') && !tst)
	      || ((code == 's' || code == 'S') && tst))
	    break;
	  v >>= 1;
	}
      if (code == 'f' || code == 's')
	{
	  fprintf (file, "%d", i);
	  return;
	}
      for (;i < 32; i++)
	{
	  HOST_WIDE_INT tst = v & 1;
	  if ((code == 'F' && tst) || (code == 'S' && !tst))
	    break;
	  v >>= 1;
	}
      fprintf (file, "%d", i - 1);
      return;

    case 'n':
      gcc_assert (GET_CODE (x) == CONST_INT);
      output_addr_const (file, GEN_INT (-INTVAL (x)));
      return;

    case 'r':
      gcc_assert (GET_CODE (x) == CONST_INT);
      v = INTVAL (x);
      if (v < 0)
	v = -v;
      output_addr_const (file, GEN_INT (v >> 1));
      return;

    case 'R':
      gcc_assert (GET_CODE (x) == CONST_INT);
      v = INTVAL (x);
      if (v < 0)
	v = -v;
      output_addr_const (file, GEN_INT (v >> 2));
      return;

    case 'd':
      gcc_assert (GET_CODE (x) == CONST_INT);
      v = INTVAL (x);
      fputs (v == 2 ? "h" : v == 4 ? "w" : "d", file);
      return;

    case 'p':
    case 'P':
      gcc_assert (GET_CODE (x) == REG);
      v = REGNO (x);
      if (code == 'P')
	v++;
      fputs (reg_names[v], file);
      return;

    case 'D':
      v = 0;
      if (GET_CODE (x) == CONST)
	{
	  x = XEXP (x, 0);
	  gcc_assert (GET_CODE (x) == PLUS);
	  gcc_assert (GET_CODE (XEXP (x, 1)) == CONST_INT);
	  v = INTVAL (XEXP (x, 1));
	  x = XEXP (x, 0);

	}
      gcc_assert (GET_CODE (x) == SYMBOL_REF);

      t = SYMBOL_REF_DECL (x);
      if (DECL_P (t))
	v |= DECL_ALIGN_UNIT (t);
      else
	v |= TYPE_ALIGN_UNIT (TREE_TYPE (t));
      if (v & 1)
	fputs ("b", file);
      else if (v & 2)
	fputs ("h", file);
      else
	fputs ("w", file);
      return;

    case 'U':
      if (MEM_P (x))
	{
	  x = XEXP (x, 0);
	  if (GET_CODE (x) == PLUS
	      || GET_RTX_CLASS (GET_CODE (x)) == RTX_AUTOINC)
	    x = XEXP (x, 0);
	  if (GET_CODE (x) == CONST || GET_CODE (x) == SYMBOL_REF)
	    {
	      gcc_assert (sdata_symbolic_operand (x, Pmode));
	      fputs ("2", file);
	      return;
	    }
	}
      gcc_assert (REG_P (x));
      if (A_REGNO_P (REGNO (x)))
	fputs ("1", file);
      if (B_REGNO_P (REGNO (x)))
	fputs ("2", file);
      return;

    default:
      switch (GET_CODE (x))
	{
	case REG:
	  if (GET_MODE_SIZE (mode) == 8)
	    fprintf (file, "%s:%s", reg_names[REGNO (x) + 1],
		     reg_names[REGNO (x)]);
	  else
	    fprintf (file, "%s", reg_names[REGNO (x)]);
	  break;

	case MEM:
	  fputc ('*', file);
	  gcc_assert (XEXP (x, 0) != stack_pointer_rtx);
	  c6x_print_address_operand (file, XEXP (x, 0), GET_MODE (x));
	  break;

	case SYMBOL_REF:
	  fputc ('(', file);
	  output_addr_const (file, x);
	  fputc (')', file);
	  break;

	case CONST_INT:
	  output_addr_const (file, x);
	  break;

	case CONST_DOUBLE:
	  output_operand_lossage ("invalid const_double operand");
	  break;

	default:
	  output_addr_const (file, x);
	}
    }
}

/* Return TRUE if OP is a valid memory address with a base register of
   class C.  If SMALL_OFFSET is true, we disallow memory references which would
   require a long offset with B14/B15.  */

bool
c6x_mem_operand (rtx op, enum reg_class c, bool small_offset)
{
  enum machine_mode mode = GET_MODE (op);
  rtx base = XEXP (op, 0);
  switch (GET_CODE (base))
    {
    case REG:
      break;
    case PLUS:
      if (small_offset
	  && (XEXP (base, 0) == stack_pointer_rtx
	      || XEXP (base, 0) == pic_offset_table_rtx))
	{
	  if (!c6x_legitimate_address_p_1 (mode, base, true, true))
	    return false;
	}

      /* fall through */
    case PRE_INC:
    case PRE_DEC:
    case PRE_MODIFY:
    case POST_INC:
    case POST_DEC:
    case POST_MODIFY:
      base = XEXP (base, 0);
      break;

    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
      gcc_assert (sdata_symbolic_operand (base, Pmode));
      return !small_offset && c == B_REGS;

    default:
      return false;
    }
  return TEST_HARD_REG_BIT (reg_class_contents[ (int) (c)], REGNO (base));
}

/* Returns true if X is a valid address for use in a memory reference
   of mode MODE.  If STRICT is true, we do not allow pseudo registers
   in the address.  NO_LARGE_OFFSET is true if we are examining an
   address for use in a load or store misaligned instruction, or
   recursively examining an operand inside a PRE/POST_MODIFY.  */

bool
c6x_legitimate_address_p_1 (enum machine_mode mode, rtx x, bool strict,
			    bool no_large_offset)
{
  int size, size1;
  HOST_WIDE_INT off;
  enum rtx_code code = GET_CODE (x);

  switch (code)
    {
    case PRE_MODIFY:
    case POST_MODIFY:
      /* We can't split these into word-sized pieces yet.  */
      if (!TARGET_STDW && GET_MODE_SIZE (mode) > UNITS_PER_WORD)
	return false;
      if (GET_CODE (XEXP (x, 1)) != PLUS)
	return false;
      if (!c6x_legitimate_address_p_1 (mode, XEXP (x, 1), strict, true))
	return false;
      if (!rtx_equal_p (XEXP (x, 0), XEXP (XEXP (x, 1), 0)))
	return false;

      /* fall through */
    case PRE_INC:
    case PRE_DEC:
    case POST_INC:
    case POST_DEC:
      /* We can't split these into word-sized pieces yet.  */
      if (!TARGET_STDW && GET_MODE_SIZE (mode) > UNITS_PER_WORD)
	return false;
      x = XEXP (x, 0);
      if (!REG_P (x))
	return false;

      /* fall through */
    case REG:
      if (strict)
	return REGNO_OK_FOR_BASE_STRICT_P (REGNO (x));
      else
	return REGNO_OK_FOR_BASE_NONSTRICT_P (REGNO (x));

    case PLUS:
      if (!REG_P (XEXP (x, 0))
	  || !c6x_legitimate_address_p_1 (mode, XEXP (x, 0), strict, false))
	return false;
      /* We cannot ensure currently that both registers end up in the
	 same register file.  */
      if (REG_P (XEXP (x, 1)))
	return false;

      if (mode == BLKmode)
	size = 4;
      else if (mode == VOIDmode)
	/* ??? This can happen during ivopts.  */
	size = 1;
      else
	size = GET_MODE_SIZE (mode);

      if (flag_pic
	  && GET_CODE (XEXP (x, 1)) == UNSPEC
	  && XINT (XEXP (x, 1), 1) == UNSPEC_LOAD_SDATA
	  && XEXP (x, 0) == pic_offset_table_rtx
	  && sdata_symbolic_operand (XVECEXP (XEXP (x, 1), 0, 0), SImode))
	return !no_large_offset && size <= 4;
      if (flag_pic == 1
	  && mode == Pmode
	  && GET_CODE (XEXP (x, 1)) == UNSPEC
	  && XINT (XEXP (x, 1), 1) == UNSPEC_LOAD_GOT
	  && XEXP (x, 0) == pic_offset_table_rtx
	  && (GET_CODE (XVECEXP (XEXP (x, 1), 0, 0)) == SYMBOL_REF
	      || GET_CODE (XVECEXP (XEXP (x, 1), 0, 0)) == LABEL_REF))
	return !no_large_offset;
      if (GET_CODE (XEXP (x, 1)) != CONST_INT)
	return false;

      off = INTVAL (XEXP (x, 1));

      /* If the machine does not have doubleword load/stores, we'll use
	 word size accesses.  */
      size1 = size;
      if (size == 2 * UNITS_PER_WORD && !TARGET_STDW)
	size = UNITS_PER_WORD;

      if (((HOST_WIDE_INT)size1 - 1) & off)
	return false;
      off /= size;
      if (off > -32 && off < (size1 == size ? 32 : 28))
	return true;
      if (no_large_offset || code != PLUS || XEXP (x, 0) != stack_pointer_rtx
	  || size1 > UNITS_PER_WORD)
	return false;
      return off >= 0 && off < 32768;

    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
      return (!no_large_offset
	      /* With -fpic, we must wrap it in an unspec to show the B14
		 dependency.  */
	      && !flag_pic
	      && GET_MODE_SIZE (mode) <= UNITS_PER_WORD
	      && sdata_symbolic_operand (x, Pmode));

    default:
      return false;
    }
}

static bool
c6x_legitimate_address_p (enum machine_mode mode, rtx x, bool strict)
{
  return c6x_legitimate_address_p_1 (mode, x, strict, false);
}

static bool
c6x_legitimate_constant_p (enum machine_mode mode ATTRIBUTE_UNUSED,
			   rtx x ATTRIBUTE_UNUSED)
{
  return true;
}

/* Implements TARGET_PREFERRED_RENAME_CLASS.  */
static reg_class_t
c6x_preferred_rename_class (reg_class_t cl)
{
  if (cl == A_REGS)
    return NONPREDICATE_A_REGS;
  if (cl == B_REGS)
    return NONPREDICATE_B_REGS;
  if (cl == ALL_REGS || cl == GENERAL_REGS)
    return NONPREDICATE_REGS;
  return NO_REGS;
}

/* Implements FINAL_PRESCAN_INSN.  */
void
c6x_final_prescan_insn (rtx insn, rtx *opvec ATTRIBUTE_UNUSED,
			int noperands ATTRIBUTE_UNUSED)
{
  c6x_current_insn = insn;
}

/* A structure to describe the stack layout of a function.  The layout is
   as follows:

   [saved frame pointer (or possibly padding0)]
   --> incoming stack pointer, new hard frame pointer
   [saved call-used regs]
   [optional padding1]
   --> soft frame pointer
   [frame]
   [outgoing arguments]
   [optional padding2]

  The structure members are laid out in this order.  */

struct c6x_frame
{
  int padding0;
  /* Number of registers to save.  */
  int nregs;
  int padding1;
  HOST_WIDE_INT frame;
  int outgoing_arguments_size;
  int padding2;

  HOST_WIDE_INT to_allocate;
  /* The offsets relative to the incoming stack pointer (which
     becomes HARD_FRAME_POINTER).  */
  HOST_WIDE_INT frame_pointer_offset;
  HOST_WIDE_INT b3_offset;

  /* True if we should call push_rts/pop_rts to save and restore
     registers.  */
  bool push_rts;
};

/* Return true if we need to save and modify the PIC register in the
   prologue.  */

static bool
must_reload_pic_reg_p (void)
{
  struct cgraph_local_info *i = NULL;

  if (!TARGET_DSBT)
    return false;

  i = cgraph_local_info (current_function_decl);

  if ((crtl->uses_pic_offset_table || !crtl->is_leaf) && !i->local)
    return true;
  return false;
}

/* Return 1 if we need to save REGNO.  */
static int
c6x_save_reg (unsigned int regno)
{
  return ((df_regs_ever_live_p (regno)
	   && !call_used_regs[regno]
	   && !fixed_regs[regno])
	  || (regno == RETURN_ADDR_REGNO
	      && (df_regs_ever_live_p (regno)
		  || !crtl->is_leaf))
	  || (regno == PIC_OFFSET_TABLE_REGNUM && must_reload_pic_reg_p ()));
}

/* Examine the number of regs NREGS we've determined we must save.
   Return true if we should use __c6xabi_push_rts/__c6xabi_pop_rts for
   prologue and epilogue.  */

static bool
use_push_rts_p (int nregs)
{
  if (TARGET_INSNS_64PLUS && optimize_function_for_size_p (cfun)
      && !cfun->machine->contains_sibcall
      && !cfun->returns_struct
      && !TARGET_LONG_CALLS
      && nregs >= 6 && !frame_pointer_needed)
    return true;
  return false;
}

/* Return number of saved general prupose registers.  */

int
c6x_nsaved_regs (void)
{
  int nregs = 0;
  int regno;

  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (c6x_save_reg (regno))
      nregs++;
  return nregs;
}

/* The safe debug order mandated by the ABI.  */
static unsigned reg_save_order[] =
{
  REG_A10, REG_A11, REG_A12, REG_A13,
  REG_A14, REG_B3,
  REG_B10, REG_B11, REG_B12, REG_B13,
  REG_B14, REG_A15
};

#define N_SAVE_ORDER (sizeof reg_save_order / sizeof *reg_save_order)

/* Compute the layout of the stack frame and store it in FRAME.  */

static void
c6x_compute_frame_layout (struct c6x_frame *frame)
{
  HOST_WIDE_INT size = get_frame_size ();
  HOST_WIDE_INT offset;
  int nregs;

  /* We use the four bytes which are technically inside the caller's frame,
     usually to save the frame pointer.  */
  offset = -4;
  frame->padding0 = 0;
  nregs = c6x_nsaved_regs ();
  frame->push_rts = false;
  frame->b3_offset = 0;
  if (use_push_rts_p (nregs))
    {
      frame->push_rts = true;
      frame->b3_offset = (TARGET_BIG_ENDIAN ? -12 : -13) * 4;
      nregs = 14;
    }
  else if (c6x_save_reg (REG_B3))
    {
      int idx;
      for (idx = N_SAVE_ORDER - 1; reg_save_order[idx] != REG_B3; idx--)
	{
	  if (c6x_save_reg (reg_save_order[idx]))
	    frame->b3_offset -= 4;
	}
    }
  frame->nregs = nregs;

  if (size == 0 && nregs == 0)
    {
      frame->padding0 = 4;
      frame->padding1 = frame->padding2 = 0;
      frame->frame_pointer_offset = frame->to_allocate = 0;
      frame->outgoing_arguments_size = 0;
      return;
    }

  if (!frame->push_rts)
    offset += frame->nregs * 4;

  if (offset == 0 && size == 0 && crtl->outgoing_args_size == 0
      && !crtl->is_leaf)
    /* Don't use the bottom of the caller's frame if we have no
       allocation of our own and call other functions.  */
    frame->padding0 = frame->padding1 = 4;
  else if (offset & 4)
    frame->padding1 = 4;
  else
    frame->padding1 = 0;

  offset += frame->padding0 + frame->padding1;
  frame->frame_pointer_offset = offset;
  offset += size;

  frame->outgoing_arguments_size = crtl->outgoing_args_size;
  offset += frame->outgoing_arguments_size;

  if ((offset & 4) == 0)
    frame->padding2 = 8;
  else
    frame->padding2 = 4;
  frame->to_allocate = offset + frame->padding2;
}

/* Return the offset between two registers, one to be eliminated, and the other
   its replacement, at the start of a routine.  */

HOST_WIDE_INT
c6x_initial_elimination_offset (int from, int to)
{
  struct c6x_frame frame;
  c6x_compute_frame_layout (&frame);

  if (from == ARG_POINTER_REGNUM && to == HARD_FRAME_POINTER_REGNUM)
    return 0;
  else if (from == FRAME_POINTER_REGNUM
	   && to == HARD_FRAME_POINTER_REGNUM)
    return -frame.frame_pointer_offset;
  else
    {
      gcc_assert (to == STACK_POINTER_REGNUM);

      if (from == ARG_POINTER_REGNUM)
	return frame.to_allocate + (frame.push_rts ? 56 : 0);

      gcc_assert (from == FRAME_POINTER_REGNUM);
      return frame.to_allocate - frame.frame_pointer_offset;
    }
}

/* Given FROM and TO register numbers, say whether this elimination is
   allowed.  Frame pointer elimination is automatically handled.  */

static bool
c6x_can_eliminate (const int from ATTRIBUTE_UNUSED, const int to)
{
  if (to == STACK_POINTER_REGNUM)
    return !frame_pointer_needed;
  return true;
}

/* Emit insns to increment the stack pointer by OFFSET.  If
   FRAME_RELATED_P, set the RTX_FRAME_RELATED_P flag on the insns.
   Does nothing if the offset is zero.  */

static void
emit_add_sp_const (HOST_WIDE_INT offset, bool frame_related_p)
{
  rtx to_add = GEN_INT (offset);
  rtx orig_to_add = to_add;
  rtx insn;

  if (offset == 0)
    return;

  if (offset < -32768 || offset > 32767)
    {
      rtx reg = gen_rtx_REG (SImode, REG_A0);
      rtx low = GEN_INT (trunc_int_for_mode (offset, HImode));

      insn = emit_insn (gen_movsi_high (reg, low));
      if (frame_related_p)
	RTX_FRAME_RELATED_P (insn) = 1;
      insn = emit_insn (gen_movsi_lo_sum (reg, reg, to_add));
      if (frame_related_p)
	RTX_FRAME_RELATED_P (insn) = 1;
      to_add = reg;
    }
  insn = emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx,
				to_add));
  if (frame_related_p)
    {
      if (REG_P (to_add))
	add_reg_note (insn, REG_FRAME_RELATED_EXPR,
		      gen_rtx_SET (VOIDmode, stack_pointer_rtx,
				   gen_rtx_PLUS (Pmode, stack_pointer_rtx,
						 orig_to_add)));

      RTX_FRAME_RELATED_P (insn) = 1;
    }
}

/* Prologue and epilogue.  */
void
c6x_expand_prologue (void)
{
  struct c6x_frame frame;
  rtx insn, mem;
  int nsaved = 0;
  HOST_WIDE_INT initial_offset, off, added_already;

  c6x_compute_frame_layout (&frame);

  if (flag_stack_usage_info)
    current_function_static_stack_size = frame.to_allocate;

  initial_offset = -frame.to_allocate;
  if (frame.push_rts)
    {
      emit_insn (gen_push_rts ());
      nsaved = frame.nregs;
    }

  /* If the offsets would be too large for the memory references we will
     create to save registers, do the stack allocation in two parts.
     Ensure by subtracting 8 that we don't store to the word pointed to
     by the stack pointer.  */
  if (initial_offset < -32768)
    initial_offset = -frame.frame_pointer_offset - 8;

  if (frame.to_allocate > 0)
    gcc_assert (initial_offset != 0);

  off = -initial_offset + 4 - frame.padding0;

  mem = gen_frame_mem (Pmode, stack_pointer_rtx);

  added_already = 0;
  if (frame_pointer_needed)
    {
      rtx fp_reg = gen_rtx_REG (SImode, REG_A15);
      /* We go through some contortions here to both follow the ABI's
	 recommendation that FP == incoming SP, and to avoid writing or
	 reading the word pointed to by the stack pointer.  */
      rtx addr = gen_rtx_POST_MODIFY (Pmode, stack_pointer_rtx,
				      gen_rtx_PLUS (Pmode, stack_pointer_rtx,
						    GEN_INT (-8)));
      insn = emit_move_insn (gen_frame_mem (Pmode, addr), fp_reg);
      RTX_FRAME_RELATED_P (insn) = 1;
      nsaved++;
      insn = emit_insn (gen_addsi3 (hard_frame_pointer_rtx, stack_pointer_rtx,
				    GEN_INT (8)));
      RTX_FRAME_RELATED_P (insn) = 1;
      off -= 4;
      added_already = -8;
    }

  emit_add_sp_const (initial_offset - added_already, true);

  if (nsaved < frame.nregs)
    {
      unsigned i;

      for (i = 0; i < N_SAVE_ORDER; i++)
	{
	  int idx = N_SAVE_ORDER - i - 1;
	  unsigned regno = reg_save_order[idx];
	  rtx reg;
	  enum machine_mode save_mode = SImode;

	  if (regno == REG_A15 && frame_pointer_needed)
	    /* Already saved.  */
	    continue;
	  if (!c6x_save_reg (regno))
	    continue;

	  if (TARGET_STDW && (off & 4) == 0 && off <= 256
	      && (regno & 1) == 1
	      && i + 1 < N_SAVE_ORDER
	      && reg_save_order[idx - 1] == regno - 1
	      && c6x_save_reg (regno - 1))
	    {
	      save_mode = DImode;
	      regno--;
	      i++;
	    }
	  reg = gen_rtx_REG (save_mode, regno);
	  off -= GET_MODE_SIZE (save_mode);

	  insn = emit_move_insn (adjust_address (mem, save_mode, off),
				 reg);
	  RTX_FRAME_RELATED_P (insn) = 1;

	  nsaved += HARD_REGNO_NREGS (regno, save_mode);
	}
    }
  gcc_assert (nsaved == frame.nregs);
  emit_add_sp_const (-frame.to_allocate - initial_offset, true);
  if (must_reload_pic_reg_p ())
    {
      if (dsbt_decl == NULL)
	{
	  tree t;

	  t = build_index_type (integer_one_node);
	  t = build_array_type (integer_type_node, t);
	  t = build_decl (BUILTINS_LOCATION, VAR_DECL,
			  get_identifier ("__c6xabi_DSBT_BASE"), t);
	  DECL_ARTIFICIAL (t) = 1;
	  DECL_IGNORED_P (t) = 1;
	  DECL_EXTERNAL (t) = 1;
	  TREE_STATIC (t) = 1;
	  TREE_PUBLIC (t) = 1;
	  TREE_USED (t) = 1;

	  dsbt_decl = t;
	}
      emit_insn (gen_setup_dsbt (pic_offset_table_rtx,
				 XEXP (DECL_RTL (dsbt_decl), 0)));
    }
}

void
c6x_expand_epilogue (bool sibcall)
{
  unsigned i;
  struct c6x_frame frame;
  rtx mem;
  HOST_WIDE_INT off;
  int nsaved = 0;

  c6x_compute_frame_layout (&frame);

  mem = gen_frame_mem (Pmode, stack_pointer_rtx);

  /* Insert a dummy set/use of the stack pointer.  This creates a
     scheduler barrier between the prologue saves and epilogue restores. */
  emit_insn (gen_epilogue_barrier (stack_pointer_rtx, stack_pointer_rtx));

  /* If the offsets would be too large for the memory references we will
     create to restore registers, do a preliminary stack adjustment here.  */
  off = frame.to_allocate - frame.frame_pointer_offset + frame.padding1;
  if (frame.push_rts)
    {
      nsaved = frame.nregs;
    }
  else
    {
      if (frame.to_allocate > 32768)
	{
	  /* Don't add the entire offset so that we leave an unused word
	     above the stack pointer.  */
	  emit_add_sp_const ((off - 16) & ~7, false);
	  off &= 7;
	  off += 16;
	}
      for (i = 0; i < N_SAVE_ORDER; i++)
	{
	  unsigned regno = reg_save_order[i];
	  rtx reg;
	  enum machine_mode save_mode = SImode;

	  if (!c6x_save_reg (regno))
	    continue;
	  if (regno == REG_A15 && frame_pointer_needed)
	    continue;

	  if (TARGET_STDW && (off & 4) == 0 && off < 256
	      && (regno & 1) == 0
	      && i + 1 < N_SAVE_ORDER
	      && reg_save_order[i + 1] == regno + 1
	      && c6x_save_reg (regno + 1))
	    {
	      save_mode = DImode;
	      i++;
	    }
	  reg = gen_rtx_REG (save_mode, regno);

	  emit_move_insn (reg, adjust_address (mem, save_mode, off));

	  off += GET_MODE_SIZE (save_mode);
	  nsaved += HARD_REGNO_NREGS (regno, save_mode);
	}
    }
  if (!frame_pointer_needed)
    emit_add_sp_const (off + frame.padding0 - 4, false);
  else
    {
      rtx fp_reg = gen_rtx_REG (SImode, REG_A15);
      rtx addr = gen_rtx_PRE_MODIFY (Pmode, stack_pointer_rtx,
				      gen_rtx_PLUS (Pmode, stack_pointer_rtx,
						    GEN_INT (8)));
      emit_insn (gen_addsi3 (stack_pointer_rtx, hard_frame_pointer_rtx,
			     GEN_INT (-8)));
      emit_move_insn (fp_reg, gen_frame_mem (Pmode, addr));
      nsaved++;
    }
  gcc_assert (nsaved == frame.nregs);
  if (!sibcall)
    {
      if (frame.push_rts)
	emit_jump_insn (gen_pop_rts ());
      else
	emit_jump_insn (gen_return_internal (gen_rtx_REG (SImode,
							  RETURN_ADDR_REGNO)));
    }
}

/* Return the value of the return address for the frame COUNT steps up
   from the current frame, after the prologue.
   We punt for everything but the current frame by returning const0_rtx.  */

rtx
c6x_return_addr_rtx (int count)
{
  if (count != 0)
    return const0_rtx;

  return get_hard_reg_initial_val (Pmode, RETURN_ADDR_REGNO);
}

/* Return true iff TYPE is one of the shadow types.  */
static bool
shadow_type_p (enum attr_type type)
{
  return (type == TYPE_SHADOW || type == TYPE_LOAD_SHADOW
	  || type == TYPE_MULT_SHADOW);
}

/* Return true iff INSN is a shadow pattern.  */
static bool
shadow_p (rtx insn)
{
  if (!NONDEBUG_INSN_P (insn) || recog_memoized (insn) < 0)
    return false;
  return shadow_type_p (get_attr_type (insn));
}

/* Return true iff INSN is a shadow or blockage pattern.  */
static bool
shadow_or_blockage_p (rtx insn)
{
  enum attr_type type;
  if (!NONDEBUG_INSN_P (insn) || recog_memoized (insn) < 0)
    return false;
  type = get_attr_type (insn);
  return shadow_type_p (type) || type == TYPE_BLOCKAGE;
}

/* Translate UNITS into a bitmask of units we can reserve for this
   insn.  */
static int
get_reservation_flags (enum attr_units units)
{
  switch (units)
    {
    case UNITS_D:
    case UNITS_D_ADDR:
      return RESERVATION_FLAG_D;
    case UNITS_L:
      return RESERVATION_FLAG_L;
    case UNITS_S:
      return RESERVATION_FLAG_S;
    case UNITS_M:
      return RESERVATION_FLAG_M;
    case UNITS_LS:
      return RESERVATION_FLAG_LS;
    case UNITS_DL:
      return RESERVATION_FLAG_DL;
    case UNITS_DS:
      return RESERVATION_FLAG_DS;
    case UNITS_DLS:
      return RESERVATION_FLAG_DLS;
    default:
      return 0;
    }
}

/* Compute the side of the machine used by INSN, which reserves UNITS.
   This must match the reservations in the scheduling description.  */
static int
get_insn_side (rtx insn, enum attr_units units)
{
  if (units == UNITS_D_ADDR)
    return (get_attr_addr_regfile (insn) == ADDR_REGFILE_A ? 0 : 1);
  else
    {
      enum attr_dest_regfile rf = get_attr_dest_regfile (insn);
      if (rf == DEST_REGFILE_ANY)
	return get_attr_type (insn) == TYPE_BRANCH ? 0 : 1;
      else
	return rf == DEST_REGFILE_A ? 0 : 1;
    }
}

/* After scheduling, walk the insns between HEAD and END and assign unit
   reservations.  */
static void
assign_reservations (rtx head, rtx end)
{
  rtx insn;
  for (insn = head; insn != NEXT_INSN (end); insn = NEXT_INSN (insn))
    {
      unsigned int sched_mask, reserved;
      rtx within, last;
      int pass;
      int rsrv[2];
      int rsrv_count[2][4];
      int i;

      if (GET_MODE (insn) != TImode)
	continue;

      reserved = 0;
      last = NULL_RTX;
      /* Find the last insn in the packet.  It has a state recorded for it,
	 which we can use to determine the units we should be using.  */
      for (within = insn;
	   (within != NEXT_INSN (end)
	    && (within == insn || GET_MODE (within) != TImode));
	   within = NEXT_INSN (within))
	{
	  int icode;
	  if (!NONDEBUG_INSN_P (within))
	    continue;
	  icode = recog_memoized (within);
	  if (icode < 0)
	    continue;
	  if (shadow_p (within))
	    continue;
	  if (INSN_INFO_ENTRY (INSN_UID (within)).reservation != 0)
	    reserved |= 1 << INSN_INFO_ENTRY (INSN_UID (within)).reservation;
	  last = within;
	}
      if (last == NULL_RTX)
	continue;

      sched_mask = INSN_INFO_ENTRY (INSN_UID (last)).unit_mask;
      sched_mask &= ~reserved;

      memset (rsrv_count, 0, sizeof rsrv_count);
      rsrv[0] = rsrv[1] = ~0;
      for (i = 0; i < 8; i++)
	{
	  int side = i / 4;
	  int unit = i & 3;
	  unsigned unit_bit = 1 << (unit + side * UNIT_QID_SIDE_OFFSET);
	  /* Clear the bits which we expect to reserve in the following loop,
	     leaving the ones set which aren't present in the scheduler's
	     state and shouldn't be reserved.  */
	  if (sched_mask & unit_bit)
	    rsrv[i / 4] &= ~(1 << unit);
	}

      /* Walk through the insns that occur in the same cycle.  We use multiple
	 passes to assign units, assigning for insns with the most specific
	 requirements first.  */
      for (pass = 0; pass < 4; pass++)
	for (within = insn;
	     (within != NEXT_INSN (end)
	      && (within == insn || GET_MODE (within) != TImode));
	     within = NEXT_INSN (within))
	  {
	    int uid = INSN_UID (within);
	    int this_rsrv, side;
	    int icode;
	    enum attr_units units;
	    enum attr_type type;
	    int j;

	    if (!NONDEBUG_INSN_P (within))
	      continue;
	    icode = recog_memoized (within);
	    if (icode < 0)
	      continue;
	    if (INSN_INFO_ENTRY (uid).reservation != 0)
	      continue;
	    units = get_attr_units (within);
	    type = get_attr_type (within);
	    this_rsrv = get_reservation_flags (units);
	    if (this_rsrv == 0)
	      continue;
	    side = get_insn_side (within, units);

	    /* Certain floating point instructions are treated specially.  If
	       an insn can choose between units it can reserve, and its
	       reservation spans more than one cycle, the reservation contains
	       special markers in the first cycle to help us reconstruct what
	       the automaton chose.  */
	    if ((type == TYPE_ADDDP || type == TYPE_FP4)
		&& units == UNITS_LS)
	      {
		int test1_code = ((type == TYPE_FP4 ? UNIT_QID_FPL1 : UNIT_QID_ADDDPL1)
				  + side * UNIT_QID_SIDE_OFFSET);
		int test2_code = ((type == TYPE_FP4 ? UNIT_QID_FPS1 : UNIT_QID_ADDDPS1)
				  + side * UNIT_QID_SIDE_OFFSET);
		if ((sched_mask & (1 << test1_code)) != 0)
		  {
		    this_rsrv = RESERVATION_FLAG_L;
		    sched_mask &= ~(1 << test1_code);
		  }
		else if ((sched_mask & (1 << test2_code)) != 0)
		  {
		    this_rsrv = RESERVATION_FLAG_S;
		    sched_mask &= ~(1 << test2_code);
		  }
	      }

	    if ((this_rsrv & (this_rsrv - 1)) == 0)
	      {
		int t = exact_log2 (this_rsrv) + side * UNIT_QID_SIDE_OFFSET;
		rsrv[side] |= this_rsrv;
		INSN_INFO_ENTRY (uid).reservation = t;
		continue;
	      }

	    if (pass == 1)
	      {
		for (j = 0; j < 4; j++)
		  if (this_rsrv & (1 << j))
		    rsrv_count[side][j]++;
		continue;
	      }
	    if ((pass == 2 && this_rsrv != RESERVATION_FLAG_DLS)
		|| (pass == 3 && this_rsrv == RESERVATION_FLAG_DLS))
	      {
		int best = -1, best_cost = INT_MAX;
		for (j = 0; j < 4; j++)
		  if ((this_rsrv & (1 << j))
		      && !(rsrv[side] & (1 << j))
		      && rsrv_count[side][j] < best_cost)
		    {
		      best_cost = rsrv_count[side][j];
		      best = j;
		    }
		gcc_assert (best != -1);
		rsrv[side] |= 1 << best;
		for (j = 0; j < 4; j++)
		  if ((this_rsrv & (1 << j)) && j != best)
		    rsrv_count[side][j]--;

		INSN_INFO_ENTRY (uid).reservation
		  = best + side * UNIT_QID_SIDE_OFFSET;
	      }
	  }
    }
}

/* Return a factor by which to weight unit imbalances for a reservation
   R.  */
static int
unit_req_factor (enum unitreqs r)
{
  switch (r)
    {
    case UNIT_REQ_D:
    case UNIT_REQ_L:
    case UNIT_REQ_S:
    case UNIT_REQ_M:
    case UNIT_REQ_X:
    case UNIT_REQ_T:
      return 1;
    case UNIT_REQ_DL:
    case UNIT_REQ_LS:
    case UNIT_REQ_DS:
      return 2;
    case UNIT_REQ_DLS:
      return 3;
    default:
      gcc_unreachable ();
    }
}

/* Examine INSN, and store in REQ1/SIDE1 and REQ2/SIDE2 the unit
   requirements.  Returns zero if INSN can't be handled, otherwise
   either one or two to show how many of the two pairs are in use.
   REQ1 is always used, it holds what is normally thought of as the
   instructions reservation, e.g. UNIT_REQ_DL.  REQ2 is used to either
   describe a cross path, or for loads/stores, the T unit.  */
static int
get_unit_reqs (rtx insn, int *req1, int *side1, int *req2, int *side2)
{
  enum attr_units units;
  enum attr_cross cross;
  int side, req;

  if (!NONDEBUG_INSN_P (insn) || recog_memoized (insn) < 0)
    return 0;
  units = get_attr_units (insn);
  if (units == UNITS_UNKNOWN)
    return 0;
  side = get_insn_side (insn, units);
  cross = get_attr_cross (insn);

  req = (units == UNITS_D ? UNIT_REQ_D
	 : units == UNITS_D_ADDR ? UNIT_REQ_D
	 : units == UNITS_DL ? UNIT_REQ_DL
	 : units == UNITS_DS ? UNIT_REQ_DS
	 : units == UNITS_L ? UNIT_REQ_L
	 : units == UNITS_LS ? UNIT_REQ_LS
	 : units == UNITS_S ? UNIT_REQ_S
	 : units == UNITS_M ? UNIT_REQ_M
	 : units == UNITS_DLS ? UNIT_REQ_DLS
	 : -1);
  gcc_assert (req != -1);
  *req1 = req;
  *side1 = side;
  if (units == UNITS_D_ADDR)
    {
      *req2 = UNIT_REQ_T;
      *side2 = side ^ (cross == CROSS_Y ? 1 : 0);
      return 2;
    }
  else if (cross == CROSS_Y)
    {
      *req2 = UNIT_REQ_X;
      *side2 = side;
      return 2;
    }
  return 1;
}

/* Walk the insns between and including HEAD and TAIL, and mark the
   resource requirements in the unit_reqs table.  */
static void
count_unit_reqs (unit_req_table reqs, rtx head, rtx tail)
{
  rtx insn;

  memset (reqs, 0, sizeof (unit_req_table));

  for (insn = head; insn != NEXT_INSN (tail); insn = NEXT_INSN (insn))
    {
      int side1, side2, req1, req2;

      switch (get_unit_reqs (insn, &req1, &side1, &req2, &side2))
	{
	case 2:
	  reqs[side2][req2]++;
	  /* fall through */
	case 1:
	  reqs[side1][req1]++;
	  break;
	}
    }
}

/* Update the table REQS by merging more specific unit reservations into
   more general ones, i.e. counting (for example) UNIT_REQ_D also in
   UNIT_REQ_DL, DS, and DLS.  */
static void
merge_unit_reqs (unit_req_table reqs)
{
  int side;
  for (side = 0; side < 2; side++)
    {
      int d = reqs[side][UNIT_REQ_D];
      int l = reqs[side][UNIT_REQ_L];
      int s = reqs[side][UNIT_REQ_S];
      int dl = reqs[side][UNIT_REQ_DL];
      int ls = reqs[side][UNIT_REQ_LS];
      int ds = reqs[side][UNIT_REQ_DS];

      reqs[side][UNIT_REQ_DL] += d;
      reqs[side][UNIT_REQ_DL] += l;
      reqs[side][UNIT_REQ_DS] += d;
      reqs[side][UNIT_REQ_DS] += s;
      reqs[side][UNIT_REQ_LS] += l;
      reqs[side][UNIT_REQ_LS] += s;
      reqs[side][UNIT_REQ_DLS] += ds + dl + ls + d + l + s;
    }
}

/* Examine the table REQS and return a measure of unit imbalance by comparing
   the two sides of the machine.  If, for example, D1 is used twice and D2
   used not at all, the return value should be 1 in the absence of other
   imbalances.  */
static int
unit_req_imbalance (unit_req_table reqs)
{
  int val = 0;
  int i;

  for (i = 0; i < UNIT_REQ_MAX; i++)
    {
      int factor = unit_req_factor ((enum unitreqs) i);
      int diff = abs (reqs[0][i] - reqs[1][i]);
      val += (diff + factor - 1) / factor / 2;
    }
  return val;
}

/* Return the resource-constrained minimum iteration interval given the
   data in the REQS table.  This must have been processed with
   merge_unit_reqs already.  */
static int
res_mii (unit_req_table reqs)
{
  int side, req;
  int worst = 1;
  for (side = 0; side < 2; side++)
    for (req = 0; req < UNIT_REQ_MAX; req++)
      {
	int factor = unit_req_factor ((enum unitreqs) req);
	worst = MAX ((reqs[side][UNIT_REQ_D] + factor - 1) / factor, worst);
      }

  return worst;
}

/* Examine INSN, and store in PMASK1 and PMASK2 bitmasks that represent
   the operands that are involved in the (up to) two reservations, as
   found by get_unit_reqs.  Return true if we did this successfully, false
   if we couldn't identify what to do with INSN.  */
static bool
get_unit_operand_masks (rtx insn, unsigned int *pmask1, unsigned int *pmask2)
{
  enum attr_op_pattern op_pat;

  if (recog_memoized (insn) < 0)
    return 0;
  if (GET_CODE (PATTERN (insn)) == COND_EXEC)
    return false;
  extract_insn (insn);
  op_pat = get_attr_op_pattern (insn);
  if (op_pat == OP_PATTERN_DT)
    {
      gcc_assert (recog_data.n_operands == 2);
      *pmask1 = 1 << 0;
      *pmask2 = 1 << 1;
      return true;
    }
  else if (op_pat == OP_PATTERN_TD)
    {
      gcc_assert (recog_data.n_operands == 2);
      *pmask1 = 1 << 1;
      *pmask2 = 1 << 0;
      return true;
    }
  else if (op_pat == OP_PATTERN_SXS)
    {
      gcc_assert (recog_data.n_operands == 3);
      *pmask1 = (1 << 0) | (1 << 2);
      *pmask2 = 1 << 1;
      return true;
    }
  else if (op_pat == OP_PATTERN_SX)
    {
      gcc_assert (recog_data.n_operands == 2);
      *pmask1 = 1 << 0;
      *pmask2 = 1 << 1;
      return true;
    }
  else if (op_pat == OP_PATTERN_SSX)
    {
      gcc_assert (recog_data.n_operands == 3);
      *pmask1 = (1 << 0) | (1 << 1);
      *pmask2 = 1 << 2;
      return true;
    }
  return false;
}

/* Try to replace a register in INSN, which has corresponding rename info
   from regrename_analyze in INFO.  OP_MASK and ORIG_SIDE provide information
   about the operands that must be renamed and the side they are on.
   REQS is the table of unit reservations in the loop between HEAD and TAIL.
   We recompute this information locally after our transformation, and keep
   it only if we managed to improve the balance.  */
static void
try_rename_operands (rtx head, rtx tail, unit_req_table reqs, rtx insn,
		     insn_rr_info *info, unsigned int op_mask, int orig_side)
{
  enum reg_class super_class = orig_side == 0 ? B_REGS : A_REGS;
  HARD_REG_SET unavailable;
  du_head_p this_head;
  struct du_chain *chain;
  int i;
  unsigned tmp_mask;
  int best_reg, old_reg;
  vec<du_head_p> involved_chains = vNULL;
  unit_req_table new_reqs;

  for (i = 0, tmp_mask = op_mask; tmp_mask; i++)
    {
      du_head_p op_chain;
      if ((tmp_mask & (1 << i)) == 0)
	continue;
      if (info->op_info[i].n_chains != 1)
	goto out_fail;
      op_chain = regrename_chain_from_id (info->op_info[i].heads[0]->id);
      involved_chains.safe_push (op_chain);
      tmp_mask &= ~(1 << i);
    }

  if (involved_chains.length () > 1)
    goto out_fail;

  this_head = involved_chains[0];
  if (this_head->cannot_rename)
    goto out_fail;

  for (chain = this_head->first; chain; chain = chain->next_use)
    {
      unsigned int mask1, mask2, mask_changed;
      int count, side1, side2, req1, req2;
      insn_rr_info *this_rr = &insn_rr[INSN_UID (chain->insn)];

      count = get_unit_reqs (chain->insn, &req1, &side1, &req2, &side2);

      if (count == 0)
	goto out_fail;

      if (!get_unit_operand_masks (chain->insn, &mask1, &mask2))
	goto out_fail;

      extract_insn (chain->insn);

      mask_changed = 0;
      for (i = 0; i < recog_data.n_operands; i++)
	{
	  int j;
	  int n_this_op = this_rr->op_info[i].n_chains;
	  for (j = 0; j < n_this_op; j++)
	    {
	      du_head_p other = this_rr->op_info[i].heads[j];
	      if (regrename_chain_from_id (other->id) == this_head)
		break;
	    }
	  if (j == n_this_op)
	    continue;

	  if (n_this_op != 1)
	    goto out_fail;
	  mask_changed |= 1 << i;
	}
      gcc_assert (mask_changed != 0);
      if (mask_changed != mask1 && mask_changed != mask2)
	goto out_fail;
    }

  /* If we get here, we can do the renaming.  */
  COMPL_HARD_REG_SET (unavailable, reg_class_contents[(int) super_class]);

  old_reg = this_head->regno;
  best_reg = find_best_rename_reg (this_head, super_class, &unavailable, old_reg);

  regrename_do_replace (this_head, best_reg);

  count_unit_reqs (new_reqs, head, PREV_INSN (tail));
  merge_unit_reqs (new_reqs);
  if (dump_file)
    {
      fprintf (dump_file, "reshuffle for insn %d, op_mask %x, "
	       "original side %d, new reg %d\n",
	       INSN_UID (insn), op_mask, orig_side, best_reg);
      fprintf (dump_file, "  imbalance %d -> %d\n",
	       unit_req_imbalance (reqs), unit_req_imbalance (new_reqs));
    }
  if (unit_req_imbalance (new_reqs) > unit_req_imbalance (reqs))
    regrename_do_replace (this_head, old_reg);
  else
    memcpy (reqs, new_reqs, sizeof (unit_req_table));

 out_fail:
  involved_chains.release ();
}

/* Find insns in LOOP which would, if shifted to the other side
   of the machine, reduce an imbalance in the unit reservations.  */
static void
reshuffle_units (basic_block loop)
{
  rtx head = BB_HEAD (loop);
  rtx tail = BB_END (loop);
  rtx insn;
  unit_req_table reqs;
  edge e;
  edge_iterator ei;
  bitmap_head bbs;

  count_unit_reqs (reqs, head, PREV_INSN (tail));
  merge_unit_reqs (reqs);

  regrename_init (true);

  bitmap_initialize (&bbs, &bitmap_default_obstack);

  FOR_EACH_EDGE (e, ei, loop->preds)
    bitmap_set_bit (&bbs, e->src->index);

  bitmap_set_bit (&bbs, loop->index);
  regrename_analyze (&bbs);

  for (insn = head; insn != NEXT_INSN (tail); insn = NEXT_INSN (insn))
    {
      enum attr_units units;
      int count, side1, side2, req1, req2;
      unsigned int mask1, mask2;
      insn_rr_info *info;

      if (!NONDEBUG_INSN_P (insn))
	continue;

      count = get_unit_reqs (insn, &req1, &side1, &req2, &side2);

      if (count == 0)
	continue;

      if (!get_unit_operand_masks (insn, &mask1, &mask2))
	continue;

      info = &insn_rr[INSN_UID (insn)];
      if (info->op_info == NULL)
	continue;

      if (reqs[side1][req1] > 1
	  && reqs[side1][req1] > 2 * reqs[side1 ^ 1][req1])
	{
	  try_rename_operands (head, tail, reqs, insn, info, mask1, side1);
	}

      units = get_attr_units (insn);
      if (units == UNITS_D_ADDR)
	{
	  gcc_assert (count == 2);
	  if (reqs[side2][req2] > 1
	      && reqs[side2][req2] > 2 * reqs[side2 ^ 1][req2])
	    {
	      try_rename_operands (head, tail, reqs, insn, info, mask2, side2);
	    }
	}
    }
  regrename_finish ();
}

/* Backend scheduling state.  */
typedef struct c6x_sched_context
{
  /* The current scheduler clock, saved in the sched_reorder hook.  */
  int curr_sched_clock;

  /* Number of insns issued so far in this cycle.  */
  int issued_this_cycle;

  /* We record the time at which each jump occurs in JUMP_CYCLES.  The
     theoretical maximum for number of jumps in flight is 12: 2 every
     cycle, with a latency of 6 cycles each.  This is a circular
     buffer; JUMP_CYCLE_INDEX is the pointer to the start.  Earlier
     jumps have a higher index.  This array should be accessed through
     the jump_cycle function.  */
  int jump_cycles[12];
  int jump_cycle_index;

  /* In parallel with jump_cycles, this array records the opposite of
     the condition used in each pending jump.  This is used to
     predicate insns that are scheduled in the jump's delay slots.  If
     this is NULL_RTX no such predication happens.  */
  rtx jump_cond[12];

  /* Similar to the jump_cycles mechanism, but here we take into
     account all insns with delay slots, to avoid scheduling asms into
     the delay slots.  */
  int delays_finished_at;

  /* The following variable value is the last issued insn.  */
  rtx last_scheduled_insn;
  /* The last issued insn that isn't a shadow of another.  */
  rtx last_scheduled_iter0;

  /* The following variable value is DFA state before issuing the
     first insn in the current clock cycle.  We do not use this member
     of the structure directly; we copy the data in and out of
     prev_cycle_state.  */
  state_t prev_cycle_state_ctx;

  int reg_n_accesses[FIRST_PSEUDO_REGISTER];
  int reg_n_xaccesses[FIRST_PSEUDO_REGISTER];
  int reg_set_in_cycle[FIRST_PSEUDO_REGISTER];

  int tmp_reg_n_accesses[FIRST_PSEUDO_REGISTER];
  int tmp_reg_n_xaccesses[FIRST_PSEUDO_REGISTER];
} *c6x_sched_context_t;

/* The current scheduling state.  */
static struct c6x_sched_context ss;

/* The following variable value is DFA state before issuing the first insn
   in the current clock cycle.  This is used in c6x_variable_issue for
   comparison with the state after issuing the last insn in a cycle.  */
static state_t prev_cycle_state;

/* Set when we discover while processing an insn that it would lead to too
   many accesses of the same register.  */
static bool reg_access_stall;

/* The highest insn uid after delayed insns were split, but before loop bodies
   were copied by the modulo scheduling code.  */
static int sploop_max_uid_iter0;

/* Look up the jump cycle with index N.  For an out-of-bounds N, we return 0,
   so the caller does not specifically have to test for it.  */
static int
get_jump_cycle (int n)
{
  if (n >= 12)
    return 0;
  n += ss.jump_cycle_index;
  if (n >= 12)
    n -= 12;
  return ss.jump_cycles[n];
}

/* Look up the jump condition with index N.  */
static rtx
get_jump_cond (int n)
{
  if (n >= 12)
    return NULL_RTX;
  n += ss.jump_cycle_index;
  if (n >= 12)
    n -= 12;
  return ss.jump_cond[n];
}

/* Return the index of the first jump that occurs after CLOCK_VAR.  If no jump
   has delay slots beyond CLOCK_VAR, return -1.  */
static int
first_jump_index (int clock_var)
{
  int retval = -1;
  int n = 0;
  for (;;)
    {
      int t = get_jump_cycle (n);
      if (t <= clock_var)
	break;
      retval = n;
      n++;
    }
  return retval;
}

/* Add a new entry in our scheduling state for a jump that occurs in CYCLE
   and has the opposite condition of COND.  */
static void
record_jump (int cycle, rtx cond)
{
  if (ss.jump_cycle_index == 0)
    ss.jump_cycle_index = 11;
  else
    ss.jump_cycle_index--;
  ss.jump_cycles[ss.jump_cycle_index] = cycle;
  ss.jump_cond[ss.jump_cycle_index] = cond;
}

/* Set the clock cycle of INSN to CYCLE.  Also clears the insn's entry in
   new_conditions.  */
static void
insn_set_clock (rtx insn, int cycle)
{
  unsigned uid = INSN_UID (insn);

  if (uid >= INSN_INFO_LENGTH)
    insn_info.safe_grow (uid * 5 / 4 + 10);

  INSN_INFO_ENTRY (uid).clock = cycle;
  INSN_INFO_ENTRY (uid).new_cond = NULL;
  INSN_INFO_ENTRY (uid).reservation = 0;
  INSN_INFO_ENTRY (uid).ebb_start = false;
}

/* Return the clock cycle we set for the insn with uid UID.  */
static int
insn_uid_get_clock (int uid)
{
  return INSN_INFO_ENTRY (uid).clock;
}

/* Return the clock cycle we set for INSN.  */
static int
insn_get_clock (rtx insn)
{
  return insn_uid_get_clock (INSN_UID (insn));
}

/* Examine INSN, and if it is a conditional jump of any kind, return
   the opposite of the condition in which it branches.  Otherwise,
   return NULL_RTX.  */
static rtx
condjump_opposite_condition (rtx insn)
{
  rtx pat = PATTERN (insn);
  int icode = INSN_CODE (insn);
  rtx x = NULL;

  if (icode == CODE_FOR_br_true || icode == CODE_FOR_br_false)
    {
      x = XEXP (SET_SRC (pat), 0);
      if (icode == CODE_FOR_br_false)
	return x;
    }
  if (GET_CODE (pat) == COND_EXEC)
    {
      rtx t = COND_EXEC_CODE (pat);
      if ((GET_CODE (t) == PARALLEL
	   && GET_CODE (XVECEXP (t, 0, 0)) == RETURN)
	  || (GET_CODE (t) == UNSPEC && XINT (t, 1) == UNSPEC_REAL_JUMP)
	  || (GET_CODE (t) == SET && SET_DEST (t) == pc_rtx))
	x = COND_EXEC_TEST (pat);
    }

  if (x != NULL_RTX)
    {
      enum rtx_code code = GET_CODE (x);
      x = gen_rtx_fmt_ee (code == EQ ? NE : EQ,
			  GET_MODE (x), XEXP (x, 0),
			  XEXP (x, 1));
    }
  return x;
}

/* Return true iff COND1 and COND2 are exactly opposite conditions
   one of them NE and the other EQ.  */
static bool
conditions_opposite_p (rtx cond1, rtx cond2)
{
  return (rtx_equal_p (XEXP (cond1, 0), XEXP (cond2, 0))
	  && rtx_equal_p (XEXP (cond1, 1), XEXP (cond2, 1))
	  && GET_CODE (cond1) == reverse_condition (GET_CODE (cond2)));
}

/* Return true if we can add a predicate COND to INSN, or if INSN
   already has that predicate.  If DOIT is true, also perform the
   modification.  */
static bool
predicate_insn (rtx insn, rtx cond, bool doit)
{
  int icode;
  if (cond == NULL_RTX)
    {
      gcc_assert (!doit);
      return false;
    }

  if (get_attr_predicable (insn) == PREDICABLE_YES
      && GET_CODE (PATTERN (insn)) != COND_EXEC)
    {
      if (doit)
	{
	  rtx newpat = gen_rtx_COND_EXEC (VOIDmode, cond, PATTERN (insn));
	  PATTERN (insn) = newpat;
	  INSN_CODE (insn) = -1;
	}
      return true;
    }
  if (GET_CODE (PATTERN (insn)) == COND_EXEC
      && rtx_equal_p (COND_EXEC_TEST (PATTERN (insn)), cond))
    return true;
  icode = INSN_CODE (insn);
  if (icode == CODE_FOR_real_jump
      || icode == CODE_FOR_jump
      || icode == CODE_FOR_indirect_jump)
    {
      rtx pat = PATTERN (insn);
      rtx dest = (icode == CODE_FOR_real_jump ? XVECEXP (pat, 0, 0)
		  : icode == CODE_FOR_jump ? XEXP (SET_SRC (pat), 0)
		  : SET_SRC (pat));
      if (doit)
	{
	  rtx newpat;
	  if (REG_P (dest))
	    newpat = gen_rtx_COND_EXEC (VOIDmode, cond, PATTERN (insn));
	  else
	    newpat = gen_br_true (cond, XEXP (cond, 0), dest);
	  PATTERN (insn) = newpat;
	  INSN_CODE (insn) = -1;
	}
      return true;
    }
  if (INSN_CODE (insn) == CODE_FOR_br_true)
    {
      rtx br_cond = XEXP (SET_SRC (PATTERN (insn)), 0);
      return rtx_equal_p (br_cond, cond);
    }
  if (INSN_CODE (insn) == CODE_FOR_br_false)
    {
      rtx br_cond = XEXP (SET_SRC (PATTERN (insn)), 0);
      return conditions_opposite_p (br_cond, cond);
    }
  return false;
}

/* Initialize SC.  Used by c6x_init_sched_context and c6x_sched_init.  */
static void
init_sched_state (c6x_sched_context_t sc)
{
  sc->last_scheduled_insn = NULL_RTX;
  sc->last_scheduled_iter0 = NULL_RTX;
  sc->issued_this_cycle = 0;
  memset (sc->jump_cycles, 0, sizeof sc->jump_cycles);
  memset (sc->jump_cond, 0, sizeof sc->jump_cond);
  sc->jump_cycle_index = 0;
  sc->delays_finished_at = 0;
  sc->curr_sched_clock = 0;

  sc->prev_cycle_state_ctx = xmalloc (dfa_state_size);

  memset (sc->reg_n_accesses, 0, sizeof sc->reg_n_accesses);
  memset (sc->reg_n_xaccesses, 0, sizeof sc->reg_n_xaccesses);
  memset (sc->reg_set_in_cycle, 0, sizeof sc->reg_set_in_cycle);

  state_reset (sc->prev_cycle_state_ctx);
}

/* Allocate store for new scheduling context.  */
static void *
c6x_alloc_sched_context (void)
{
  return xmalloc (sizeof (struct c6x_sched_context));
}

/* If CLEAN_P is true then initializes _SC with clean data,
   and from the global context otherwise.  */
static void
c6x_init_sched_context (void *_sc, bool clean_p)
{
  c6x_sched_context_t sc = (c6x_sched_context_t) _sc;

  if (clean_p)
    {
      init_sched_state (sc);
    }
  else
    {
      *sc = ss;
      sc->prev_cycle_state_ctx = xmalloc (dfa_state_size);
      memcpy (sc->prev_cycle_state_ctx, prev_cycle_state, dfa_state_size);
    }
}

/* Sets the global scheduling context to the one pointed to by _SC.  */
static void
c6x_set_sched_context (void *_sc)
{
  c6x_sched_context_t sc = (c6x_sched_context_t) _sc;

  gcc_assert (sc != NULL);
  ss = *sc;
  memcpy (prev_cycle_state, sc->prev_cycle_state_ctx, dfa_state_size);
}

/* Clear data in _SC.  */
static void
c6x_clear_sched_context (void *_sc)
{
  c6x_sched_context_t sc = (c6x_sched_context_t) _sc;
  gcc_assert (_sc != NULL);

  free (sc->prev_cycle_state_ctx);
}

/* Free _SC.  */
static void
c6x_free_sched_context (void *_sc)
{
  free (_sc);
}

/* True if we are currently performing a preliminary scheduling
   pass before modulo scheduling; we can't allow the scheduler to
   modify instruction patterns using packetization assumptions,
   since there will be another scheduling pass later if modulo
   scheduling fails.  */
static bool in_hwloop;

/* Provide information about speculation capabilities, and set the
   DO_BACKTRACKING flag.  */
static void
c6x_set_sched_flags (spec_info_t spec_info)
{
  unsigned int *flags = &(current_sched_info->flags);

  if (*flags & SCHED_EBB)
    {
      *flags |= DO_BACKTRACKING | DO_PREDICATION;
    }
  if (in_hwloop)
    *flags |= DONT_BREAK_DEPENDENCIES;

  spec_info->mask = 0;
}

/* Implement the TARGET_SCHED_ISSUE_RATE hook.  */

static int
c6x_issue_rate (void)
{
  return 8;
}

/* Used together with the collapse_ndfa option, this ensures that we reach a
   deterministic automaton state before trying to advance a cycle.
   With collapse_ndfa, genautomata creates advance cycle arcs only for
   such deterministic states.  */

static rtx
c6x_sched_dfa_pre_cycle_insn (void)
{
  return const0_rtx;
}

/* We're beginning a new block.  Initialize data structures as necessary.  */

static void
c6x_sched_init (FILE *dump ATTRIBUTE_UNUSED,
		int sched_verbose ATTRIBUTE_UNUSED,
		int max_ready ATTRIBUTE_UNUSED)
{
  if (prev_cycle_state == NULL)
    {
      prev_cycle_state = xmalloc (dfa_state_size);
    }
  init_sched_state (&ss);
  state_reset (prev_cycle_state);
}

/* We are about to being issuing INSN.  Return nonzero if we cannot
   issue it on given cycle CLOCK and return zero if we should not sort
   the ready queue on the next clock start.
   For C6X, we use this function just to copy the previous DFA state
   for comparison purposes.  */

static int
c6x_dfa_new_cycle (FILE *dump ATTRIBUTE_UNUSED, int verbose ATTRIBUTE_UNUSED,
		   rtx insn ATTRIBUTE_UNUSED, int last_clock ATTRIBUTE_UNUSED,
		   int clock ATTRIBUTE_UNUSED, int *sort_p ATTRIBUTE_UNUSED)
{
  if (clock != last_clock)
    memcpy (prev_cycle_state, curr_state, dfa_state_size);
  return 0;
}

static void
c6x_mark_regno_read (int regno, bool cross)
{
  int t = ++ss.tmp_reg_n_accesses[regno];

  if (t > 4)
    reg_access_stall = true;

  if (cross)
    {
      int set_cycle = ss.reg_set_in_cycle[regno];
      /* This must be done in this way rather than by tweaking things in
	 adjust_cost, since the stall occurs even for insns with opposite
	 predicates, and the scheduler may not even see a dependency.  */
      if (set_cycle > 0 && set_cycle == ss.curr_sched_clock)
	reg_access_stall = true;
      /* This doesn't quite do anything yet as we're only modeling one
	 x unit.  */
      ++ss.tmp_reg_n_xaccesses[regno];
    }
}

/* Note that REG is read in the insn being examined.  If CROSS, it
   means the access is through a cross path.  Update the temporary reg
   access arrays, and set REG_ACCESS_STALL if the insn can't be issued
   in the current cycle.  */

static void
c6x_mark_reg_read (rtx reg, bool cross)
{
  unsigned regno = REGNO (reg);
  unsigned nregs = hard_regno_nregs[regno][GET_MODE (reg)];

  while (nregs-- > 0)
    c6x_mark_regno_read (regno + nregs, cross);
}

/* Note that register REG is written in cycle CYCLES.  */

static void
c6x_mark_reg_written (rtx reg, int cycles)
{
  unsigned regno = REGNO (reg);
  unsigned nregs = hard_regno_nregs[regno][GET_MODE (reg)];

  while (nregs-- > 0)
    ss.reg_set_in_cycle[regno + nregs] = cycles;
}

/* Update the register state information for an instruction whose
   body is X.  Return true if the instruction has to be delayed until the
   next cycle.  */

static bool
c6x_registers_update (rtx insn)
{
  enum attr_cross cross;
  enum attr_dest_regfile destrf;
  int i, nops;
  rtx x;

  if (!reload_completed || recog_memoized (insn) < 0)
    return false;

  reg_access_stall = false;
  memcpy (ss.tmp_reg_n_accesses, ss.reg_n_accesses,
	  sizeof ss.tmp_reg_n_accesses);
  memcpy (ss.tmp_reg_n_xaccesses, ss.reg_n_xaccesses,
	  sizeof ss.tmp_reg_n_xaccesses);

  extract_insn (insn);

  cross = get_attr_cross (insn);
  destrf = get_attr_dest_regfile (insn);

  nops = recog_data.n_operands;
  x = PATTERN (insn);
  if (GET_CODE (x) == COND_EXEC)
    {
      c6x_mark_reg_read (XEXP (XEXP (x, 0), 0), false);
      nops -= 2;
    }

  for (i = 0; i < nops; i++)
    {
      rtx op = recog_data.operand[i];
      if (recog_data.operand_type[i] == OP_OUT)
	continue;
      if (REG_P (op))
	{
	  bool this_cross = cross;
	  if (destrf == DEST_REGFILE_A && A_REGNO_P (REGNO (op)))
	    this_cross = false;
	  if (destrf == DEST_REGFILE_B && B_REGNO_P (REGNO (op)))
	    this_cross = false;
	  c6x_mark_reg_read (op, this_cross);
	}
      else if (MEM_P (op))
	{
	  op = XEXP (op, 0);
	  switch (GET_CODE (op))
	    {
	    case POST_INC:
	    case PRE_INC:
	    case POST_DEC:
	    case PRE_DEC:
	      op = XEXP (op, 0);
	      /* fall through */
	    case REG:
	      c6x_mark_reg_read (op, false);
	      break;
	    case POST_MODIFY:
	    case PRE_MODIFY:
	      op = XEXP (op, 1);
	      gcc_assert (GET_CODE (op) == PLUS);
	      /* fall through */
	    case PLUS:
	      c6x_mark_reg_read (XEXP (op, 0), false);
	      if (REG_P (XEXP (op, 1)))
		c6x_mark_reg_read (XEXP (op, 1), false);
	      break;
	    case SYMBOL_REF:
	    case LABEL_REF:
	    case CONST:
	      c6x_mark_regno_read (REG_B14, false);
	      break;
	    default:
	      gcc_unreachable ();
	    }
	}
      else if (!CONSTANT_P (op) && strlen (recog_data.constraints[i]) > 0)
	gcc_unreachable ();
    }
  return reg_access_stall;
}

/* Helper function for the TARGET_SCHED_REORDER and
   TARGET_SCHED_REORDER2 hooks.  If scheduling an insn would be unsafe
   in the current cycle, move it down in the ready list and return the
   number of non-unsafe insns.  */

static int
c6x_sched_reorder_1 (rtx *ready, int *pn_ready, int clock_var)
{
  int n_ready = *pn_ready;
  rtx *e_ready = ready + n_ready;
  rtx *insnp;
  int first_jump;

  /* Keep track of conflicts due to a limit number of register accesses,
     and due to stalls incurred by too early accesses of registers using
     cross paths.  */

  for (insnp = ready; insnp < e_ready; insnp++)
    {
      rtx insn = *insnp;
      int icode = recog_memoized (insn);
      bool is_asm = (icode < 0
		     && (GET_CODE (PATTERN (insn)) == ASM_INPUT
			 || asm_noperands (PATTERN (insn)) >= 0));
      bool no_parallel = (is_asm || icode == CODE_FOR_sploop
			  || (icode >= 0
			      && get_attr_type (insn) == TYPE_ATOMIC));

      /* We delay asm insns until all delay slots are exhausted.  We can't
	 accurately tell how many cycles an asm takes, and the main scheduling
	 code always assumes at least 1 cycle, which may be wrong.  */
      if ((no_parallel
	   && (ss.issued_this_cycle > 0 || clock_var < ss.delays_finished_at))
	  || c6x_registers_update (insn)
	  || (ss.issued_this_cycle > 0 && icode == CODE_FOR_sploop))
	{
	  memmove (ready + 1, ready, (insnp - ready) * sizeof (rtx));
	  *ready = insn;
	  n_ready--;
	  ready++;
	}
      else if (shadow_p (insn))
	{
	  memmove (ready + 1, ready, (insnp - ready) * sizeof (rtx));
	  *ready = insn;
	}
    }

  /* Ensure that no other jump is scheduled in jump delay slots, since
     it would put the machine into the wrong state.  Also, we must
     avoid scheduling insns that have a latency longer than the
     remaining jump delay slots, as the code at the jump destination
     won't be prepared for it.

     However, we can relax this condition somewhat.  The rest of the
     scheduler will automatically avoid scheduling an insn on which
     the jump shadow depends so late that its side effect happens
     after the jump.  This means that if we see an insn with a longer
     latency here, it can safely be scheduled if we can ensure that it
     has a predicate opposite of the previous jump: the side effect
     will happen in what we think of as the same basic block.  In
     c6x_variable_issue, we will record the necessary predicate in
     new_conditions, and after scheduling is finished, we will modify
     the insn.

     Special care must be taken whenever there is more than one jump
     in flight.  */

  first_jump = first_jump_index (clock_var);
  if (first_jump != -1)
    {
      int first_cycle = get_jump_cycle (first_jump);
      rtx first_cond = get_jump_cond (first_jump);
      int second_cycle = 0;

      if (first_jump > 0)
	second_cycle = get_jump_cycle (first_jump - 1);

      for (insnp = ready; insnp < e_ready; insnp++)
	{
	  rtx insn = *insnp;
	  int icode = recog_memoized (insn);
	  bool is_asm = (icode < 0
			 && (GET_CODE (PATTERN (insn)) == ASM_INPUT
			     || asm_noperands (PATTERN (insn)) >= 0));
	  int this_cycles, rsrv_cycles;
	  enum attr_type type;

	  gcc_assert (!is_asm);
	  if (icode < 0)
	    continue;
	  this_cycles = get_attr_cycles (insn);
	  rsrv_cycles = get_attr_reserve_cycles (insn);
	  type = get_attr_type (insn);
	  /* Treat branches specially; there is also a hazard if two jumps
	     end at the same cycle.  */
	  if (type == TYPE_BRANCH || type == TYPE_CALL)
	    this_cycles++;
	  if (clock_var + this_cycles <= first_cycle)
	    continue;
	  if ((first_jump > 0 && clock_var + this_cycles > second_cycle)
	      || clock_var + rsrv_cycles > first_cycle
	      || !predicate_insn (insn, first_cond, false))
	    {
	      memmove (ready + 1, ready, (insnp - ready) * sizeof (rtx));
	      *ready = insn;
	      n_ready--;
	      ready++;
	    }
	}
    }

  return n_ready;
}

/* Implement the TARGET_SCHED_REORDER hook.  We save the current clock
   for later and clear the register access information for the new
   cycle.  We also move asm statements out of the way if they would be
   scheduled in a delay slot.  */

static int
c6x_sched_reorder (FILE *dump ATTRIBUTE_UNUSED,
		   int sched_verbose ATTRIBUTE_UNUSED,
		   rtx *ready ATTRIBUTE_UNUSED,
		   int *pn_ready ATTRIBUTE_UNUSED, int clock_var)
{
  ss.curr_sched_clock = clock_var;
  ss.issued_this_cycle = 0;
  memset (ss.reg_n_accesses, 0, sizeof ss.reg_n_accesses);
  memset (ss.reg_n_xaccesses, 0, sizeof ss.reg_n_xaccesses);

  if (ready == NULL)
    return 0;

  return c6x_sched_reorder_1 (ready, pn_ready, clock_var);
}

/* Implement the TARGET_SCHED_REORDER2 hook.  We use this to record the clock
   cycle for every insn.  */

static int
c6x_sched_reorder2 (FILE *dump ATTRIBUTE_UNUSED,
		    int sched_verbose ATTRIBUTE_UNUSED,
		    rtx *ready ATTRIBUTE_UNUSED,
		    int *pn_ready ATTRIBUTE_UNUSED, int clock_var)
{
  /* FIXME: the assembler rejects labels inside an execute packet.
     This can occur if prologue insns are scheduled in parallel with
     others, so we avoid this here.  Also make sure that nothing is
     scheduled in parallel with a TYPE_ATOMIC insn or after a jump.  */
  if (RTX_FRAME_RELATED_P (ss.last_scheduled_insn)
      || JUMP_P (ss.last_scheduled_insn)
      || (recog_memoized (ss.last_scheduled_insn) >= 0
	  && get_attr_type (ss.last_scheduled_insn) == TYPE_ATOMIC))
    {
      int n_ready = *pn_ready;
      rtx *e_ready = ready + n_ready;
      rtx *insnp;

      for (insnp = ready; insnp < e_ready; insnp++)
	{
	  rtx insn = *insnp;
	  if (!shadow_p (insn))
	    {
	      memmove (ready + 1, ready, (insnp - ready) * sizeof (rtx));
	      *ready = insn;
	      n_ready--;
	      ready++;
	    }
	}
      return n_ready;
    }

  return c6x_sched_reorder_1 (ready, pn_ready, clock_var);
}

/* Subroutine of maybe_clobber_cond, called through note_stores.  */

static void
clobber_cond_1 (rtx x, const_rtx pat ATTRIBUTE_UNUSED, void *data1)
{
  rtx *cond = (rtx *)data1;
  if (*cond != NULL_RTX && reg_overlap_mentioned_p (x, *cond))
    *cond = NULL_RTX;
}

/* Examine INSN, and if it destroys the conditions have recorded for
   any of the jumps in flight, clear that condition so that we don't
   predicate any more insns.  CLOCK_VAR helps us limit the search to
   only those jumps which are still in flight.  */

static void
maybe_clobber_cond (rtx insn, int clock_var)
{
  int n, idx;
  idx = ss.jump_cycle_index;
  for (n = 0; n < 12; n++, idx++)
    {
      rtx cond, link;
      int cycle;

      if (idx >= 12)
	idx -= 12;
      cycle = ss.jump_cycles[idx];
      if (cycle <= clock_var)
	return;

      cond = ss.jump_cond[idx];
      if (cond == NULL_RTX)
	continue;

      if (CALL_P (insn))
	{
	  ss.jump_cond[idx] = NULL_RTX;
	  continue;
	}

      note_stores (PATTERN (insn), clobber_cond_1, ss.jump_cond + idx);
      for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
	if (REG_NOTE_KIND (link) == REG_INC)
	  clobber_cond_1 (XEXP (link, 0), NULL_RTX, ss.jump_cond + idx);
    }
}

/* Implement the TARGET_SCHED_VARIABLE_ISSUE hook.  We are about to
   issue INSN.  Return the number of insns left on the ready queue
   that can be issued this cycle.
   We use this hook to record clock cycles and reservations for every insn.  */

static int
c6x_variable_issue (FILE *dump ATTRIBUTE_UNUSED,
		    int sched_verbose ATTRIBUTE_UNUSED,
		    rtx insn, int can_issue_more ATTRIBUTE_UNUSED)
{
  ss.last_scheduled_insn = insn;
  if (INSN_UID (insn) < sploop_max_uid_iter0 && !JUMP_P (insn))
    ss.last_scheduled_iter0 = insn;
  if (GET_CODE (PATTERN (insn)) != USE && GET_CODE (PATTERN (insn)) != CLOBBER)
    ss.issued_this_cycle++;
  if (insn_info.exists ())
    {
      state_t st_after = alloca (dfa_state_size);
      int curr_clock = ss.curr_sched_clock;
      int uid = INSN_UID (insn);
      int icode = recog_memoized (insn);
      rtx first_cond;
      int first, first_cycle;
      unsigned int mask;
      int i;

      insn_set_clock (insn, curr_clock);
      INSN_INFO_ENTRY (uid).ebb_start
	= curr_clock == 0 && ss.issued_this_cycle == 1;

      first = first_jump_index (ss.curr_sched_clock);
      if (first == -1)
	{
	  first_cycle = 0;
	  first_cond = NULL_RTX;
	}
      else
	{
	  first_cycle = get_jump_cycle (first);
	  first_cond = get_jump_cond (first);
	}
      if (icode >= 0
	  && first_cycle > curr_clock
	  && first_cond != NULL_RTX
	  && (curr_clock + get_attr_cycles (insn) > first_cycle
	      || get_attr_type (insn) == TYPE_BRANCH
	      || get_attr_type (insn) == TYPE_CALL))
	INSN_INFO_ENTRY (uid).new_cond = first_cond;

      memcpy (st_after, curr_state, dfa_state_size);
      state_transition (st_after, const0_rtx);

      mask = 0;
      for (i = 0; i < 2 * UNIT_QID_SIDE_OFFSET; i++)
	if (cpu_unit_reservation_p (st_after, c6x_unit_codes[i])
	    && !cpu_unit_reservation_p (prev_cycle_state, c6x_unit_codes[i]))
	  mask |= 1 << i;
      INSN_INFO_ENTRY (uid).unit_mask = mask;

      maybe_clobber_cond (insn, curr_clock);

      if (icode >= 0)
	{
	  int i, cycles;

	  c6x_registers_update (insn);
	  memcpy (ss.reg_n_accesses, ss.tmp_reg_n_accesses,
		  sizeof ss.reg_n_accesses);
	  memcpy (ss.reg_n_xaccesses, ss.tmp_reg_n_accesses,
		  sizeof ss.reg_n_xaccesses);

	  cycles = get_attr_cycles (insn);
	  if (ss.delays_finished_at < ss.curr_sched_clock + cycles)
	    ss.delays_finished_at = ss.curr_sched_clock + cycles;
	  if (get_attr_type (insn) == TYPE_BRANCH
	      || get_attr_type (insn) == TYPE_CALL)
	    {
	      rtx opposite = condjump_opposite_condition (insn);
	      record_jump (ss.curr_sched_clock + cycles, opposite);
	    }

	  /* Mark the cycles in which the destination registers are written.
	     This is used for calculating stalls when using cross units.  */
	  extract_insn (insn);
	  /* Cross-path stalls don't apply to results of load insns.  */
	  if (get_attr_type (insn) == TYPE_LOAD
	      || get_attr_type (insn) == TYPE_LOADN
	      || get_attr_type (insn) == TYPE_LOAD_SHADOW)
	    cycles--;
	  for (i = 0; i < recog_data.n_operands; i++)
	    {
	      rtx op = recog_data.operand[i];
	      if (MEM_P (op))
		{
		  rtx addr = XEXP (op, 0);
		  if (GET_RTX_CLASS (GET_CODE (addr)) == RTX_AUTOINC)
		    c6x_mark_reg_written (XEXP (addr, 0),
					  insn_uid_get_clock (uid) + 1);
		}
	      if (recog_data.operand_type[i] != OP_IN
		  && REG_P (op))
		{
		  c6x_mark_reg_written (op,
					insn_uid_get_clock (uid) + cycles);
		}
	    }
	}
    }
  return can_issue_more;
}

/* Implement the TARGET_SCHED_ADJUST_COST hook.  We need special handling for
   anti- and output dependencies.  */

static int
c6x_adjust_cost (rtx insn, rtx link, rtx dep_insn, int cost)
{
  enum attr_type insn_type = TYPE_UNKNOWN, dep_insn_type = TYPE_UNKNOWN;
  int dep_insn_code_number, insn_code_number;
  int shadow_bonus = 0;
  enum reg_note kind;
  dep_insn_code_number = recog_memoized (dep_insn);
  insn_code_number = recog_memoized (insn);

  if (dep_insn_code_number >= 0)
    dep_insn_type = get_attr_type (dep_insn);

  if (insn_code_number >= 0)
    insn_type = get_attr_type (insn);

  kind = REG_NOTE_KIND (link);
  if (kind == 0)
    {
      /* If we have a dependency on a load, and it's not for the result of
	 the load, it must be for an autoincrement.  Reduce the cost in that
	 case.  */
      if (dep_insn_type == TYPE_LOAD)
	{
	  rtx set = PATTERN (dep_insn);
	  if (GET_CODE (set) == COND_EXEC)
	    set = COND_EXEC_CODE (set);
	  if (GET_CODE (set) == UNSPEC)
	    cost = 1;
	  else
	    {
	      gcc_assert (GET_CODE (set) == SET);
	      if (!reg_overlap_mentioned_p (SET_DEST (set), PATTERN (insn)))
		cost = 1;
	    }
	}
    }

  /* A jump shadow needs to have its latency decreased by one.  Conceptually,
     it occurs in between two cycles, but we schedule it at the end of the
     first cycle.  */
  if (shadow_type_p (insn_type))
    shadow_bonus = 1;

  /* Anti and output dependencies usually have zero cost, but we want
     to insert a stall after a jump, and after certain floating point
     insns that take more than one cycle to read their inputs.  In the
     future, we should try to find a better algorithm for scheduling
     jumps.  */
  if (kind != 0)
    {
      /* We can get anti-dependencies against shadow insns.  Treat these
	 like output dependencies, so that the insn is entirely finished
	 before the branch takes place.  */
      if (kind == REG_DEP_ANTI && insn_type == TYPE_SHADOW)
	kind = REG_DEP_OUTPUT;
      switch (dep_insn_type)
	{
	case TYPE_CALLP:
	  return 1;
	case TYPE_BRANCH:
	case TYPE_CALL:
	  if (get_attr_has_shadow (dep_insn) == HAS_SHADOW_Y)
	    /* This is a real_jump/real_call insn.  These don't have
	       outputs, and ensuring the validity of scheduling things
	       in the delay slot is the job of
	       c6x_sched_reorder_1.  */
	    return 0;
	  /* Unsplit calls can happen - e.g. for divide insns.  */
	  return 6;
	case TYPE_LOAD:
	case TYPE_LOADN:
	case TYPE_INTDP:
	  if (kind == REG_DEP_OUTPUT)
	    return 5 - shadow_bonus;
	  return 0;
	case TYPE_MPY4:
	case TYPE_FP4:
	  if (kind == REG_DEP_OUTPUT)
	    return 4 - shadow_bonus;
	  return 0;
	case TYPE_MPY2:
	  if (kind == REG_DEP_OUTPUT)
	    return 2 - shadow_bonus;
	  return 0;
	case TYPE_CMPDP:
	  if (kind == REG_DEP_OUTPUT)
	    return 2 - shadow_bonus;
	  return 2;
	case TYPE_ADDDP:
	case TYPE_MPYSPDP:
	  if (kind == REG_DEP_OUTPUT)
	    return 7 - shadow_bonus;
	  return 2;
	case TYPE_MPYSP2DP:
	  if (kind == REG_DEP_OUTPUT)
	    return 5 - shadow_bonus;
	  return 2;
	case TYPE_MPYI:
	  if (kind == REG_DEP_OUTPUT)
	    return 9 - shadow_bonus;
	  return 4;
	case TYPE_MPYID:
	case TYPE_MPYDP:
	  if (kind == REG_DEP_OUTPUT)
	    return 10 - shadow_bonus;
	  return 4;

	default:
	  if (insn_type == TYPE_SPKERNEL)
	    return 0;
	  if (kind == REG_DEP_OUTPUT)
	    return 1 - shadow_bonus;

	  return 0;
	}
    }

  return cost - shadow_bonus;
}

/* Create a SEQUENCE rtx to replace the instructions in SLOT, of which there
   are N_FILLED.  REAL_FIRST identifies the slot if the insn that appears
   first in the original stream.  */

static void
gen_one_bundle (rtx *slot, int n_filled, int real_first)
{
  rtx bundle;
  rtx t;
  int i;

  bundle = gen_rtx_SEQUENCE (VOIDmode, gen_rtvec_v (n_filled, slot));
  bundle = make_insn_raw (bundle);
  BLOCK_FOR_INSN (bundle) = BLOCK_FOR_INSN (slot[0]);
  INSN_LOCATION (bundle) = INSN_LOCATION (slot[0]);
  PREV_INSN (bundle) = PREV_INSN (slot[real_first]);

  t = NULL_RTX;

  for (i = 0; i < n_filled; i++)
    {
      rtx insn = slot[i];
      remove_insn (insn);
      PREV_INSN (insn) = t ? t : PREV_INSN (bundle);
      if (t != NULL_RTX)
	NEXT_INSN (t) = insn;
      t = insn;
      if (i > 0)
	INSN_LOCATION (slot[i]) = INSN_LOCATION (bundle);
    }

  NEXT_INSN (bundle) = NEXT_INSN (PREV_INSN (bundle));
  NEXT_INSN (t) = NEXT_INSN (bundle);
  NEXT_INSN (PREV_INSN (bundle)) = bundle;
  PREV_INSN (NEXT_INSN (bundle)) = bundle;
}

/* Move all parallel instructions into SEQUENCEs, so that no subsequent passes
   try to insert labels in the middle.  */

static void
c6x_gen_bundles (void)
{
  basic_block bb;
  rtx insn, next, last_call;

  FOR_EACH_BB_FN (bb, cfun)
    {
      rtx insn, next;
      /* The machine is eight insns wide.  We can have up to six shadow
	 insns, plus an extra slot for merging the jump shadow.  */
      rtx slot[15];
      int n_filled = 0;
      int first_slot = 0;

      for (insn = BB_HEAD (bb);; insn = next)
	{
	  int at_end;
	  rtx delete_this = NULL_RTX;

	  if (NONDEBUG_INSN_P (insn))
	    {
	      /* Put calls at the start of the sequence.  */
	      if (CALL_P (insn))
		{
		  first_slot++;
		  if (n_filled)
		    {
		      memmove (&slot[1], &slot[0],
			       n_filled * sizeof (slot[0]));
		    }
		  if (!shadow_p (insn))
		    {
		      PUT_MODE (insn, TImode);
		      if (n_filled)
			PUT_MODE (slot[1], VOIDmode);
		    }
		  n_filled++;
		  slot[0] = insn;
		}
	      else
		{
		  slot[n_filled++] = insn;
		}
	    }

	  next = NEXT_INSN (insn);
	  while (next && insn != BB_END (bb)
		 && !(NONDEBUG_INSN_P (next)
		      && GET_CODE (PATTERN (next)) != USE
		      && GET_CODE (PATTERN (next)) != CLOBBER))
	    {
	      insn = next;
	      next = NEXT_INSN (insn);
	    }

	  at_end = insn == BB_END (bb);
	  if (delete_this == NULL_RTX
	      && (at_end || (GET_MODE (next) == TImode
			     && !(shadow_p (next) && CALL_P (next)))))
	    {
	      if (n_filled >= 2)
		gen_one_bundle (slot, n_filled, first_slot);

	      n_filled = 0;
	      first_slot = 0;
	    }
	  if (at_end)
	    break;
	}
    }
  /* Bundling, and emitting nops, can separate
     NOTE_INSN_CALL_ARG_LOCATION from the corresponding calls.  Fix
     that up here.  */
  last_call = NULL_RTX;
  for (insn = get_insns (); insn; insn = next)
    {
      next = NEXT_INSN (insn);
      if (CALL_P (insn)
	  || (INSN_P (insn) && GET_CODE (PATTERN (insn)) == SEQUENCE
	      && CALL_P (XVECEXP (PATTERN (insn), 0, 0))))
	last_call = insn;
      if (!NOTE_P (insn) || NOTE_KIND (insn) != NOTE_INSN_CALL_ARG_LOCATION)
	continue;
      if (NEXT_INSN (last_call) == insn)
	continue;
      NEXT_INSN (PREV_INSN (insn)) = NEXT_INSN (insn);
      PREV_INSN (NEXT_INSN (insn)) = PREV_INSN (insn);
      PREV_INSN (insn) = last_call;
      NEXT_INSN (insn) = NEXT_INSN (last_call);
      PREV_INSN (NEXT_INSN (insn)) = insn;
      NEXT_INSN (PREV_INSN (insn)) = insn;
      last_call = insn;
    }
}

/* Emit a NOP instruction for CYCLES cycles after insn AFTER.  Return it.  */

static rtx
emit_nop_after (int cycles, rtx after)
{
  rtx insn;

  /* mpydp has 9 delay slots, and we may schedule a stall for a cross-path
     operation.  We don't need the extra NOP since in this case, the hardware
     will automatically insert the required stall.  */
  if (cycles == 10)
    cycles--;

  gcc_assert (cycles < 10);

  insn = emit_insn_after (gen_nop_count (GEN_INT (cycles)), after);
  PUT_MODE (insn, TImode);

  return insn;
}

/* Determine whether INSN is a call that needs to have a return label
   placed.  */

static bool
returning_call_p (rtx insn)
{
  if (CALL_P (insn))
    return (!SIBLING_CALL_P (insn)
	    && get_attr_type (insn) != TYPE_CALLP
	    && get_attr_type (insn) != TYPE_SHADOW);
  if (recog_memoized (insn) < 0)
    return false;
  if (get_attr_type (insn) == TYPE_CALL)
    return true;
  return false;
}

/* Determine whether INSN's pattern can be converted to use callp.  */
static bool
can_use_callp (rtx insn)
{
  int icode = recog_memoized (insn);
  if (!TARGET_INSNS_64PLUS
      || icode < 0
      || GET_CODE (PATTERN (insn)) == COND_EXEC)
    return false;

  return ((icode == CODE_FOR_real_call
	   || icode == CODE_FOR_call_internal
	   || icode == CODE_FOR_call_value_internal)
	  && get_attr_dest_regfile (insn) == DEST_REGFILE_ANY);
}

/* Convert the pattern of INSN, which must be a CALL_INSN, into a callp.  */
static void
convert_to_callp (rtx insn)
{
  rtx lab;
  extract_insn (insn);
  if (GET_CODE (PATTERN (insn)) == SET)
    {
      rtx dest = recog_data.operand[0];
      lab = recog_data.operand[1];
      PATTERN (insn) = gen_callp_value (dest, lab);
      INSN_CODE (insn) = CODE_FOR_callp_value;
    }
  else
    {
      lab = recog_data.operand[0];
      PATTERN (insn) = gen_callp (lab);
      INSN_CODE (insn) = CODE_FOR_callp;
    }
}

/* Scan forwards from INSN until we find the next insn that has mode TImode
   (indicating it starts a new cycle), and occurs in cycle CLOCK.
   Return it if we find such an insn, NULL_RTX otherwise.  */
static rtx
find_next_cycle_insn (rtx insn, int clock)
{
  rtx t = insn;
  if (GET_MODE (t) == TImode)
    t = next_real_insn (t);
  while (t && GET_MODE (t) != TImode)
    t = next_real_insn (t);

  if (t && insn_get_clock (t) == clock)
    return t;
  return NULL_RTX;
}

/* If COND_INSN has a COND_EXEC condition, wrap the same condition
   around PAT.  Return PAT either unchanged or modified in this
   way.  */
static rtx
duplicate_cond (rtx pat, rtx cond_insn)
{
  rtx cond_pat = PATTERN (cond_insn);
  if (GET_CODE (cond_pat) == COND_EXEC)
    pat = gen_rtx_COND_EXEC (VOIDmode, copy_rtx (COND_EXEC_TEST (cond_pat)),
			     pat);
  return pat;
}

/* Walk forward from INSN to find the last insn that issues in the same clock
   cycle.  */
static rtx
find_last_same_clock (rtx insn)
{
  rtx retval = insn;
  rtx t = next_real_insn (insn);

  while (t && GET_MODE (t) != TImode)
    {
      if (!DEBUG_INSN_P (t) && recog_memoized (t) >= 0)
	retval = t;
      t = next_real_insn (t);
    }
  return retval;
}

/* For every call insn in the function, emit code to load the return
   address.  For each call we create a return label and store it in
   CALL_LABELS.  If are not scheduling, we emit the labels here,
   otherwise the caller will do it later.
   This function is called after final insn scheduling, but before creating
   the SEQUENCEs that represent execute packets.  */

static void
reorg_split_calls (rtx *call_labels)
{
  unsigned int reservation_mask = 0;
  rtx insn = get_insns ();
  gcc_assert (NOTE_P (insn));
  insn = next_real_insn (insn);
  while (insn)
    {
      int uid;
      rtx next = next_real_insn (insn);

      if (DEBUG_INSN_P (insn))
	goto done;

      if (GET_MODE (insn) == TImode)
	reservation_mask = 0;
      uid = INSN_UID (insn);
      if (c6x_flag_schedule_insns2 && recog_memoized (insn) >= 0)
	reservation_mask |= 1 << INSN_INFO_ENTRY (uid).reservation;

      if (returning_call_p (insn))
	{
	  rtx label = gen_label_rtx ();
	  rtx labelref = gen_rtx_LABEL_REF (Pmode, label);
	  rtx reg = gen_rtx_REG (SImode, RETURN_ADDR_REGNO);

	  LABEL_NUSES (label) = 2;
	  if (!c6x_flag_schedule_insns2)
	    {
	      if (can_use_callp (insn))
		convert_to_callp (insn);
	      else
		{
		  rtx t;
		  rtx slot[4];
		  emit_label_after (label, insn);

		  /* Bundle the call and its delay slots into a single
		     SEQUENCE.  While these do not issue in parallel
		     we need to group them into a single EH region.  */
		  slot[0] = insn;
		  PUT_MODE (insn, TImode);
		  if (TARGET_INSNS_64)
		    {
		      t = gen_addkpc (reg, labelref, GEN_INT (4));
		      slot[1] = emit_insn_after (duplicate_cond (t, insn),
						 insn);
		      PUT_MODE (slot[1], TImode);
		      gen_one_bundle (slot, 2, 0);
		    }
		  else
		    {
		      slot[3] = emit_insn_after (gen_nop_count (GEN_INT (3)),
						 insn);
		      PUT_MODE (slot[3], TImode);
		      t = gen_movsi_lo_sum (reg, reg, labelref);
		      slot[2] = emit_insn_after (duplicate_cond (t, insn),
						  insn);
		      PUT_MODE (slot[2], TImode);
		      t = gen_movsi_high (reg, labelref);
		      slot[1] = emit_insn_after (duplicate_cond (t, insn),
						 insn);
		      PUT_MODE (slot[1], TImode);
		      gen_one_bundle (slot, 4, 0);
		    }
		}
	    }
	  else
	    {
	      /* If we scheduled, we reserved the .S2 unit for one or two
		 cycles after the call.  Emit the insns in these slots,
		 unless it's possible to create a CALLP insn.
		 Note that this works because the dependencies ensure that
		 no insn setting/using B3 is scheduled in the delay slots of
		 a call.  */
	      int this_clock = insn_get_clock (insn);
	      rtx last_same_clock;
	      rtx after1;

	      call_labels[INSN_UID (insn)] = label;

	      last_same_clock = find_last_same_clock (insn);

	      if (can_use_callp (insn))
		{
		  /* Find the first insn of the next execute packet.  If it
		     is the shadow insn corresponding to this call, we may
		     use a CALLP insn.  */
		  rtx shadow = next_nonnote_nondebug_insn (last_same_clock);

		  if (CALL_P (shadow)
		      && insn_get_clock (shadow) == this_clock + 5)
		    {
		      convert_to_callp (shadow);
		      insn_set_clock (shadow, this_clock);
		      INSN_INFO_ENTRY (INSN_UID (shadow)).reservation
			= RESERVATION_S2;
		      INSN_INFO_ENTRY (INSN_UID (shadow)).unit_mask
			= INSN_INFO_ENTRY (INSN_UID (last_same_clock)).unit_mask;
		      if (GET_MODE (insn) == TImode)
			{
			  rtx new_cycle_first = NEXT_INSN (insn);
			  while (!NONDEBUG_INSN_P (new_cycle_first)
				 || GET_CODE (PATTERN (new_cycle_first)) == USE
				 || GET_CODE (PATTERN (new_cycle_first)) == CLOBBER)
			    new_cycle_first = NEXT_INSN (new_cycle_first);
			  PUT_MODE (new_cycle_first, TImode);
			  if (new_cycle_first != shadow)
			    PUT_MODE (shadow, VOIDmode);
			  INSN_INFO_ENTRY (INSN_UID (new_cycle_first)).ebb_start
			    = INSN_INFO_ENTRY (INSN_UID (insn)).ebb_start;
			}
		      else
			PUT_MODE (shadow, VOIDmode);
		      delete_insn (insn);
		      goto done;
		    }
		}
	      after1 = find_next_cycle_insn (last_same_clock, this_clock + 1);
	      if (after1 == NULL_RTX)
		after1 = last_same_clock;
	      else
		after1 = find_last_same_clock (after1);
	      if (TARGET_INSNS_64)
		{
		  rtx x1 = gen_addkpc (reg, labelref, const0_rtx);
		  x1 = emit_insn_after (duplicate_cond (x1, insn), after1);
		  insn_set_clock (x1, this_clock + 1);
		  INSN_INFO_ENTRY (INSN_UID (x1)).reservation = RESERVATION_S2;
		  if (after1 == last_same_clock)
		    PUT_MODE (x1, TImode);
		  else
		    INSN_INFO_ENTRY (INSN_UID (x1)).unit_mask
		      = INSN_INFO_ENTRY (INSN_UID (after1)).unit_mask;
		}
	      else
		{
		  rtx x1, x2;
		  rtx after2 = find_next_cycle_insn (after1, this_clock + 2);
		  if (after2 == NULL_RTX)
		    after2 = after1;
		  x2 = gen_movsi_lo_sum (reg, reg, labelref);
		  x2 = emit_insn_after (duplicate_cond (x2, insn), after2);
		  x1 = gen_movsi_high (reg, labelref);
		  x1 = emit_insn_after (duplicate_cond (x1, insn), after1);
		  insn_set_clock (x1, this_clock + 1);
		  insn_set_clock (x2, this_clock + 2);
		  INSN_INFO_ENTRY (INSN_UID (x1)).reservation = RESERVATION_S2;
		  INSN_INFO_ENTRY (INSN_UID (x2)).reservation = RESERVATION_S2;
		  if (after1 == last_same_clock)
		    PUT_MODE (x1, TImode);
		  else
		    INSN_INFO_ENTRY (INSN_UID (x1)).unit_mask
		      = INSN_INFO_ENTRY (INSN_UID (after1)).unit_mask;
		  if (after1 == after2)
		    PUT_MODE (x2, TImode);
		  else
		    INSN_INFO_ENTRY (INSN_UID (x2)).unit_mask
		      = INSN_INFO_ENTRY (INSN_UID (after2)).unit_mask;
		}
	    }
	}
    done:
      insn = next;
    }
}

/* Called as part of c6x_reorg.  This function emits multi-cycle NOP
   insns as required for correctness.  CALL_LABELS is the array that
   holds the return labels for call insns; we emit these here if
   scheduling was run earlier.  */

static void
reorg_emit_nops (rtx *call_labels)
{
  bool first;
  rtx prev, last_call;
  int prev_clock, earliest_bb_end;
  int prev_implicit_nops;
  rtx insn = get_insns ();

  /* We look at one insn (or bundle inside a sequence) in each iteration, storing
     its issue time in PREV_CLOCK for the next iteration.  If there is a gap in
     clocks, we must insert a NOP.
     EARLIEST_BB_END tracks in which cycle all insns that have been issued in the
     current basic block will finish.  We must not allow the next basic block to
     begin before this cycle.
     PREV_IMPLICIT_NOPS tells us whether we've seen an insn that implicitly contains
     a multi-cycle nop.  The code is scheduled such that subsequent insns will
     show the cycle gap, but we needn't insert a real NOP instruction.  */
  insn = next_real_insn (insn);
  last_call = prev = NULL_RTX;
  prev_clock = -1;
  earliest_bb_end = 0;
  prev_implicit_nops = 0;
  first = true;
  while (insn)
    {
      int this_clock = -1;
      rtx next;
      int max_cycles = 0;

      next = next_real_insn (insn);

      if (DEBUG_INSN_P (insn)
	  || GET_CODE (PATTERN (insn)) == USE
	  || GET_CODE (PATTERN (insn)) == CLOBBER
	  || shadow_or_blockage_p (insn)
	  || JUMP_TABLE_DATA_P (insn))
	goto next_insn;

      if (!c6x_flag_schedule_insns2)
	/* No scheduling; ensure that no parallel issue happens.  */
	PUT_MODE (insn, TImode);
      else
	{
	  int cycles;

	  this_clock = insn_get_clock (insn);
	  if (this_clock != prev_clock)
	    {
	      PUT_MODE (insn, TImode);

	      if (!first)
		{
		  cycles = this_clock - prev_clock;

		  cycles -= prev_implicit_nops;
		  if (cycles > 1)
		    {
		      rtx nop = emit_nop_after (cycles - 1, prev);
		      insn_set_clock (nop, prev_clock + prev_implicit_nops + 1);
		    }
		}
	      prev_clock = this_clock;

	      if (last_call
		  && insn_get_clock (last_call) + 6 <= this_clock)
		{
		  emit_label_before (call_labels[INSN_UID (last_call)], insn);
		  last_call = NULL_RTX;
		}
	      prev_implicit_nops = 0;
	    }
	}

      /* Examine how many cycles the current insn takes, and adjust
	 LAST_CALL, EARLIEST_BB_END and PREV_IMPLICIT_NOPS.  */
      if (recog_memoized (insn) >= 0
	  /* If not scheduling, we've emitted NOPs after calls already.  */
	  && (c6x_flag_schedule_insns2 || !returning_call_p (insn)))
	{
	  max_cycles = get_attr_cycles (insn);
	  if (get_attr_type (insn) == TYPE_CALLP)
	    prev_implicit_nops = 5;
	}
      else
	max_cycles = 1;
      if (returning_call_p (insn))
	last_call = insn;

      if (c6x_flag_schedule_insns2)
	{
	  gcc_assert (this_clock >= 0);
	  if (earliest_bb_end < this_clock + max_cycles)
	    earliest_bb_end = this_clock + max_cycles;
	}
      else if (max_cycles > 1)
	emit_nop_after (max_cycles - 1, insn);

      prev = insn;
      first = false;

    next_insn:
      if (c6x_flag_schedule_insns2
	  && (next == NULL_RTX
	      || (GET_MODE (next) == TImode
		  && INSN_INFO_ENTRY (INSN_UID (next)).ebb_start))
	  && earliest_bb_end > 0)
	{
	  int cycles = earliest_bb_end - prev_clock;
	  if (cycles > 1)
	    {
	      prev = emit_nop_after (cycles - 1, prev);
	      insn_set_clock (prev, prev_clock + prev_implicit_nops + 1);
	    }
	  earliest_bb_end = 0;
	  prev_clock = -1;
	  first = true;

	  if (last_call)
	    emit_label_after (call_labels[INSN_UID (last_call)], prev);
	  last_call = NULL_RTX;
	}
      insn = next;
    }
}

/* If possible, split INSN, which we know is either a jump or a call, into a real
   insn and its shadow.  */
static void
split_delayed_branch (rtx insn)
{
  int code = recog_memoized (insn);
  rtx i1, newpat;
  rtx pat = PATTERN (insn);

  if (GET_CODE (pat) == COND_EXEC)
    pat = COND_EXEC_CODE (pat);

  if (CALL_P (insn))
    {
      rtx src = pat, dest = NULL_RTX;
      rtx callee;
      if (GET_CODE (pat) == SET)
	{
	  dest = SET_DEST (pat);
	  src = SET_SRC (pat);
	}
      callee = XEXP (XEXP (src, 0), 0);
      if (SIBLING_CALL_P (insn))
	{
	  if (REG_P (callee))
	    newpat = gen_indirect_sibcall_shadow ();
	  else
	    newpat = gen_sibcall_shadow (callee);
	  pat = gen_real_jump (callee);
	}
      else if (dest != NULL_RTX)
	{
	  if (REG_P (callee))
	    newpat = gen_indirect_call_value_shadow (dest);
	  else
	    newpat = gen_call_value_shadow (dest, callee);
	  pat = gen_real_call (callee);
	}
      else
	{
	  if (REG_P (callee))
	    newpat = gen_indirect_call_shadow ();
	  else
	    newpat = gen_call_shadow (callee);
	  pat = gen_real_call (callee);
	}
      pat = duplicate_cond (pat, insn);
      newpat = duplicate_cond (newpat, insn);
    }
  else
    {
      rtx src, op;
      if (GET_CODE (pat) == PARALLEL
	  && GET_CODE (XVECEXP (pat, 0, 0)) == RETURN)
	{
	  newpat = gen_return_shadow ();
	  pat = gen_real_ret (XEXP (XVECEXP (pat, 0, 1), 0));
	  newpat = duplicate_cond (newpat, insn);
	}
      else
	switch (code)
	  {
	  case CODE_FOR_br_true:
	  case CODE_FOR_br_false:
	    src = SET_SRC (pat);
	    op = XEXP (src, code == CODE_FOR_br_true ? 1 : 2);
	    newpat = gen_condjump_shadow (op);
	    pat = gen_real_jump (op);
	    if (code == CODE_FOR_br_true)
	      pat = gen_rtx_COND_EXEC (VOIDmode, XEXP (src, 0), pat);
	    else
	      pat = gen_rtx_COND_EXEC (VOIDmode,
				       reversed_comparison (XEXP (src, 0),
							    VOIDmode),
				       pat);
	    break;

	  case CODE_FOR_jump:
	    op = SET_SRC (pat);
	    newpat = gen_jump_shadow (op);
	    break;

	  case CODE_FOR_indirect_jump:
	    newpat = gen_indirect_jump_shadow ();
	    break;

	  case CODE_FOR_return_internal:
	    newpat = gen_return_shadow ();
	    pat = gen_real_ret (XEXP (XVECEXP (pat, 0, 1), 0));
	    break;

	  default:
	    return;
	  }
    }
  i1 = emit_insn_before (pat, insn);
  PATTERN (insn) = newpat;
  INSN_CODE (insn) = -1;
  record_delay_slot_pair (i1, insn, 5, 0);
}

/* If INSN is a multi-cycle insn that should be handled properly in
   modulo-scheduling, split it into a real insn and a shadow.
   Return true if we made a change.

   It is valid for us to fail to split an insn; the caller has to deal
   with the possibility.  Currently we handle loads and most mpy2 and
   mpy4 insns.  */
static bool
split_delayed_nonbranch (rtx insn)
{
  int code = recog_memoized (insn);
  enum attr_type type;
  rtx i1, newpat, src, dest;
  rtx pat = PATTERN (insn);
  rtvec rtv;
  int delay;

  if (GET_CODE (pat) == COND_EXEC)
    pat = COND_EXEC_CODE (pat);

  if (code < 0 || GET_CODE (pat) != SET)
    return false;
  src = SET_SRC (pat);
  dest = SET_DEST (pat);
  if (!REG_P (dest))
    return false;

  type = get_attr_type (insn);
  if (code >= 0
      && (type == TYPE_LOAD
	  || type == TYPE_LOADN))
    {
      if (!MEM_P (src)
	  && (GET_CODE (src) != ZERO_EXTEND
	      || !MEM_P (XEXP (src, 0))))
	return false;

      if (GET_MODE_SIZE (GET_MODE (dest)) > 4
	  && (GET_MODE_SIZE (GET_MODE (dest)) != 8 || !TARGET_LDDW))
	return false;

      rtv = gen_rtvec (2, GEN_INT (REGNO (SET_DEST (pat))),
		       SET_SRC (pat));
      newpat = gen_load_shadow (SET_DEST (pat));
      pat = gen_rtx_UNSPEC (VOIDmode, rtv, UNSPEC_REAL_LOAD);
      delay = 4;
    }
  else if (code >= 0
	   && (type == TYPE_MPY2
	       || type == TYPE_MPY4))
    {
      /* We don't handle floating point multiplies yet.  */
      if (GET_MODE (dest) == SFmode)
	return false;

      rtv = gen_rtvec (2, GEN_INT (REGNO (SET_DEST (pat))),
		       SET_SRC (pat));
      newpat = gen_mult_shadow (SET_DEST (pat));
      pat = gen_rtx_UNSPEC (VOIDmode, rtv, UNSPEC_REAL_MULT);
      delay = type == TYPE_MPY2 ? 1 : 3;
    }
  else
    return false;

  pat = duplicate_cond (pat, insn);
  newpat = duplicate_cond (newpat, insn);
  i1 = emit_insn_before (pat, insn);
  PATTERN (insn) = newpat;
  INSN_CODE (insn) = -1;
  recog_memoized (insn);
  recog_memoized (i1);
  record_delay_slot_pair (i1, insn, delay, 0);
  return true;
}

/* Examine if INSN is the result of splitting a load into a real load and a
   shadow, and if so, undo the transformation.  */
static void
undo_split_delayed_nonbranch (rtx insn)
{
  int icode = recog_memoized (insn);
  enum attr_type type;
  rtx prev_pat, insn_pat, prev;

  if (icode < 0)
    return;
  type = get_attr_type (insn);
  if (type != TYPE_LOAD_SHADOW && type != TYPE_MULT_SHADOW)
    return;
  prev = PREV_INSN (insn);
  prev_pat = PATTERN (prev);
  insn_pat = PATTERN (insn);
  if (GET_CODE (prev_pat) == COND_EXEC)
    {
      prev_pat = COND_EXEC_CODE (prev_pat);
      insn_pat = COND_EXEC_CODE (insn_pat);
    }

  gcc_assert (GET_CODE (prev_pat) == UNSPEC
	      && ((XINT (prev_pat, 1) == UNSPEC_REAL_LOAD
		   && type == TYPE_LOAD_SHADOW)
		  || (XINT (prev_pat, 1) == UNSPEC_REAL_MULT
		      && type == TYPE_MULT_SHADOW)));
  insn_pat = gen_rtx_SET (VOIDmode, SET_DEST (insn_pat),
			  XVECEXP (prev_pat, 0, 1));
  insn_pat = duplicate_cond (insn_pat, prev);
  PATTERN (insn) = insn_pat;
  INSN_CODE (insn) = -1;
  delete_insn (prev);
}

/* Split every insn (i.e. jumps and calls) which can have delay slots into
   two parts: the first one is scheduled normally and emits the instruction,
   while the second one is a shadow insn which shows the side effect taking
   place. The second one is placed in the right cycle by the scheduler, but
   not emitted as an assembly instruction.  */

static void
split_delayed_insns (void)
{
  rtx insn;
  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      if (JUMP_P (insn) || CALL_P (insn))
	split_delayed_branch (insn);
    }
}

/* For every insn that has an entry in the new_conditions vector, give it
   the appropriate predicate.  */
static void
conditionalize_after_sched (void)
{
  basic_block bb;
  rtx insn;
  FOR_EACH_BB_FN (bb, cfun)
    FOR_BB_INSNS (bb, insn)
      {
	unsigned uid = INSN_UID (insn);
	rtx cond;
	if (!NONDEBUG_INSN_P (insn) || uid >= INSN_INFO_LENGTH)
	  continue;
	cond = INSN_INFO_ENTRY (uid).new_cond;
	if (cond == NULL_RTX)
	  continue;
	if (dump_file)
	  fprintf (dump_file, "Conditionalizing insn %d\n", uid);
	predicate_insn (insn, cond, true);
      }
}

/* A callback for the hw-doloop pass.  This function examines INSN; if
   it is a loop_end pattern we recognize, return the reg rtx for the
   loop counter.  Otherwise, return NULL_RTX.  */

static rtx
hwloop_pattern_reg (rtx insn)
{
  rtx pat, reg;

  if (!JUMP_P (insn) || recog_memoized (insn) != CODE_FOR_loop_end)
    return NULL_RTX;

  pat = PATTERN (insn);
  reg = SET_DEST (XVECEXP (pat, 0, 1));
  if (!REG_P (reg))
    return NULL_RTX;
  return reg;
}

/* Return the number of cycles taken by BB, as computed by scheduling,
   including the latencies of all insns with delay slots.  IGNORE is
   an insn we should ignore in the calculation, usually the final
   branch.  */
static int
bb_earliest_end_cycle (basic_block bb, rtx ignore)
{
  int earliest = 0;
  rtx insn;

  FOR_BB_INSNS (bb, insn)
    {
      int cycles, this_clock;

      if (LABEL_P (insn) || NOTE_P (insn) || DEBUG_INSN_P (insn)
	  || GET_CODE (PATTERN (insn)) == USE
	  || GET_CODE (PATTERN (insn)) == CLOBBER
	  || insn == ignore)
	continue;

      this_clock = insn_get_clock (insn);
      cycles = get_attr_cycles (insn);

      if (earliest < this_clock + cycles)
	earliest = this_clock + cycles;
    }
  return earliest;
}

/* Examine the insns in BB and remove all which have a uid greater or
   equal to MAX_UID.  */
static void
filter_insns_above (basic_block bb, int max_uid)
{
  rtx insn, next;
  bool prev_ti = false;
  int prev_cycle = -1;

  FOR_BB_INSNS_SAFE (bb, insn, next)
    {
      int this_cycle;
      if (!NONDEBUG_INSN_P (insn))
	continue;
      if (insn == BB_END (bb))
	return;
      this_cycle = insn_get_clock (insn);
      if (prev_ti && this_cycle == prev_cycle)
	{
	  gcc_assert (GET_MODE (insn) != TImode);
	  PUT_MODE (insn, TImode);
	}
      prev_ti = false;
      if (INSN_UID (insn) >= max_uid)
	{
	  if (GET_MODE (insn) == TImode)
	    {
	      prev_ti = true;
	      prev_cycle = this_cycle;
	    }
	  delete_insn (insn);
	}
    }
}

/* Implement TARGET_ASM_EMIT_EXCEPT_PERSONALITY.  */

static void
c6x_asm_emit_except_personality (rtx personality)
{
  fputs ("\t.personality\t", asm_out_file);
  output_addr_const (asm_out_file, personality);
  fputc ('\n', asm_out_file);
}

/* Use a special assembly directive rather than a regular setion for
   unwind table data.  */

static void
c6x_asm_init_sections (void)
{
  exception_section = get_unnamed_section (0, output_section_asm_op,
					   "\t.handlerdata");
}

/* A callback for the hw-doloop pass.  Called to optimize LOOP in a
   machine-specific fashion; returns true if successful and false if
   the hwloop_fail function should be called.  */

static bool
hwloop_optimize (hwloop_info loop)
{
  basic_block entry_bb, bb;
  rtx seq, insn, prev, entry_after, end_packet;
  rtx head_insn, tail_insn, new_insns, last_insn;
  int loop_earliest;
  int n_execute_packets;
  edge entry_edge;
  unsigned ix;
  int max_uid_before, delayed_splits;
  int i, sp_ii, min_ii, max_ii, max_parallel, n_insns, n_real_insns, stages;
  rtx *orig_vec;
  rtx *copies;
  rtx **insn_copies;

  if (!c6x_flag_modulo_sched || !c6x_flag_schedule_insns2
      || !TARGET_INSNS_64PLUS)
    return false;

  if (loop->iter_reg_used || loop->depth > 1)
    return false;
  if (loop->has_call || loop->has_asm)
    return false;

  if (loop->head != loop->tail)
    return false;

  gcc_assert (loop->incoming_dest == loop->head);

  entry_edge = NULL;
  FOR_EACH_VEC_SAFE_ELT (loop->incoming, i, entry_edge)
    if (entry_edge->flags & EDGE_FALLTHRU)
      break;
  if (entry_edge == NULL)
    return false;

  reshuffle_units (loop->head);

  in_hwloop = true;
  schedule_ebbs_init ();
  schedule_ebb (BB_HEAD (loop->tail), loop->loop_end, true);
  schedule_ebbs_finish ();
  in_hwloop = false;

  bb = loop->head;
  loop_earliest = bb_earliest_end_cycle (bb, loop->loop_end) + 1;

  max_uid_before = get_max_uid ();

  /* Split all multi-cycle operations, such as loads.  For normal
     scheduling, we only do this for branches, as the generated code
     would otherwise not be interrupt-safe.  When using sploop, it is
     safe and beneficial to split them.  If any multi-cycle operations
     remain after splitting (because we don't handle them yet), we
     cannot pipeline the loop.  */
  delayed_splits = 0;
  FOR_BB_INSNS (bb, insn)
    {
      if (NONDEBUG_INSN_P (insn))
	{
	  recog_memoized (insn);
	  if (split_delayed_nonbranch (insn))
	    delayed_splits++;
	  else if (INSN_CODE (insn) >= 0
		   && get_attr_cycles (insn) > 1)
	    goto undo_splits;
	}
    }

  /* Count the number of insns as well as the number real insns, and save
     the original sequence of insns in case we must restore it later.  */
  n_insns = n_real_insns = 0;
  FOR_BB_INSNS (bb, insn)
    {
      n_insns++;
      if (NONDEBUG_INSN_P (insn) && insn != loop->loop_end)
	n_real_insns++;
    }
  orig_vec = XNEWVEC (rtx, n_insns);
  n_insns = 0;
  FOR_BB_INSNS (bb, insn)
    orig_vec[n_insns++] = insn;

  /* Count the unit reservations, and compute a minimum II from that
     table.  */
  count_unit_reqs (unit_reqs, loop->start_label,
		   PREV_INSN (loop->loop_end));
  merge_unit_reqs (unit_reqs);

  min_ii = res_mii (unit_reqs);
  max_ii = loop_earliest < 15 ? loop_earliest : 14;

  /* Make copies of the loop body, up to a maximum number of stages we want
     to handle.  */
  max_parallel = loop_earliest / min_ii + 1;

  copies = XCNEWVEC (rtx, (max_parallel + 1) * n_real_insns);
  insn_copies = XNEWVEC (rtx *, max_parallel + 1);
  for (i = 0; i < max_parallel + 1; i++)
    insn_copies[i] = copies + i * n_real_insns;

  head_insn = next_nonnote_nondebug_insn (loop->start_label);
  tail_insn = prev_real_insn (BB_END (bb));

  i = 0;
  FOR_BB_INSNS (bb, insn)
    if (NONDEBUG_INSN_P (insn) && insn != loop->loop_end)
      insn_copies[0][i++] = insn;

  sploop_max_uid_iter0 = get_max_uid ();

  /* Generate the copies of the loop body, and save them in the
     INSN_COPIES array.  */
  start_sequence ();
  for (i = 0; i < max_parallel; i++)
    {
      int j;
      rtx this_iter;

      this_iter = duplicate_insn_chain (head_insn, tail_insn);
      j = 0;
      while (this_iter)
	{
	  rtx prev_stage_insn = insn_copies[i][j];
	  gcc_assert (INSN_CODE (this_iter) == INSN_CODE (prev_stage_insn));

	  if (INSN_CODE (this_iter) >= 0
	      && (get_attr_type (this_iter) == TYPE_LOAD_SHADOW
		  || get_attr_type (this_iter) == TYPE_MULT_SHADOW))
	    {
	      rtx prev = PREV_INSN (this_iter);
	      record_delay_slot_pair (prev, this_iter,
				      get_attr_cycles (prev) - 1, 0);
	    }
	  else
	    record_delay_slot_pair (prev_stage_insn, this_iter, i, 1);

	  insn_copies[i + 1][j] = this_iter;
	  j++;
	  this_iter = next_nonnote_nondebug_insn (this_iter);
	}
    }
  new_insns = get_insns ();
  last_insn = insn_copies[max_parallel][n_real_insns - 1];
  end_sequence ();
  emit_insn_before (new_insns, BB_END (bb));

  /* Try to schedule the loop using varying initiation intervals,
     starting with the smallest possible and incrementing it
     on failure.  */
  for (sp_ii = min_ii; sp_ii <= max_ii; sp_ii++)
    {
      basic_block tmp_bb;
      if (dump_file)
	fprintf (dump_file, "Trying to schedule for II %d\n", sp_ii);

      df_clear_flags (DF_LR_RUN_DCE);

      schedule_ebbs_init ();
      set_modulo_params (sp_ii, max_parallel, n_real_insns,
			 sploop_max_uid_iter0);
      tmp_bb = schedule_ebb (BB_HEAD (bb), last_insn, true);
      schedule_ebbs_finish ();

      if (tmp_bb)
	{
	  if (dump_file)
	    fprintf (dump_file, "Found schedule with II %d\n", sp_ii);
	  break;
	}
    }

  discard_delay_pairs_above (max_uid_before);

  if (sp_ii > max_ii)
    goto restore_loop;

  stages = insn_get_clock (ss.last_scheduled_iter0) / sp_ii + 1;

  if (stages == 1 && sp_ii > 5)
    goto restore_loop;

  /* At this point, we know we've been successful, unless we find later that
     there are too many execute packets for the loop buffer to hold.  */

  /* Assign reservations to the instructions in the loop.  We must find
     the stage that contains the full loop kernel, and transfer the
     reservations of the instructions contained in it to the corresponding
     instructions from iteration 0, which are the only ones we'll keep.  */
  assign_reservations (BB_HEAD (bb), ss.last_scheduled_insn);
  PREV_INSN (BB_END (bb)) = ss.last_scheduled_iter0;
  NEXT_INSN (ss.last_scheduled_iter0) = BB_END (bb);
  filter_insns_above (bb, sploop_max_uid_iter0);

  for (i = 0; i < n_real_insns; i++)
    {
      rtx insn = insn_copies[0][i];
      int uid = INSN_UID (insn);
      int stage = insn_uid_get_clock (uid) / sp_ii;

      if (stage + 1 < stages)
	{
	  int copy_uid;
	  stage = stages - stage - 1;
	  copy_uid = INSN_UID (insn_copies[stage][i]);
	  INSN_INFO_ENTRY (uid).reservation
	    = INSN_INFO_ENTRY (copy_uid).reservation;
	}
    }
  if (stages == 1)
    stages++;

  /* Compute the number of execute packets the pipelined form of the loop will
     require.  */
  prev = NULL_RTX;
  n_execute_packets = 0;
  for (insn = loop->start_label; insn != loop->loop_end; insn = NEXT_INSN (insn))
    {
      if (NONDEBUG_INSN_P (insn) && GET_MODE (insn) == TImode
	  && !shadow_p (insn))
	{
	  n_execute_packets++;
	  if (prev && insn_get_clock (prev) + 1 != insn_get_clock (insn))
	    /* We need an extra NOP instruction.  */
	    n_execute_packets++;

	  prev = insn;
	}
    }

  end_packet = ss.last_scheduled_iter0;
  while (!NONDEBUG_INSN_P (end_packet) || GET_MODE (end_packet) != TImode)
    end_packet = PREV_INSN (end_packet);

  /* The earliest cycle in which we can emit the SPKERNEL instruction.  */
  loop_earliest = (stages - 1) * sp_ii;
  if (loop_earliest > insn_get_clock (end_packet))
    {
      n_execute_packets++;
      end_packet = loop->loop_end;
    }
  else
    loop_earliest = insn_get_clock (end_packet);

  if (n_execute_packets > 14)
    goto restore_loop;

  /* Generate the spkernel instruction, and place it at the appropriate
     spot.  */
  PUT_MODE (end_packet, VOIDmode);

  insn = gen_spkernel (GEN_INT (stages - 1),
		       const0_rtx, JUMP_LABEL (loop->loop_end));
  insn = emit_jump_insn_before (insn, end_packet);
  JUMP_LABEL (insn) = JUMP_LABEL (loop->loop_end);
  insn_set_clock (insn, loop_earliest);
  PUT_MODE (insn, TImode);
  INSN_INFO_ENTRY (INSN_UID (insn)).ebb_start = false;
  delete_insn (loop->loop_end);

  /* Place the mvc and sploop instructions before the loop.  */
  entry_bb = entry_edge->src;

  start_sequence ();

  insn = emit_insn (gen_mvilc (loop->iter_reg));
  insn = emit_insn (gen_sploop (GEN_INT (sp_ii)));

  seq = get_insns ();

  if (!single_succ_p (entry_bb) || vec_safe_length (loop->incoming) > 1)
    {
      basic_block new_bb;
      edge e;
      edge_iterator ei;

      emit_insn_before (seq, BB_HEAD (loop->head));
      seq = emit_label_before (gen_label_rtx (), seq);

      new_bb = create_basic_block (seq, insn, entry_bb);
      FOR_EACH_EDGE (e, ei, loop->incoming)
	{
	  if (!(e->flags & EDGE_FALLTHRU))
	    redirect_edge_and_branch_force (e, new_bb);
	  else
	    redirect_edge_succ (e, new_bb);
	}
      make_edge (new_bb, loop->head, 0);
    }
  else
    {
      entry_after = BB_END (entry_bb);
      while (DEBUG_INSN_P (entry_after)
	     || (NOTE_P (entry_after)
		 && NOTE_KIND (entry_after) != NOTE_INSN_BASIC_BLOCK))
	entry_after = PREV_INSN (entry_after);
      emit_insn_after (seq, entry_after);
    }

  end_sequence ();

  /* Make sure we don't try to schedule this loop again.  */
  for (ix = 0; loop->blocks.iterate (ix, &bb); ix++)
    bb->flags |= BB_DISABLE_SCHEDULE;

  return true;

 restore_loop:
  if (dump_file)
    fprintf (dump_file, "Unable to pipeline loop.\n");

  for (i = 1; i < n_insns; i++)
    {
      NEXT_INSN (orig_vec[i - 1]) = orig_vec[i];
      PREV_INSN (orig_vec[i]) = orig_vec[i - 1];
    }
  PREV_INSN (orig_vec[0]) = PREV_INSN (BB_HEAD (bb));
  NEXT_INSN (PREV_INSN (BB_HEAD (bb))) = orig_vec[0];
  NEXT_INSN (orig_vec[n_insns - 1]) = NEXT_INSN (BB_END (bb));
  PREV_INSN (NEXT_INSN (BB_END (bb))) = orig_vec[n_insns - 1];
  BB_HEAD (bb) = orig_vec[0];
  BB_END (bb) = orig_vec[n_insns - 1];
 undo_splits:
  free_delay_pairs ();
  FOR_BB_INSNS (bb, insn)
    if (NONDEBUG_INSN_P (insn))
      undo_split_delayed_nonbranch (insn);
  return false;
}

/* A callback for the hw-doloop pass.  Called when a loop we have discovered
   turns out not to be optimizable; we have to split the doloop_end pattern
   into a subtract and a test.  */
static void
hwloop_fail (hwloop_info loop)
{
  rtx insn, test, testreg;

  if (dump_file)
    fprintf (dump_file, "splitting doloop insn %d\n",
	     INSN_UID (loop->loop_end));
  insn = gen_addsi3 (loop->iter_reg, loop->iter_reg, constm1_rtx);
  /* See if we can emit the add at the head of the loop rather than at the
     end.  */
  if (loop->head == NULL
      || loop->iter_reg_used_outside
      || loop->iter_reg_used
      || TEST_HARD_REG_BIT (loop->regs_set_in_loop, REGNO (loop->iter_reg))
      || loop->incoming_dest != loop->head
      || EDGE_COUNT (loop->head->preds) != 2)
    emit_insn_before (insn, loop->loop_end);
  else
    {
      rtx t = loop->start_label;
      while (!NOTE_P (t) || NOTE_KIND (t) != NOTE_INSN_BASIC_BLOCK)
	t = NEXT_INSN (t);
      emit_insn_after (insn, t);
    }

  testreg = SET_DEST (XVECEXP (PATTERN (loop->loop_end), 0, 2));
  if (GET_CODE (testreg) == SCRATCH)
    testreg = loop->iter_reg;
  else
    emit_insn_before (gen_movsi (testreg, loop->iter_reg), loop->loop_end);

  test = gen_rtx_NE (VOIDmode, testreg, const0_rtx);
  insn = emit_jump_insn_before (gen_cbranchsi4 (test, testreg, const0_rtx,
						loop->start_label),
				loop->loop_end);

  JUMP_LABEL (insn) = loop->start_label;
  LABEL_NUSES (loop->start_label)++;
  delete_insn (loop->loop_end);
}

static struct hw_doloop_hooks c6x_doloop_hooks =
{
  hwloop_pattern_reg,
  hwloop_optimize,
  hwloop_fail
};

/* Run the hw-doloop pass to modulo-schedule hardware loops, or split the
   doloop_end patterns where such optimizations are impossible.  */
static void
c6x_hwloops (void)
{
  if (optimize)
    reorg_loops (true, &c6x_doloop_hooks);
}

/* Implement the TARGET_MACHINE_DEPENDENT_REORG pass.  We split call insns here
   into a sequence that loads the return register and performs the call,
   and emit the return label.
   If scheduling after reload is requested, it happens here.  */

static void
c6x_reorg (void)
{
  basic_block bb;
  rtx *call_labels;
  bool do_selsched = (c6x_flag_schedule_insns2 && flag_selective_scheduling2
		      && !maybe_skip_selective_scheduling ());

  /* We are freeing block_for_insn in the toplev to keep compatibility
     with old MDEP_REORGS that are not CFG based.  Recompute it now.  */
  compute_bb_for_insn ();

  df_clear_flags (DF_LR_RUN_DCE);
  df_note_add_problem ();

  /* If optimizing, we'll have split before scheduling.  */
  if (optimize == 0)
    split_all_insns ();

  df_analyze ();

  if (c6x_flag_schedule_insns2)
    {
      int sz = get_max_uid () * 3 / 2 + 1;

      insn_info.create (sz);
    }

  /* Make sure the real-jump insns we create are not deleted.  When modulo-
     scheduling, situations where a reg is only stored in a loop can also
     cause dead code when doing the initial unrolling.  */
  sched_no_dce = true;

  c6x_hwloops ();

  if (c6x_flag_schedule_insns2)
    {
      split_delayed_insns ();
      timevar_push (TV_SCHED2);
      if (do_selsched)
	run_selective_scheduling ();
      else
	schedule_ebbs ();
      conditionalize_after_sched ();
      timevar_pop (TV_SCHED2);

      free_delay_pairs ();
    }
  sched_no_dce = false;

  call_labels = XCNEWVEC (rtx, get_max_uid () + 1);

  reorg_split_calls (call_labels);

  if (c6x_flag_schedule_insns2)
    {
      FOR_EACH_BB_FN (bb, cfun)
	if ((bb->flags & BB_DISABLE_SCHEDULE) == 0)
	  assign_reservations (BB_HEAD (bb), BB_END (bb));
    }

  if (c6x_flag_var_tracking)
    {
      timevar_push (TV_VAR_TRACKING);
      variable_tracking_main ();
      timevar_pop (TV_VAR_TRACKING);
    }

  reorg_emit_nops (call_labels);

  /* Post-process the schedule to move parallel insns into SEQUENCEs.  */
  if (c6x_flag_schedule_insns2)
    {
      free_delay_pairs ();
      c6x_gen_bundles ();
    }

  df_finish_pass (false);
}

/* Called when a function has been assembled.  It should perform all the
   tasks of ASM_DECLARE_FUNCTION_SIZE in elfos.h, plus target-specific
   tasks.
   We free the reservation (and other scheduling) information here now that
   all insns have been output.  */
void
c6x_function_end (FILE *file, const char *fname)
{
  c6x_output_fn_unwind (file);

  insn_info.release ();

  if (!flag_inhibit_size_directive)
    ASM_OUTPUT_MEASURED_SIZE (file, fname);
}

/* Determine whether X is a shift with code CODE and an integer amount
   AMOUNT.  */
static bool
shift_p (rtx x, enum rtx_code code, int amount)
{
  return (GET_CODE (x) == code && GET_CODE (XEXP (x, 1)) == CONST_INT
	  && INTVAL (XEXP (x, 1)) == amount);
}

/* Compute a (partial) cost for rtx X.  Return true if the complete
   cost has been computed, and false if subexpressions should be
   scanned.  In either case, *TOTAL contains the cost result.  */

static bool
c6x_rtx_costs (rtx x, int code, int outer_code, int opno, int *total,
	       bool speed)
{
  int cost2 = COSTS_N_INSNS (1);
  rtx op0, op1;

  switch (code)
    {
    case CONST_INT:
      if (outer_code == SET || outer_code == PLUS)
        *total = satisfies_constraint_IsB (x) ? 0 : cost2;
      else if (outer_code == AND || outer_code == IOR || outer_code == XOR
	       || outer_code == MINUS)
	*total = satisfies_constraint_Is5 (x) ? 0 : cost2;
      else if (GET_RTX_CLASS (outer_code) == RTX_COMPARE
	       || GET_RTX_CLASS (outer_code) == RTX_COMM_COMPARE)
	*total = satisfies_constraint_Iu4 (x) ? 0 : cost2;
      else if (outer_code == ASHIFT || outer_code == ASHIFTRT
	       || outer_code == LSHIFTRT)
	*total = satisfies_constraint_Iu5 (x) ? 0 : cost2;
      else
	*total = cost2;
      return true;

    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
    case CONST_DOUBLE:
      *total = COSTS_N_INSNS (2);
      return true;

    case TRUNCATE:
      /* Recognize a mult_highpart operation.  */
      if ((GET_MODE (x) == HImode || GET_MODE (x) == SImode)
	  && GET_CODE (XEXP (x, 0)) == LSHIFTRT
	  && GET_MODE (XEXP (x, 0)) == GET_MODE_2XWIDER_MODE (GET_MODE (x))
	  && GET_CODE (XEXP (XEXP (x, 0), 0)) == MULT
	  && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
	  && INTVAL (XEXP (XEXP (x, 0), 1)) == GET_MODE_BITSIZE (GET_MODE (x)))
	{
	  rtx mul = XEXP (XEXP (x, 0), 0);
	  rtx op0 = XEXP (mul, 0);
	  rtx op1 = XEXP (mul, 1);
	  enum rtx_code code0 = GET_CODE (op0);
	  enum rtx_code code1 = GET_CODE (op1);

	  if ((code0 == code1
	       && (code0 == SIGN_EXTEND || code0 == ZERO_EXTEND))
	      || (GET_MODE (x) == HImode
		  && code0 == ZERO_EXTEND && code1 == SIGN_EXTEND))
	    {
	      if (GET_MODE (x) == HImode)
		*total = COSTS_N_INSNS (2);
	      else
		*total = COSTS_N_INSNS (12);
	      *total += rtx_cost (XEXP (op0, 0), code0, 0, speed);
	      *total += rtx_cost (XEXP (op1, 0), code1, 0, speed);
	      return true;
	    }
	}
      return false;

    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
      if (GET_MODE (x) == DImode)
	*total = COSTS_N_INSNS (CONSTANT_P (XEXP (x, 1)) ? 4 : 15);
      else
	*total = COSTS_N_INSNS (1);
      return false;

    case PLUS:
    case MINUS:
      *total = COSTS_N_INSNS (1);
      op0 = code == PLUS ? XEXP (x, 0) : XEXP (x, 1);
      op1 = code == PLUS ? XEXP (x, 1) : XEXP (x, 0);
      if (GET_MODE_SIZE (GET_MODE (x)) <= UNITS_PER_WORD
	  && INTEGRAL_MODE_P (GET_MODE (x))
	  && GET_CODE (op0) == MULT
	  && GET_CODE (XEXP (op0, 1)) == CONST_INT
	  && (INTVAL (XEXP (op0, 1)) == 2
	      || INTVAL (XEXP (op0, 1)) == 4
	      || (code == PLUS && INTVAL (XEXP (op0, 1)) == 8)))
	{
	  *total += rtx_cost (XEXP (op0, 0), ASHIFT, 0, speed);
	  *total += rtx_cost (op1, (enum rtx_code) code, 1, speed);
	  return true;
	}
      return false;

    case MULT:
      op0 = XEXP (x, 0);
      op1 = XEXP (x, 1);
      if (GET_MODE (x) == DFmode)
	{
	  if (TARGET_FP)
	    *total = COSTS_N_INSNS (speed ? 10 : 1);
	  else
	    *total = COSTS_N_INSNS (speed ? 200 : 4);
	}
      else if (GET_MODE (x) == SFmode)
	{
	  if (TARGET_FP)
	    *total = COSTS_N_INSNS (speed ? 4 : 1);
	  else
	    *total = COSTS_N_INSNS (speed ? 100 : 4);
	}
      else if (GET_MODE (x) == DImode)
	{
	  if (TARGET_MPY32
	      && GET_CODE (op0) == GET_CODE (op1)
	      && (GET_CODE (op0) == ZERO_EXTEND
		  || GET_CODE (op0) == SIGN_EXTEND))
	    {
	      *total = COSTS_N_INSNS (speed ? 2 : 1);
	      op0 = XEXP (op0, 0);
	      op1 = XEXP (op1, 0);
	    }
	  else
	    /* Maybe improve this laster.  */
	    *total = COSTS_N_INSNS (20);
	}
      else if (GET_MODE (x) == SImode)
	{
	  if (((GET_CODE (op0) == ZERO_EXTEND
		|| GET_CODE (op0) == SIGN_EXTEND
		|| shift_p (op0, LSHIFTRT, 16))
	       && (GET_CODE (op1) == SIGN_EXTEND
		   || GET_CODE (op1) == ZERO_EXTEND
		   || scst5_operand (op1, SImode)
		   || shift_p (op1, ASHIFTRT, 16)
		   || shift_p (op1, LSHIFTRT, 16)))
	      || (shift_p (op0, ASHIFTRT, 16)
		  && (GET_CODE (op1) == SIGN_EXTEND
		      || shift_p (op1, ASHIFTRT, 16))))
	    {
	      *total = COSTS_N_INSNS (speed ? 2 : 1);
	      op0 = XEXP (op0, 0);
	      if (scst5_operand (op1, SImode))
		op1 = NULL_RTX;
	      else
		op1 = XEXP (op1, 0);
	    }
	  else if (!speed)
	    *total = COSTS_N_INSNS (1);
	  else if (TARGET_MPY32)
	    *total = COSTS_N_INSNS (4);
	  else
	    *total = COSTS_N_INSNS (6);
	}
      else if (GET_MODE (x) == HImode)
	*total = COSTS_N_INSNS (speed ? 2 : 1);

      if (GET_CODE (op0) != REG
	  && (GET_CODE (op0) != SUBREG || GET_CODE (SUBREG_REG (op0)) != REG))
	*total += rtx_cost (op0, MULT, 0, speed);
      if (op1 && GET_CODE (op1) != REG
	  && (GET_CODE (op1) != SUBREG || GET_CODE (SUBREG_REG (op1)) != REG))
	*total += rtx_cost (op1, MULT, 1, speed);
      return true;

    case UDIV:
    case DIV:
      /* This is a bit random; assuming on average there'll be 16 leading
	 zeros.  FIXME: estimate better for constant dividends.  */
      *total = COSTS_N_INSNS (6 + 3 * 16);
      return false;

    case IF_THEN_ELSE:
      /* Recognize the cmp_and/ior patterns.  */
      op0 = XEXP (x, 0);
      if ((GET_CODE (op0) == EQ || GET_CODE (op0) == NE)
	  && REG_P (XEXP (op0, 0))
	  && XEXP (op0, 1) == const0_rtx
	  && rtx_equal_p (XEXP (x, 1), XEXP (op0, 0)))
	{
	  *total = rtx_cost (XEXP (x, 1), (enum rtx_code) outer_code,
			     opno, speed);
	  return false;
	}
      return false;

    default:
      return false;
    }
}

/* Implements target hook vector_mode_supported_p.  */

static bool
c6x_vector_mode_supported_p (enum machine_mode mode)
{
  switch (mode)
    {
    case V2HImode:
    case V4QImode:
    case V2SImode:
    case V4HImode:
    case V8QImode:
      return true;
    default:
      return false;
    }
}

/* Implements TARGET_VECTORIZE_PREFERRED_SIMD_MODE.  */
static enum machine_mode
c6x_preferred_simd_mode (enum machine_mode mode)
{
  switch (mode)
    {
    case HImode:
      return V2HImode;
    case QImode:
      return V4QImode;

    default:
      return word_mode;
    }
}

/* Implement TARGET_SCALAR_MODE_SUPPORTED_P.  */

static bool
c6x_scalar_mode_supported_p (enum machine_mode mode)
{
  if (ALL_FIXED_POINT_MODE_P (mode)
      && GET_MODE_PRECISION (mode) <= 2 * BITS_PER_WORD)
    return true;

  return default_scalar_mode_supported_p (mode);
}

/* Output a reference from a function exception table to the type_info
   object X.  Output these via a special assembly directive.  */

static bool
c6x_output_ttype (rtx x)
{
  /* Use special relocations for symbol references.  */
  if (GET_CODE (x) != CONST_INT)
    fputs ("\t.ehtype\t", asm_out_file);
  else
    fputs ("\t.word\t", asm_out_file);
  output_addr_const (asm_out_file, x);
  fputc ('\n', asm_out_file);

  return TRUE;
}

/* Modify the return address of the current function.  */

void
c6x_set_return_address (rtx source, rtx scratch)
{
  struct c6x_frame frame;
  rtx addr;
  HOST_WIDE_INT offset;

  c6x_compute_frame_layout (&frame);
  if (! c6x_save_reg (RETURN_ADDR_REGNO))
    emit_move_insn (gen_rtx_REG (Pmode, RETURN_ADDR_REGNO), source);
  else
    {

      if (frame_pointer_needed)
	{
	  addr = hard_frame_pointer_rtx;
	  offset = frame.b3_offset;
	}
      else
	{
	  addr = stack_pointer_rtx;
	  offset = frame.to_allocate - frame.b3_offset;
	}

      /* TODO: Use base+offset loads where possible.  */
      if (offset)
	{
	  HOST_WIDE_INT low = trunc_int_for_mode (offset, HImode);

	  emit_insn (gen_movsi_high (scratch, GEN_INT (low)));
	  if (low != offset)
	    emit_insn (gen_movsi_lo_sum (scratch, scratch, GEN_INT(offset)));
	  emit_insn (gen_addsi3 (scratch, addr, scratch));
	  addr = scratch;
	}

      emit_move_insn (gen_frame_mem (Pmode, addr), source);
    }
}

/* We save pairs of registers using a DImode store.  Describe the component
   registers for DWARF generation code.  */

static rtx
c6x_dwarf_register_span (rtx rtl)
{
    unsigned regno;
    unsigned real_regno;
    int nregs;
    int i;
    rtx p;

    regno = REGNO (rtl);
    nregs = HARD_REGNO_NREGS (regno, GET_MODE (rtl));
    if (nregs == 1)
      return  NULL_RTX;

    p = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc(nregs));
    for (i = 0; i < nregs; i++)
      {
	if (TARGET_BIG_ENDIAN)
	  real_regno = regno + nregs - (i + 1);
	else
	  real_regno = regno + i;

	XVECEXP (p, 0, i) = gen_rtx_REG (SImode, real_regno);
      }

    return p;
}

/* Codes for all the C6X builtins.  */
enum c6x_builtins
{
  C6X_BUILTIN_SADD,
  C6X_BUILTIN_SSUB,
  C6X_BUILTIN_ADD2,
  C6X_BUILTIN_SUB2,
  C6X_BUILTIN_ADD4,
  C6X_BUILTIN_SUB4,
  C6X_BUILTIN_SADD2,
  C6X_BUILTIN_SSUB2,
  C6X_BUILTIN_SADDU4,

  C6X_BUILTIN_SMPY,
  C6X_BUILTIN_SMPYH,
  C6X_BUILTIN_SMPYHL,
  C6X_BUILTIN_SMPYLH,
  C6X_BUILTIN_MPY2,
  C6X_BUILTIN_SMPY2,

  C6X_BUILTIN_CLRR,
  C6X_BUILTIN_EXTR,
  C6X_BUILTIN_EXTRU,

  C6X_BUILTIN_SSHL,
  C6X_BUILTIN_SUBC,
  C6X_BUILTIN_ABS,
  C6X_BUILTIN_ABS2,
  C6X_BUILTIN_AVG2,
  C6X_BUILTIN_AVGU4,

  C6X_BUILTIN_MAX
};


static GTY(()) tree c6x_builtin_decls[C6X_BUILTIN_MAX];

/* Return the C6X builtin for CODE.  */
static tree
c6x_builtin_decl (unsigned code, bool initialize_p ATTRIBUTE_UNUSED)
{
  if (code >= C6X_BUILTIN_MAX)
    return error_mark_node;

  return c6x_builtin_decls[code];
}

#define def_builtin(NAME, TYPE, CODE)					\
do {									\
  tree bdecl;								\
  bdecl = add_builtin_function ((NAME), (TYPE), (CODE), BUILT_IN_MD,	\
				NULL, NULL_TREE);			\
  c6x_builtin_decls[CODE] = bdecl;					\
} while (0)

/* Set up all builtin functions for this target.  */
static void
c6x_init_builtins (void)
{
  tree V4QI_type_node = build_vector_type (unsigned_intQI_type_node, 4);
  tree V2HI_type_node = build_vector_type (intHI_type_node, 2);
  tree V2SI_type_node = build_vector_type (intSI_type_node, 2);
  tree int_ftype_int
    = build_function_type_list (integer_type_node, integer_type_node,
				NULL_TREE);
  tree int_ftype_int_int
    = build_function_type_list (integer_type_node, integer_type_node,
				integer_type_node, NULL_TREE);
  tree v2hi_ftype_v2hi
    = build_function_type_list (V2HI_type_node, V2HI_type_node, NULL_TREE);
  tree v4qi_ftype_v4qi_v4qi
    = build_function_type_list (V4QI_type_node, V4QI_type_node,
				V4QI_type_node, NULL_TREE);
  tree v2hi_ftype_v2hi_v2hi
    = build_function_type_list (V2HI_type_node, V2HI_type_node,
				V2HI_type_node, NULL_TREE);
  tree v2si_ftype_v2hi_v2hi
    = build_function_type_list (V2SI_type_node, V2HI_type_node,
				V2HI_type_node, NULL_TREE);
  
  def_builtin ("__builtin_c6x_sadd", int_ftype_int_int,
	       C6X_BUILTIN_SADD);
  def_builtin ("__builtin_c6x_ssub", int_ftype_int_int,
	       C6X_BUILTIN_SSUB);
  def_builtin ("__builtin_c6x_add2", v2hi_ftype_v2hi_v2hi,
	       C6X_BUILTIN_ADD2);
  def_builtin ("__builtin_c6x_sub2", v2hi_ftype_v2hi_v2hi,
	       C6X_BUILTIN_SUB2);
  def_builtin ("__builtin_c6x_add4", v4qi_ftype_v4qi_v4qi,
	       C6X_BUILTIN_ADD4);
  def_builtin ("__builtin_c6x_sub4", v4qi_ftype_v4qi_v4qi,
	       C6X_BUILTIN_SUB4);
  def_builtin ("__builtin_c6x_mpy2", v2si_ftype_v2hi_v2hi,
	       C6X_BUILTIN_MPY2);
  def_builtin ("__builtin_c6x_sadd2", v2hi_ftype_v2hi_v2hi,
	       C6X_BUILTIN_SADD2);
  def_builtin ("__builtin_c6x_ssub2", v2hi_ftype_v2hi_v2hi,
	       C6X_BUILTIN_SSUB2);
  def_builtin ("__builtin_c6x_saddu4", v4qi_ftype_v4qi_v4qi,
	       C6X_BUILTIN_SADDU4);
  def_builtin ("__builtin_c6x_smpy2", v2si_ftype_v2hi_v2hi,
	       C6X_BUILTIN_SMPY2);

  def_builtin ("__builtin_c6x_smpy", int_ftype_int_int,
	       C6X_BUILTIN_SMPY);
  def_builtin ("__builtin_c6x_smpyh", int_ftype_int_int,
	       C6X_BUILTIN_SMPYH);
  def_builtin ("__builtin_c6x_smpyhl", int_ftype_int_int,
	       C6X_BUILTIN_SMPYHL);
  def_builtin ("__builtin_c6x_smpylh", int_ftype_int_int,
	       C6X_BUILTIN_SMPYLH);

  def_builtin ("__builtin_c6x_sshl", int_ftype_int_int,
	       C6X_BUILTIN_SSHL);
  def_builtin ("__builtin_c6x_subc", int_ftype_int_int,
	       C6X_BUILTIN_SUBC);

  def_builtin ("__builtin_c6x_avg2", v2hi_ftype_v2hi_v2hi,
	       C6X_BUILTIN_AVG2);
  def_builtin ("__builtin_c6x_avgu4", v4qi_ftype_v4qi_v4qi,
	       C6X_BUILTIN_AVGU4);

  def_builtin ("__builtin_c6x_clrr", int_ftype_int_int,
	       C6X_BUILTIN_CLRR);
  def_builtin ("__builtin_c6x_extr", int_ftype_int_int,
	       C6X_BUILTIN_EXTR);
  def_builtin ("__builtin_c6x_extru", int_ftype_int_int,
	       C6X_BUILTIN_EXTRU);

  def_builtin ("__builtin_c6x_abs", int_ftype_int, C6X_BUILTIN_ABS);
  def_builtin ("__builtin_c6x_abs2", v2hi_ftype_v2hi, C6X_BUILTIN_ABS2);
}


struct builtin_description
{
  const enum insn_code icode;
  const char *const name;
  const enum c6x_builtins code;
};

static const struct builtin_description bdesc_2arg[] =
{
  { CODE_FOR_saddsi3, "__builtin_c6x_sadd", C6X_BUILTIN_SADD },
  { CODE_FOR_ssubsi3, "__builtin_c6x_ssub", C6X_BUILTIN_SSUB },
  { CODE_FOR_addv2hi3, "__builtin_c6x_add2", C6X_BUILTIN_ADD2 },
  { CODE_FOR_subv2hi3, "__builtin_c6x_sub2", C6X_BUILTIN_SUB2 },
  { CODE_FOR_addv4qi3, "__builtin_c6x_add4", C6X_BUILTIN_ADD4 },
  { CODE_FOR_subv4qi3, "__builtin_c6x_sub4", C6X_BUILTIN_SUB4 },
  { CODE_FOR_ss_addv2hi3, "__builtin_c6x_sadd2", C6X_BUILTIN_SADD2 },
  { CODE_FOR_ss_subv2hi3, "__builtin_c6x_ssub2", C6X_BUILTIN_SSUB2 },
  { CODE_FOR_us_addv4qi3, "__builtin_c6x_saddu4", C6X_BUILTIN_SADDU4 },

  { CODE_FOR_subcsi3, "__builtin_c6x_subc", C6X_BUILTIN_SUBC },
  { CODE_FOR_ss_ashlsi3, "__builtin_c6x_sshl", C6X_BUILTIN_SSHL },

  { CODE_FOR_avgv2hi3, "__builtin_c6x_avg2", C6X_BUILTIN_AVG2 },
  { CODE_FOR_uavgv4qi3, "__builtin_c6x_avgu4", C6X_BUILTIN_AVGU4 },

  { CODE_FOR_mulhqsq3, "__builtin_c6x_smpy", C6X_BUILTIN_SMPY },
  { CODE_FOR_mulhqsq3_hh, "__builtin_c6x_smpyh", C6X_BUILTIN_SMPYH },
  { CODE_FOR_mulhqsq3_lh, "__builtin_c6x_smpylh", C6X_BUILTIN_SMPYLH },
  { CODE_FOR_mulhqsq3_hl, "__builtin_c6x_smpyhl", C6X_BUILTIN_SMPYHL },

  { CODE_FOR_mulv2hqv2sq3, "__builtin_c6x_smpy2", C6X_BUILTIN_SMPY2 },

  { CODE_FOR_clrr, "__builtin_c6x_clrr", C6X_BUILTIN_CLRR },
  { CODE_FOR_extr, "__builtin_c6x_extr", C6X_BUILTIN_EXTR },
  { CODE_FOR_extru, "__builtin_c6x_extru", C6X_BUILTIN_EXTRU }
};

static const struct builtin_description bdesc_1arg[] =
{
  { CODE_FOR_ssabssi2, "__builtin_c6x_abs", C6X_BUILTIN_ABS },
  { CODE_FOR_ssabsv2hi2, "__builtin_c6x_abs2", C6X_BUILTIN_ABS2 }
};

/* Errors in the source file can cause expand_expr to return const0_rtx
   where we expect a vector.  To avoid crashing, use one of the vector
   clear instructions.  */
static rtx
safe_vector_operand (rtx x, enum machine_mode mode)
{
  if (x != const0_rtx)
    return x;
  x = gen_reg_rtx (SImode);

  emit_insn (gen_movsi (x, CONST0_RTX (SImode)));
  return gen_lowpart (mode, x);
}

/* Subroutine of c6x_expand_builtin to take care of binop insns.  MACFLAG is -1
   if this is a normal binary op, or one of the MACFLAG_xxx constants.  */

static rtx
c6x_expand_binop_builtin (enum insn_code icode, tree exp, rtx target,
			  bool match_op)
{
  int offs = match_op ? 1 : 0;
  rtx pat;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  tree arg1 = CALL_EXPR_ARG (exp, 1);
  rtx op0 = expand_expr (arg0, NULL_RTX, VOIDmode, EXPAND_NORMAL);
  rtx op1 = expand_expr (arg1, NULL_RTX, VOIDmode, EXPAND_NORMAL);
  enum machine_mode op0mode = GET_MODE (op0);
  enum machine_mode op1mode = GET_MODE (op1);
  enum machine_mode tmode = insn_data[icode].operand[0].mode;
  enum machine_mode mode0 = insn_data[icode].operand[1 + offs].mode;
  enum machine_mode mode1 = insn_data[icode].operand[2 + offs].mode;
  rtx ret = target;

  if (VECTOR_MODE_P (mode0))
    op0 = safe_vector_operand (op0, mode0);
  if (VECTOR_MODE_P (mode1))
    op1 = safe_vector_operand (op1, mode1);

  if (! target
      || GET_MODE (target) != tmode
      || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
    {
      if (tmode == SQmode || tmode == V2SQmode)
	{
	  ret = gen_reg_rtx (tmode == SQmode ? SImode : V2SImode);
	  target = gen_lowpart (tmode, ret);
	}
      else
	target = gen_reg_rtx (tmode);
    }

  if ((op0mode == V2HImode || op0mode == SImode || op0mode == VOIDmode)
      && (mode0 == V2HQmode || mode0 == HQmode || mode0 == SQmode))
    {
      op0mode = mode0;
      op0 = gen_lowpart (mode0, op0);
    }
  if ((op1mode == V2HImode || op1mode == SImode || op1mode == VOIDmode)
      && (mode1 == V2HQmode || mode1 == HQmode || mode1 == SQmode))
    {
      op1mode = mode1;
      op1 = gen_lowpart (mode1, op1);
    }
  /* In case the insn wants input operands in modes different from
     the result, abort.  */
  gcc_assert ((op0mode == mode0 || op0mode == VOIDmode)
	      && (op1mode == mode1 || op1mode == VOIDmode));

  if (! (*insn_data[icode].operand[1 + offs].predicate) (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);
  if (! (*insn_data[icode].operand[2 + offs].predicate) (op1, mode1))
    op1 = copy_to_mode_reg (mode1, op1);

  if (match_op)
    pat = GEN_FCN (icode) (target, target, op0, op1);
  else
    pat = GEN_FCN (icode) (target, op0, op1);

  if (! pat)
    return 0;

  emit_insn (pat);

  return ret;
}

/* Subroutine of c6x_expand_builtin to take care of unop insns.  */

static rtx
c6x_expand_unop_builtin (enum insn_code icode, tree exp,
			  rtx target)
{
  rtx pat;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  rtx op0 = expand_expr (arg0, NULL_RTX, VOIDmode, EXPAND_NORMAL);
  enum machine_mode op0mode = GET_MODE (op0);
  enum machine_mode tmode = insn_data[icode].operand[0].mode;
  enum machine_mode mode0 = insn_data[icode].operand[1].mode;

  if (! target
      || GET_MODE (target) != tmode
      || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
    target = gen_reg_rtx (tmode);

  if (VECTOR_MODE_P (mode0))
    op0 = safe_vector_operand (op0, mode0);

  if (op0mode == SImode && mode0 == HImode)
    {
      op0mode = HImode;
      op0 = gen_lowpart (HImode, op0);
    }
  gcc_assert (op0mode == mode0 || op0mode == VOIDmode);

  if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);

  pat = GEN_FCN (icode) (target, op0);
  if (! pat)
    return 0;
  emit_insn (pat);
  return target;
}

/* Expand an expression EXP that calls a built-in function,
   with result going to TARGET if that's convenient
   (and in mode MODE if that's convenient).
   SUBTARGET may be used as the target for computing one of EXP's operands.
   IGNORE is nonzero if the value is to be ignored.  */

static rtx
c6x_expand_builtin (tree exp, rtx target ATTRIBUTE_UNUSED,
		     rtx subtarget ATTRIBUTE_UNUSED,
		     enum machine_mode mode ATTRIBUTE_UNUSED,
		     int ignore ATTRIBUTE_UNUSED)
{
  size_t i;
  const struct builtin_description *d;
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  unsigned int fcode = DECL_FUNCTION_CODE (fndecl);

  for (i = 0, d = bdesc_2arg; i < ARRAY_SIZE (bdesc_2arg); i++, d++)
    if (d->code == fcode)
      return c6x_expand_binop_builtin (d->icode, exp, target,
				       fcode == C6X_BUILTIN_CLRR);

  for (i = 0, d = bdesc_1arg; i < ARRAY_SIZE (bdesc_1arg); i++, d++)
    if (d->code == fcode)
      return c6x_expand_unop_builtin (d->icode, exp, target);

  gcc_unreachable ();
}

/* Target unwind frame info is generated from dwarf CFI directives, so
   always output dwarf2 unwind info.  */

static enum unwind_info_type
c6x_debug_unwind_info (void)
{
  if (flag_unwind_tables || flag_exceptions)
    return UI_DWARF2;

  return default_debug_unwind_info ();
}

/* Target Structure.  */

/* Initialize the GCC target structure.  */
#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG c6x_function_arg
#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE c6x_function_arg_advance
#undef TARGET_FUNCTION_ARG_BOUNDARY
#define TARGET_FUNCTION_ARG_BOUNDARY c6x_function_arg_boundary
#undef TARGET_FUNCTION_ARG_ROUND_BOUNDARY
#define TARGET_FUNCTION_ARG_ROUND_BOUNDARY \
  c6x_function_arg_round_boundary
#undef TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P c6x_function_value_regno_p
#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE c6x_function_value
#undef TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE c6x_libcall_value
#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY c6x_return_in_memory
#undef TARGET_RETURN_IN_MSB
#define TARGET_RETURN_IN_MSB c6x_return_in_msb
#undef TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE c6x_pass_by_reference
#undef TARGET_CALLEE_COPIES
#define TARGET_CALLEE_COPIES c6x_callee_copies
#undef TARGET_STRUCT_VALUE_RTX
#define TARGET_STRUCT_VALUE_RTX c6x_struct_value_rtx
#undef TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL c6x_function_ok_for_sibcall

#undef TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK c6x_output_mi_thunk
#undef TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK c6x_can_output_mi_thunk

#undef TARGET_BUILD_BUILTIN_VA_LIST
#define TARGET_BUILD_BUILTIN_VA_LIST c6x_build_builtin_va_list

#undef TARGET_ASM_TRAMPOLINE_TEMPLATE
#define TARGET_ASM_TRAMPOLINE_TEMPLATE c6x_asm_trampoline_template
#undef TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT c6x_initialize_trampoline

#undef TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P c6x_legitimate_constant_p
#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P c6x_legitimate_address_p

#undef TARGET_IN_SMALL_DATA_P
#define TARGET_IN_SMALL_DATA_P c6x_in_small_data_p
#undef	TARGET_ASM_SELECT_RTX_SECTION
#define TARGET_ASM_SELECT_RTX_SECTION  c6x_select_rtx_section
#undef TARGET_ASM_SELECT_SECTION
#define TARGET_ASM_SELECT_SECTION  c6x_elf_select_section
#undef TARGET_ASM_UNIQUE_SECTION
#define TARGET_ASM_UNIQUE_SECTION  c6x_elf_unique_section
#undef TARGET_SECTION_TYPE_FLAGS
#define TARGET_SECTION_TYPE_FLAGS  c6x_section_type_flags
#undef TARGET_HAVE_SRODATA_SECTION
#define TARGET_HAVE_SRODATA_SECTION true
#undef TARGET_ASM_MERGEABLE_RODATA_PREFIX
#define TARGET_ASM_MERGEABLE_RODATA_PREFIX ".const"

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE c6x_option_override
#undef TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE c6x_conditional_register_usage

#undef TARGET_INIT_LIBFUNCS
#define TARGET_INIT_LIBFUNCS c6x_init_libfuncs
#undef TARGET_LIBFUNC_GNU_PREFIX
#define TARGET_LIBFUNC_GNU_PREFIX true

#undef TARGET_SCALAR_MODE_SUPPORTED_P
#define TARGET_SCALAR_MODE_SUPPORTED_P c6x_scalar_mode_supported_p
#undef TARGET_VECTOR_MODE_SUPPORTED_P
#define TARGET_VECTOR_MODE_SUPPORTED_P c6x_vector_mode_supported_p
#undef TARGET_VECTORIZE_PREFERRED_SIMD_MODE
#define TARGET_VECTORIZE_PREFERRED_SIMD_MODE c6x_preferred_simd_mode

#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS c6x_rtx_costs

#undef TARGET_SCHED_INIT
#define TARGET_SCHED_INIT c6x_sched_init
#undef TARGET_SCHED_SET_SCHED_FLAGS
#define TARGET_SCHED_SET_SCHED_FLAGS c6x_set_sched_flags
#undef TARGET_SCHED_ADJUST_COST
#define TARGET_SCHED_ADJUST_COST c6x_adjust_cost
#undef TARGET_SCHED_ISSUE_RATE
#define TARGET_SCHED_ISSUE_RATE c6x_issue_rate
#undef TARGET_SCHED_VARIABLE_ISSUE
#define TARGET_SCHED_VARIABLE_ISSUE c6x_variable_issue
#undef TARGET_SCHED_REORDER
#define TARGET_SCHED_REORDER c6x_sched_reorder
#undef TARGET_SCHED_REORDER2
#define TARGET_SCHED_REORDER2 c6x_sched_reorder2
#undef TARGET_SCHED_DFA_NEW_CYCLE
#define TARGET_SCHED_DFA_NEW_CYCLE c6x_dfa_new_cycle
#undef TARGET_SCHED_DFA_PRE_CYCLE_INSN
#define TARGET_SCHED_DFA_PRE_CYCLE_INSN c6x_sched_dfa_pre_cycle_insn
#undef TARGET_SCHED_EXPOSED_PIPELINE
#define TARGET_SCHED_EXPOSED_PIPELINE true

#undef TARGET_SCHED_ALLOC_SCHED_CONTEXT
#define TARGET_SCHED_ALLOC_SCHED_CONTEXT c6x_alloc_sched_context
#undef TARGET_SCHED_INIT_SCHED_CONTEXT
#define TARGET_SCHED_INIT_SCHED_CONTEXT c6x_init_sched_context
#undef TARGET_SCHED_SET_SCHED_CONTEXT
#define TARGET_SCHED_SET_SCHED_CONTEXT c6x_set_sched_context
#undef TARGET_SCHED_CLEAR_SCHED_CONTEXT
#define TARGET_SCHED_CLEAR_SCHED_CONTEXT c6x_clear_sched_context
#undef TARGET_SCHED_FREE_SCHED_CONTEXT
#define TARGET_SCHED_FREE_SCHED_CONTEXT c6x_free_sched_context

#undef TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE c6x_can_eliminate

#undef TARGET_PREFERRED_RENAME_CLASS
#define TARGET_PREFERRED_RENAME_CLASS c6x_preferred_rename_class

#undef TARGET_MACHINE_DEPENDENT_REORG
#define TARGET_MACHINE_DEPENDENT_REORG c6x_reorg

#undef TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START c6x_file_start

#undef  TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND c6x_print_operand
#undef  TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS c6x_print_operand_address
#undef  TARGET_PRINT_OPERAND_PUNCT_VALID_P
#define TARGET_PRINT_OPERAND_PUNCT_VALID_P c6x_print_operand_punct_valid_p

/* C6x unwinding tables use a different format for the typeinfo tables.  */
#undef TARGET_ASM_TTYPE
#define TARGET_ASM_TTYPE c6x_output_ttype

/* The C6x ABI follows the ARM EABI exception handling rules.  */
#undef TARGET_ARM_EABI_UNWINDER
#define TARGET_ARM_EABI_UNWINDER true

#undef TARGET_ASM_EMIT_EXCEPT_PERSONALITY
#define TARGET_ASM_EMIT_EXCEPT_PERSONALITY c6x_asm_emit_except_personality

#undef TARGET_ASM_INIT_SECTIONS
#define TARGET_ASM_INIT_SECTIONS c6x_asm_init_sections

#undef TARGET_DEBUG_UNWIND_INFO
#define TARGET_DEBUG_UNWIND_INFO  c6x_debug_unwind_info

#undef TARGET_DWARF_REGISTER_SPAN
#define TARGET_DWARF_REGISTER_SPAN c6x_dwarf_register_span

#undef TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS c6x_init_builtins
#undef TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN c6x_expand_builtin
#undef  TARGET_BUILTIN_DECL
#define TARGET_BUILTIN_DECL c6x_builtin_decl

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-c6x.h"
