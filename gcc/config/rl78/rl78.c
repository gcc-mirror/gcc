/* Subroutines used for code generation on Renesas RL78 processors.
   Copyright (C) 2011-2016 Free Software Foundation, Inc.
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
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "df.h"
#include "tm_p.h"
#include "stringpool.h"
#include "optabs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "diagnostic-core.h"
#include "varasm.h"
#include "stor-layout.h"
#include "calls.h"
#include "output.h"
#include "insn-attr.h"
#include "explow.h"
#include "expr.h"
#include "reload.h"
#include "cfgrtl.h"
#include "langhooks.h"
#include "tree-pass.h"
#include "context.h"
#include "tm-constrs.h" /* for satisfies_constraint_*().  */
#include "builtins.h"

/* This file should be included last.  */
#include "target-def.h"

static inline bool is_interrupt_func (const_tree decl);
static inline bool is_brk_interrupt_func (const_tree decl);
static void rl78_reorg (void);
static const char *rl78_strip_name_encoding (const char *);
static const char *rl78_strip_nonasm_name_encoding (const char *);
static section * rl78_select_section (tree, int, unsigned HOST_WIDE_INT);


/* Debugging statements are tagged with DEBUG0 only so that they can
   be easily enabled individually, by replacing the '0' with '1' as
   needed.  */
#define DEBUG0 0
#define DEBUG1 1

/* REGISTER_NAMES has the names for individual 8-bit registers, but
   these have the names we need to use when referring to 16-bit
   register pairs.  */
static const char * const word_regnames[] =
{
  "ax", "AX", "bc", "BC", "de", "DE", "hl", "HL",
  "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15",
  "r16", "r17", "r18", "r19", "r20", "r21", "r22", "r23",
  "r24", "r25", "r26", "r27", "r28", "r29", "r30", "r31",
  "sp", "ap", "psw", "es", "cs"
};

/* Structure for G13 MDUC registers.  */
struct mduc_reg_type
{
  unsigned int       address;
  enum machine_mode  mode;
};

struct mduc_reg_type  mduc_regs[] =
{
  {0xf00e8, QImode},
  {0xffff0, HImode},
  {0xffff2, HImode},
  {0xf2224, HImode},
  {0xf00e0, HImode},
  {0xf00e2, HImode}
};

struct GTY(()) machine_function
{
  /* If set, the rest of the fields have been computed.  */
  int computed;
  /* Which register pairs need to be pushed in the prologue.  */
  int need_to_push [FIRST_PSEUDO_REGISTER / 2];

  /* These fields describe the frame layout...  */
  /* arg pointer */
  /* 4 bytes for saved PC */
  int framesize_regs;
  /* frame pointer */
  int framesize_locals;
  int framesize_outgoing;
  /* stack pointer */
  int framesize;

  /* If set, recog is allowed to match against the "real" patterns.  */
  int real_insns_ok;
  /* If set, recog is allowed to match against the "virtual" patterns.  */
  int virt_insns_ok;
  /* Set if the current function needs to clean up any trampolines.  */
  int trampolines_used;
  /* True if the ES register is used and hence
     needs to be saved inside interrupt handlers.  */
  bool uses_es;
};

/* This is our init_machine_status, as set in
   rl78_option_override.  */
static struct machine_function *
rl78_init_machine_status (void)
{
  struct machine_function *m;

  m = ggc_cleared_alloc<machine_function> ();
  m->virt_insns_ok = 1;

  return m;
}

/* This pass converts virtual instructions using virtual registers, to
   real instructions using real registers.  Rather than run it as
   reorg, we reschedule it before vartrack to help with debugging.  */
namespace
{
  const pass_data pass_data_rl78_devirt =
    {
      RTL_PASS, /* type */
      "devirt", /* name */
      OPTGROUP_NONE, /* optinfo_flags */
      TV_MACH_DEP, /* tv_id */
      0, /* properties_required */
      0, /* properties_provided */
      0, /* properties_destroyed */
      0, /* todo_flags_start */
      0, /* todo_flags_finish */
    };

  class pass_rl78_devirt : public rtl_opt_pass
  {
  public:
    pass_rl78_devirt (gcc::context *ctxt)
      : rtl_opt_pass (pass_data_rl78_devirt, ctxt)
      {
      }

    /* opt_pass methods: */
    virtual unsigned int execute (function *)
    {
      rl78_reorg ();
      return 0;
    }
  };
} // anon namespace

rtl_opt_pass *
make_pass_rl78_devirt (gcc::context *ctxt)
{
  return new pass_rl78_devirt (ctxt);
}

/* Redundant move elimination pass.  Must be run after the basic block
   reordering pass for the best effect.  */

static unsigned int
move_elim_pass (void)
{
  rtx_insn *insn, *ninsn;
  rtx prev = NULL_RTX;

  for (insn = get_insns (); insn; insn = ninsn)
    {
      rtx set;

      ninsn = next_nonnote_nondebug_insn (insn);

      if ((set = single_set (insn)) == NULL_RTX)
	{
	  prev = NULL_RTX;
	  continue;
	}

      /* If we have two SET insns in a row (without anything
	 between them) and the source of the second one is the
	 destination of the first one, and vice versa, then we
	 can eliminate the second SET.  */
      if (prev
	  && rtx_equal_p (SET_DEST (prev), SET_SRC (set))
	  && rtx_equal_p (SET_DEST (set), SET_SRC (prev))
	  /* ... and none of the operands are volatile.  */
	  && ! volatile_refs_p (SET_SRC (prev))
	  && ! volatile_refs_p (SET_DEST (prev))
	  && ! volatile_refs_p (SET_SRC (set))
	  && ! volatile_refs_p (SET_DEST (set)))
	{
	  if (dump_file)
	    fprintf (dump_file, " Delete insn %d because it is redundant\n",
		     INSN_UID (insn));

	  delete_insn (insn);
	  prev = NULL_RTX;
	}
      else
	prev = set;
    }

  if (dump_file)
    print_rtl_with_bb (dump_file, get_insns (), 0);

  return 0;
}

namespace
{
  const pass_data pass_data_rl78_move_elim =
    {
      RTL_PASS, /* type */
      "move_elim", /* name */
      OPTGROUP_NONE, /* optinfo_flags */
      TV_MACH_DEP, /* tv_id */
      0, /* properties_required */
      0, /* properties_provided */
      0, /* properties_destroyed */
      0, /* todo_flags_start */
      0, /* todo_flags_finish */
    };

  class pass_rl78_move_elim : public rtl_opt_pass
  {
  public:
    pass_rl78_move_elim (gcc::context *ctxt)
      : rtl_opt_pass (pass_data_rl78_move_elim, ctxt)
      {
      }

    /* opt_pass methods: */
    virtual unsigned int execute (function *) { return move_elim_pass (); }
  };
} // anon namespace

rtl_opt_pass *
make_pass_rl78_move_elim (gcc::context *ctxt)
{
  return new pass_rl78_move_elim (ctxt);
}

#undef  TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START rl78_asm_file_start

static void
rl78_asm_file_start (void)
{
  int i;

  if (TARGET_G10)
    {
      /* The memory used is 0xffec8 to 0xffedf; real registers are in
	 0xffee0 to 0xffee7.  */
      for (i = 8; i < 32; i++)
	fprintf (asm_out_file, "r%d\t=\t0x%x\n", i, 0xffec0 + i);
    }
  else
    {
      for (i = 0; i < 8; i++)
	{
	  fprintf (asm_out_file, "r%d\t=\t0x%x\n", 8 + i, 0xffef0 + i);
	  fprintf (asm_out_file, "r%d\t=\t0x%x\n", 16 + i, 0xffee8 + i);
	  fprintf (asm_out_file, "r%d\t=\t0x%x\n", 24 + i, 0xffee0 + i);
	}
    }

  opt_pass *rl78_devirt_pass = make_pass_rl78_devirt (g);
  struct register_pass_info rl78_devirt_info =
    {
      rl78_devirt_pass,
      "pro_and_epilogue",
      1,
      PASS_POS_INSERT_BEFORE
    };

  opt_pass *rl78_move_elim_pass = make_pass_rl78_move_elim (g);
  struct register_pass_info rl78_move_elim_info =
    {
      rl78_move_elim_pass,
      "bbro",
      1,
      PASS_POS_INSERT_AFTER
    };

  register_pass (& rl78_devirt_info);
  register_pass (& rl78_move_elim_info);
}

void
rl78_output_symbol_ref (FILE * file, rtx sym)
{
  tree type = SYMBOL_REF_DECL (sym);
  const char *str = XSTR (sym, 0);

  if (str[0] == '*')
    {
      fputs (str + 1, file);
    }
  else
    {
      str = rl78_strip_nonasm_name_encoding (str);
      if (type && TREE_CODE (type) == FUNCTION_DECL)
	{
	  fprintf (file, "%%code(");
	  assemble_name (file, str);
	  fprintf (file, ")");
	}
      else
	assemble_name (file, str);
    }
}

#undef  TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE		rl78_option_override

#define MUST_SAVE_MDUC_REGISTERS			\
  (TARGET_SAVE_MDUC_REGISTERS				\
   && (is_interrupt_func (NULL_TREE)) && RL78_MUL_G13)

static void
rl78_option_override (void)
{
  flag_omit_frame_pointer = 1;
  flag_no_function_cse = 1;
  flag_split_wide_types = 0;

  init_machine_status = rl78_init_machine_status;

  if (TARGET_ALLREGS)
    {
      int i;

      for (i = 24; i < 32; i++)
	fixed_regs[i] = 0;
    }

  if (TARGET_ES0
      && strcmp (lang_hooks.name, "GNU C")
      && strcmp (lang_hooks.name, "GNU C11")
      && strcmp (lang_hooks.name, "GNU C89")
      && strcmp (lang_hooks.name, "GNU C99")
      /* Compiling with -flto results in a language of GNU GIMPLE being used... */
      && strcmp (lang_hooks.name, "GNU GIMPLE"))
    /* Address spaces are currently only supported by C.  */
    error ("-mes0 can only be used with C");

  if (TARGET_SAVE_MDUC_REGISTERS && !(TARGET_G13 || RL78_MUL_G13))
    warning (0, "mduc registers only saved for G13 target");

  switch (rl78_cpu_type)
    {
    case CPU_UNINIT:
      rl78_cpu_type = CPU_G14;
      if (rl78_mul_type == MUL_UNINIT)
	rl78_mul_type = MUL_NONE;
      break;

    case CPU_G10:
      switch (rl78_mul_type)
	{
	case MUL_UNINIT: rl78_mul_type = MUL_NONE; break;
	case MUL_NONE:   break;
	case MUL_G13:  	 error ("-mmul=g13 cannot be used with -mcpu=g10"); break;
	case MUL_G14:  	 error ("-mmul=g14 cannot be used with -mcpu=g10"); break;
	}
      break;

    case CPU_G13:
      switch (rl78_mul_type)
	{
	case MUL_UNINIT: rl78_mul_type = MUL_G13; break;
	case MUL_NONE:   break;
	case MUL_G13:  	break;
	  /* The S2 core does not have mul/div instructions.  */
	case MUL_G14: 	error ("-mmul=g14 cannot be used with -mcpu=g13"); break;
	}
      break;

    case CPU_G14:
      switch (rl78_mul_type)
	{
	case MUL_UNINIT: rl78_mul_type = MUL_G14; break;
	case MUL_NONE:   break;
	case MUL_G14:  	break;
	/* The G14 core does not have the hardware multiply peripheral used by the
	   G13 core, hence you cannot use G13 multipliy routines on G14 hardware.  */
	case MUL_G13: 	error ("-mmul=g13 cannot be used with -mcpu=g14"); break;
	}
      break;
    }
}

/* Most registers are 8 bits.  Some are 16 bits because, for example,
   gcc doesn't like dealing with $FP as a register pair (the second
   half of $fp is also 2 to keep reload happy wrt register pairs, but
   no register class includes it).  This table maps register numbers
   to size in bytes.  */
static const int register_sizes[] =
{
  1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 2, 2,
  1, 1, 1, 1, 1, 1, 1, 1,
  2, 2, 1, 1, 1
};

/* Predicates used in the MD patterns.  This one is true when virtual
   insns may be matched, which typically means before (or during) the
   devirt pass.  */
bool
rl78_virt_insns_ok (void)
{
  if (cfun)
    return cfun->machine->virt_insns_ok;
  return true;
}

/* Predicates used in the MD patterns.  This one is true when real
   insns may be matched, which typically means after (or during) the
   devirt pass.  */
bool
rl78_real_insns_ok (void)
{
  if (cfun)
    return cfun->machine->real_insns_ok;
  return false;
}

/* Implements HARD_REGNO_NREGS.  */
int
rl78_hard_regno_nregs (int regno, machine_mode mode)
{
  int rs = register_sizes[regno];
  if (rs < 1)
    rs = 1;
  return ((GET_MODE_SIZE (mode) + rs - 1) / rs);
}

/* Implements HARD_REGNO_MODE_OK.  */
int
rl78_hard_regno_mode_ok (int regno, machine_mode mode)
{
  int s = GET_MODE_SIZE (mode);

  if (s < 1)
    return 0;
  /* These are not to be used by gcc.  */
  if (regno == 23 || regno == ES_REG || regno == CS_REG)
    return 0;
  /* $fp can always be accessed as a 16-bit value.  */
  if (regno == FP_REG && s == 2)
    return 1;
  if (regno < SP_REG)
    {
      /* Since a reg-reg move is really a reg-mem move, we must
	 enforce alignment.  */
      if (s > 1 && (regno % 2))
	return 0;
      return 1;
    }
  if (s == CC_REGNUM)
    return (mode == BImode);
  /* All other registers must be accessed in their natural sizes.  */
  if (s == register_sizes [regno])
    return 1;
  return 0;
}

/* Simplify_gen_subreg() doesn't handle memory references the way we
   need it to below, so we use this function for when we must get a
   valid subreg in a "natural" state.  */
static rtx
rl78_subreg (machine_mode mode, rtx r, machine_mode omode, int byte)
{
  if (GET_CODE (r) == MEM)
    return adjust_address (r, mode, byte);
  else
    return simplify_gen_subreg (mode, r, omode, byte);
}

/* Used by movsi.  Split SImode moves into two HImode moves, using
   appropriate patterns for the upper and lower halves of symbols.  */
void
rl78_expand_movsi (rtx *operands)
{
  rtx op00, op02, op10, op12;

  op00 = rl78_subreg (HImode, operands[0], SImode, 0);
  op02 = rl78_subreg (HImode, operands[0], SImode, 2);
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
      op10 = rl78_subreg (HImode, operands[1], SImode, 0);
      op12 = rl78_subreg (HImode, operands[1], SImode, 2);
    }

  if (rtx_equal_p (operands[0], operands[1]))
    ;
  else if (rtx_equal_p (op00, op12))
    {
      emit_move_insn (op02, op12);
      emit_move_insn (op00, op10);
    }
  else
    {
      emit_move_insn (op00, op10);
      emit_move_insn (op02, op12);
    }
}

/* Generate code to move an SImode value.  */
void
rl78_split_movsi (rtx *operands, enum machine_mode omode)
{
  rtx op00, op02, op10, op12;

  op00 = rl78_subreg (HImode, operands[0], omode, 0);
  op02 = rl78_subreg (HImode, operands[0], omode, 2);

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
      op10 = rl78_subreg (HImode, operands[1], omode, 0);
      op12 = rl78_subreg (HImode, operands[1], omode, 2);
    }

  if (rtx_equal_p (operands[0], operands[1]))
    ;
  else if (rtx_equal_p (op00, op12))
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

/* Used by various two-operand expanders which cannot accept all
   operands in the "far" namespace.  Force some such operands into
   registers so that each pattern has at most one far operand.  */
int
rl78_force_nonfar_2 (rtx *operands, rtx (*gen)(rtx,rtx))
{
  int did = 0;
  rtx temp_reg = NULL;

  /* FIXME: in the future, be smarter about only doing this if the
     other operand is also far, assuming the devirtualizer can also
     handle that.  */
  if (rl78_far_p (operands[0]))
    {
      temp_reg = operands[0];
      operands[0] = gen_reg_rtx (GET_MODE (operands[0]));
      did = 1;
    }
  if (!did)
    return 0;

  emit_insn (gen (operands[0], operands[1]));
  if (temp_reg)
    emit_move_insn (temp_reg, operands[0]);
  return 1;
}

/* Likewise, but for three-operand expanders.  */
int
rl78_force_nonfar_3 (rtx *operands, rtx (*gen)(rtx,rtx,rtx))
{
  int did = 0;
  rtx temp_reg = NULL;

  /* FIXME: Likewise.  */
  if (rl78_far_p (operands[1]))
    {
      rtx temp_reg = gen_reg_rtx (GET_MODE (operands[1]));
      emit_move_insn (temp_reg, operands[1]);
      operands[1] = temp_reg;
      did = 1;
    }
  if (rl78_far_p (operands[0]))
    {
      temp_reg = operands[0];
      operands[0] = gen_reg_rtx (GET_MODE (operands[0]));
      did = 1;
    }
  if (!did)
    return 0;

  emit_insn (gen (operands[0], operands[1], operands[2]));
  if (temp_reg)
    emit_move_insn (temp_reg, operands[0]);
  return 1;
}

int
rl78_one_far_p (rtx *operands, int n)
{
  rtx which = NULL;
  int i, c = 0;

  for (i = 0; i < n; i ++)
    if (rl78_far_p (operands[i]))
      {
	if (which == NULL)
	  which = operands[i];
	else if (rtx_equal_p (operands[i], which))
	  continue;
	c ++;
      }
  return c <= 1;
}

#undef  TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE		rl78_can_eliminate

static bool
rl78_can_eliminate (const int from ATTRIBUTE_UNUSED, const int to ATTRIBUTE_UNUSED)
{
  return true;
}

/* Returns true if the given register needs to be saved by the
   current function.  */
static bool
need_to_save (unsigned int regno)
{
  if (is_interrupt_func (cfun->decl))
    {
      /* We don't know what devirt will need */
      if (regno < 8)
	return true;

       /* We don't need to save registers that have
	  been reserved for interrupt handlers.  */
      if (regno > 23)
	return false;

      /* If the handler is a non-leaf function then it may call
	 non-interrupt aware routines which will happily clobber
	 any call_used registers, so we have to preserve them.
         We do not have to worry about the frame pointer register
	 though, as that is handled below.  */
      if (!crtl->is_leaf && call_used_regs[regno] && regno < 22)
	return true;

      /* Otherwise we only have to save a register, call_used
	 or not, if it is used by this handler.  */
      return df_regs_ever_live_p (regno);
    }

  if (regno == FRAME_POINTER_REGNUM
      && (frame_pointer_needed || df_regs_ever_live_p (regno)))
    return true;
  if (fixed_regs[regno])
    return false;
  if (crtl->calls_eh_return)
    return true;
  if (df_regs_ever_live_p (regno)
      && !call_used_regs[regno])
    return true;
  return false;
}

/* We use this to wrap all emitted insns in the prologue.  */
static rtx
F (rtx x)
{
  RTX_FRAME_RELATED_P (x) = 1;
  return x;
}

/* Compute all the frame-related fields in our machine_function
   structure.  */
static void
rl78_compute_frame_info (void)
{
  int i;

  cfun->machine->computed = 1;
  cfun->machine->framesize_regs = 0;
  cfun->machine->framesize_locals = get_frame_size ();
  cfun->machine->framesize_outgoing = crtl->outgoing_args_size;

  for (i = 0; i < 16; i ++)
    if (need_to_save (i * 2) || need_to_save (i * 2 + 1))
      {
	cfun->machine->need_to_push [i] = 1;
	cfun->machine->framesize_regs += 2;
      }
    else
      cfun->machine->need_to_push [i] = 0;

  if ((cfun->machine->framesize_locals + cfun->machine->framesize_outgoing) & 1)
    cfun->machine->framesize_locals ++;

  cfun->machine->framesize = (cfun->machine->framesize_regs
			      + cfun->machine->framesize_locals
			      + cfun->machine->framesize_outgoing);
}

/* Returns true if the provided function has the specified attribute.  */
static inline bool
has_func_attr (const_tree decl, const char * func_attr)
{
  if (decl == NULL_TREE)
    decl = current_function_decl;

  return lookup_attribute (func_attr, DECL_ATTRIBUTES (decl)) != NULL_TREE;
}

/* Returns true if the provided function has the "interrupt" attribute.  */
static inline bool
is_interrupt_func (const_tree decl)
{
  return has_func_attr (decl, "interrupt") || has_func_attr (decl, "brk_interrupt");
}

/* Returns true if the provided function has the "brk_interrupt" attribute.  */
static inline bool
is_brk_interrupt_func (const_tree decl)
{
  return has_func_attr (decl, "brk_interrupt");
}

/* Check "interrupt" attributes.  */
static tree
rl78_handle_func_attribute (tree * node,
			    tree   name,
			    tree   args,
			    int    flags ATTRIBUTE_UNUSED,
			    bool * no_add_attrs)
{
  gcc_assert (DECL_P (* node));
  gcc_assert (args == NULL_TREE);

  if (TREE_CODE (* node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute only applies to functions",
	       name);
      * no_add_attrs = true;
    }

  /* FIXME: We ought to check that the interrupt and exception
     handler attributes have been applied to void functions.  */
  return NULL_TREE;
}

/* Check "naked" attributes.  */
static tree
rl78_handle_naked_attribute (tree * node,
			     tree   name ATTRIBUTE_UNUSED,
			     tree   args,
			     int    flags ATTRIBUTE_UNUSED,
			     bool * no_add_attrs)
{
  gcc_assert (DECL_P (* node));
  gcc_assert (args == NULL_TREE);

  if (TREE_CODE (* node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "naked attribute only applies to functions");
      * no_add_attrs = true;
    }

  /* Disable warnings about this function - eg reaching the end without
     seeing a return statement - because the programmer is doing things
     that gcc does not know about.  */
  TREE_NO_WARNING (* node) = 1;

  return NULL_TREE;
}

/* Check "saddr" attributes.  */
static tree
rl78_handle_saddr_attribute (tree * node,
			     tree   name,
			     tree   args ATTRIBUTE_UNUSED,
			     int    flags ATTRIBUTE_UNUSED,
			     bool * no_add_attrs)
{
  gcc_assert (DECL_P (* node));

  if (TREE_CODE (* node) == FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute doesn't apply to functions",
	       name);
      * no_add_attrs = true;
    }

  return NULL_TREE;
}

#undef  TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE		rl78_attribute_table

/* Table of RL78-specific attributes.  */
const struct attribute_spec rl78_attribute_table[] =
{
  /* Name, min_len, max_len, decl_req, type_req, fn_type_req, handler,
     affects_type_identity.  */
  { "interrupt",      0, 0, true, false, false, rl78_handle_func_attribute,
    false },
  { "brk_interrupt",  0, 0, true, false, false, rl78_handle_func_attribute,
    false },
  { "naked",          0, 0, true, false, false, rl78_handle_naked_attribute,
    false },
  { "saddr",          0, 0, true, false, false, rl78_handle_saddr_attribute,
    false },
  { NULL,             0, 0, false, false, false, NULL, false }
};



/* Break down an address RTX into its component base/index/addend
   portions and return TRUE if the address is of a valid form, else
   FALSE.  */
static bool
characterize_address (rtx x, rtx *base, rtx *index, rtx *addend)
{
  *base = NULL_RTX;
  *index = NULL_RTX;
  *addend = NULL_RTX;

  if (GET_CODE (x) == UNSPEC
      && XINT (x, 1) == UNS_ES_ADDR)
    x = XVECEXP (x, 0, 1);

  if (GET_CODE (x) == REG)
    {
      *base = x;
      return true;
    }

  /* We sometimes get these without the CONST wrapper */
  if (GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x, 0)) == SYMBOL_REF
      && GET_CODE (XEXP (x, 1)) == CONST_INT)
    {
      *addend = x;
      return true;
    }

  if (GET_CODE (x) == PLUS)
    {
      *base = XEXP (x, 0);
      x = XEXP (x, 1);

      if (GET_CODE (*base) == SUBREG)
	{
	  if (GET_MODE (*base) == HImode
	      && GET_MODE (XEXP (*base, 0)) == SImode
	      && GET_CODE (XEXP (*base, 0)) == REG)
	    {
	      /* This is a throw-away rtx just to tell everyone
		 else what effective register we're using.  */
	      *base = gen_rtx_REG (HImode, REGNO (XEXP (*base, 0)));
	    }
	}

      if (GET_CODE (*base) != REG
	  && GET_CODE (x) == REG)
	{
	  rtx tmp = *base;
	  *base = x;
	  x = tmp;
	}

      if (GET_CODE (*base) != REG)
	return false;

      if (GET_CODE (x) == ZERO_EXTEND
	  && GET_CODE (XEXP (x, 0)) == REG)
	{
	  *index = XEXP (x, 0);
	  return false;
	}
    }

  switch (GET_CODE (x))
    {
    case PLUS:
      if (GET_CODE (XEXP (x, 0)) == SYMBOL_REF
	  && GET_CODE (XEXP (x, 0)) == CONST_INT)
	{
	  *addend = x;
	  return true;
	}
      /* fall through */
    case MEM:
    case REG:
      return false;

    case SUBREG:
      switch (GET_CODE (XEXP (x, 0)))
	{
	case CONST:
	case SYMBOL_REF:
	case CONST_INT:
	  *addend = x;
	  return true;
	default:
	  return false;
	}

    case CONST:
    case SYMBOL_REF:
    case CONST_INT:
      *addend = x;
      return true;

    default:
      return false;
    }

  return false;
}

/* Used by the Whb constraint.  Match addresses that use HL+B or HL+C
   addressing.  */
bool
rl78_hl_b_c_addr_p (rtx op)
{
  rtx hl, bc;

  if (GET_CODE (op) != PLUS)
    return false;
  hl = XEXP (op, 0);
  bc = XEXP (op, 1);
  if (GET_CODE (hl) == ZERO_EXTEND)
    {
      rtx tmp = hl;
      hl = bc;
      bc = tmp;
    }
  if (GET_CODE (hl) != REG)
    return false;
  if (GET_CODE (bc) != ZERO_EXTEND)
    return false;
  bc = XEXP (bc, 0);
  if (GET_CODE (bc) != REG)
    return false;
  if (REGNO (hl) != HL_REG)
    return false;
  if (REGNO (bc) != B_REG && REGNO (bc) != C_REG)
    return false;

  return true;
}

#define REG_IS(r, regno) (((r) == (regno)) || ((r) >= FIRST_PSEUDO_REGISTER && !(strict)))

/* Return the appropriate mode for a named address address.  */

#undef  TARGET_ADDR_SPACE_ADDRESS_MODE
#define TARGET_ADDR_SPACE_ADDRESS_MODE rl78_addr_space_address_mode

static enum machine_mode
rl78_addr_space_address_mode (addr_space_t addrspace)
{
  switch (addrspace)
    {
    case ADDR_SPACE_GENERIC:
      return HImode;
    case ADDR_SPACE_NEAR:
      return HImode;
    case ADDR_SPACE_FAR:
      return SImode;
    default:
      gcc_unreachable ();
    }
}

/* Used in various constraints and predicates to match operands in the
   "far" address space.  */
int
rl78_far_p (rtx x)
{
  if (! MEM_P (x))
    return 0;
#if DEBUG0
  fprintf (stderr, "\033[35mrl78_far_p: "); debug_rtx (x);
  fprintf (stderr, " = %d\033[0m\n", MEM_ADDR_SPACE (x) == ADDR_SPACE_FAR);
#endif

  /* Not all far addresses are legitimate, because the devirtualizer
     can't handle them.  */
  if (! rl78_as_legitimate_address (GET_MODE (x), XEXP (x, 0), false, ADDR_SPACE_FAR))
    return 0;

  return GET_MODE_BITSIZE (rl78_addr_space_address_mode (MEM_ADDR_SPACE (x))) == 32;
}

/* Return the appropriate mode for a named address pointer.  */
#undef  TARGET_ADDR_SPACE_POINTER_MODE
#define TARGET_ADDR_SPACE_POINTER_MODE rl78_addr_space_pointer_mode

static machine_mode
rl78_addr_space_pointer_mode (addr_space_t addrspace)
{
  switch (addrspace)
    {
    case ADDR_SPACE_GENERIC:
      return HImode;
    case ADDR_SPACE_NEAR:
      return HImode;
    case ADDR_SPACE_FAR:
      return SImode;
    default:
      gcc_unreachable ();
    }
}

/* Returns TRUE for valid addresses.  */
#undef  TARGET_VALID_POINTER_MODE
#define TARGET_VALID_POINTER_MODE rl78_valid_pointer_mode

static bool
rl78_valid_pointer_mode (machine_mode m)
{
  return (m == HImode || m == SImode);
}

#undef  TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P		rl78_is_legitimate_constant

static bool
rl78_is_legitimate_constant (machine_mode mode ATTRIBUTE_UNUSED, rtx x ATTRIBUTE_UNUSED)
{
  return true;
}

#undef TARGET_LRA_P
#define TARGET_LRA_P hook_bool_void_false

#undef  TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P
#define TARGET_ADDR_SPACE_LEGITIMATE_ADDRESS_P	rl78_as_legitimate_address

bool
rl78_as_legitimate_address (machine_mode mode ATTRIBUTE_UNUSED, rtx x,
			    bool strict ATTRIBUTE_UNUSED, addr_space_t as ATTRIBUTE_UNUSED)
{
  rtx base, index, addend;
  bool is_far_addr = false;
  int as_bits;

  as_bits = GET_MODE_BITSIZE (rl78_addr_space_address_mode (as));

  if (GET_CODE (x) == UNSPEC
      && XINT (x, 1) == UNS_ES_ADDR)
    {
      x = XVECEXP (x, 0, 1);
      is_far_addr = true;
    }

  if (as_bits == 16 && is_far_addr)
    return false;

  if (! characterize_address (x, &base, &index, &addend))
    return false;

  /* We can't extract the high/low portions of a PLUS address
     involving a register during devirtualization, so make sure all
     such __far addresses do not have addends.  This forces GCC to do
     the sum separately.  */
  if (addend && base && as_bits == 32 && GET_MODE (base) == SImode)
    return false;

  if (base && index)
    {
      int ir = REGNO (index);
      int br = REGNO (base);

#define OK(test, debug) if (test) { /*fprintf(stderr, "%d: OK %s\n", __LINE__, debug);*/ return true; }
      OK (REG_IS (br, HL_REG) && REG_IS (ir, B_REG), "[hl+b]");
      OK (REG_IS (br, HL_REG) && REG_IS (ir, C_REG), "[hl+c]");
      return false;
    }

  if (strict && base && GET_CODE (base) == REG && REGNO (base) >= FIRST_PSEUDO_REGISTER)
    return false;

  if (! cfun->machine->virt_insns_ok && base && GET_CODE (base) == REG
      && REGNO (base) >= 8 && REGNO (base) <= 31)
    return false;

  return true;
}

/* Determine if one named address space is a subset of another.  */
#undef  TARGET_ADDR_SPACE_SUBSET_P
#define TARGET_ADDR_SPACE_SUBSET_P rl78_addr_space_subset_p

static bool
rl78_addr_space_subset_p (addr_space_t subset, addr_space_t superset)
{
  int subset_bits;
  int superset_bits;

  subset_bits = GET_MODE_BITSIZE (rl78_addr_space_address_mode (subset));
  superset_bits = GET_MODE_BITSIZE (rl78_addr_space_address_mode (superset));

  return (subset_bits <= superset_bits);
}

#undef  TARGET_ADDR_SPACE_CONVERT
#define TARGET_ADDR_SPACE_CONVERT rl78_addr_space_convert

/* Convert from one address space to another.  */
static rtx
rl78_addr_space_convert (rtx op, tree from_type, tree to_type)
{
  addr_space_t from_as = TYPE_ADDR_SPACE (TREE_TYPE (from_type));
  addr_space_t to_as = TYPE_ADDR_SPACE (TREE_TYPE (to_type));
  rtx result;
  int to_bits;
  int from_bits;

  to_bits = GET_MODE_BITSIZE (rl78_addr_space_address_mode (to_as));
  from_bits = GET_MODE_BITSIZE (rl78_addr_space_address_mode (from_as));

  if (to_bits < from_bits)
    {
      rtx tmp;
      /* This is unpredictable, as we're truncating off usable address
	 bits.  */

      warning (OPT_Waddress, "converting far pointer to near pointer");
      result = gen_reg_rtx (HImode);
      if (GET_CODE (op) == SYMBOL_REF
	  || (GET_CODE (op) == REG && REGNO (op) >= FIRST_PSEUDO_REGISTER))
	tmp = gen_rtx_raw_SUBREG (HImode, op, 0);
      else
	tmp = simplify_subreg (HImode, op, SImode, 0);
      gcc_assert (tmp != NULL_RTX);
      emit_move_insn (result, tmp);
      return result;
    }
  else if (to_bits > from_bits)
    {
      /* This always works.  */
      result = gen_reg_rtx (SImode);
      emit_move_insn (rl78_subreg (HImode, result, SImode, 0), op);
      if (TREE_CODE (from_type) == POINTER_TYPE
	  && TREE_CODE (TREE_TYPE (from_type)) == FUNCTION_TYPE)
	emit_move_insn (rl78_subreg (HImode, result, SImode, 2), const0_rtx);
      else
	emit_move_insn (rl78_subreg (HImode, result, SImode, 2), GEN_INT (0x0f));
      return result;
    }
  else
    return op;
  gcc_unreachable ();
}

/* Implements REGNO_MODE_CODE_OK_FOR_BASE_P.  */
bool
rl78_regno_mode_code_ok_for_base_p (int regno, machine_mode mode ATTRIBUTE_UNUSED,
				    addr_space_t address_space ATTRIBUTE_UNUSED,
				    int outer_code ATTRIBUTE_UNUSED, int index_code)
{
  if (regno <= SP_REG && regno >= 16)
    return true;
  if (index_code == REG)
    return (regno == HL_REG);
  if (regno == C_REG || regno == B_REG || regno == E_REG || regno == L_REG)
    return true;
  return false;
}

/* Implements MODE_CODE_BASE_REG_CLASS.  */
enum reg_class
rl78_mode_code_base_reg_class (machine_mode mode ATTRIBUTE_UNUSED,
			       addr_space_t address_space ATTRIBUTE_UNUSED,
			       int outer_code ATTRIBUTE_UNUSED,
			       int index_code ATTRIBUTE_UNUSED)
{
  return V_REGS;
}

/* Typical stack layout should looks like this after the function's prologue:

                            |    |
                              --                       ^
                            |    | \                   |
                            |    |   arguments saved   | Increasing
                            |    |   on the stack      |  addresses
    PARENT   arg pointer -> |    | /
  -------------------------- ---- -------------------
    CHILD                   |ret |   return address
                              --
                            |    | \
                            |    |   call saved
                            |    |   registers
	frame pointer ->    |    | /
                              --
                            |    | \
                            |    |   local
                            |    |   variables
                            |    | /
                              --
                            |    | \
                            |    |   outgoing          | Decreasing
                            |    |   arguments         |  addresses
   current stack pointer -> |    | /                   |
  -------------------------- ---- ------------------   V
                            |    |                 */

/* Implements INITIAL_ELIMINATION_OFFSET.  The frame layout is
   described in the machine_Function struct definition, above.  */
int
rl78_initial_elimination_offset (int from, int to)
{
  int rv = 0; /* as if arg to arg */

  rl78_compute_frame_info ();

  switch (to)
    {
    case STACK_POINTER_REGNUM:
      rv += cfun->machine->framesize_outgoing;
      rv += cfun->machine->framesize_locals;
      /* Fall through.  */
    case FRAME_POINTER_REGNUM:
      rv += cfun->machine->framesize_regs;
      rv += 4;
      break;
    default:
      gcc_unreachable ();
    }

  switch (from)
    {
    case FRAME_POINTER_REGNUM:
      rv -= 4;
      rv -= cfun->machine->framesize_regs;
    case ARG_POINTER_REGNUM:
      break;
    default:
      gcc_unreachable ();
    }

  return rv;
}

static bool
rl78_is_naked_func (void)
{
  return (lookup_attribute ("naked", DECL_ATTRIBUTES (current_function_decl)) != NULL_TREE);
}

/* Check if the block uses mul/div insns for G13 target.  */

static bool
check_mduc_usage (void)
{
  rtx_insn * insn;
  basic_block bb;

  FOR_EACH_BB_FN (bb, cfun)
    {
      FOR_BB_INSNS (bb, insn)
        {
          if (INSN_P (insn)
              && (get_attr_is_g13_muldiv_insn (insn) == IS_G13_MULDIV_INSN_YES))
	    return true;
	}
    }
  return false;
}

/* Expand the function prologue (from the prologue pattern).  */

void
rl78_expand_prologue (void)
{
  int i, fs;
  rtx sp = gen_rtx_REG (HImode, STACK_POINTER_REGNUM);
  rtx ax = gen_rtx_REG (HImode, AX_REG);
  int rb = 0;

  if (rl78_is_naked_func ())
    return;

  /* Always re-compute the frame info - the register usage may have changed.  */
  rl78_compute_frame_info ();

  if (MUST_SAVE_MDUC_REGISTERS && (!crtl->is_leaf || check_mduc_usage ()))
    cfun->machine->framesize += ARRAY_SIZE (mduc_regs) * 2;

  if (flag_stack_usage_info)
    current_function_static_stack_size = cfun->machine->framesize;

  if (is_interrupt_func (cfun->decl) && !TARGET_G10)
    for (i = 0; i < 4; i++)
      if (cfun->machine->need_to_push [i])
	{
	  /* Select Bank 0 if we are using any registers from Bank 0.   */
	  emit_insn (gen_sel_rb (GEN_INT (0)));
	  break;
	}

  for (i = 0; i < 16; i++)
    if (cfun->machine->need_to_push [i])
      {
	int reg = i * 2;

	if (TARGET_G10)
	  {
	    if (reg >= 8)
	      {
		emit_move_insn (ax, gen_rtx_REG (HImode, reg));
		reg = AX_REG;
	      }
	  }
	else
	  {
	    int need_bank = i/4;

	    if (need_bank != rb)
	      {
		emit_insn (gen_sel_rb (GEN_INT (need_bank)));
		rb = need_bank;
	      }
	  }

	F (emit_insn (gen_push (gen_rtx_REG (HImode, reg))));
      }

  if (rb != 0)
    emit_insn (gen_sel_rb (GEN_INT (0)));

  /* Save ES register inside interrupt functions if it is used.  */
  if (is_interrupt_func (cfun->decl) && cfun->machine->uses_es)
    {
      emit_insn (gen_movqi_from_es (gen_rtx_REG (QImode, A_REG)));
      F (emit_insn (gen_push (ax)));
    }

  /* Save MDUC registers inside interrupt routine.  */
  if (MUST_SAVE_MDUC_REGISTERS && (!crtl->is_leaf || check_mduc_usage ()))
    {
      for (unsigned i = 0; i < ARRAY_SIZE (mduc_regs); i++)
        {
          mduc_reg_type *reg = mduc_regs + i;
          rtx mem_mduc = gen_rtx_MEM (reg->mode, GEN_INT (reg->address));

          MEM_VOLATILE_P (mem_mduc) = 1;
          if (reg->mode == QImode)
            emit_insn (gen_movqi (gen_rtx_REG (QImode, A_REG), mem_mduc));
          else
            emit_insn (gen_movhi (gen_rtx_REG (HImode, AX_REG), mem_mduc));

          emit_insn (gen_push (gen_rtx_REG (HImode, AX_REG)));
        }
    }

  if (frame_pointer_needed)
    {
      F (emit_move_insn (ax, sp));
      F (emit_move_insn (gen_rtx_REG (HImode, FRAME_POINTER_REGNUM), ax));
    }

  fs = cfun->machine->framesize_locals + cfun->machine->framesize_outgoing;
  if (fs > 0)
    {
      /* If we need to subtract more than 254*3 then it is faster and
	 smaller to move SP into AX and perform the subtraction there.  */
      if (fs > 254 * 3)
	{
	  rtx insn;

	  emit_move_insn (ax, sp);
	  emit_insn (gen_subhi3 (ax, ax, GEN_INT (fs)));
	  insn = F (emit_move_insn (sp, ax));
	  add_reg_note (insn, REG_FRAME_RELATED_EXPR,
			gen_rtx_SET (sp, gen_rtx_PLUS (HImode, sp,
						       GEN_INT (-fs))));
	}
      else
	{
	  while (fs > 0)
	    {
	      int fs_byte = (fs > 254) ? 254 : fs;

	      F (emit_insn (gen_subhi3 (sp, sp, GEN_INT (fs_byte))));
	      fs -= fs_byte;
	    }
	}
    }
}

/* Expand the function epilogue (from the epilogue pattern).  */
void
rl78_expand_epilogue (void)
{
  int i, fs;
  rtx sp = gen_rtx_REG (HImode, STACK_POINTER_REGNUM);
  rtx ax = gen_rtx_REG (HImode, AX_REG);
  int rb = 0;

  if (rl78_is_naked_func ())
    return;

  if (frame_pointer_needed)
    {
      emit_move_insn (ax, gen_rtx_REG (HImode, FRAME_POINTER_REGNUM));
      emit_move_insn (sp, ax);
    }
  else
    {
      fs = cfun->machine->framesize_locals + cfun->machine->framesize_outgoing;
      if (fs > 254 * 3)
	{
	  emit_move_insn (ax, sp);
	  emit_insn (gen_addhi3 (ax, ax, GEN_INT (fs)));
	  emit_move_insn (sp, ax);
	}
      else
	{
	  while (fs > 0)
	    {
	      int fs_byte = (fs > 254) ? 254 : fs;

	      emit_insn (gen_addhi3 (sp, sp, GEN_INT (fs_byte)));
	      fs -= fs_byte;
	    }
	}
    }

  /* Restore MDUC registers from interrupt routine.  */
  if (MUST_SAVE_MDUC_REGISTERS && (!crtl->is_leaf || check_mduc_usage ()))
    {
      for (int i = ARRAY_SIZE (mduc_regs) - 1; i >= 0; i--)
        {
          mduc_reg_type *reg = mduc_regs + i;
          rtx mem_mduc = gen_rtx_MEM (reg->mode, GEN_INT (reg->address));

          emit_insn (gen_pop (gen_rtx_REG (HImode, AX_REG)));
          MEM_VOLATILE_P (mem_mduc) = 1;
          if (reg->mode == QImode)
            emit_insn (gen_movqi (mem_mduc, gen_rtx_REG (QImode, A_REG)));
          else
            emit_insn (gen_movhi (mem_mduc, gen_rtx_REG (HImode, AX_REG)));
        }
    }

  if (is_interrupt_func (cfun->decl) && cfun->machine->uses_es)
    {
      emit_insn (gen_pop (gen_rtx_REG (HImode, AX_REG)));
      emit_insn (gen_movqi_to_es (gen_rtx_REG (QImode, A_REG)));
    }

  for (i = 15; i >= 0; i--)
    if (cfun->machine->need_to_push [i])
      {
	rtx dest = gen_rtx_REG (HImode, i * 2);

	if (TARGET_G10)
	  {
	    if (i < 8)
	      emit_insn (gen_pop (dest));
	    else
	      {
		emit_insn (gen_pop (ax));
		emit_move_insn (dest, ax);
		/* Generate a USE of the pop'd register so that DCE will not eliminate the move.  */
		emit_insn (gen_use (dest));
	      }
	  }
	else
	  {
	    int need_bank = i / 4;

	    if (need_bank != rb)
	      {
		emit_insn (gen_sel_rb (GEN_INT (need_bank)));
		rb = need_bank;
	      }
	    emit_insn (gen_pop (dest));
	  }
      }

  if (rb != 0)
    emit_insn (gen_sel_rb (GEN_INT (0)));

  if (cfun->machine->trampolines_used)
    emit_insn (gen_trampoline_uninit ());

  if (is_brk_interrupt_func (cfun->decl))
    emit_jump_insn (gen_brk_interrupt_return ());
  else if (is_interrupt_func (cfun->decl))
    emit_jump_insn (gen_interrupt_return ());
  else
    emit_jump_insn (gen_rl78_return ());
}

/* Likewise, for exception handlers.  */
void
rl78_expand_eh_epilogue (rtx x ATTRIBUTE_UNUSED)
{
  /* FIXME - replace this with an indirect jump with stack adjust.  */
  emit_jump_insn (gen_rl78_return ());
}

#undef  TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE	rl78_start_function

/* We don't use this to actually emit the function prologue.  We use
   this to insert a comment in the asm file describing the
   function.  */
static void
rl78_start_function (FILE *file, HOST_WIDE_INT hwi_local ATTRIBUTE_UNUSED)
{
  int i;

  if (cfun->machine->framesize == 0)
    return;
  fprintf (file, "\t; start of function\n");

  if (cfun->machine->framesize_regs)
    {
      fprintf (file, "\t; push %d:", cfun->machine->framesize_regs);
      for (i = 0; i < 16; i ++)
	if (cfun->machine->need_to_push[i])
	  fprintf (file, " %s", word_regnames[i*2]);
      fprintf (file, "\n");
    }

  if (frame_pointer_needed)
    fprintf (file, "\t; $fp points here (r22)\n");

  if (cfun->machine->framesize_locals)
    fprintf (file, "\t; locals: %d byte%s\n", cfun->machine->framesize_locals,
	     cfun->machine->framesize_locals == 1 ? "" : "s");

  if (cfun->machine->framesize_outgoing)
    fprintf (file, "\t; outgoing: %d byte%s\n", cfun->machine->framesize_outgoing,
	     cfun->machine->framesize_outgoing == 1 ? "" : "s");

  if (cfun->machine->uses_es)
    fprintf (file, "\t; uses ES register\n");

  if (MUST_SAVE_MDUC_REGISTERS)
    fprintf (file, "\t; preserves MDUC registers\n");
}

/* Return an RTL describing where a function return value of type RET_TYPE
   is held.  */

#undef  TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE		rl78_function_value

static rtx
rl78_function_value (const_tree ret_type,
		     const_tree fn_decl_or_type ATTRIBUTE_UNUSED,
		     bool       outgoing ATTRIBUTE_UNUSED)
{
  machine_mode mode = TYPE_MODE (ret_type);

  return gen_rtx_REG (mode, 8);
}

#undef  TARGET_PROMOTE_FUNCTION_MODE
#define TARGET_PROMOTE_FUNCTION_MODE rl78_promote_function_mode

static machine_mode
rl78_promote_function_mode (const_tree type ATTRIBUTE_UNUSED,
			    machine_mode mode,
			    int *punsignedp ATTRIBUTE_UNUSED,
			    const_tree funtype ATTRIBUTE_UNUSED, int for_return ATTRIBUTE_UNUSED)
{
  return mode;
}

/* Return an RTL expression describing the register holding a function
   parameter of mode MODE and type TYPE or NULL_RTX if the parameter should
   be passed on the stack.  CUM describes the previous parameters to the
   function and NAMED is false if the parameter is part of a variable
   parameter list, or the last named parameter before the start of a
   variable parameter list.  */

#undef  TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG     	rl78_function_arg

static rtx
rl78_function_arg (cumulative_args_t cum_v ATTRIBUTE_UNUSED,
		   machine_mode mode ATTRIBUTE_UNUSED,
		   const_tree type ATTRIBUTE_UNUSED,
		   bool named ATTRIBUTE_UNUSED)
{
  return NULL_RTX;
}

#undef  TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE     rl78_function_arg_advance

static void
rl78_function_arg_advance (cumulative_args_t cum_v, machine_mode mode, const_tree type,
			   bool named ATTRIBUTE_UNUSED)
{
  int rounded_size;
  CUMULATIVE_ARGS * cum = get_cumulative_args (cum_v);

  rounded_size = ((mode == BLKmode)
		  ? int_size_in_bytes (type) : GET_MODE_SIZE (mode));
  if (rounded_size & 1)
    rounded_size ++;
  (*cum) += rounded_size;
}

#undef  TARGET_FUNCTION_ARG_BOUNDARY
#define	TARGET_FUNCTION_ARG_BOUNDARY rl78_function_arg_boundary

static unsigned int
rl78_function_arg_boundary (machine_mode mode ATTRIBUTE_UNUSED,
			    const_tree type ATTRIBUTE_UNUSED)
{
  return 16;
}

/* Supported modifier letters:

   A - address of a MEM
   S - SADDR form of a real register
   v - real register corresponding to a virtual register
   m - minus - negative of CONST_INT value.
   C - inverse of a conditional (NE vs EQ for example)
   C - complement of an integer
   z - collapsed conditional
   s - shift count mod 8
   S - shift count mod 16
   r - reverse shift count (8-(count mod 8))
   B - bit position

   h - bottom HI of an SI
   H - top HI of an SI
   q - bottom QI of an HI
   Q - top QI of an HI
   e - third QI of an SI (i.e. where the ES register gets values from)
   E - fourth QI of an SI (i.e. MSB)

   p - Add +0 to a zero-indexed HL based address.
*/

/* Implements the bulk of rl78_print_operand, below.  We do it this
   way because we need to test for a constant at the top level and
   insert the '#', but not test for it anywhere else as we recurse
   down into the operand.  */
static void
rl78_print_operand_1 (FILE * file, rtx op, int letter)
{
  int need_paren;

  switch (GET_CODE (op))
    {
    case MEM:
      if (letter == 'A')
	rl78_print_operand_1 (file, XEXP (op, 0), letter);
      else
	{
	  if (rl78_far_p (op))
	    {
	      fprintf (file, "es:");
	      if (GET_CODE (XEXP (op, 0)) == UNSPEC)
		op = gen_rtx_MEM (GET_MODE (op), XVECEXP (XEXP (op, 0), 0, 1));
	    }
	  if (letter == 'H')
	    {
	      op = adjust_address (op, HImode, 2);
	      letter = 0;
	    }
	  if (letter == 'h')
	    {
	      op = adjust_address (op, HImode, 0);
	      letter = 0;
	    }
	  if (letter == 'Q')
	    {
	      op = adjust_address (op, QImode, 1);
	      letter = 0;
	    }
	  if (letter == 'q')
	    {
	      op = adjust_address (op, QImode, 0);
	      letter = 0;
	    }
	  if (letter == 'e')
	    {
	      op = adjust_address (op, QImode, 2);
	      letter = 0;
	    }
	  if (letter == 'E')
	    {
	      op = adjust_address (op, QImode, 3);
	      letter = 0;
	    }
	  if (CONSTANT_P (XEXP (op, 0)))
	    {
	      if (!rl78_saddr_p (op))
		fprintf (file, "!");
	      rl78_print_operand_1 (file, XEXP (op, 0), letter);
	    }
	  else if (GET_CODE (XEXP (op, 0)) == PLUS
		   && GET_CODE (XEXP (XEXP (op, 0), 0)) == SYMBOL_REF)
	    {
	      if (!rl78_saddr_p (op))
		fprintf (file, "!");
	      rl78_print_operand_1 (file, XEXP (op, 0), letter);
	    }
	  else if (GET_CODE (XEXP (op, 0)) == PLUS
		   && GET_CODE (XEXP (XEXP (op, 0), 0)) == REG
		   && REGNO (XEXP (XEXP (op, 0), 0)) == 2)
	    {
	      rl78_print_operand_1 (file, XEXP (XEXP (op, 0), 1), 'u');
	      fprintf (file, "[");
	      rl78_print_operand_1 (file, XEXP (XEXP (op, 0), 0), 0);
	      if (letter == 'p' && GET_CODE (XEXP (op, 0)) == REG)
		fprintf (file, "+0");
	      fprintf (file, "]");
	    }
	  else
	    {
	      op = XEXP (op, 0);
	      fprintf (file, "[");
	      rl78_print_operand_1 (file, op, letter);
	      if (letter == 'p' && REG_P (op) && REGNO (op) == 6)
		fprintf (file, "+0");
	      fprintf (file, "]");
	    }
	}
      break;

    case REG:
      if (letter == 'Q')
	fprintf (file, "%s", reg_names [REGNO (op) | 1]);
      else if (letter == 'H')
	fprintf (file, "%s", reg_names [REGNO (op) + 2]);
      else if (letter == 'q')
	fprintf (file, "%s", reg_names [REGNO (op) & ~1]);
      else if (letter == 'e')
	fprintf (file, "%s", reg_names [REGNO (op) + 2]);
      else if (letter == 'E')
	fprintf (file, "%s", reg_names [REGNO (op) + 3]);
      else if (letter == 'S')
	fprintf (file, "0x%x", 0xffef8 + REGNO (op));
      else if (GET_MODE (op) == HImode
	       && ! (REGNO (op) & ~0xfe))
	{
	  if (letter == 'v')
	    fprintf (file, "%s", word_regnames [REGNO (op) % 8]);
	  else
	    fprintf (file, "%s", word_regnames [REGNO (op)]);
	}
      else
	fprintf (file, "%s", reg_names [REGNO (op)]);
      break;

    case CONST_INT:
      if (letter == 'Q')
	fprintf (file, "%ld", INTVAL (op) >> 8);
      else if (letter == 'H')
	fprintf (file, "%ld", INTVAL (op) >> 16);
      else if (letter == 'q')
	fprintf (file, "%ld", INTVAL (op) & 0xff);
      else if (letter == 'h')
	fprintf (file, "%ld", INTVAL (op) & 0xffff);
      else if (letter == 'e')
	fprintf (file, "%ld", (INTVAL (op) >> 16) & 0xff);
      else if (letter == 'B')
	{
	  int ival = INTVAL (op);
	  if (ival == -128)
	    ival = 0x80;
	  if (exact_log2 (ival) >= 0)
	    fprintf (file, "%d", exact_log2 (ival));
	  else
	    fprintf (file, "%d", exact_log2 (~ival & 0xff));
	}
      else if (letter == 'E')
	fprintf (file, "%ld", (INTVAL (op) >> 24) & 0xff);
      else if (letter == 'm')
	fprintf (file, "%ld", - INTVAL (op));
      else if (letter == 's')
	fprintf (file, "%ld", INTVAL (op) % 8);
      else if (letter == 'S')
	fprintf (file, "%ld", INTVAL (op) % 16);
      else if (letter == 'r')
	fprintf (file, "%ld", 8 - (INTVAL (op) % 8));
      else if (letter == 'C')
	fprintf (file, "%ld", (INTVAL (op) ^ 0x8000) & 0xffff);
      else
	fprintf (file, "%ld", INTVAL (op));
      break;

    case CONST:
      rl78_print_operand_1 (file, XEXP (op, 0), letter);
      break;

    case ZERO_EXTRACT:
      {
	int bits = INTVAL (XEXP (op, 1));
	int ofs = INTVAL (XEXP (op, 2));
	if (bits == 16 && ofs == 0)
	  fprintf (file, "%%lo16(");
	else if (bits == 16 && ofs == 16)
	  fprintf (file, "%%hi16(");
	else if (bits == 8 && ofs == 16)
	  fprintf (file, "%%hi8(");
	else
	  gcc_unreachable ();
	rl78_print_operand_1 (file, XEXP (op, 0), 0);
	fprintf (file, ")");
      }
      break;

    case ZERO_EXTEND:
      if (GET_CODE (XEXP (op, 0)) == REG)
	fprintf (file, "%s", reg_names [REGNO (XEXP (op, 0))]);
      else
	print_rtl (file, op);
      break;

    case PLUS:
      need_paren = 0;
      if (letter == 'H')
	{
	  fprintf (file, "%%hi16(");
	  need_paren = 1;
	  letter = 0;
	}
      if (letter == 'h')
	{
	  fprintf (file, "%%lo16(");
	  need_paren = 1;
	  letter = 0;
	}
      if (letter == 'e')
	{
	  fprintf (file, "%%hi8(");
	  need_paren = 1;
	  letter = 0;
	}
      if (letter == 'q' || letter == 'Q')
	output_operand_lossage ("q/Q modifiers invalid for symbol references");

      if (GET_CODE (XEXP (op, 0)) == ZERO_EXTEND)
	{
	  if (GET_CODE (XEXP (op, 1)) == SYMBOL_REF
	      && SYMBOL_REF_DECL (XEXP (op, 1))
	      && TREE_CODE (SYMBOL_REF_DECL (XEXP (op, 1))) == FUNCTION_DECL)
	    {
	      fprintf (file, "%%code(");
	      assemble_name (file, rl78_strip_nonasm_name_encoding (XSTR (XEXP (op, 1), 0)));
	      fprintf (file, "+");
	      rl78_print_operand_1 (file, XEXP (op, 0), letter);
	      fprintf (file, ")");
	    }
	  else
	    {
	      rl78_print_operand_1 (file, XEXP (op, 1), letter);
	      fprintf (file, "+");
	      rl78_print_operand_1 (file, XEXP (op, 0), letter);
	    }
	}
      else
	{
	  if (GET_CODE (XEXP (op, 0)) == SYMBOL_REF
	      && SYMBOL_REF_DECL (XEXP (op, 0))
	      && TREE_CODE (SYMBOL_REF_DECL (XEXP (op, 0))) == FUNCTION_DECL)
	    {
	      fprintf (file, "%%code(");
	      assemble_name (file, rl78_strip_nonasm_name_encoding (XSTR (XEXP (op, 0), 0)));
	      fprintf (file, "+");
	      rl78_print_operand_1 (file, XEXP (op, 1), letter);
	      fprintf (file, ")");
	    }
	  else
	    {
	      rl78_print_operand_1 (file, XEXP (op, 0), letter);
	      fprintf (file, "+");
	      rl78_print_operand_1 (file, XEXP (op, 1), letter);
	    }
	}
      if (need_paren)
	fprintf (file, ")");
      break;

    case SUBREG:
      if (GET_MODE (op) == HImode
	  && SUBREG_BYTE (op) == 0)
	{
	  fprintf (file, "%%lo16(");
	  rl78_print_operand_1 (file, SUBREG_REG (op), 0);
	  fprintf (file, ")");
	}
      else if (GET_MODE (op) == HImode
	       && SUBREG_BYTE (op) == 2)
	{
	  fprintf (file, "%%hi16(");
	  rl78_print_operand_1 (file, SUBREG_REG (op), 0);
	  fprintf (file, ")");
	}
      else
	{
	  fprintf (file, "(%s)", GET_RTX_NAME (GET_CODE (op)));
	}
      break;

    case SYMBOL_REF:
      need_paren = 0;
      if (letter == 'H')
	{
	  fprintf (file, "%%hi16(");
	  need_paren = 1;
	  letter = 0;
	}
      if (letter == 'h')
	{
	  fprintf (file, "%%lo16(");
	  need_paren = 1;
	  letter = 0;
	}
      if (letter == 'e')
	{
	  fprintf (file, "%%hi8(");
	  need_paren = 1;
	  letter = 0;
	}
      if (letter == 'q' || letter == 'Q')
	output_operand_lossage ("q/Q modifiers invalid for symbol references");

      if (SYMBOL_REF_DECL (op) && TREE_CODE (SYMBOL_REF_DECL (op)) == FUNCTION_DECL)
	{
	  fprintf (file, "%%code(");
	  assemble_name (file, rl78_strip_nonasm_name_encoding (XSTR (op, 0)));
	  fprintf (file, ")");
	}
      else
        assemble_name (file, rl78_strip_nonasm_name_encoding (XSTR (op, 0)));
      if (need_paren)
	fprintf (file, ")");
      break;

    case CODE_LABEL:
    case LABEL_REF:
      output_asm_label (op);
      break;

    case LTU:
      if (letter == 'z')
	fprintf (file, "#comparison eliminated");
      else
	fprintf (file, letter == 'C' ? "nc" : "c");
      break;
    case LEU:
      if (letter == 'z')
	fprintf (file, "br");
      else
	fprintf (file, letter == 'C' ? "h" : "nh");
      break;
    case GEU:
      if (letter == 'z')
	fprintf (file, "br");
      else
	fprintf (file, letter == 'C' ? "c" : "nc");
      break;
    case GTU:
      if (letter == 'z')
	fprintf (file, "#comparison eliminated");
      else
	fprintf (file, letter == 'C' ? "nh" : "h");
      break;
    case EQ:
      if (letter == 'z')
	fprintf (file, "br");
      else
	fprintf (file, letter == 'C' ? "nz" : "z");
      break;
    case NE:
      if (letter == 'z')
	fprintf (file, "#comparison eliminated");
      else
	fprintf (file, letter == 'C' ? "z" : "nz");
      break;

    /* Note: these assume appropriate adjustments were made so that
       unsigned comparisons, which is all this chip has, will
       work.  */
    case LT:
      if (letter == 'z')
	fprintf (file, "#comparison eliminated");
      else
	fprintf (file, letter == 'C' ? "nc" : "c");
      break;
    case LE:
      if (letter == 'z')
	fprintf (file, "br");
      else
        fprintf (file, letter == 'C' ? "h" : "nh");
      break;
    case GE:
      if (letter == 'z')
	fprintf (file, "br");
      else
	fprintf (file, letter == 'C' ? "c" : "nc");
      break;
    case GT:
      if (letter == 'z')
	fprintf (file, "#comparison eliminated");
      else
	fprintf (file, letter == 'C' ? "nh" : "h");
      break;

    default:
      fprintf (file, "(%s)", GET_RTX_NAME (GET_CODE (op)));
      break;
    }
}

#undef  TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND		rl78_print_operand

static void
rl78_print_operand (FILE * file, rtx op, int letter)
{
  if (CONSTANT_P (op) && letter != 'u' && letter != 's' && letter != 'r' && letter != 'S' && letter != 'B')
    fprintf (file, "#");
  rl78_print_operand_1 (file, op, letter);
}

#undef  TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT rl78_trampoline_init

/* Note that the RL78's addressing makes it very difficult to do
   trampolines on the stack.  So, libgcc has a small pool of
   trampolines from which one is allocated to this task.  */
static void
rl78_trampoline_init (rtx m_tramp, tree fndecl, rtx static_chain)
{
  rtx mov_addr, thunk_addr;
  rtx function = XEXP (DECL_RTL (fndecl), 0);

  mov_addr = adjust_address (m_tramp, HImode, 0);
  thunk_addr = gen_reg_rtx (HImode);

  function = force_reg (HImode, function);
  static_chain = force_reg (HImode, static_chain);

  emit_insn (gen_trampoline_init (thunk_addr, function, static_chain));
  emit_move_insn (mov_addr, thunk_addr);

  cfun->machine->trampolines_used = 1;
}

#undef  TARGET_TRAMPOLINE_ADJUST_ADDRESS
#define TARGET_TRAMPOLINE_ADJUST_ADDRESS rl78_trampoline_adjust_address

static rtx
rl78_trampoline_adjust_address (rtx m_tramp)
{
  rtx x = gen_rtx_MEM (HImode, m_tramp);
  return x;
}

/* Expander for cbranchqi4 and cbranchhi4.  RL78 is missing some of
   the "normal" compares, specifically, it only has unsigned compares,
   so we must synthesize the missing ones.  */
void
rl78_expand_compare (rtx *operands)
{
  if (GET_CODE (operands[2]) == MEM)
    operands[2] = copy_to_mode_reg (GET_MODE (operands[2]), operands[2]);
}



/* Define this to 1 if you are debugging the peephole optimizers.  */
#define DEBUG_PEEP 0

/* Predicate used to enable the peephole2 patterns in rl78-virt.md.
   The default "word" size is a byte so we can effectively use all the
   registers, but we want to do 16-bit moves whenever possible.  This
   function determines when such a move is an option.  */
bool
rl78_peep_movhi_p (rtx *operands)
{
  int i;
  rtx m, a;

  /* (set (op0) (op1))
     (set (op2) (op3)) */

  if (! rl78_virt_insns_ok ())
    return false;

#if DEBUG_PEEP
  fprintf (stderr, "\033[33m");
  debug_rtx (operands[0]);
  debug_rtx (operands[1]);
  debug_rtx (operands[2]);
  debug_rtx (operands[3]);
  fprintf (stderr, "\033[0m");
#endif

  /* You can move a constant to memory as QImode, but not HImode.  */
  if (GET_CODE (operands[0]) == MEM
      && GET_CODE (operands[1]) != REG)
    {
#if DEBUG_PEEP
      fprintf (stderr, "no peep: move constant to memory\n");
#endif
      return false;
    }

  if (rtx_equal_p (operands[0], operands[3]))
    {
#if DEBUG_PEEP
      fprintf (stderr, "no peep: overlapping\n");
#endif
      return false;
    }

  for (i = 0; i < 2; i ++)
    {
      if (GET_CODE (operands[i]) != GET_CODE (operands[i+2]))
	{
#if DEBUG_PEEP
	  fprintf (stderr, "no peep: different codes\n");
#endif
	  return false;
	}
      if (GET_MODE (operands[i]) != GET_MODE (operands[i+2]))
	{
#if DEBUG_PEEP
	  fprintf (stderr, "no peep: different modes\n");
#endif
	  return false;
	}

      switch (GET_CODE (operands[i]))
	{
	case REG:
	  /*   LSB                      MSB  */
	  if (REGNO (operands[i]) + 1 != REGNO (operands[i+2])
	      || GET_MODE (operands[i]) != QImode)
	    {
#if DEBUG_PEEP
	      fprintf (stderr, "no peep: wrong regnos %d %d %d\n",
		       REGNO (operands[i]), REGNO (operands[i+2]),
		       i);
#endif
	      return false;
	    }
	  if (! rl78_hard_regno_mode_ok (REGNO (operands[i]), HImode))
	    {
#if DEBUG_PEEP
	      fprintf (stderr, "no peep: reg %d not HI\n", REGNO (operands[i]));
#endif
	      return false;
	    }
	  break;

	case CONST_INT:
	  break;

	case MEM:
	  if (GET_MODE (operands[i]) != QImode)
	    return false;
	  if (MEM_ALIGN (operands[i]) < 16)
	    return false;
	  a = XEXP (operands[i], 0);
	  if (GET_CODE (a) == CONST)
	    a = XEXP (a, 0);
	  if (GET_CODE (a) == PLUS)
	    a = XEXP (a, 1);
	  if (GET_CODE (a) == CONST_INT
	      && INTVAL (a) & 1)
	    {
#if DEBUG_PEEP
	      fprintf (stderr, "no peep: misaligned mem %d\n", i);
	      debug_rtx (operands[i]);
#endif
	      return false;
	    }
	  m = adjust_address (operands[i], QImode, 1);
	  if (! rtx_equal_p (m, operands[i+2]))
	    {
#if DEBUG_PEEP
	      fprintf (stderr, "no peep: wrong mem %d\n", i);
	      debug_rtx (m);
	      debug_rtx (operands[i+2]);
#endif
	      return false;
	    }
	  break;

	default:
#if DEBUG_PEEP
	  fprintf (stderr, "no peep: wrong rtx %d\n", i);
#endif
	  return false;
	}
    }
#if DEBUG_PEEP
  fprintf (stderr, "\033[32mpeep!\033[0m\n");
#endif
  return true;
}

/* Likewise, when a peephole is activated, this function helps compute
   the new operands.  */
void
rl78_setup_peep_movhi (rtx *operands)
{
  int i;

  for (i = 0; i < 2; i ++)
    {
      switch (GET_CODE (operands[i]))
	{
	case REG:
	  operands[i+4] = gen_rtx_REG (HImode, REGNO (operands[i]));
	  break;

	case CONST_INT:
	  operands[i+4] = GEN_INT ((INTVAL (operands[i]) & 0xff) + ((char) INTVAL (operands[i+2])) * 256);
	  break;

	case MEM:
	  operands[i+4] = adjust_address (operands[i], HImode, 0);
	  break;

	default:
	  break;
	}
    }
}

/*
	How Devirtualization works in the RL78 GCC port

Background

The RL78 is an 8-bit port with some 16-bit operations.  It has 32
bytes of register space, in four banks, memory-mapped.  One bank is
the "selected" bank and holds the registers used for primary
operations.  Since the registers are memory mapped, often you can
still refer to the unselected banks via memory accesses.

Virtual Registers

The GCC port uses bank 0 as the "selected" registers (A, X, BC, etc)
and refers to the other banks via their memory addresses, although
they're treated as regular registers internally.  These "virtual"
registers are R8 through R23 (bank3 is reserved for asm-based
interrupt handlers).

There are four machine description files:

rl78.md        - common register-independent patterns and definitions
rl78-expand.md - expanders
rl78-virt.md   - patterns that match BEFORE devirtualization
rl78-real.md   - patterns that match AFTER devirtualization

At least through register allocation and reload, gcc is told that it
can do pretty much anything - but may only use the virtual registers.
GCC cannot properly create the varying addressing modes that the RL78
supports in an efficient way.

Sometime after reload, the RL78 backend "devirtualizes" the RTL.  It
uses the "valloc" attribute in rl78-virt.md for determining the rules
by which it will replace virtual registers with real registers (or
not) and how to make up addressing modes.  For example, insns tagged
with "ro1" have a single read-only parameter, which may need to be
moved from memory/constant/vreg to a suitable real register.  As part
of devirtualization, a flag is toggled, disabling the rl78-virt.md
patterns and enabling the rl78-real.md patterns.  The new patterns'
constraints are used to determine the real registers used.  NOTE:
patterns in rl78-virt.md essentially ignore the constrains and rely on
predicates, where the rl78-real.md ones essentially ignore the
predicates and rely on the constraints.

The devirtualization pass is scheduled via the pass manager (despite
being called "rl78_reorg") so it can be scheduled prior to var-track
(the idea is to let gdb know about the new registers).  Ideally, it
would be scheduled right after pro/epilogue generation, so the
post-reload optimizers could operate on the real registers, but when I
tried that there were some issues building the target libraries.

During devirtualization, a simple register move optimizer is run.  It
would be better to run a full CSE/propogation pass on it though, but
that has not yet been attempted.

 */
#define DEBUG_ALLOC 0

#define OP(x) (*recog_data.operand_loc[x])

/* This array is used to hold knowledge about the contents of the
   real registers (A ... H), the memory-based registers (r8 ... r31)
   and the first NUM_STACK_LOCS words on the stack.  We use this to
   avoid generating redundant move instructions.

   A value in the range 0 .. 31 indicates register A .. r31.
   A value in the range 32 .. 63 indicates stack slot (value - 32).
   A value of NOT_KNOWN indicates that the contents of that location
   are not known.  */

#define NUM_STACK_LOCS	32
#define NOT_KNOWN       127

static unsigned char content_memory [32 + NUM_STACK_LOCS];

static unsigned char saved_update_index = NOT_KNOWN;
static unsigned char saved_update_value;
static machine_mode saved_update_mode;


static inline void
clear_content_memory (void)
{
  memset (content_memory, NOT_KNOWN, sizeof content_memory);
  if (dump_file)
    fprintf (dump_file, "  clear content memory\n");
  saved_update_index = NOT_KNOWN;
}

/* Convert LOC into an index into the content_memory array.
   If LOC cannot be converted, return NOT_KNOWN.  */

static unsigned char
get_content_index (rtx loc)
{
  machine_mode mode;

  if (loc == NULL_RTX)
    return NOT_KNOWN;

  if (REG_P (loc))
    {
      if (REGNO (loc) < 32)
	return REGNO (loc);
      return NOT_KNOWN;
    }

  mode = GET_MODE (loc);

  if (! rl78_stack_based_mem (loc, mode))
    return NOT_KNOWN;

  loc = XEXP (loc, 0);

  if (REG_P (loc))
    /* loc = MEM (SP) */
    return 32;

  /* loc = MEM (PLUS (SP, INT)).  */
  loc = XEXP (loc, 1);

  if (INTVAL (loc) < NUM_STACK_LOCS)
    return 32 + INTVAL (loc);

  return NOT_KNOWN;
}

/* Return a string describing content INDEX in mode MODE.
   WARNING: Can return a pointer to a static buffer.  */
static const char *
get_content_name (unsigned char index, machine_mode mode)
{
  static char buffer [128];

  if (index == NOT_KNOWN)
    return "Unknown";

  if (index > 31)
    sprintf (buffer, "stack slot %d", index - 32);
  else if (mode == HImode)
    sprintf (buffer, "%s%s",
	     reg_names [index + 1], reg_names [index]);
  else
    return reg_names [index];

  return buffer;
}

#if DEBUG_ALLOC

static void
display_content_memory (FILE * file)
{
  unsigned int i;

  fprintf (file, " Known memory contents:\n");

  for (i = 0; i < sizeof content_memory; i++)
    if (content_memory[i] != NOT_KNOWN)
      {
	fprintf (file, "   %s contains a copy of ", get_content_name (i, QImode));
	fprintf (file, "%s\n", get_content_name (content_memory [i], QImode));
      }
}
#endif

static void
update_content (unsigned char index, unsigned char val, machine_mode mode)
{
  unsigned int i;

  gcc_assert (index < sizeof content_memory);

  content_memory [index] = val;
  if (val != NOT_KNOWN)
    content_memory [val] = index;

  /* Make the entry in dump_file *before* VAL is increased below.  */
  if (dump_file)
    {
      fprintf (dump_file, "  %s now contains ", get_content_name (index, mode));
      if (val == NOT_KNOWN)
	fprintf (dump_file, "Unknown\n");
      else
	fprintf (dump_file, "%s and vice versa\n", get_content_name (val, mode));
    }

  if (mode == HImode)
    {
      val = val == NOT_KNOWN ? val : val + 1;

      content_memory [index + 1] = val;
      if (val != NOT_KNOWN)
	{
	  content_memory [val] = index + 1;
	  -- val;
	}
    }

  /* Any other places that had INDEX recorded as their contents are now invalid.  */
  for (i = 0; i < sizeof content_memory; i++)
    {
      if (i == index
	  || (val != NOT_KNOWN && i == val))
	{
	  if (mode == HImode)
	    ++ i;
	  continue;
	}

      if (content_memory[i] == index
	  || (val != NOT_KNOWN && content_memory[i] == val))
	{
	  content_memory[i] = NOT_KNOWN;

	  if (dump_file)
	    fprintf (dump_file, "  %s cleared\n", get_content_name (i, mode));

	  if (mode == HImode)
	    content_memory[++ i] = NOT_KNOWN;
	}
    }
}

/* Record that LOC contains VALUE.
   For HImode locations record that LOC+1 contains VALUE+1.
   If LOC is not a register or stack slot, do nothing.
   If VALUE is not a register or stack slot, clear the recorded content.  */

static void
record_content (rtx loc, rtx value)
{
  machine_mode mode;
  unsigned char index;
  unsigned char val;

  if ((index = get_content_index (loc)) == NOT_KNOWN)
    return;

  val = get_content_index (value);

  mode = GET_MODE (loc);

  if (val == index)
    {
      if (! optimize)
	return;

      /* This should not happen when optimizing.  */
#if 1
      fprintf (stderr, "ASSIGNMENT of location to itself detected! [%s]\n",
	       get_content_name (val, mode));
      return;
#else
      gcc_unreachable ();
#endif
    }

  update_content (index, val, mode);
}

/* Returns TRUE if LOC already contains a copy of VALUE.  */

static bool
already_contains (rtx loc, rtx value)
{
  unsigned char index;
  unsigned char val;

  if ((index = get_content_index (loc)) == NOT_KNOWN)
    return false;

  if ((val = get_content_index (value)) == NOT_KNOWN)
    return false;

  if (content_memory [index] != val)
    return false;

  if (GET_MODE (loc) == HImode)
    return content_memory [index + 1] == val + 1;

  return true;
}

bool
rl78_es_addr (rtx addr)
{
  if (GET_CODE (addr) == MEM)
    addr = XEXP (addr, 0);
  if (GET_CODE (addr) != UNSPEC)
    return false;
  if (XINT (addr, 1) != UNS_ES_ADDR)
    return false;
  return true;
}

rtx
rl78_es_base (rtx addr)
{
  if (GET_CODE (addr) == MEM)
    addr = XEXP (addr, 0);
  addr = XVECEXP (addr, 0, 1);
  if (GET_CODE (addr) == CONST
      && GET_CODE (XEXP (addr, 0)) == ZERO_EXTRACT)
    addr = XEXP (XEXP (addr, 0), 0);
  /* Mode doesn't matter here.  */
  return gen_rtx_MEM (HImode, addr);
}

/* Rescans an insn to see if it's recognized again.  This is done
   carefully to ensure that all the constraint information is accurate
   for the newly matched insn.  */
static bool
insn_ok_now (rtx_insn * insn)
{
  rtx pattern = PATTERN (insn);
  int i;

  INSN_CODE (insn) = -1;

  if (recog (pattern, insn, 0) > -1)
    {
      extract_insn (insn);
      if (constrain_operands (1, get_preferred_alternatives (insn)))
	{
#if DEBUG_ALLOC
	  fprintf (stderr, "\033[32m");
	  debug_rtx (insn);
	  fprintf (stderr, "\033[0m");
#endif
	  if (SET_P (pattern))
	    record_content (SET_DEST (pattern), SET_SRC (pattern));

	  /* We need to detect far addresses that haven't been
	     converted to es/lo16 format.  */
	  for (i=0; i<recog_data.n_operands; i++)
	    if (GET_CODE (OP (i)) == MEM
		&& GET_MODE (XEXP (OP (i), 0)) == SImode
		&& GET_CODE (XEXP (OP (i), 0)) != UNSPEC)
	      return false;

	  return true;
	}
    }
  else
    {
      /* We need to re-recog the insn with virtual registers to get
	 the operands.  */
      cfun->machine->virt_insns_ok = 1;
      if (recog (pattern, insn, 0) > -1)
	{
	  extract_insn (insn);
	  if (constrain_operands (0, get_preferred_alternatives (insn)))
	    {
	      cfun->machine->virt_insns_ok = 0;
	      return false;
	    }
	}

#if DEBUG_ALLOC
      fprintf (stderr, "\033[41;30m Unrecognized *virtual* insn \033[0m\n");
      debug_rtx (insn);
#endif
      gcc_unreachable ();
    }

#if DEBUG_ALLOC
  fprintf (stderr, "\033[31m");
  debug_rtx (insn);
  fprintf (stderr, "\033[0m");
#endif
  return false;
}

#if DEBUG_ALLOC
#define WORKED      fprintf (stderr, "\033[48;5;22m Worked at line %d \033[0m\n", __LINE__)
#define FAILEDSOFAR fprintf (stderr, "\033[48;5;52m FAILED at line %d \033[0m\n", __LINE__)
#define FAILED      fprintf (stderr, "\033[48;5;52m FAILED at line %d \033[0m\n", __LINE__), gcc_unreachable ()
#define MAYBE_OK(insn) if (insn_ok_now (insn)) { WORKED; return; } else { FAILEDSOFAR; }
#define MUST_BE_OK(insn) if (insn_ok_now (insn)) { WORKED; return; } FAILED
#else
#define FAILED gcc_unreachable ()
#define MAYBE_OK(insn) if (insn_ok_now (insn)) return;
#define MUST_BE_OK(insn) if (insn_ok_now (insn)) return; FAILED
#endif

/* Registers into which we move the contents of virtual registers.  */
#define X gen_rtx_REG (QImode, X_REG)
#define A gen_rtx_REG (QImode, A_REG)
#define C gen_rtx_REG (QImode, C_REG)
#define B gen_rtx_REG (QImode, B_REG)
#define E gen_rtx_REG (QImode, E_REG)
#define D gen_rtx_REG (QImode, D_REG)
#define L gen_rtx_REG (QImode, L_REG)
#define H gen_rtx_REG (QImode, H_REG)

#define AX gen_rtx_REG (HImode, AX_REG)
#define BC gen_rtx_REG (HImode, BC_REG)
#define DE gen_rtx_REG (HImode, DE_REG)
#define HL gen_rtx_REG (HImode, HL_REG)

/* Returns TRUE if R is a virtual register.  */
static inline bool
is_virtual_register (rtx r)
{
  return (GET_CODE (r) == REG
	  && REGNO (r) >= 8
	  && REGNO (r) < 32);
}

/* In all these alloc routines, we expect the following: the insn
   pattern is unshared, the insn was previously recognized and failed
   due to predicates or constraints, and the operand data is in
   recog_data.  */

static int virt_insn_was_frame;

/* Hook for all insns we emit.  Re-mark them as FRAME_RELATED if
   needed.  */
static rtx
EM2 (int line ATTRIBUTE_UNUSED, rtx r)
{
#if DEBUG_ALLOC
  fprintf (stderr, "\033[36m%d: ", line);
  debug_rtx (r);
  fprintf (stderr, "\033[0m");
#endif
  /*SCHED_GROUP_P (r) = 1;*/
  if (virt_insn_was_frame)
    RTX_FRAME_RELATED_P (r) = 1;
  return r;
}

#define EM(x) EM2 (__LINE__, x)

/* Return a suitable RTX for the low half of a __far address.  */
static rtx
rl78_lo16 (rtx addr)
{
  rtx r;

  if (GET_CODE (addr) == SYMBOL_REF
      || GET_CODE (addr) == CONST)
    {
      r = gen_rtx_ZERO_EXTRACT (HImode, addr, GEN_INT (16), GEN_INT (0));
      r = gen_rtx_CONST (HImode, r);
    }
  else
    r = rl78_subreg (HImode, addr, SImode, 0);

  r = gen_es_addr (r);
  cfun->machine->uses_es = true;

  return r;
}

/* Return a suitable RTX for the high half's lower byte of a __far address.  */
static rtx
rl78_hi8 (rtx addr)
{
  if (GET_CODE (addr) == SYMBOL_REF
      || GET_CODE (addr) == CONST)
    {
      rtx r = gen_rtx_ZERO_EXTRACT (QImode, addr, GEN_INT (8), GEN_INT (16));
      r = gen_rtx_CONST (QImode, r);
      return r;
    }
  return rl78_subreg (QImode, addr, SImode, 2);
}

static void
add_postponed_content_update (rtx to, rtx value)
{
  unsigned char index;

  if ((index = get_content_index (to)) == NOT_KNOWN)
    return;

  gcc_assert (saved_update_index == NOT_KNOWN);
  saved_update_index = index;
  saved_update_value = get_content_index (value);
  saved_update_mode  = GET_MODE (to);
}

static void
process_postponed_content_update (void)
{
  if (saved_update_index != NOT_KNOWN)
    {
      update_content (saved_update_index, saved_update_value, saved_update_mode);
      saved_update_index = NOT_KNOWN;
    }
}

/* Generate and emit a move of (register) FROM into TO.  if WHERE is not NULL
   then if BEFORE is true then emit the insn before WHERE, otherwise emit it
   after WHERE.  If TO already contains FROM then do nothing.  Returns TO if
   BEFORE is true, FROM otherwise.  */
static rtx
gen_and_emit_move (rtx to, rtx from, rtx where, bool before)
{
  machine_mode mode = GET_MODE (to);

  if (optimize && before && already_contains (to, from))
    {
#if DEBUG_ALLOC
      display_content_memory (stderr);
#endif
      if (dump_file)
	{
	  fprintf (dump_file, " Omit move of %s into ",
		   get_content_name (get_content_index (from), mode));
	  fprintf (dump_file, "%s as it already contains this value\n",
		   get_content_name (get_content_index (to), mode));
	}
    }
  else
    {
      rtx move = mode == QImode ? gen_movqi (to, from) : gen_movhi (to, from);

      EM (move);

      if (where == NULL_RTX)
	emit_insn (move);
      else if (before)
	emit_insn_before (move, where);
      else
	{
	  rtx note = find_reg_note (where, REG_EH_REGION, NULL_RTX);

	  /* If necessary move REG_EH_REGION notes forward.
	     cf. compiling gcc.dg/pr44545.c.  */
	  if (note != NULL_RTX)
	    {
	      add_reg_note (move, REG_EH_REGION, XEXP (note, 0));
	      remove_note (where, note);
	    }

	  emit_insn_after (move, where);
	}

      if (before)
	record_content (to, from);
      else
	add_postponed_content_update (to, from);
    }

  return before ? to : from;
}

/* If M is MEM(REG) or MEM(PLUS(REG,INT)) and REG is virtual then
   copy it into NEWBASE and return the updated MEM.  Otherwise just
   return M.  Any needed insns are emitted before BEFORE.  */
static rtx
transcode_memory_rtx (rtx m, rtx newbase, rtx before)
{
  rtx base, index, addendr;
  int addend = 0;
  int need_es = 0;

  if (! MEM_P (m))
    return m;

  if (GET_MODE (XEXP (m, 0)) == SImode)
    {
      rtx new_m;
      rtx seg = rl78_hi8 (XEXP (m, 0));

      if (!TARGET_ES0)
	{
          emit_insn_before (EM (gen_movqi (A, seg)), before);
          emit_insn_before (EM (gen_movqi_to_es (A)), before);
        }

      record_content (A, NULL_RTX);

      new_m = gen_rtx_MEM (GET_MODE (m), rl78_lo16 (XEXP (m, 0)));
      MEM_COPY_ATTRIBUTES (new_m, m);
      m = new_m;
      need_es = 1;
    }

  characterize_address (XEXP (m, 0), & base, & index, & addendr);
  gcc_assert (index == NULL_RTX);

  if (base == NULL_RTX)
    return m;

  if (addendr && GET_CODE (addendr) == CONST_INT)
    addend = INTVAL (addendr);

  gcc_assert (REG_P (base));
  gcc_assert (REG_P (newbase));

  int limit = 256 - GET_MODE_SIZE (GET_MODE (m));

  if (REGNO (base) == SP_REG)
    {
      if (addend >= 0 && addend <= limit)
	return m;
    }

  /* BASE should be a virtual register.  We copy it to NEWBASE.  If
     the addend is out of range for DE/HL, we use AX to compute the full
     address.  */

  if (addend < 0
      || (addend > limit && REGNO (newbase) != BC_REG)
      || (addendr
	  && (GET_CODE (addendr) != CONST_INT)
	  && ((REGNO (newbase) != BC_REG))
	  ))
    {
      /* mov ax, vreg
	 add ax, #imm
	 mov hl, ax	*/
      EM (emit_insn_before (gen_movhi (AX, base), before));
      EM (emit_insn_before (gen_addhi3 (AX, AX, addendr), before));
      EM (emit_insn_before (gen_movhi (newbase, AX), before));
      record_content (AX, NULL_RTX);
      record_content (newbase, NULL_RTX);

      base = newbase;
      addend = 0;
      addendr = 0;
    }
  else
    {
      base = gen_and_emit_move (newbase, base, before, true);
    }

  if (addend)
    {
      record_content (base, NULL_RTX);
      base = gen_rtx_PLUS (HImode, base, GEN_INT (addend));
    }
  else if (addendr)
    {
      record_content (base, NULL_RTX);
      base = gen_rtx_PLUS (HImode, base, addendr);
    }

  if (need_es)
    {
      m = change_address (m, GET_MODE (m), gen_es_addr (base));
      cfun->machine->uses_es = true;
    }
  else
    m = change_address (m, GET_MODE (m), base);
  return m;
}

/* Copy SRC to accumulator (A or AX), placing any generated insns
   before BEFORE.  Returns accumulator RTX.  */
static rtx
move_to_acc (int opno, rtx before)
{
  rtx src = OP (opno);
  machine_mode mode = GET_MODE (src);

  if (REG_P (src) && REGNO (src) < 2)
    return src;

  if (mode == VOIDmode)
    mode = recog_data.operand_mode[opno];

  return gen_and_emit_move (mode == QImode ? A : AX, src, before, true);
}

static void
force_into_acc (rtx src, rtx before)
{
  machine_mode mode = GET_MODE (src);
  rtx move;

  if (REG_P (src) && REGNO (src) < 2)
    return;

  move = mode == QImode ? gen_movqi (A, src) : gen_movhi (AX, src);

  EM (move);

  emit_insn_before (move, before);
  record_content (AX, NULL_RTX);
}

/* Copy accumulator (A or AX) to DEST, placing any generated insns
   after AFTER.  Returns accumulator RTX.  */
static rtx
move_from_acc (unsigned int opno, rtx after)
{
  rtx dest = OP (opno);
  machine_mode mode = GET_MODE (dest);

  if (REG_P (dest) && REGNO (dest) < 2)
    return dest;

  return gen_and_emit_move (dest, mode == QImode ? A : AX, after, false);
}

/* Copy accumulator (A or AX) to REGNO, placing any generated insns
   before BEFORE.  Returns reg RTX.  */
static rtx
move_acc_to_reg (rtx acc, int regno, rtx before)
{
  machine_mode mode = GET_MODE (acc);
  rtx reg;

  reg = gen_rtx_REG (mode, regno);

  return gen_and_emit_move (reg, acc, before, true);
}

/* Copy SRC to X, placing any generated insns before BEFORE.
   Returns X RTX.  */
static rtx
move_to_x (int opno, rtx before)
{
  rtx src = OP (opno);
  machine_mode mode = GET_MODE (src);
  rtx reg;

  if (mode == VOIDmode)
    mode = recog_data.operand_mode[opno];
  reg = (mode == QImode) ? X : AX;

  if (mode == QImode || ! is_virtual_register (OP (opno)))
    {
      OP (opno) = move_to_acc (opno, before);
      OP (opno) = move_acc_to_reg (OP (opno), X_REG, before);
      return reg;
    }

  return gen_and_emit_move (reg, src, before, true);
}

/* Copy OP (opno) to H or HL, placing any generated insns before BEFORE.
   Returns H/HL RTX.  */
static rtx
move_to_hl (int opno, rtx before)
{
  rtx src = OP (opno);
  machine_mode mode = GET_MODE (src);
  rtx reg;

  if (mode == VOIDmode)
    mode = recog_data.operand_mode[opno];
  reg = (mode == QImode) ? L : HL;

  if (mode == QImode || ! is_virtual_register (OP (opno)))
    {
      OP (opno) = move_to_acc (opno, before);
      OP (opno) = move_acc_to_reg (OP (opno), L_REG, before);
      return reg;
    }

  return gen_and_emit_move (reg, src, before, true);
}

/* Copy OP (opno) to E or DE, placing any generated insns before BEFORE.
   Returns E/DE RTX.  */
static rtx
move_to_de (int opno, rtx before)
{
  rtx src = OP (opno);
  machine_mode mode = GET_MODE (src);
  rtx reg;

  if (mode == VOIDmode)
    mode = recog_data.operand_mode[opno];

  reg = (mode == QImode) ? E : DE;

  if (mode == QImode || ! is_virtual_register (OP (opno)))
    {
      OP (opno) = move_to_acc (opno, before);
      OP (opno) = move_acc_to_reg (OP (opno), E_REG, before);
    }
  else
    {
      gen_and_emit_move (reg, src, before, true);
    }

  return reg;
}

/* Devirtualize an insn of the form (SET (op) (unop (op))).  */
static void
rl78_alloc_physical_registers_op1 (rtx_insn * insn)
{
  /* op[0] = func op[1] */

  /* We first try using A as the destination, then copying it
     back.  */
  if (rtx_equal_p (OP (0), OP (1)))
    {
      OP (0) =
      OP (1) = transcode_memory_rtx (OP (1), DE, insn);
    }
  else
    {
      /* If necessary, load the operands into BC and HL.
	 Check to see if we already have OP (0) in HL
	 and if so, swap the order.

	 It is tempting to perform this optimization when OP(0) does
	 not hold a MEM, but this leads to bigger code in general.
	 The problem is that if OP(1) holds a MEM then swapping it
	 into BC means a BC-relative load is used and these are 3
	 bytes long vs 1 byte for an HL load.  */
      if (MEM_P (OP (0))
	  && already_contains (HL, XEXP (OP (0), 0)))
	{
	  OP (0) = transcode_memory_rtx (OP (0), HL, insn);
	  OP (1) = transcode_memory_rtx (OP (1), BC, insn);
	}
      else
	{
	  OP (0) = transcode_memory_rtx (OP (0), BC, insn);
	  OP (1) = transcode_memory_rtx (OP (1), HL, insn);
	}
    }

  MAYBE_OK (insn);

  OP (0) = move_from_acc (0, insn);

  MAYBE_OK (insn);

  /* Try copying the src to acc first, then.  This is for, for
     example, ZERO_EXTEND or NOT.  */
  OP (1) = move_to_acc (1, insn);

  MUST_BE_OK (insn);
}

/* Returns true if operand OPNUM contains a constraint of type CONSTRAINT.
   Assumes that the current insn has already been recognised and hence the
   constraint data has been filled in.  */
static bool
has_constraint (unsigned int opnum, enum constraint_num constraint)
{
  const char * p = recog_data.constraints[opnum];

  /* No constraints means anything is accepted.  */
  if (p == NULL || *p == 0 || *p == ',')
    return true;

  do
    {
      char c;
      unsigned int len;

      c = *p;
      len = CONSTRAINT_LEN (c, p);
      gcc_assert (len > 0);

      switch (c)
	{
	case 0:
	case ',':
	  return false;
	default:
	  if (lookup_constraint (p) == constraint)
	    return true;
	}
      p += len;
    }
  while (1);
}

/* Devirtualize an insn of the form (SET (op) (binop (op) (op))).  */
static void
rl78_alloc_physical_registers_op2 (rtx_insn * insn)
{
  rtx prev;
  rtx first;
  bool hl_used;
  int tmp_id;
  rtx saved_op1;

  if (rtx_equal_p (OP (0), OP (1)))
    {
      if (MEM_P (OP (2)))
	{
	  OP (0) =
	  OP (1) = transcode_memory_rtx (OP (1), DE, insn);
	  OP (2) = transcode_memory_rtx (OP (2), HL, insn);
	}
      else
	{
	  OP (0) =
	  OP (1) = transcode_memory_rtx (OP (1), HL, insn);
	  OP (2) = transcode_memory_rtx (OP (2), DE, insn);
	}
    }
  else if (rtx_equal_p (OP (0), OP (2)))
    {
      OP (1) = transcode_memory_rtx (OP (1), DE, insn);
      OP (0) =
      OP (2) = transcode_memory_rtx (OP (2), HL, insn);
    }
  else
    {
      OP (0) = transcode_memory_rtx (OP (0), BC, insn);
      OP (1) = transcode_memory_rtx (OP (1), DE, insn);
      OP (2) = transcode_memory_rtx (OP (2), HL, insn);
    }

  MAYBE_OK (insn);

  prev = prev_nonnote_nondebug_insn (insn);
  if (recog_data.constraints[1][0] == '%'
      && is_virtual_register (OP (1))
      && ! is_virtual_register (OP (2))
      && ! CONSTANT_P (OP (2)))
    {
      rtx tmp = OP (1);
      OP (1) = OP (2);
      OP (2) = tmp;
    }

  /* Make a note of whether (H)L is being used.  It matters
     because if OP (2) also needs reloading, then we must take
     care not to corrupt HL.  */
  hl_used = reg_mentioned_p (L, OP (0)) || reg_mentioned_p (L, OP (1));

  /* If HL is not currently being used and dest == op1 then there are
     some possible optimizations available by reloading one of the
     operands into HL, before trying to use the accumulator.  */
  if (optimize
      && ! hl_used
      && rtx_equal_p (OP (0), OP (1)))
    {
      /* If op0 is a Ws1 type memory address then switching the base
	 address register to HL might allow us to perform an in-memory
	 operation.  (eg for the INCW instruction).

	 FIXME: Adding the move into HL is costly if this optimization is not
	 going to work, so for now, make sure that we know that the new insn will
	 match the requirements of the addhi3_real pattern.  Really we ought to
	 generate a candidate sequence, test that, and then install it if the
	 results are good.  */
      if (satisfies_constraint_Ws1 (OP (0))
	  && has_constraint (0, CONSTRAINT_Wh1)
	  && (satisfies_constraint_K (OP (2)) || satisfies_constraint_L (OP (2))))
	{
	  rtx base, index, addend, newbase;

	  characterize_address (XEXP (OP (0), 0), & base, & index, & addend);
	  gcc_assert (index == NULL_RTX);
	  gcc_assert (REG_P (base) && REGNO (base) == SP_REG);

	  /* Ws1 addressing allows an offset of 0, Wh1 addressing requires a non-zero offset.  */
	  if (addend != NULL_RTX)
	    {
	      newbase = gen_and_emit_move (HL, base, insn, true);
	      record_content (newbase, NULL_RTX);
	      newbase = gen_rtx_PLUS (HImode, newbase, addend);

	      OP (0) = OP (1) = change_address (OP (0), VOIDmode, newbase);

	      /* We do not want to fail here as this means that
		 we have inserted useless insns into the stream.  */
	      MUST_BE_OK (insn);
	    }
	}
      else if (REG_P (OP (0))
	       && satisfies_constraint_Ws1 (OP (2))
	       && has_constraint (2, CONSTRAINT_Wh1))
	{
	  rtx base, index, addend, newbase;

	  characterize_address (XEXP (OP (2), 0), & base, & index, & addend);
	  gcc_assert (index == NULL_RTX);
	  gcc_assert (REG_P (base) && REGNO (base) == SP_REG);

	  /* Ws1 addressing allows an offset of 0, Wh1 addressing requires a non-zero offset.  */
	  if (addend != NULL_RTX)
	    {
	      gen_and_emit_move (HL, base, insn, true);

	      if (REGNO (OP (0)) != X_REG)
		{
		  OP (1) = move_to_acc (1, insn);
		  OP (0) = move_from_acc (0, insn);
		}

	      record_content (HL, NULL_RTX);
	      newbase = gen_rtx_PLUS (HImode, HL, addend);

	      OP (2) = change_address (OP (2), VOIDmode, newbase);

	      /* We do not want to fail here as this means that
		 we have inserted useless insns into the stream.  */
	      MUST_BE_OK (insn);
	    }
	}
    }

  OP (0) = move_from_acc (0, insn);

  tmp_id = get_max_insn_count ();
  saved_op1 = OP (1);

  if (rtx_equal_p (OP (1), OP (2)))
    OP (2) = OP (1) = move_to_acc (1, insn);
  else
    OP (1) = move_to_acc (1, insn);

  MAYBE_OK (insn);

  /* If we omitted the move of OP1 into the accumulator (because
     it was already there from a previous insn), then force the
     generation of the move instruction now.  We know that we
     are about to emit a move into HL (or DE) via AX, and hence
     our optimization to remove the load of OP1 is no longer valid.  */
  if (tmp_id == get_max_insn_count ())
    force_into_acc (saved_op1, insn);

  /* We have to copy op2 to HL (or DE), but that involves AX, which
     already has a live value.  Emit it before those insns.  */

  if (prev)
    first = next_nonnote_nondebug_insn (prev);
  else
    for (first = insn; prev_nonnote_nondebug_insn (first); first = prev_nonnote_nondebug_insn (first))
      ;

  OP (2) = hl_used ? move_to_de (2, first) : move_to_hl (2, first);

  MUST_BE_OK (insn);
}

/* Devirtualize an insn of the form SET (PC) (MEM/REG).  */
static void
rl78_alloc_physical_registers_ro1 (rtx_insn * insn)
{
  OP (0) = transcode_memory_rtx (OP (0), BC, insn);

  MAYBE_OK (insn);

  OP (0) = move_to_acc (0, insn);

  MUST_BE_OK (insn);
}

/* Devirtualize a compare insn.  */
static void
rl78_alloc_physical_registers_cmp (rtx_insn * insn)
{
  int tmp_id;
  rtx saved_op1;
  rtx_insn *prev = prev_nonnote_nondebug_insn (insn);
  rtx first;

  OP (1) = transcode_memory_rtx (OP (1), DE, insn);
  OP (2) = transcode_memory_rtx (OP (2), HL, insn);

  /* HI compares have to have OP (1) in AX, but QI
     compares do not, so it is worth checking here.  */
  MAYBE_OK (insn);

  /* For an HImode compare, OP (1) must always be in AX.
     But if OP (1) is a REG (and not AX), then we can avoid
     a reload of OP (1) if we reload OP (2) into AX and invert
     the comparison.  */
  if (REG_P (OP (1))
      && REGNO (OP (1)) != AX_REG
      && GET_MODE (OP (1)) == HImode
      && MEM_P (OP (2)))
    {
      rtx cmp = XEXP (SET_SRC (PATTERN (insn)), 0);

      OP (2) = move_to_acc (2, insn);

      switch (GET_CODE (cmp))
	{
	case EQ:
	case NE:
	  break;
	case LTU: cmp = gen_rtx_GTU (HImode, OP (2), OP (1)); break;
	case GTU: cmp = gen_rtx_LTU (HImode, OP (2), OP (1)); break;
	case LEU: cmp = gen_rtx_GEU (HImode, OP (2), OP (1)); break;
	case GEU: cmp = gen_rtx_LEU (HImode, OP (2), OP (1)); break;

	case LT:
	case GT:
	case LE:
	case GE:
#if DEBUG_ALLOC
	  debug_rtx (insn);
#endif
	default:
	  gcc_unreachable ();
	}

      if (GET_CODE (cmp) == EQ || GET_CODE (cmp) == NE)
	PATTERN (insn) = gen_cbranchhi4_real (cmp, OP (2), OP (1), OP (3));
      else
	PATTERN (insn) = gen_cbranchhi4_real_inverted (cmp, OP (2), OP (1), OP (3));

      MUST_BE_OK (insn);
    }

  /* Surprisingly, gcc can generate a comparison of a register with itself, but this
     should be handled by the second alternative of the cbranchhi_real pattern.  */
  if (rtx_equal_p (OP (1), OP (2)))
    {
      OP (1) = OP (2) = BC;
      MUST_BE_OK (insn);
    }

  tmp_id = get_max_insn_count ();
  saved_op1 = OP (1);

  OP (1) = move_to_acc (1, insn);

  MAYBE_OK (insn);

  /* If we omitted the move of OP1 into the accumulator (because
     it was already there from a previous insn), then force the
     generation of the move instruction now.  We know that we
     are about to emit a move into HL via AX, and hence our
     optimization to remove the load of OP1 is no longer valid.  */
  if (tmp_id == get_max_insn_count ())
    force_into_acc (saved_op1, insn);

  /* We have to copy op2 to HL, but that involves the acc, which
     already has a live value.  Emit it before those insns.  */
  if (prev)
    first = next_nonnote_nondebug_insn (prev);
  else
    for (first = insn; prev_nonnote_nondebug_insn (first); first = prev_nonnote_nondebug_insn (first))
      ;
  OP (2) = move_to_hl (2, first);

  MUST_BE_OK (insn);
}

/* Like op2, but AX = A * X.  */
static void
rl78_alloc_physical_registers_umul (rtx_insn * insn)
{
  rtx_insn *prev = prev_nonnote_nondebug_insn (insn);
  rtx first;
  int tmp_id;
  rtx saved_op1;

  OP (0) = transcode_memory_rtx (OP (0), BC, insn);
  OP (1) = transcode_memory_rtx (OP (1), DE, insn);
  OP (2) = transcode_memory_rtx (OP (2), HL, insn);

  MAYBE_OK (insn);

  if (recog_data.constraints[1][0] == '%'
      && is_virtual_register (OP (1))
      && !is_virtual_register (OP (2))
      && !CONSTANT_P (OP (2)))
    {
      rtx tmp = OP (1);
      OP (1) = OP (2);
      OP (2) = tmp;
    }

  OP (0) = move_from_acc (0, insn);

  tmp_id = get_max_insn_count ();
  saved_op1 = OP (1);

  if (rtx_equal_p (OP (1), OP (2)))
    {
      gcc_assert (GET_MODE (OP (2)) == QImode);
      /* The MULU instruction does not support duplicate arguments
	 but we know that if we copy OP (2) to X it will do so via
	 A and thus OP (1) will already be loaded into A.  */
      OP (2) = move_to_x (2, insn);
      OP (1) = A;
    }
  else
    OP (1) = move_to_acc (1, insn);

  MAYBE_OK (insn);

  /* If we omitted the move of OP1 into the accumulator (because
     it was already there from a previous insn), then force the
     generation of the move instruction now.  We know that we
     are about to emit a move into HL (or DE) via AX, and hence
     our optimization to remove the load of OP1 is no longer valid.  */
  if (tmp_id == get_max_insn_count ())
    force_into_acc (saved_op1, insn);

  /* We have to copy op2 to X, but that involves the acc, which
     already has a live value.  Emit it before those insns.  */

  if (prev)
    first = next_nonnote_nondebug_insn (prev);
  else
    for (first = insn; prev_nonnote_nondebug_insn (first); first = prev_nonnote_nondebug_insn (first))
      ;
  OP (2) = move_to_x (2, first);

  MUST_BE_OK (insn);
}

static void
rl78_alloc_address_registers_macax (rtx_insn * insn)
{
  int which, op;
  bool replace_in_op0 = false;
  bool replace_in_op1 = false;

  MAYBE_OK (insn);

  /* Two different MEMs are not allowed.  */
  which = 0;
  for (op = 2; op >= 0; op --)
    {
      if (MEM_P (OP (op)))
	{
	  if (op == 0 && replace_in_op0)
	    continue;
	  if (op == 1 && replace_in_op1)
	    continue;

	  switch (which)
	    {
	    case 0:
	      /* If we replace a MEM, make sure that we replace it for all
		 occurrences of the same MEM in the insn.  */
	      replace_in_op0 = (op > 0 && rtx_equal_p (OP (op), OP (0)));
	      replace_in_op1 = (op > 1 && rtx_equal_p (OP (op), OP (1)));

	      OP (op) = transcode_memory_rtx (OP (op), HL, insn);
	      if (op == 2
		  && MEM_P (OP (op))
		  && ((GET_CODE (XEXP (OP (op), 0)) == REG
		       && REGNO (XEXP (OP (op), 0)) == SP_REG)
		      || (GET_CODE (XEXP (OP (op), 0)) == PLUS
			  && REGNO (XEXP (XEXP (OP (op), 0), 0)) == SP_REG)))
		{
		  emit_insn_before (gen_movhi (HL, gen_rtx_REG (HImode, SP_REG)), insn);
		  OP (op) = replace_rtx (OP (op), gen_rtx_REG (HImode, SP_REG), HL);
		}
	      if (replace_in_op0)
		OP (0) = OP (op);
	      if (replace_in_op1)
		OP (1) = OP (op);
	      break;
	    case 1:
	      OP (op) = transcode_memory_rtx (OP (op), DE, insn);
	      break;
	    case 2:
	      OP (op) = transcode_memory_rtx (OP (op), BC, insn);
	      break;
	    }
	  which ++;
	}
    }

  MUST_BE_OK (insn);
}

static void
rl78_alloc_address_registers_div (rtx_insn * insn)
{
  MUST_BE_OK (insn);
}

/* Scan all insns and devirtualize them.  */
static void
rl78_alloc_physical_registers (void)
{
  /* During most of the compile, gcc is dealing with virtual
     registers.  At this point, we need to assign physical registers
     to the vitual ones, and copy in/out as needed.  */

  rtx_insn *insn, *curr;
  enum attr_valloc valloc_method;

  for (insn = get_insns (); insn; insn = curr)
    {
      int i;

      curr = next_nonnote_nondebug_insn (insn);

      if (INSN_P (insn)
	  && (GET_CODE (PATTERN (insn)) == SET
	      || GET_CODE (PATTERN (insn)) == CALL)
	  && INSN_CODE (insn) == -1)
	{
	  if (GET_CODE (SET_SRC (PATTERN (insn))) == ASM_OPERANDS)
	    continue;
	  i = recog (PATTERN (insn), insn, 0);
	  if (i == -1)
	    {
	      debug_rtx (insn);
	      gcc_unreachable ();
	    }
	  INSN_CODE (insn) = i;
	}
    }

  cfun->machine->virt_insns_ok = 0;
  cfun->machine->real_insns_ok = 1;

  clear_content_memory ();

  for (insn = get_insns (); insn; insn = curr)
    {
      rtx pattern;

      curr = insn ? next_nonnote_nondebug_insn (insn) : NULL;

      if (!INSN_P (insn))
	{
	  if (LABEL_P (insn))
	    clear_content_memory ();

 	  continue;
	}

      if (dump_file)
	fprintf (dump_file, "Converting insn %d\n", INSN_UID (insn));

      pattern = PATTERN (insn);
      if (GET_CODE (pattern) == PARALLEL)
	pattern = XVECEXP (pattern, 0, 0);
      if (JUMP_P (insn) || CALL_P (insn) || GET_CODE (pattern) == CALL)
	clear_content_memory ();
      if (GET_CODE (pattern) != SET
	  && GET_CODE (pattern) != CALL)
	continue;
      if (GET_CODE (pattern) == SET
	  && GET_CODE (SET_SRC (pattern)) == ASM_OPERANDS)
	continue;

      valloc_method = get_attr_valloc (insn);

      PATTERN (insn) = copy_rtx_if_shared (PATTERN (insn));

      if (valloc_method == VALLOC_MACAX)
	{
	  record_content (AX, NULL_RTX);
	  record_content (BC, NULL_RTX);
	  record_content (DE, NULL_RTX);
	}
      else if (valloc_method == VALLOC_DIVHI)
	{
	  record_content (AX, NULL_RTX);
	  record_content (BC, NULL_RTX);
	}
      else if (valloc_method == VALLOC_DIVSI)
	{
	  record_content (AX, NULL_RTX);
	  record_content (BC, NULL_RTX);
	  record_content (DE, NULL_RTX);
	  record_content (HL, NULL_RTX);
	}

      if (insn_ok_now (insn))
	continue;

      INSN_CODE (insn) = -1;

      if (RTX_FRAME_RELATED_P (insn))
	virt_insn_was_frame = 1;
      else
	virt_insn_was_frame = 0;

      switch (valloc_method)
	{
	case VALLOC_OP1:
	  rl78_alloc_physical_registers_op1 (insn);
	  break;
	case VALLOC_OP2:
	  rl78_alloc_physical_registers_op2 (insn);
	  break;
	case VALLOC_RO1:
	  rl78_alloc_physical_registers_ro1 (insn);
	  break;
	case VALLOC_CMP:
	  rl78_alloc_physical_registers_cmp (insn);
	  break;
	case VALLOC_UMUL:
	  rl78_alloc_physical_registers_umul (insn);
	  record_content (AX, NULL_RTX);
	  break;
	case VALLOC_MACAX:
	  /* Macro that clobbers AX.  */
	  rl78_alloc_address_registers_macax (insn);
	  record_content (AX, NULL_RTX);
	  record_content (BC, NULL_RTX);
	  record_content (DE, NULL_RTX);
	  break;
	case VALLOC_DIVSI:
	  rl78_alloc_address_registers_div (insn);
	  record_content (AX, NULL_RTX);
	  record_content (BC, NULL_RTX);
	  record_content (DE, NULL_RTX);
	  record_content (HL, NULL_RTX);
	  break;
	case VALLOC_DIVHI:
	  rl78_alloc_address_registers_div (insn);
	  record_content (AX, NULL_RTX);
	  record_content (BC, NULL_RTX);
	  break;
	default:
	  gcc_unreachable ();
	}

      if (JUMP_P (insn) || CALL_P (insn) || GET_CODE (pattern) == CALL)
	clear_content_memory ();
      else
	process_postponed_content_update ();
    }

#if DEBUG_ALLOC
  fprintf (stderr, "\033[0m");
#endif
}

/* Add REG_DEAD notes using DEAD[reg] for rtx S which is part of INSN.
   This function scans for uses of registers; the last use (i.e. first
   encounter when scanning backwards) triggers a REG_DEAD note if the
   reg was previously in DEAD[].  */
static void
rl78_note_reg_uses (char *dead, rtx s, rtx insn)
{
  const char *fmt;
  int i, r;
  enum rtx_code code;

  if (!s)
    return;

  code = GET_CODE (s);

  switch (code)
    {
      /* Compare registers by number.  */
    case REG:
      r = REGNO (s);
      if (dump_file)
	{
	  fprintf (dump_file, "note use reg %d size %d on insn %d\n",
		   r, GET_MODE_SIZE (GET_MODE (s)), INSN_UID (insn));
	  print_rtl_single (dump_file, s);
	}
      if (dead [r])
	add_reg_note (insn, REG_DEAD, gen_rtx_REG (GET_MODE (s), r));
      for (i = 0; i < GET_MODE_SIZE (GET_MODE (s)); i ++)
	dead [r + i] = 0;
      return;

      /* These codes have no constituent expressions
	 and are unique.  */
    case SCRATCH:
    case CC0:
    case PC:
      return;

    case CONST_INT:
    case CONST_VECTOR:
    case CONST_DOUBLE:
    case CONST_FIXED:
      /* These are kept unique for a given value.  */
      return;

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);

  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  int j;
	  for (j = XVECLEN (s, i) - 1; j >= 0; j--)
	    rl78_note_reg_uses (dead, XVECEXP (s, i, j), insn);
	}
      else if (fmt[i] == 'e')
	rl78_note_reg_uses (dead, XEXP (s, i), insn);
    }
}

/* Like the previous function, but scan for SETs instead.  */
static void
rl78_note_reg_set (char *dead, rtx d, rtx insn)
{
  int r, i;

  if (GET_CODE (d) == MEM)
    rl78_note_reg_uses (dead, XEXP (d, 0), insn);

  if (GET_CODE (d) != REG)
    return;

  r = REGNO (d);
  if (dead [r])
    add_reg_note (insn, REG_UNUSED, gen_rtx_REG (GET_MODE (d), r));
  if (dump_file)
    fprintf (dump_file, "note set reg %d size %d\n", r, GET_MODE_SIZE (GET_MODE (d)));
  for (i = 0; i < GET_MODE_SIZE (GET_MODE (d)); i ++)
    dead [r + i] = 1;
}

/* This is a rather crude register death pass.  Death status is reset
   at every jump or call insn.  */
static void
rl78_calculate_death_notes (void)
{
  char dead[FIRST_PSEUDO_REGISTER];
  rtx insn, p, s, d;
  int i;

  memset (dead, 0, sizeof (dead));

  for (insn = get_last_insn ();
       insn;
       insn = prev_nonnote_nondebug_insn (insn))
    {
      if (dump_file)
	{
	  fprintf (dump_file, "\n--------------------------------------------------");
	  fprintf (dump_file, "\nDead:");
	  for (i = 0; i < FIRST_PSEUDO_REGISTER; i ++)
	    if (dead[i])
	      fprintf (dump_file, " %s", reg_names[i]);
	  fprintf (dump_file, "\n");
	  print_rtl_single (dump_file, insn);
	}

      switch (GET_CODE (insn))
	{
	case INSN:
	  p = PATTERN (insn);
	  if (GET_CODE (p) == PARALLEL)
	    {
	      rtx q = XVECEXP (p, 0 ,1);

	      /* This happens with the DIV patterns.  */
	      if (GET_CODE (q) == SET)
		{
		  s = SET_SRC (q);
		  d = SET_DEST (q);
		  rl78_note_reg_set (dead, d, insn);
		  rl78_note_reg_uses (dead, s, insn);

		}
	      p = XVECEXP (p, 0, 0);
	    }

	  switch (GET_CODE (p))
	    {
	    case SET:
	      s = SET_SRC (p);
	      d = SET_DEST (p);
	      rl78_note_reg_set (dead, d, insn);
	      rl78_note_reg_uses (dead, s, insn);
	      break;

	    case USE:
	      rl78_note_reg_uses (dead, p, insn);
	      break;

	    default:
	      break;
	    }
	  break;

	case JUMP_INSN:
	  if (INSN_CODE (insn) == CODE_FOR_rl78_return)
	    {
	      memset (dead, 1, sizeof (dead));
	      /* We expect a USE just prior to this, which will mark
		 the actual return registers.  The USE will have a
		 death note, but we aren't going to be modifying it
		 after this pass.  */
	      break;
	    }
	case CALL_INSN:
	  memset (dead, 0, sizeof (dead));
	  break;

	default:
	  break;
	}
      if (dump_file)
	print_rtl_single (dump_file, insn);
    }
}

/* Helper function to reset the origins in RP and the age in AGE for
   all registers.  */
static void
reset_origins (int *rp, int *age)
{
  int i;
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      rp[i] = i;
      age[i] = 0;
    }
}

static void
set_origin (rtx pat, rtx_insn * insn, int * origins, int * age)
{
  rtx src = SET_SRC (pat);
  rtx dest = SET_DEST (pat);
  int mb = GET_MODE_SIZE (GET_MODE (dest));
  int i;

  if (GET_CODE (dest) == REG)
    {
      int dr = REGNO (dest);

      if (GET_CODE (src) == REG)
	{
	  int sr = REGNO (src);
	  bool same = true;
	  int best_age, best_reg;

	  /* See if the copy is not needed.  */
	  for (i = 0; i < mb; i ++)
	    if (origins[dr + i] != origins[sr + i])
	      same = false;

	  if (same)
	    {
	      if (dump_file)
		fprintf (dump_file, "deleting because dest already has correct value\n");
	      delete_insn (insn);
	      return;
	    }

	  if (dr < 8 || sr >= 8)
	    {
	      int ar;

	      best_age = -1;
	      best_reg = -1;

	      /* See if the copy can be made from another
		 bank 0 register instead, instead of the
		 virtual src register.  */
	      for (ar = 0; ar < 8; ar += mb)
		{
		  same = true;

		  for (i = 0; i < mb; i ++)
		    if (origins[ar + i] != origins[sr + i])
		      same = false;

		  /* The chip has some reg-reg move limitations.  */
		  if (mb == 1 && dr > 3)
		    same = false;

		  if (same)
		    {
		      if (best_age == -1 || best_age > age[sr + i])
			{
			  best_age = age[sr + i];
			  best_reg = sr;
			}
		    }
		}

	      if (best_reg != -1)
		{
		  /* FIXME: copy debug info too.  */
		  SET_SRC (pat) = gen_rtx_REG (GET_MODE (src), best_reg);
		  sr = best_reg;
		}
	    }

	  for (i = 0; i < mb; i++)
	    {
	      origins[dr + i] = origins[sr + i];
	      age[dr + i] = age[sr + i] + 1;
	    }
	}
      else
	{
	  /* The destination is computed, its origin is itself.  */
	  if (dump_file)
	    fprintf (dump_file, "resetting origin of r%d for %d byte%s\n",
		     dr, mb, mb == 1 ? "" : "s");

	  for (i = 0; i < mb; i ++)
	    {
	      origins[dr + i] = dr + i;
	      age[dr + i] = 0;
	    }
	}

      /* Any registers marked with that reg as an origin are reset.  */
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (origins[i] >= dr && origins[i] < dr + mb)
	  {
	    origins[i] = i;
	    age[i] = 0;
	  }
    }

  /* Special case - our MUL patterns uses AX and sometimes BC.  */
  if (get_attr_valloc (insn) == VALLOC_MACAX)
    {
      if (dump_file)
	fprintf (dump_file, "Resetting origin of AX/BC for MUL pattern.\n");

      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (i <= 3 || origins[i] <= 3)
	  {
	    origins[i] = i;
	    age[i] = 0;
	  }
    }
  else if (get_attr_valloc (insn) == VALLOC_DIVHI)
    {
      if (dump_file)
	fprintf (dump_file, "Resetting origin of AX/DE for DIVHI pattern.\n");

      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (i == A_REG
	    || i == X_REG
	    || i == D_REG
	    || i == E_REG
	    || origins[i] == A_REG
	    || origins[i] == X_REG
	    || origins[i] == D_REG
	    || origins[i] == E_REG)
	  {
	    origins[i] = i;
	    age[i] = 0;
	  }
    }
  else if (get_attr_valloc (insn) == VALLOC_DIVSI)
    {
      if (dump_file)
	fprintf (dump_file, "Resetting origin of AX/BC/DE/HL for DIVSI pattern.\n");

      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (i <= 7 || origins[i] <= 7)
	  {
	    origins[i] = i;
	    age[i] = 0;
	  }
    }

  if (GET_CODE (src) == ASHIFT
      || GET_CODE (src) == ASHIFTRT
      || GET_CODE (src) == LSHIFTRT)
    {
      rtx count = XEXP (src, 1);

      if (GET_CODE (count) == REG)
	{
	  /* Special case - our pattern clobbers the count register.  */
	  int r = REGNO (count);

	  if (dump_file)
	    fprintf (dump_file, "Resetting origin of r%d for shift.\n", r);

	  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	    if (i == r || origins[i] == r)
	      {
		origins[i] = i;
		age[i] = 0;
	      }
	}
    }
}

/* The idea behind this optimization is to look for cases where we
   move data from A to B to C, and instead move from A to B, and A to
   C.  If B is a virtual register or memory, this is a big win on its
   own.  If B turns out to be unneeded after this, it's a bigger win.
   For each register, we try to determine where it's value originally
   came from, if it's propogated purely through moves (and not
   computes).  The ORIGINS[] array has the regno for the "origin" of
   the value in the [regno] it's indexed by.  */
static void
rl78_propogate_register_origins (void)
{
  int origins[FIRST_PSEUDO_REGISTER];
  int age[FIRST_PSEUDO_REGISTER];
  int i;
  rtx_insn *insn, *ninsn = NULL;
  rtx pat;

  reset_origins (origins, age);

  for (insn = get_insns (); insn; insn = ninsn)
    {
      ninsn = next_nonnote_nondebug_insn (insn);

      if (dump_file)
	{
	  fprintf (dump_file, "\n");
	  fprintf (dump_file, "Origins:");
	  for (i = 0; i < FIRST_PSEUDO_REGISTER; i ++)
	    if (origins[i] != i)
	      fprintf (dump_file, " r%d=r%d", i, origins[i]);
	  fprintf (dump_file, "\n");
	  print_rtl_single (dump_file, insn);
	}

      switch (GET_CODE (insn))
	{
	case CODE_LABEL:
	case BARRIER:
	case CALL_INSN:
	case JUMP_INSN:
	  reset_origins (origins, age);
	  break;

	default:
	  break;

	case INSN:
	  pat = PATTERN (insn);

	  if (GET_CODE (pat) == PARALLEL)
	    {
	      rtx clobber = XVECEXP (pat, 0, 1);
	      pat = XVECEXP (pat, 0, 0);
	      if (GET_CODE (clobber) == CLOBBER
		  && GET_CODE (XEXP (clobber, 0)) == REG)
		{
		  int cr = REGNO (XEXP (clobber, 0));
		  int mb = GET_MODE_SIZE (GET_MODE (XEXP (clobber, 0)));
		  if (dump_file)
		    fprintf (dump_file, "reset origins of %d regs at %d\n", mb, cr);
		  for (i = 0; i < mb; i++)
		    {
		      origins[cr + i] = cr + i;
		      age[cr + i] = 0;
		    }
		}
	      /* This happens with the DIV patterns.  */
	      else if (GET_CODE (clobber) == SET)
		{
		  set_origin (clobber, insn, origins, age);
		}
	      else
		break;
	    }

	  if (GET_CODE (pat) == SET)
	    {
	      set_origin (pat, insn, origins, age);
	    }
	  else if (GET_CODE (pat) == CLOBBER
		   && GET_CODE (XEXP (pat, 0)) == REG)
	    {
	      if (REG_P (XEXP (pat, 0)))
		{
		  unsigned int reg = REGNO (XEXP (pat, 0));

		  origins[reg] = reg;
		  age[reg] = 0;
		}
	    }
	}
    }
}

/* Remove any SETs where the destination is unneeded.  */
static void
rl78_remove_unused_sets (void)
{
  rtx_insn *insn, *ninsn = NULL;
  rtx dest;

  for (insn = get_insns (); insn; insn = ninsn)
    {
      ninsn = next_nonnote_nondebug_insn (insn);

      rtx set = single_set (insn);
      if (set == NULL)
	continue;

      dest = SET_DEST (set);

      if (GET_CODE (dest) != REG || REGNO (dest) > 23)
	continue;

      if (find_regno_note (insn, REG_UNUSED, REGNO (dest)))
	{
	  if (dump_file)
	    fprintf (dump_file, "deleting because the set register is never used.\n");
	  delete_insn (insn);
	}
    }
}

/* This is the top of the devritualization pass.  */
static void
rl78_reorg (void)
{
  /* split2 only happens when optimizing, but we need all movSIs to be
     split now.  */
  if (optimize <= 0)
    split_all_insns ();

  rl78_alloc_physical_registers ();

  if (dump_file)
    {
      fprintf (dump_file, "\n================DEVIRT:=AFTER=ALLOC=PHYSICAL=REGISTERS================\n");
      print_rtl_with_bb (dump_file, get_insns (), 0);
    }

  rl78_propogate_register_origins ();
  rl78_calculate_death_notes ();

  if (dump_file)
    {
      fprintf (dump_file, "\n================DEVIRT:=AFTER=PROPOGATION=============================\n");
      print_rtl_with_bb (dump_file, get_insns (), 0);
      fprintf (dump_file, "\n======================================================================\n");
    }

  rl78_remove_unused_sets ();

  /* The code after devirtualizing has changed so much that at this point
     we might as well just rescan everything.  Note that
     df_rescan_all_insns is not going to help here because it does not
     touch the artificial uses and defs.  */
  df_finish_pass (true);
  if (optimize > 1)
    df_live_add_problem ();
  df_scan_alloc (NULL);
  df_scan_blocks ();

  if (optimize)
    df_analyze ();
}

#undef  TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY rl78_return_in_memory

static bool
rl78_return_in_memory (const_tree type, const_tree fntype ATTRIBUTE_UNUSED)
{
  const HOST_WIDE_INT size = int_size_in_bytes (type);
  return (size == -1 || size > 8);
}


#undef  TARGET_RTX_COSTS
#define TARGET_RTX_COSTS rl78_rtx_costs

static bool
rl78_rtx_costs (rtx          x,
		machine_mode mode,
		int          outer_code ATTRIBUTE_UNUSED,
		int          opno ATTRIBUTE_UNUSED,
		int *        total,
		bool         speed ATTRIBUTE_UNUSED)
{
  int code = GET_CODE (x);

  if (code == IF_THEN_ELSE)
    {
      *total = COSTS_N_INSNS (10);
      return true;
    }

  if (mode == HImode)
    {
      if (code == MULT && ! speed)
	{
	  * total = COSTS_N_INSNS (8);
	  return true;
	}
      return false;
    }

  if (mode == SImode)
    {
      switch (code)
	{
	case MULT:
	  if (! speed)
	    /* If we are compiling for space then we do not want to use the
	       inline SImode multiplication patterns or shift sequences.
	       The cost is not set to 1 or 5 however as we have to allow for
	       the possibility that we might be converting a leaf function
	       into a non-leaf function.  (There is no way to tell here).
	       A value of 13 seems to be a reasonable compromise for the
	       moment.  */
	    * total = COSTS_N_INSNS (13);
	  else if (RL78_MUL_G14)
	    *total = COSTS_N_INSNS (14);
	  else if (RL78_MUL_G13)
	    *total = COSTS_N_INSNS (29);
	  else
	    *total = COSTS_N_INSNS (500);
	  return true;

	case PLUS:
	  *total = COSTS_N_INSNS (8);
	  return true;

	case ASHIFT:
	case ASHIFTRT:
	case LSHIFTRT:
	  if (GET_CODE (XEXP (x, 1)) == CONST_INT)
	    {
	      switch (INTVAL (XEXP (x, 1)))
		{
		case 0:  *total = COSTS_N_INSNS (0);	break;
		case 1:  *total = COSTS_N_INSNS (6);	break;
		case 2: case 3: case 4: case 5: case 6: case 7:
		  *total = COSTS_N_INSNS (10); break;
		case 8:  *total = COSTS_N_INSNS (6);	break;
		case 9: case 10: case 11: case 12: case 13: case 14: case 15:
		  *total = COSTS_N_INSNS (10); break;
		case 16: *total = COSTS_N_INSNS (3);	break;
		case 17: case 18: case 19: case 20: case 21: case 22: case 23:
		  *total = COSTS_N_INSNS (4); break;
		case 24: *total = COSTS_N_INSNS (4);	break;
		case 25: case 26: case 27: case 28: case 29: case 30: case 31:
		  *total = COSTS_N_INSNS (5); break;
		}
	    }
	  else
	    *total = COSTS_N_INSNS (10+4*16);
	  return true;

	default:
	  break;
	}
    }
  return false;
}


static GTY(()) section * saddr_section;
static GTY(()) section * frodata_section;

int
rl78_saddr_p (rtx x)
{
  const char * c;

  if (MEM_P (x))
    x = XEXP (x, 0);
  if (GET_CODE (x) == PLUS)
    x = XEXP (x, 0);
  if (GET_CODE (x) != SYMBOL_REF)
    return 0;

  c = XSTR (x, 0);
  if (memcmp (c, "@s.", 3) == 0)
    return 1;

  return 0;
}

int
rl78_sfr_p (rtx x)
{
  if (MEM_P (x))
    x = XEXP (x, 0);
  if (GET_CODE (x) != CONST_INT)
    return 0;

  if ((INTVAL (x) & 0xFF00) != 0xFF00)
    return 0;

  return 1;
}

#undef  TARGET_STRIP_NAME_ENCODING
#define TARGET_STRIP_NAME_ENCODING rl78_strip_name_encoding

static const char *
rl78_strip_name_encoding (const char * sym)
{
  while (1)
    {
      if (*sym == '*')
	sym++;
      else if (*sym == '@' && sym[2] == '.')
	sym += 3;
      else
	return sym;
    }
}

/* Like rl78_strip_name_encoding, but does not strip leading asterisks.  This
   is important if the stripped name is going to be passed to assemble_name()
   as that handles asterisk prefixed names in a special manner.  */

static const char *
rl78_strip_nonasm_name_encoding (const char * sym)
{
  while (1)
    {
      if (*sym == '@' && sym[2] == '.')
	sym += 3;
      else
	return sym;
    }
}


static int
rl78_attrlist_to_encoding (tree list, tree decl ATTRIBUTE_UNUSED)
{
  while (list)
    {
      if (is_attribute_p ("saddr", TREE_PURPOSE (list)))
	return 's';
      list = TREE_CHAIN (list);
    }

  return 0;
}

#define RL78_ATTRIBUTES(decl)				\
  (TYPE_P (decl)) ? TYPE_ATTRIBUTES (decl)		\
                : DECL_ATTRIBUTES (decl)		\
                  ? (DECL_ATTRIBUTES (decl))		\
		  : TYPE_ATTRIBUTES (TREE_TYPE (decl))

#undef  TARGET_ENCODE_SECTION_INFO
#define TARGET_ENCODE_SECTION_INFO rl78_encode_section_info

static void
rl78_encode_section_info (tree decl, rtx rtl, int first)
{
  rtx rtlname;
  const char * oldname;
  char encoding;
  char * newname;
  tree idp;
  tree type;
  tree rl78_attributes;

  if (!first)
    return;

  rtlname = XEXP (rtl, 0);

  if (GET_CODE (rtlname) == SYMBOL_REF)
    oldname = XSTR (rtlname, 0);
  else if (GET_CODE (rtlname) == MEM
	   && GET_CODE (XEXP (rtlname, 0)) == SYMBOL_REF)
    oldname = XSTR (XEXP (rtlname, 0), 0);
  else
    gcc_unreachable ();

  type = TREE_TYPE (decl);
  if (type == error_mark_node)
    return;
  if (! DECL_P (decl))
    return;
  rl78_attributes = RL78_ATTRIBUTES (decl);

  encoding = rl78_attrlist_to_encoding (rl78_attributes, decl);

  if (encoding)
    {
      newname = (char *) alloca (strlen (oldname) + 4);
      sprintf (newname, "@%c.%s", encoding, oldname);
      idp = get_identifier (newname);
      XEXP (rtl, 0) =
	gen_rtx_SYMBOL_REF (Pmode, IDENTIFIER_POINTER (idp));
      SYMBOL_REF_WEAK (XEXP (rtl, 0)) = DECL_WEAK (decl);
      SET_SYMBOL_REF_DECL (XEXP (rtl, 0), decl);
    }
}

#undef  TARGET_ASM_INIT_SECTIONS
#define TARGET_ASM_INIT_SECTIONS 	rl78_asm_init_sections

static void
rl78_asm_init_sections (void)
{
  saddr_section
    = get_unnamed_section (SECTION_WRITE, output_section_asm_op,
			   "\t.section .saddr,\"aw\",@progbits");
  frodata_section
    = get_unnamed_section (SECTION_WRITE, output_section_asm_op,
			   "\t.section .frodata,\"aw\",@progbits");
}

#undef  TARGET_ASM_SELECT_SECTION
#define TARGET_ASM_SELECT_SECTION	rl78_select_section

static section *
rl78_select_section (tree decl,
		     int reloc,
		     unsigned HOST_WIDE_INT align)
{
  int readonly = 1;

  switch (TREE_CODE (decl))
    {
    case VAR_DECL:
      if (!TREE_READONLY (decl)
	  || TREE_SIDE_EFFECTS (decl)
	  || !DECL_INITIAL (decl)
	  || (DECL_INITIAL (decl) != error_mark_node
	      && !TREE_CONSTANT (DECL_INITIAL (decl))))
	readonly = 0;
      break;
    case CONSTRUCTOR:
      if (! TREE_CONSTANT (decl))
	readonly = 0;
      break;

    default:
      break;
    }

  if (TREE_CODE (decl) == VAR_DECL)
    {
      const char *name = XSTR (XEXP (DECL_RTL (decl), 0), 0);

      if (name[0] == '@' && name[2] == '.')
	switch (name[1])
	  {
	  case 's':
	    return saddr_section;
	  }

      if (TYPE_ADDR_SPACE (TREE_TYPE (decl)) == ADDR_SPACE_FAR
	  && readonly)
	{
	  return frodata_section;
	}
    }

  if (readonly)
    return TARGET_ES0 ? frodata_section : readonly_data_section;

  switch (categorize_decl_for_section (decl, reloc))
    {
    case SECCAT_TEXT:   return text_section;
    case SECCAT_DATA:   return data_section;
    case SECCAT_BSS:    return bss_section;
    case SECCAT_RODATA: return TARGET_ES0 ? frodata_section : readonly_data_section;
    default:
      return default_select_section (decl, reloc, align);
    }
}

void
rl78_output_labelref (FILE *file, const char *str)
{
  const char *str2;

  str2 = targetm.strip_name_encoding (str);
  if (str2[0] != '.')
    fputs (user_label_prefix, file);
  fputs (str2, file);
}

void
rl78_output_aligned_common (FILE *stream,
			    tree decl ATTRIBUTE_UNUSED,
			    const char *name,
			    int size, int align, int global)
{
  /* We intentionally don't use rl78_section_tag() here.  */
  if (name[0] == '@' && name[2] == '.')
    {
      const char *sec = 0;
      switch (name[1])
	{
	case 's':
	  switch_to_section (saddr_section);
	  sec = ".saddr";
	  break;
	}
      if (sec)
	{
	  const char *name2;
	  int p2align = 0;

	  while (align > BITS_PER_UNIT)
	    {
	      align /= 2;
	      p2align ++;
	    }
	  name2 = targetm.strip_name_encoding (name);
	  if (global)
	    fprintf (stream, "\t.global\t_%s\n", name2);
	  fprintf (stream, "\t.p2align %d\n", p2align);
	  fprintf (stream, "\t.type\t_%s,@object\n", name2);
	  fprintf (stream, "\t.size\t_%s,%d\n", name2, size);
	  fprintf (stream, "_%s:\n\t.zero\t%d\n", name2, size);
	  return;
	}
    }

  if (!global)
    {
      fprintf (stream, "\t.local\t");
      assemble_name (stream, name);
      fprintf (stream, "\n");
    }
  fprintf (stream, "\t.comm\t");
  assemble_name (stream, name);
  fprintf (stream, ",%u,%u\n", size, align / BITS_PER_UNIT);
}

#undef  TARGET_INSERT_ATTRIBUTES
#define TARGET_INSERT_ATTRIBUTES rl78_insert_attributes

static void
rl78_insert_attributes (tree decl, tree *attributes ATTRIBUTE_UNUSED)
{
  if (TARGET_ES0
      && TREE_CODE (decl) == VAR_DECL
      && TREE_READONLY (decl)
      && TREE_ADDRESSABLE (decl)
      && TYPE_ADDR_SPACE (TREE_TYPE (decl)) == ADDR_SPACE_GENERIC)
    {
      tree type = TREE_TYPE (decl);
      tree attr = TYPE_ATTRIBUTES (type);
      int q = TYPE_QUALS_NO_ADDR_SPACE (type) | ENCODE_QUAL_ADDR_SPACE (ADDR_SPACE_FAR);

      TREE_TYPE (decl) = build_type_attribute_qual_variant (type, attr, q);
    }
}

#undef  TARGET_ASM_INTEGER
#define TARGET_ASM_INTEGER rl78_asm_out_integer

static bool
rl78_asm_out_integer (rtx x, unsigned int size, int aligned_p)
{
  if (default_assemble_integer (x, size, aligned_p))
    return true;

  if (size == 4)
    {
      assemble_integer_with_op (".long\t", x);
      return true;
    }

  return false;
}

#undef  TARGET_UNWIND_WORD_MODE
#define TARGET_UNWIND_WORD_MODE rl78_unwind_word_mode

static machine_mode
rl78_unwind_word_mode (void)
{
  return HImode;
}

#ifndef USE_COLLECT2
#undef  TARGET_ASM_CONSTRUCTOR
#define TARGET_ASM_CONSTRUCTOR rl78_asm_constructor
#undef  TARGET_ASM_DESTRUCTOR
#define TARGET_ASM_DESTRUCTOR  rl78_asm_destructor

static void
rl78_asm_ctor_dtor (rtx symbol, int priority, bool is_ctor)
{
  section *sec;

  if (priority != DEFAULT_INIT_PRIORITY)
    {
      /* This section of the function is based upon code copied
	 from: gcc/varasm.c:get_cdtor_priority_section().  */
      char buf[16];

      sprintf (buf, "%s.%.5u", is_ctor ? ".ctors" : ".dtors",
	       MAX_INIT_PRIORITY - priority);
      sec = get_section (buf, 0, NULL);
    }
  else
    sec = is_ctor ? ctors_section : dtors_section;

  assemble_addr_to_section (symbol, sec);
}

static void
rl78_asm_constructor (rtx symbol, int priority)
{
  rl78_asm_ctor_dtor (symbol, priority, true);
}

static void
rl78_asm_destructor (rtx symbol, int priority)
{
  rl78_asm_ctor_dtor (symbol, priority, false);
}
#endif /* ! USE_COLLECT2 */

/* Scan backwards through the insn chain looking to see if the flags
   have been set for a comparison of OP against OPERAND.  Start with
   the insn *before* the current insn.  */

bool
rl78_flags_already_set (rtx op, rtx operand)
{
  /* We only track the Z flag.  */
  if (GET_CODE (op) != EQ && GET_CODE (op) != NE)
    return false;

  /* This should not happen, but let's be paranoid.  */
  if (current_output_insn == NULL_RTX)
    return false;

  rtx_insn *insn;
  bool res = false;

  for (insn = prev_nonnote_nondebug_insn (current_output_insn);
       insn != NULL_RTX;
       insn = prev_nonnote_nondebug_insn (insn))
    {
      if (LABEL_P (insn))
	break;

      if (! INSN_P (insn))
	continue;

      /* Make sure that the insn can be recognized.  */
      if (recog_memoized (insn) == -1)
	continue;

      enum attr_update_Z updated = get_attr_update_Z (insn);

      rtx set = single_set (insn);
      bool must_break = (set != NULL_RTX && rtx_equal_p (operand, SET_DEST (set)));

      switch (updated)
	{
	case UPDATE_Z_NO:
	  break;
	case UPDATE_Z_CLOBBER:
	  must_break = true;
	  break;
	case UPDATE_Z_UPDATE_Z:
	  res = must_break;
	  must_break = true;
	  break;
	default:
	  gcc_unreachable ();
	}

      if (must_break)
	break;
    }

  /* We have to re-recognize the current insn as the call(s) to
     get_attr_update_Z() above will have overwritten the recog_data cache.  */
  recog_memoized (current_output_insn);
  cleanup_subreg_operands (current_output_insn);
  constrain_operands_cached (current_output_insn, 1);

  return res;
}

const char *
rl78_addsi3_internal (rtx * operands, unsigned int alternative)
{
  /* If we are adding in a constant symbolic address when -mes0
     is active then we know that the address must be <64K and
     that it is invalid to access anything above 64K relative to
     this address.  So we can skip adding in the high bytes.  */
  if (TARGET_ES0
      && GET_CODE (operands[2]) == SYMBOL_REF
      && TREE_CODE (SYMBOL_REF_DECL (operands[2])) == VAR_DECL
      && TREE_READONLY (SYMBOL_REF_DECL (operands[2]))
      && ! TREE_SIDE_EFFECTS (SYMBOL_REF_DECL (operands[2])))
    return "movw ax, %h1\n\taddw ax, %h2\n\tmovw %h0, ax";

  switch (alternative)
    {
    case 0:
    case 1:
      return "movw ax, %h1\n\taddw ax, %h2\n\tmovw %h0, ax\n\tmovw ax, %H1\n\tsknc\n\tincw ax\n\taddw ax, %H2\n\tmovw %H0, ax";
    case 2:
      return "movw ax, %h1\n\taddw ax,%h2\n\tmovw bc, ax\n\tmovw ax, %H1\n\tsknc\n\tincw ax\n\taddw ax, %H2\n\tmovw %H0, ax\n\tmovw ax, bc\n\tmovw %h0, ax";
    default:
      gcc_unreachable ();
    }
}


#undef  TARGET_PREFERRED_RELOAD_CLASS
#define TARGET_PREFERRED_RELOAD_CLASS rl78_preferred_reload_class

static reg_class_t
rl78_preferred_reload_class (rtx x ATTRIBUTE_UNUSED, reg_class_t rclass)
{
  if (rclass == NO_REGS)
    rclass = V_REGS;

  return rclass;
}


struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-rl78.h"
