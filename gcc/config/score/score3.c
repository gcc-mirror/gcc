/* score3.c for Sunplus S+CORE processor
   Copyright (C) 2005, 2007, 2008, 2009, 2010 Free Software Foundation, Inc.
   Contributed by Sunnorth

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
#include "regs.h"
#include "hard-reg-set.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-attr.h"
#include "recog.h"
#include "diagnostic-core.h"
#include "output.h"
#include "tree.h"
#include "function.h"
#include "expr.h"
#include "optabs.h"
#include "flags.h"
#include "reload.h"
#include "tm_p.h"
#include "ggc.h"
#include "gstab.h"
#include "hashtab.h"
#include "debug.h"
#include "target.h"
#include "target-def.h"
#include "integrate.h"
#include "langhooks.h"
#include "cfglayout.h"
#include "score3.h"
#include "df.h"

#define BITSET_P(VALUE, BIT)      (((VALUE) & (1L << (BIT))) != 0)
#define INS_BUF_SZ                128

extern enum reg_class score_char_to_class[256];

static int score3_sdata_max;
static char score3_ins[INS_BUF_SZ + 8];

/* Return true if SYMBOL is a SYMBOL_REF and OFFSET + SYMBOL points
   to the same object as SYMBOL.  */
static int
score3_offset_within_object_p (rtx symbol, HOST_WIDE_INT offset)
{
  if (GET_CODE (symbol) != SYMBOL_REF)
    return 0;

  if (CONSTANT_POOL_ADDRESS_P (symbol)
      && offset >= 0
      && offset < (int)GET_MODE_SIZE (get_pool_mode (symbol)))
    return 1;

  if (SYMBOL_REF_DECL (symbol) != 0
      && offset >= 0
      && offset < int_size_in_bytes (TREE_TYPE (SYMBOL_REF_DECL (symbol))))
    return 1;

  return 0;
}

/* Split X into a base and a constant offset, storing them in *BASE
   and *OFFSET respectively.  */
static void
score3_split_const (rtx x, rtx *base, HOST_WIDE_INT *offset)
{
  *offset = 0;

  if (GET_CODE (x) == CONST)
    x = XEXP (x, 0);

  if (GET_CODE (x) == PLUS && GET_CODE (XEXP (x, 1)) == CONST_INT)
    {
      *offset += INTVAL (XEXP (x, 1));
      x = XEXP (x, 0);
    }

  *base = x;
}

/* Classify symbol X, which must be a SYMBOL_REF or a LABEL_REF.  */
static enum score_symbol_type
score3_classify_symbol (rtx x)
{
  if (GET_CODE (x) == LABEL_REF)
    return SYMBOL_GENERAL;

  gcc_assert (GET_CODE (x) == SYMBOL_REF);

  if (CONSTANT_POOL_ADDRESS_P (x))
    {
      if (GET_MODE_SIZE (get_pool_mode (x)) <= SCORE3_SDATA_MAX)
        return SYMBOL_SMALL_DATA;
      return SYMBOL_GENERAL;
    }
  if (SYMBOL_REF_SMALL_P (x))
    return SYMBOL_SMALL_DATA;
  return SYMBOL_GENERAL;
}

/* Return true if the current function must save REGNO.  */
static int
score3_save_reg_p (unsigned int regno)
{
  /* Check call-saved registers.  */
  if (df_regs_ever_live_p (regno) && !call_used_regs[regno])
    return 1;

  /* We need to save the old frame pointer before setting up a new one.  */
  if (regno == HARD_FRAME_POINTER_REGNUM && frame_pointer_needed)
    return 1;

  /* We need to save the incoming return address if it is ever clobbered
     within the function.  */
  if (regno == RA_REGNUM && df_regs_ever_live_p (regno))
    return 1;

  return 0;
}

/* Return one word of double-word value OP, taking into account the fixed
   endianness of certain registers.  HIGH_P is true to select the high part,
   false to select the low part.  */
static rtx
score3_subw (rtx op, int high_p)
{
  unsigned int byte;
  enum machine_mode mode = GET_MODE (op);

  if (mode == VOIDmode)
    mode = DImode;

  byte = (TARGET_LITTLE_ENDIAN ? high_p : !high_p) ? UNITS_PER_WORD : 0;

  if (GET_CODE (op) == REG && REGNO (op) == HI_REGNUM)
    return gen_rtx_REG (SImode, high_p ? HI_REGNUM : LO_REGNUM);

  if (GET_CODE (op) == MEM)
    return adjust_address (op, SImode, byte);

  return simplify_gen_subreg (SImode, op, mode, byte);
}

static struct score3_frame_info *
score3_cached_frame (void)
{
  static struct score3_frame_info _frame_info;
  return &_frame_info;
}

/* Return the bytes needed to compute the frame pointer from the current
   stack pointer.  SIZE is the size (in bytes) of the local variables.  */
static struct score3_frame_info *
score3_compute_frame_size (HOST_WIDE_INT size)
{
  unsigned int regno;
  struct score3_frame_info *f = score3_cached_frame ();

  memset (f, 0, sizeof (struct score3_frame_info));
  f->gp_reg_size = 0;
  f->mask = 0;
  f->var_size = SCORE3_STACK_ALIGN (size);
  f->args_size = crtl->outgoing_args_size;
  f->cprestore_size = flag_pic ? UNITS_PER_WORD : 0;

  if (f->var_size == 0 && current_function_is_leaf)
    f->args_size = f->cprestore_size = 0;

  if (f->args_size == 0 && cfun->calls_alloca)
    f->args_size = UNITS_PER_WORD;

  f->total_size = f->var_size + f->args_size + f->cprestore_size;
  for (regno = GP_REG_FIRST; regno <= GP_REG_LAST; regno++)
    {
      if (score3_save_reg_p (regno))
        {
          f->gp_reg_size += GET_MODE_SIZE (SImode);
          f->mask |= 1 << (regno - GP_REG_FIRST);
        }
    }

  if (crtl->calls_eh_return)
    {
      unsigned int i;
      for (i = 0;; ++i)
        {
          regno = EH_RETURN_DATA_REGNO (i);
          if (regno == INVALID_REGNUM)
            break;
          f->gp_reg_size += GET_MODE_SIZE (SImode);
          f->mask |= 1 << (regno - GP_REG_FIRST);
        }
    }

  f->total_size += f->gp_reg_size;
  f->num_gp = f->gp_reg_size / UNITS_PER_WORD;

  if (f->mask)
    {
      HOST_WIDE_INT offset;
      offset = (f->args_size + f->cprestore_size + f->var_size
                + f->gp_reg_size - GET_MODE_SIZE (SImode));
      f->gp_sp_offset = offset;
    }
  else
    f->gp_sp_offset = 0;

  return f;
}

/* Return true if X is a valid base register for the given mode.
   Allow only hard registers if STRICT.  */
static int
score3_valid_base_register_p (rtx x, int strict)
{
  if (!strict && GET_CODE (x) == SUBREG)
    x = SUBREG_REG (x);

  return (GET_CODE (x) == REG
          && score3_regno_mode_ok_for_base_p (REGNO (x), strict));
}

/* Return true if X is a valid address for machine mode MODE.  If it is,
   fill in INFO appropriately.  STRICT is true if we should only accept
   hard base registers.  */
static int
score3_classify_address (struct score3_address_info *info,
                         enum machine_mode mode, rtx x, int strict)
{
  info->code = GET_CODE (x);

  switch (info->code)
    {
    case REG:
    case SUBREG:
      info->type = SCORE3_ADD_REG;
      info->reg = x;
      info->offset = const0_rtx;
      return score3_valid_base_register_p (info->reg, strict);
    case PLUS:
      info->type = SCORE3_ADD_REG;
      info->reg = XEXP (x, 0);
      info->offset = XEXP (x, 1);
      return (score3_valid_base_register_p (info->reg, strict)
              && GET_CODE (info->offset) == CONST_INT
              && IMM_IN_RANGE (INTVAL (info->offset), 15, 1));
    case PRE_DEC:
    case POST_DEC:
    case PRE_INC:
    case POST_INC:
      if (GET_MODE_SIZE (mode) > GET_MODE_SIZE (SImode))
        return false;
      info->type = SCORE3_ADD_REG;
      info->reg = XEXP (x, 0);
      info->offset = GEN_INT (GET_MODE_SIZE (mode));
      return score3_valid_base_register_p (info->reg, strict);
    case CONST_INT:
      info->type = SCORE3_ADD_CONST_INT;
      return 1;
    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
      info->type = SCORE3_ADD_SYMBOLIC;
      return (score3_symbolic_constant_p (x, &info->symbol_type)
              && (info->symbol_type == SYMBOL_GENERAL
                  || info->symbol_type == SYMBOL_SMALL_DATA));
    default:
      return 0;
    }
}

bool
score3_return_in_memory (const_tree type, const_tree fndecl ATTRIBUTE_UNUSED)
{
    return ((TYPE_MODE (type) == BLKmode)
            || (int_size_in_bytes (type) > 2 * UNITS_PER_WORD)
            || (int_size_in_bytes (type) == -1));
}

/* Return a legitimate address for REG + OFFSET.  */
static rtx
score3_add_offset (rtx reg, HOST_WIDE_INT offset)
{
  if (!IMM_IN_RANGE (offset, 15, 1))
    {
      reg = expand_simple_binop (GET_MODE (reg), PLUS,
                                 gen_int_mode (offset & 0xffffc000,
                                               GET_MODE (reg)),
                                 reg, NULL, 0, OPTAB_WIDEN);
      offset &= 0x3fff;
    }

  return plus_constant (reg, offset);
}

/* Implement TARGET_ASM_OUTPUT_MI_THUNK.  Generate rtl rather than asm text
   in order to avoid duplicating too much logic from elsewhere.  */
void
score3_output_mi_thunk (FILE *file, tree thunk_fndecl ATTRIBUTE_UNUSED,
                        HOST_WIDE_INT delta, HOST_WIDE_INT vcall_offset,
                        tree function)
{
  rtx this_rtx, temp1, insn, fnaddr;

  /* Pretend to be a post-reload pass while generating rtl.  */
  reload_completed = 1;

  /* Mark the end of the (empty) prologue.  */
  emit_note (NOTE_INSN_PROLOGUE_END);

  /* We need two temporary registers in some cases.  */
  temp1 = gen_rtx_REG (Pmode, 8);

  /* Find out which register contains the "this" pointer.  */
  if (aggregate_value_p (TREE_TYPE (TREE_TYPE (function)), function))
    this_rtx = gen_rtx_REG (Pmode, ARG_REG_FIRST + 1);
  else
    this_rtx = gen_rtx_REG (Pmode, ARG_REG_FIRST);

  /* Add DELTA to THIS_RTX.  */
  if (delta != 0)
    {
      rtx offset = GEN_INT (delta);
      if (!CONST_OK_FOR_LETTER_P (delta, 'L'))
        {
          emit_move_insn (temp1, offset);
          offset = temp1;
        }
      emit_insn (gen_add3_insn (this_rtx, this_rtx, offset));
    }

  /* If needed, add *(*THIS_RTX + VCALL_OFFSET) to THIS_RTX.  */
  if (vcall_offset != 0)
    {
      rtx addr;

      /* Set TEMP1 to *THIS_RTX.  */
      emit_move_insn (temp1, gen_rtx_MEM (Pmode, this_rtx));

      /* Set ADDR to a legitimate address for *THIS_RTX + VCALL_OFFSET.  */
      addr = score3_add_offset (temp1, vcall_offset);

      /* Load the offset and add it to THIS_RTX.  */
      emit_move_insn (temp1, gen_rtx_MEM (Pmode, addr));
      emit_insn (gen_add3_insn (this_rtx, this_rtx, temp1));
    }

  /* Jump to the target function.  */
  fnaddr = XEXP (DECL_RTL (function), 0);
  insn = emit_call_insn (gen_sibcall_internal_score3 (fnaddr, const0_rtx));
  SIBLING_CALL_P (insn) = 1;

  /* Run just enough of rest_of_compilation.  This sequence was
     "borrowed" from alpha.c.  */
  insn = get_insns ();
  insn_locators_alloc ();
  split_all_insns_noflow ();
  shorten_branches (insn);
  final_start_function (insn, file, 1);
  final (insn, file, 1);
  final_end_function ();

  /* Clean up the vars set above.  Note that final_end_function resets
     the global pointer for us.  */
  reload_completed = 0;
}

/* Copy VALUE to a register and return that register.  If new psuedos
   are allowed, copy it into a new register, otherwise use DEST.  */
static rtx
score3_force_temporary (rtx dest, rtx value)
{
  if (can_create_pseudo_p ())
    return force_reg (Pmode, value);
  else
    {
      emit_move_insn (copy_rtx (dest), value);
      return dest;
    }
}

/* Return a LO_SUM expression for ADDR.  TEMP is as for score_force_temporary
   and is used to load the high part into a register.  */
static rtx
score3_split_symbol (rtx temp, rtx addr)
{
  rtx high = score3_force_temporary (temp,
                                     gen_rtx_HIGH (Pmode, copy_rtx (addr)));
  return gen_rtx_LO_SUM (Pmode, high, addr);
}

/* This function is used to implement LEGITIMIZE_ADDRESS.  If X can
   be legitimized in a way that the generic machinery might not expect,
   return the new address.  */
rtx
score3_legitimize_address (rtx x)
{
  enum score_symbol_type symbol_type;

  if (score3_symbolic_constant_p (x, &symbol_type)
      && symbol_type == SYMBOL_GENERAL)
    return score3_split_symbol (0, x);

  if (GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x, 1)) == CONST_INT)
    {
      rtx reg = XEXP (x, 0);
      if (!score3_valid_base_register_p (reg, 0))
        reg = copy_to_mode_reg (Pmode, reg);
      return score3_add_offset (reg, INTVAL (XEXP (x, 1)));
    }

  return x;
}

/* Fill INFO with information about a single argument.  CUM is the
   cumulative state for earlier arguments.  MODE is the mode of this
   argument and TYPE is its type (if known).  NAMED is true if this
   is a named (fixed) argument rather than a variable one.  */
static void
score3_classify_arg (const CUMULATIVE_ARGS *cum, enum machine_mode mode,
                     const_tree type, bool named, struct score3_arg_info *info)
{
  int even_reg_p;
  unsigned int num_words, max_regs;

  even_reg_p = 0;
  if (GET_MODE_CLASS (mode) == MODE_INT
      || GET_MODE_CLASS (mode) == MODE_FLOAT)
    even_reg_p = (GET_MODE_SIZE (mode) > UNITS_PER_WORD);
  else
    if (type != NULL_TREE && TYPE_ALIGN (type) > BITS_PER_WORD && named)
      even_reg_p = 1;

  if (TARGET_MUST_PASS_IN_STACK (mode, type))
    info->reg_offset = ARG_REG_NUM;
  else
    {
      info->reg_offset = cum->num_gprs;
      if (even_reg_p)
        info->reg_offset += info->reg_offset & 1;
    }

  if (mode == BLKmode)
    info->num_bytes = int_size_in_bytes (type);
  else
    info->num_bytes = GET_MODE_SIZE (mode);

  num_words = (info->num_bytes + UNITS_PER_WORD - 1) / UNITS_PER_WORD;
  max_regs = ARG_REG_NUM - info->reg_offset;

  /* Partition the argument between registers and stack.  */
  info->reg_words = MIN (num_words, max_regs);
  info->stack_words = num_words - info->reg_words;

  /* The alignment applied to registers is also applied to stack arguments.  */
  if (info->stack_words)
    {
      info->stack_offset = cum->stack_words;
      if (even_reg_p)
        info->stack_offset += info->stack_offset & 1;
    }
}

/* Set up the stack and frame (if desired) for the function.  */
void
score3_function_prologue (FILE *file, HOST_WIDE_INT size ATTRIBUTE_UNUSED)
{
  const char *fnname;
  struct score3_frame_info *f = score3_cached_frame ();
  HOST_WIDE_INT tsize = f->total_size;

  fnname = XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0);
  if (!flag_inhibit_size_directive)
    {
      fputs ("\t.ent\t", file);
      assemble_name (file, fnname);
      fputs ("\n", file);
    }
  assemble_name (file, fnname);
  fputs (":\n", file);

  if (!flag_inhibit_size_directive)
    {
      fprintf (file,
               "\t.frame\t%s," HOST_WIDE_INT_PRINT_DEC ",%s, %d\t\t"
               "# vars= " HOST_WIDE_INT_PRINT_DEC ", regs= %d"
               ", args= " HOST_WIDE_INT_PRINT_DEC
               ", gp= " HOST_WIDE_INT_PRINT_DEC "\n",
               (reg_names[(frame_pointer_needed)
                ? HARD_FRAME_POINTER_REGNUM : STACK_POINTER_REGNUM]),
               tsize,
               reg_names[RA_REGNUM],
               current_function_is_leaf ? 1 : 0,
               f->var_size,
               f->num_gp,
               f->args_size,
               f->cprestore_size);

      fprintf(file, "\t.mask\t0x%08x," HOST_WIDE_INT_PRINT_DEC "\n",
              f->mask,
              (f->gp_sp_offset - f->total_size));
    }
}

/* Do any necessary cleanup after a function to restore stack, frame,
   and regs.  */
void
score3_function_epilogue (FILE *file,
                          HOST_WIDE_INT size ATTRIBUTE_UNUSED)
{
  if (!flag_inhibit_size_directive)
    {
      const char *fnname;
      fnname = XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0);
      fputs ("\t.end\t", file);
      assemble_name (file, fnname);
      fputs ("\n", file);
    }
}

/* Returns true if X contains a SYMBOL_REF.  */
static bool
score3_symbolic_expression_p (rtx x)
{
  if (GET_CODE (x) == SYMBOL_REF)
    return true;

  if (GET_CODE (x) == CONST)
    return score3_symbolic_expression_p (XEXP (x, 0));

  if (UNARY_P (x))
    return score3_symbolic_expression_p (XEXP (x, 0));

  if (ARITHMETIC_P (x))
    return (score3_symbolic_expression_p (XEXP (x, 0))
            || score3_symbolic_expression_p (XEXP (x, 1)));

  return false;
}

/* Choose the section to use for the constant rtx expression X that has
   mode MODE.  */
section *
score3_select_rtx_section (enum machine_mode mode, rtx x,
                           unsigned HOST_WIDE_INT align)
{
  if (GET_MODE_SIZE (mode) <= SCORE3_SDATA_MAX)
    return get_named_section (0, ".sdata", 0);
  else if (flag_pic && score3_symbolic_expression_p (x))
    return get_named_section (0, ".data.rel.ro", 3);
  else
    return mergeable_constant_section (mode, align, 0);
}

/* Implement TARGET_IN_SMALL_DATA_P.  */
bool
score3_in_small_data_p (const_tree decl)
{
  HOST_WIDE_INT size;

  if (TREE_CODE (decl) == STRING_CST
      || TREE_CODE (decl) == FUNCTION_DECL)
    return false;

  if (TREE_CODE (decl) == VAR_DECL && DECL_SECTION_NAME (decl) != 0)
    {
      const char *name;
      name = TREE_STRING_POINTER (DECL_SECTION_NAME (decl));
      if (strcmp (name, ".sdata") != 0
          && strcmp (name, ".sbss") != 0)
        return true;
      if (!DECL_EXTERNAL (decl))
        return false;
    }
  size = int_size_in_bytes (TREE_TYPE (decl));
  return (size > 0 && size <= SCORE3_SDATA_MAX);
}

/* Implement TARGET_ASM_FILE_START.  */
void
score3_asm_file_start (void)
{
  default_file_start ();
  fprintf (asm_out_file, ASM_COMMENT_START
           "GCC for S+core %s \n", SCORE_GCC_VERSION);

  if (flag_pic)
    fprintf (asm_out_file, "\t.set pic\n");
}

/* Implement TARGET_ASM_FILE_END.  When using assembler macros, emit
   .externs for any small-data variables that turned out to be external.  */
void
score3_asm_file_end (void)
{
  tree name_tree;
  struct extern_list *p;
  if (extern_head)
    {
      fputs ("\n", asm_out_file);
      for (p = extern_head; p != 0; p = p->next)
        {
          name_tree = get_identifier (p->name);
          if (!TREE_ASM_WRITTEN (name_tree)
              && TREE_SYMBOL_REFERENCED (name_tree))
            {
              TREE_ASM_WRITTEN (name_tree) = 1;
              fputs ("\t.extern\t", asm_out_file);
              assemble_name (asm_out_file, p->name);
              fprintf (asm_out_file, ", %d\n", p->size);
            }
        }
    }
}

/* Implement TARGET_OPTION_OVERRIDE hook.  */
void
score3_option_override (void)
{
  flag_pic = false;
  if (!flag_pic)
    score3_sdata_max = (global_options_set.x_g_switch_value
			? g_switch_value
			: SCORE3_DEFAULT_SDATA_MAX);
  else
    {
      score3_sdata_max = 0;
      if (global_options_set.x_g_switch_value && (g_switch_value != 0))
        warning (0, "-fPIC and -G are incompatible");
    }

  score_char_to_class['d'] = G32_REGS;
  score_char_to_class['e'] = G16_REGS;
  score_char_to_class['t'] = T32_REGS;

  score_char_to_class['h'] = HI_REG;
  score_char_to_class['l'] = LO_REG;
  score_char_to_class['x'] = CE_REGS;

  score_char_to_class['q'] = CN_REG;
  score_char_to_class['y'] = LC_REG;
  score_char_to_class['z'] = SC_REG;
  score_char_to_class['a'] = SP_REGS;

  score_char_to_class['c'] = CR_REGS;
}

/* Implement REGNO_REG_CLASS macro.  */
int
score3_reg_class (int regno)
{
  int c;
  gcc_assert (regno >= 0 && regno < FIRST_PSEUDO_REGISTER);

  if (regno == FRAME_POINTER_REGNUM
      || regno == ARG_POINTER_REGNUM)
    return ALL_REGS;

  for (c = 0; c < N_REG_CLASSES; c++)
    if (TEST_HARD_REG_BIT (reg_class_contents[c], regno))
      return c;

  return NO_REGS;
}

/* Implement PREFERRED_RELOAD_CLASS macro.  */
enum reg_class
score3_preferred_reload_class (rtx x ATTRIBUTE_UNUSED, enum reg_class rclass)
{
  if (reg_class_subset_p (G16_REGS, rclass))
    return G16_REGS;
  if (reg_class_subset_p (G32_REGS, rclass))
    return G32_REGS;
  return rclass;
}

/* Implement SECONDARY_INPUT_RELOAD_CLASS
   and SECONDARY_OUTPUT_RELOAD_CLASS macro.  */
enum reg_class
score3_secondary_reload_class (enum reg_class rclass,
                               enum machine_mode mode ATTRIBUTE_UNUSED,
                               rtx x)
{
  int regno = -1;
  if (GET_CODE (x) == REG || GET_CODE(x) == SUBREG)
    regno = true_regnum (x);

  if (!GR_REG_CLASS_P (rclass))
    return GP_REG_P (regno) ? NO_REGS : G32_REGS;
  return NO_REGS;
}

/* Implement CONST_OK_FOR_LETTER_P macro.  */
/* imm constraints
   I        imm16 << 16
   J        uimm5
   K        uimm16
   L        simm16
   M        uimm14
   N        simm14
   O        simm14
   P        simm5
   Q        uimm32  */
int
score3_const_ok_for_letter_p (HOST_WIDE_INT value, char c)
{
  switch (c)
    {
    case 'I': return ((value & 0xffff) == 0);
    case 'J': return IMM_IN_RANGE (value, 5, 0);
    case 'K': return IMM_IN_RANGE (value, 16, 0);
    case 'L': return IMM_IN_RANGE (value, 16, 1);
    case 'M': return IMM_IN_RANGE (value, 14, 0);
    case 'N': return IMM_IN_RANGE (value, 14, 1);
    case 'O': return IMM_IN_RANGE (value, 5, 1);
    case 'P': return IMM_IN_RANGE (value, 6, 1);
    case 'Q': return score_extra_constraint (GEN_INT(value), c);
    default : return 0;
    }
}

/* Implement EXTRA_CONSTRAINT macro.  */
/*
   Q        uimm32
   Z        symbol_ref  */
int
score3_extra_constraint (rtx op, char c)
{
  switch (c)
    {
    case 'Q': return IMM_IN_RANGE (INTVAL(op), 32, 0);
    case 'Z':
      return GET_CODE (op) == SYMBOL_REF;
    default:
      gcc_unreachable ();
    }
}

/* Return truth value on whether or not a given hard register
   can support a given mode.  */
int
score3_hard_regno_mode_ok (unsigned int regno, enum machine_mode mode)
{
  int size = GET_MODE_SIZE (mode);
  enum mode_class mclass = GET_MODE_CLASS (mode);

  if (mclass == MODE_CC)
    return regno == CC_REGNUM;
  else if (regno == FRAME_POINTER_REGNUM
           || regno == ARG_POINTER_REGNUM)
    return mclass == MODE_INT;
  else if (GP_REG_P (regno))
    return !(regno & 1) || (size <= UNITS_PER_WORD);
  else if (CE_REG_P (regno))
    return (mclass == MODE_INT
            && ((size <= UNITS_PER_WORD)
                || (regno == CE_REG_FIRST && size == 2 * UNITS_PER_WORD)));
  else
    return (mclass == MODE_INT) && (size <= UNITS_PER_WORD);
}

/* Implement INITIAL_ELIMINATION_OFFSET.  FROM is either the frame
   pointer or argument pointer.  TO is either the stack pointer or
   hard frame pointer.  */
HOST_WIDE_INT
score3_initial_elimination_offset (int from,
                                   int to ATTRIBUTE_UNUSED)
{
  struct score3_frame_info *f = score3_compute_frame_size (get_frame_size ());
  switch (from)
    {
    case ARG_POINTER_REGNUM:
      return f->total_size;
    case FRAME_POINTER_REGNUM:
      return 0;
    default:
      gcc_unreachable ();
    }
}

/* Implement TARGET_FUNCTION_ARG_ADVANCE hook.  */
void
score3_function_arg_advance (CUMULATIVE_ARGS *cum, enum machine_mode mode,
                             const_tree type, bool named)
{
  struct score3_arg_info info;
  score3_classify_arg (cum, mode, type, named, &info);
  cum->num_gprs = info.reg_offset + info.reg_words;
  if (info.stack_words > 0)
    cum->stack_words = info.stack_offset + info.stack_words;
  cum->arg_number++;
}

/* Implement TARGET_ARG_PARTIAL_BYTES macro.  */
int
score3_arg_partial_bytes (CUMULATIVE_ARGS *cum,
                          enum machine_mode mode, tree type, bool named)
{
  struct score3_arg_info info;
  score3_classify_arg (cum, mode, type, named, &info);
  return info.stack_words > 0 ? info.reg_words * UNITS_PER_WORD : 0;
}

/* Implement TARGET_FUNCTION_ARG hook.  */
rtx
score3_function_arg (const CUMULATIVE_ARGS *cum, enum machine_mode mode,
                     const_tree type, bool named)
{
  struct score3_arg_info info;

  if (mode == VOIDmode || !named)
    return 0;

  score3_classify_arg (cum, mode, type, named, &info);

  if (info.reg_offset == ARG_REG_NUM)
    return 0;

  if (!info.stack_words)
    return gen_rtx_REG (mode, ARG_REG_FIRST + info.reg_offset);
  else
    {
      rtx ret = gen_rtx_PARALLEL (mode, rtvec_alloc (info.reg_words));
      unsigned int i, part_offset = 0;
      for (i = 0; i < info.reg_words; i++)
        {
          rtx reg;
          reg = gen_rtx_REG (SImode, ARG_REG_FIRST + info.reg_offset + i);
          XVECEXP (ret, 0, i) = gen_rtx_EXPR_LIST (SImode, reg,
                                                   GEN_INT (part_offset));
          part_offset += UNITS_PER_WORD;
        }
      return ret;
    }
}

/* Implement FUNCTION_VALUE and LIBCALL_VALUE.  For normal calls,
   VALTYPE is the return type and MODE is VOIDmode.  For libcalls,
   VALTYPE is null and MODE is the mode of the return value.  */
rtx
score3_function_value (const_tree valtype, const_tree func,
		       enum machine_mode mode)
{
  if (valtype)
    {
      int unsignedp;
      mode = TYPE_MODE (valtype);
      unsignedp = TYPE_UNSIGNED (valtype);
      mode = promote_function_mode (valtype, mode, &unsignedp, func, 1);
    }
  return gen_rtx_REG (mode, RT_REGNUM);
}

/* Implement TARGET_ASM_TRAMPOLINE_TEMPLATE.  */

void
score3_asm_trampoline_template (FILE *f)
{
  fprintf (f, "\t.set r1\n");
  fprintf (f, "\tmv! r31, r3\n");
  fprintf (f, "\tnop!\n");
  fprintf (f, "\tbl nextinsn\n");
  fprintf (f, "nextinsn:\n");
  fprintf (f, "\tlw! r1, [r3, 6*4-8]\n");
  fprintf (f, "\tnop!\n");
  fprintf (f, "\tlw r23, [r3, 6*4-4]\n");
  fprintf (f, "\tmv! r3, r31\n");
  fprintf (f, "\tnop!\n");
  fprintf (f, "\tbr! r1\n");
  fprintf (f, "\tnop!\n");
  fprintf (f, "\t.set nor1\n");
}

/* Implement TARGET_TRAMPOLINE_INIT.  */
void
score3_trampoline_init (rtx m_tramp, tree fndecl, rtx chain_value)
{
#define FFCACHE          "_flush_cache"
#define CODE_SIZE        (TRAMPOLINE_INSNS * UNITS_PER_WORD)

  rtx fnaddr = XEXP (DECL_RTL (fndecl), 0);
  rtx addr = XEXP (m_tramp, 0);
  rtx mem;

  emit_block_move (m_tramp, assemble_trampoline_template (),
		   GEN_INT (TRAMPOLINE_SIZE), BLOCK_OP_NORMAL);

  mem = adjust_address (m_tramp, SImode, CODE_SIZE);
  emit_move_insn (mem, fnaddr);
  mem = adjust_address (m_tramp, SImode, CODE_SIZE + GET_MODE_SIZE (SImode));
  emit_move_insn (mem, chain_value);

  emit_library_call (gen_rtx_SYMBOL_REF (Pmode, FFCACHE),
                     LCT_NORMAL, VOIDmode, 2,
                     addr, Pmode,
                     GEN_INT (TRAMPOLINE_SIZE), SImode);
#undef FFCACHE
#undef CODE_SIZE
}

/* This function is used to implement REG_MODE_OK_FOR_BASE_P macro.  */
int
score3_regno_mode_ok_for_base_p (int regno, int strict)
{
  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      if (!strict)
        return 1;
      regno = reg_renumber[regno];
    }
  if (regno == ARG_POINTER_REGNUM
      || regno == FRAME_POINTER_REGNUM)
    return 1;
  return GP_REG_P (regno);
}

/* Implement TARGET_LEGITIMATE_ADDRESS_P macro.  */
bool
score3_legitimate_address_p (enum machine_mode mode, rtx x, bool strict)
{
  struct score3_address_info addr;

  return score3_classify_address (&addr, mode, x, strict);
}

/* Return a number assessing the cost of moving a register in class
   FROM to class TO. */
int
score3_register_move_cost (enum machine_mode mode ATTRIBUTE_UNUSED,
                           enum reg_class from, enum reg_class to)
{
  if (GR_REG_CLASS_P (from))
    {
      if (GR_REG_CLASS_P (to))
        return 2;
      else if (SP_REG_CLASS_P (to))
        return 4;
      else if (CP_REG_CLASS_P (to))
        return 5;
      else if (CE_REG_CLASS_P (to))
        return 6;
    }
  if (GR_REG_CLASS_P (to))
    {
      if (GR_REG_CLASS_P (from))
        return 2;
      else if (SP_REG_CLASS_P (from))
        return 4;
      else if (CP_REG_CLASS_P (from))
        return 5;
      else if (CE_REG_CLASS_P (from))
        return 6;
    }
  return 12;
}

/* Return the number of instructions needed to load a symbol of the
   given type into a register.  */
static int
score3_symbol_insns (enum score_symbol_type type)
{
  switch (type)
    {
    case SYMBOL_GENERAL:
      return 2;

    case SYMBOL_SMALL_DATA:
      return 1;
    }

  gcc_unreachable ();
}

/* Return the number of instructions needed to load or store a value
   of mode MODE at X.  Return 0 if X isn't valid for MODE.  */
static int
score3_address_insns (rtx x, enum machine_mode mode)
{
  struct score3_address_info addr;
  int factor;

  if (mode == BLKmode)
    factor = 1;
  else
    factor = (GET_MODE_SIZE (mode) + UNITS_PER_WORD - 1) / UNITS_PER_WORD;

  if (score3_classify_address (&addr, mode, x, false))
    switch (addr.type)
      {
      case SCORE3_ADD_REG:
      case SCORE3_ADD_CONST_INT:
        return factor;

      case SCORE3_ADD_SYMBOLIC:
        return factor * score3_symbol_insns (addr.symbol_type);
      }
  return 0;
}

/* Implement TARGET_RTX_COSTS macro.  */
bool
score3_rtx_costs (rtx x, int code, int outer_code, int *total,
		  bool speed ATTRIBUTE_UNUSED)
{
  enum machine_mode mode = GET_MODE (x);

  switch (code)
    {
    case CONST_INT:
      if (outer_code == SET)
        {
          if (CONST_OK_FOR_LETTER_P (INTVAL (x), 'I')
              || CONST_OK_FOR_LETTER_P (INTVAL (x), 'L'))
            *total = COSTS_N_INSNS (1);
          else
            *total = COSTS_N_INSNS (2);
        }
      else if (outer_code == PLUS || outer_code == MINUS)
        {
          if (CONST_OK_FOR_LETTER_P (INTVAL (x), 'N'))
            *total = 0;
          else if (CONST_OK_FOR_LETTER_P (INTVAL (x), 'I')
                   || CONST_OK_FOR_LETTER_P (INTVAL (x), 'L'))
            *total = 1;
          else
            *total = COSTS_N_INSNS (2);
        }
      else if (outer_code == AND || outer_code == IOR)
        {
          if (CONST_OK_FOR_LETTER_P (INTVAL (x), 'M'))
            *total = 0;
          else if (CONST_OK_FOR_LETTER_P (INTVAL (x), 'I')
                   || CONST_OK_FOR_LETTER_P (INTVAL (x), 'K'))
            *total = 1;
          else
            *total = COSTS_N_INSNS (2);
        }
      else
        {
          *total = 0;
        }
      return true;

    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
    case CONST_DOUBLE:
      *total = COSTS_N_INSNS (2);
      return true;

    case MEM:
      {
        /* If the address is legitimate, return the number of
           instructions it needs, otherwise use the default handling.  */
        int n = score3_address_insns (XEXP (x, 0), GET_MODE (x));
        if (n > 0)
          {
            *total = COSTS_N_INSNS (n + 1);
            return true;
          }
        return false;
      }

    case FFS:
      *total = COSTS_N_INSNS (6);
      return true;

    case NOT:
      *total = COSTS_N_INSNS (1);
      return true;

    case AND:
    case IOR:
    case XOR:
      if (mode == DImode)
        {
          *total = COSTS_N_INSNS (2);
          return true;
        }
      return false;

    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
      if (mode == DImode)
        {
          *total = COSTS_N_INSNS ((GET_CODE (XEXP (x, 1)) == CONST_INT)
                                  ? 4 : 12);
          return true;
        }
      return false;

    case ABS:
      *total = COSTS_N_INSNS (4);
      return true;

    case PLUS:
    case MINUS:
      if (mode == DImode)
        {
          *total = COSTS_N_INSNS (4);
          return true;
        }
      *total = COSTS_N_INSNS (1);
      return true;

    case NEG:
      if (mode == DImode)
        {
          *total = COSTS_N_INSNS (4);
          return true;
        }
      return false;

    case MULT:
      *total = optimize_size ? COSTS_N_INSNS (2) : COSTS_N_INSNS (12);
      return true;

    case DIV:
    case MOD:
    case UDIV:
    case UMOD:
      *total = optimize_size ? COSTS_N_INSNS (2) : COSTS_N_INSNS (33);
      return true;

    case SIGN_EXTEND:
    case ZERO_EXTEND:
      switch (GET_MODE (XEXP (x, 0)))
        {
        case QImode:
        case HImode:
          if (GET_CODE (XEXP (x, 0)) == MEM)
            {
              *total = COSTS_N_INSNS (2);

              if (!TARGET_LITTLE_ENDIAN &&
                  side_effects_p (XEXP (XEXP (x, 0), 0)))
                *total = 100;
            }
          else
            *total = COSTS_N_INSNS (1);
          break;

        default:
          *total = COSTS_N_INSNS (1);
          break;
        }
      return true;

    default:
      return false;
    }
}

/* Implement TARGET_ADDRESS_COST macro.  */
int
score3_address_cost (rtx addr)
{
  return score3_address_insns (addr, SImode);
}

/* Implement ASM_OUTPUT_EXTERNAL macro.  */
int
score3_output_external (FILE *file ATTRIBUTE_UNUSED,
                        tree decl, const char *name)
{
  register struct extern_list *p;

  if (score3_in_small_data_p (decl))
    {
      p = ggc_alloc_extern_list ();
      p->next = extern_head;
      p->name = name;
      p->size = int_size_in_bytes (TREE_TYPE (decl));
      extern_head = p;
    }
  return 0;
}

/* Implement RETURN_ADDR_RTX.  Note, we do not support moving
   back to a previous frame.  */
rtx
score3_return_addr (int count, rtx frame ATTRIBUTE_UNUSED)
{
  if (count != 0)
    return const0_rtx;
  return get_hard_reg_initial_val (Pmode, RA_REGNUM);
}

/* Implement PRINT_OPERAND macro.  */
/* Score-specific operand codes:
   '['        print .set nor1 directive
   ']'        print .set r1 directive
   'U'        print hi part of a CONST_INT rtx
   'E'        print log2(v)
   'F'        print log2(~v)
   'D'        print SFmode const double
   'S'        selectively print "!" if operand is 15bit instruction accessible
   'V'        print "v!" if operand is 15bit instruction accessible, or "lfh!"
   'L'        low  part of DImode reg operand
   'H'        high part of DImode reg operand
   'C'        print part of opcode for a branch condition.  */
void
score3_print_operand (FILE *file, rtx op, int c)
{
  enum rtx_code code = UNKNOWN;
  if (!PRINT_OPERAND_PUNCT_VALID_P (c))
    code = GET_CODE (op);

  if (c == '[')
    {
      fprintf (file, ".set r1\n");
    }
  else if (c == ']')
    {
      fprintf (file, "\n\t.set nor1");
    }
  else if (c == 'U')
    {
      gcc_assert (code == CONST_INT);
      fprintf (file, HOST_WIDE_INT_PRINT_HEX,
               (INTVAL (op) >> 16) & 0xffff);
    }
  else if (c == 'D')
    {
      if (GET_CODE (op) == CONST_DOUBLE)
        {
          rtx temp = gen_lowpart (SImode, op);
          gcc_assert (GET_MODE (op) == SFmode);
          fprintf (file, HOST_WIDE_INT_PRINT_HEX, INTVAL (temp) & 0xffffffff);
        }
      else
        output_addr_const (file, op);
    }
  else if (c == 'S')
    {
      gcc_assert (code == REG);
      if (G16_REG_P (REGNO (op)))
        fprintf (file, "!");
    }
  else if (c == 'V')
    {
      gcc_assert (code == REG);
      fprintf (file, G16_REG_P (REGNO (op)) ? "v!" : "lfh!");
    }
  else if (c == 'C')
    {
      enum machine_mode mode = GET_MODE (XEXP (op, 0));

      switch (code)
        {
        case EQ: fputs ("eq!", file); break;
        case NE: fputs ("ne!", file); break;
        case GT: fputs ("gt!", file); break;
        case GE: fputs (mode != CCmode ? "pl" : "ge", file); break;
        case LT: fputs (mode != CCmode ? "mi" : "lt", file); break;
        case LE: fputs ("le!", file); break;
        case GTU: fputs ("gtu!", file); break;
        case GEU: fputs ("cs", file); break;
        case LTU: fputs ("cc", file); break;
        case LEU: fputs ("leu!", file); break;
        default:
          output_operand_lossage ("invalid operand for code: '%c'", code);
        }
    }
  else if (c == 'G')  /* Seperate from b<cond>, use for mv<cond>.  */
    {
      enum machine_mode mode = GET_MODE (XEXP (op, 0));

      switch (code)
        {
        case EQ: fputs ("eq", file); break;
        case NE: fputs ("ne", file); break;
        case GT: fputs ("gt", file); break;
        case GE: fputs (mode != CCmode ? "pl" : "ge", file); break;
        case LT: fputs (mode != CCmode ? "mi" : "lt", file); break;
        case LE: fputs ("le", file); break;
        case GTU: fputs ("gtu", file); break;
        case GEU: fputs ("cs", file); break;
        case LTU: fputs ("cc", file); break;
        case LEU: fputs ("leu", file); break;
        default:
          output_operand_lossage ("invalid operand for code: '%c'", code);
        }
    }
  else if (c == 'E')
    {
      unsigned HOST_WIDE_INT i;
      unsigned HOST_WIDE_INT pow2mask = 1;
      unsigned HOST_WIDE_INT val;

      val = INTVAL (op);
      for (i = 0; i < 32; i++)
        {
          if (val == pow2mask)
            break;
          pow2mask <<= 1;
        }
      gcc_assert (i < 32);
      fprintf (file, HOST_WIDE_INT_PRINT_HEX, i);
    }
  else if (c == 'F')
    {
      unsigned HOST_WIDE_INT i;
      unsigned HOST_WIDE_INT pow2mask = 1;
      unsigned HOST_WIDE_INT val;

      val = ~INTVAL (op);
      for (i = 0; i < 32; i++)
        {
          if (val == pow2mask)
            break;
          pow2mask <<= 1;
        }
      gcc_assert (i < 32);
      fprintf (file, HOST_WIDE_INT_PRINT_HEX, i);
    }
  else if (code == REG)
    {
      int regnum = REGNO (op);
      if ((c == 'H' && !WORDS_BIG_ENDIAN)
          || (c == 'L' && WORDS_BIG_ENDIAN))
        regnum ++;
      fprintf (file, "%s", reg_names[regnum]);
    }
  else
    {
      switch (code)
        {
        case MEM:
          score3_print_operand_address (file, op);
          break;
        default:
          output_addr_const (file, op);
        }
    }
}

/* Implement PRINT_OPERAND_ADDRESS macro.  */
void
score3_print_operand_address (FILE *file, rtx x)
{
  struct score3_address_info addr;
  enum rtx_code code = GET_CODE (x);
  enum machine_mode mode = GET_MODE (x);

  if (code == MEM)
    x = XEXP (x, 0);

  if (score3_classify_address (&addr, mode, x, true))
    {
      switch (addr.type)
        {
        case SCORE3_ADD_REG:
          {
            switch (addr.code)
              {
              case PRE_DEC:
                fprintf (file, "[%s,-%ld]+", reg_names[REGNO (addr.reg)],
                         INTVAL (addr.offset));
                break;
              case POST_DEC:
                fprintf (file, "[%s]+,-%ld", reg_names[REGNO (addr.reg)],
                         INTVAL (addr.offset));
                break;
              case PRE_INC:
                fprintf (file, "[%s, %ld]+", reg_names[REGNO (addr.reg)],
                         INTVAL (addr.offset));
                break;
              case POST_INC:
                fprintf (file, "[%s]+, %ld", reg_names[REGNO (addr.reg)],
                         INTVAL (addr.offset));
                break;
              default:
                if (INTVAL(addr.offset) == 0)
                  fprintf(file, "[%s]", reg_names[REGNO (addr.reg)]);
                else
                  fprintf(file, "[%s, %ld]", reg_names[REGNO (addr.reg)],
                          INTVAL(addr.offset));
                break;
              }
          }
          return;
        case SCORE3_ADD_CONST_INT:
        case SCORE3_ADD_SYMBOLIC:
          output_addr_const (file, x);
          return;
        }
    }
  print_rtl (stderr, x);
  gcc_unreachable ();
}

/* Implement SELECT_CC_MODE macro.  */
enum machine_mode
score3_select_cc_mode (enum rtx_code op, rtx x, rtx y)
{
  if ((op == EQ || op == NE || op == LT || op == GE)
      && y == const0_rtx
      && GET_MODE (x) == SImode)
    {
      switch (GET_CODE (x))
        {
        case PLUS:
        case MINUS:
        case NEG:
        case AND:
        case IOR:
        case XOR:
        case NOT:
        case ASHIFT:
        case LSHIFTRT:
        case ASHIFTRT:
          return CC_NZmode;

        case SIGN_EXTEND:
        case ZERO_EXTEND:
        case ROTATE:
        case ROTATERT:
          return (op == LT || op == GE) ? CC_Nmode : CCmode;

        default:
          return CCmode;
        }
    }

  if ((op == EQ || op == NE)
      && (GET_CODE (y) == NEG)
      && register_operand (XEXP (y, 0), SImode)
      && register_operand (x, SImode))
    {
      return CC_NZmode;
    }

  return CCmode;
}

#define EMIT_PL(_rtx)        RTX_FRAME_RELATED_P (_rtx) = 1
/* return 0, no more bit set in mask.  */
static int rpush_first (int mask, int sb, int *rd)
{
  int i, cnt = 1;

  if ((mask & (1 << sb)) == 0)
    return 0;

  *rd = sb;

  for (i = sb-1; i >= 0; i--)
    {
      if (mask & (1 << i))
        {
          cnt ++;
          continue;
        }

      *rd = i+1;
      break;;
    }

  return cnt;
}

static void
rpush (int rd, int cnt)
{
  rtx mem = gen_rtx_MEM (SImode, gen_rtx_PRE_DEC (SImode, stack_pointer_rtx));
  rtx reg = gen_rtx_REG (SImode, rd);

  if (!crtl->calls_eh_return)
    MEM_READONLY_P (mem) = 1;

  if (cnt == 1)
    EMIT_PL (emit_insn (gen_pushsi_score3 (mem, reg)));
  else
    {
      int i;
      rtx insn = gen_store_multiple (gen_rtx_MEM (SImode, stack_pointer_rtx),
                                     gen_rtx_REG (SImode, rd),
                                     GEN_INT (cnt));

      rtx pat = PATTERN (insn);

      for (i = 0; i < XVECLEN (pat, 0); i++)
        if (GET_CODE (XVECEXP (pat, 0, i)) == SET)
          RTX_FRAME_RELATED_P (XVECEXP (pat, 0, i)) = 1;

      EMIT_PL (emit_insn (insn));
    }
}

/* Generate the prologue instructions for entry into a S+core function.  */
void
score3_prologue (void)
{
  struct score3_frame_info *f = score3_compute_frame_size (get_frame_size ());
  HOST_WIDE_INT size;
  int regno;

  size = f->total_size - f->gp_reg_size;

  if (flag_pic)
    emit_insn (gen_cpload_score3 ());

  {
    int cnt, rd;

    for (regno = (int) GP_REG_LAST; regno >= (int) GP_REG_FIRST; regno--)
      {
        cnt = rpush_first (f->mask, regno, &rd);
        if (cnt != 0)
          {
            rpush (rd, cnt);
            regno = regno - cnt;
          }
      }
  }

  if (size > 0)
    {
      rtx insn;

      if (CONST_OK_FOR_LETTER_P (-size, 'L'))
        EMIT_PL (emit_insn (gen_add3_insn (stack_pointer_rtx,
                                           stack_pointer_rtx,
                                           GEN_INT (-size))));
      else
        {
          EMIT_PL (emit_move_insn (gen_rtx_REG (Pmode, SCORE3_PROLOGUE_TEMP_REGNUM),
                                   GEN_INT (size)));
          EMIT_PL (emit_insn
                   (gen_sub3_insn (stack_pointer_rtx,
                                   stack_pointer_rtx,
                                   gen_rtx_REG (Pmode,
                                                SCORE3_PROLOGUE_TEMP_REGNUM))));
        }
      insn = get_last_insn ();
      REG_NOTES (insn) =
        alloc_EXPR_LIST (REG_FRAME_RELATED_EXPR,
                         gen_rtx_SET (VOIDmode, stack_pointer_rtx,
                                      plus_constant (stack_pointer_rtx,
                                                     -size)),
                                      REG_NOTES (insn));
    }

  if (frame_pointer_needed)
    EMIT_PL (emit_move_insn (hard_frame_pointer_rtx, stack_pointer_rtx));

  if (flag_pic && f->cprestore_size)
    {
      if (frame_pointer_needed)
        emit_insn (gen_cprestore_use_fp_score3 (GEN_INT (size - f->cprestore_size)));
      else
        emit_insn (gen_cprestore_use_sp_score3 (GEN_INT (size - f->cprestore_size)));
    }
}

/* return 0, no more bit set in mask.  */
static int
rpop_first (int mask, int sb, int *rd)
{
  int i, cnt = 1;

  if ((mask & (1 << sb)) == 0)
    return 0;

  *rd = sb;

  for (i = sb+1; i < 32; i++)
    if (mask & (1 << i))
      cnt++;
    else
      break;;

  return cnt;
}

static void
rpop (int rd, int cnt)
{
  rtx mem = gen_rtx_MEM (SImode, gen_rtx_POST_INC (SImode, stack_pointer_rtx));
  rtx reg = gen_rtx_REG (SImode, rd);

  if (!crtl->calls_eh_return)
    MEM_READONLY_P (mem) = 1;

  if (cnt == 1)
    emit_insn (gen_popsi_score3 (reg, mem));
  else
    emit_insn (gen_load_multiple (reg,
                                  gen_rtx_MEM (SImode, stack_pointer_rtx),
                                  GEN_INT (cnt)));
}

/* Generate the epilogue instructions in a S+core function.  */
void
score3_epilogue (int sibcall_p)
{
  struct score3_frame_info *f = score3_compute_frame_size (get_frame_size ());
  HOST_WIDE_INT size;
  int regno;
  rtx base;

  size = f->total_size - f->gp_reg_size;

  if (!frame_pointer_needed)
    base = stack_pointer_rtx;
  else
    base = hard_frame_pointer_rtx;

  if (size)
    {
      if (CONST_OK_FOR_LETTER_P (size, 'L'))
        emit_insn (gen_add3_insn (base, base, GEN_INT (size)));
      else
        {
          emit_move_insn (gen_rtx_REG (Pmode, SCORE3_EPILOGUE_TEMP_REGNUM),
                          GEN_INT (size));
          emit_insn (gen_add3_insn (base, base,
                                    gen_rtx_REG (Pmode,
                                                 SCORE3_EPILOGUE_TEMP_REGNUM)));
        }
    }

  if (base != stack_pointer_rtx)
    emit_move_insn (stack_pointer_rtx, base);

  if (crtl->calls_eh_return)
    emit_insn (gen_add3_insn (stack_pointer_rtx,
                              stack_pointer_rtx,
                              EH_RETURN_STACKADJ_RTX));

  {
    int cnt, rd;

    for (regno = (int) GP_REG_FIRST; regno <= (int) GP_REG_LAST; regno++)
      {
        cnt = rpop_first (f->mask, regno, &rd);
        if (cnt != 0)
          {
            rpop (rd, cnt);
            regno = regno + cnt;
          }
      }
  }

  if (!sibcall_p)
    emit_jump_insn (gen_return_internal_score3 (gen_rtx_REG (Pmode, RA_REGNUM)));
}

/* Return true if X is a symbolic constant that can be calculated in
   the same way as a bare symbol.  If it is, store the type of the
   symbol in *SYMBOL_TYPE.  */
int
score3_symbolic_constant_p (rtx x, enum score_symbol_type *symbol_type)
{
  HOST_WIDE_INT offset;

  score3_split_const (x, &x, &offset);
  if (GET_CODE (x) == SYMBOL_REF || GET_CODE (x) == LABEL_REF)
    *symbol_type = score3_classify_symbol (x);
  else
    return 0;

  if (offset == 0)
    return 1;

  /* if offset > 15bit, must reload  */
  if (!IMM_IN_RANGE (offset, 15, 1))
    return 0;

  switch (*symbol_type)
    {
    case SYMBOL_GENERAL:
      return 1;
    case SYMBOL_SMALL_DATA:
      return score3_offset_within_object_p (x, offset);
    }
  gcc_unreachable ();
}

void
score3_movsicc (rtx *ops)
{
  enum machine_mode mode;

  mode = score3_select_cc_mode (GET_CODE (ops[1]), ops[2], ops[3]);
  emit_insn (gen_rtx_SET (VOIDmode, gen_rtx_REG (mode, CC_REGNUM),
                          gen_rtx_COMPARE (mode, XEXP (ops[1], 0),
					   XEXP (ops[1], 1))));
}

/* Call and sibcall pattern all need call this function.  */
void
score3_call (rtx *ops, bool sib)
{
  rtx addr = XEXP (ops[0], 0);
  if (!call_insn_operand (addr, VOIDmode))
    {
      rtx oaddr = addr;
      addr = gen_reg_rtx (Pmode);
      gen_move_insn (addr, oaddr);
    }

  if (sib)
    emit_call_insn (gen_sibcall_internal_score3 (addr, ops[1]));
  else
    emit_call_insn (gen_call_internal_score3 (addr, ops[1]));
}

/* Call value and sibcall value pattern all need call this function.  */
void
score3_call_value (rtx *ops, bool sib)
{
  rtx result = ops[0];
  rtx addr = XEXP (ops[1], 0);
  rtx arg = ops[2];

  if (!call_insn_operand (addr, VOIDmode))
    {
      rtx oaddr = addr;
      addr = gen_reg_rtx (Pmode);
      gen_move_insn (addr, oaddr);
    }

  if (sib)
    emit_call_insn (gen_sibcall_value_internal_score3 (result, addr, arg));
  else
    emit_call_insn (gen_call_value_internal_score3 (result, addr, arg));
}

/* Machine Split  */
void
score3_movdi (rtx *ops)
{
  rtx dst = ops[0];
  rtx src = ops[1];
  rtx dst0 = score3_subw (dst, 0);
  rtx dst1 = score3_subw (dst, 1);
  rtx src0 = score3_subw (src, 0);
  rtx src1 = score3_subw (src, 1);

  if (GET_CODE (dst0) == REG && reg_overlap_mentioned_p (dst0, src))
    {
      emit_move_insn (dst1, src1);
      emit_move_insn (dst0, src0);
    }
  else
    {
      emit_move_insn (dst0, src0);
      emit_move_insn (dst1, src1);
    }
}

void
score3_zero_extract_andi (rtx *ops)
{
  if (INTVAL (ops[1]) == 1 && const_uimm5 (ops[2], SImode))
    emit_insn (gen_zero_extract_bittst_score3 (ops[0], ops[2]));
  else
    {
      unsigned HOST_WIDE_INT mask;
      mask = (0xffffffffU & ((1U << INTVAL (ops[1])) - 1U));
      mask = mask << INTVAL (ops[2]);
      emit_insn (gen_andsi3_cmp_score3 (ops[3], ops[0],
                                        gen_int_mode (mask, SImode)));
    }
}

const char *
score3_rpush (rtx *ops)
{
  snprintf (score3_ins, INS_BUF_SZ, "rpush!\t%%1, %d", XVECLEN (ops[0], 0));
  return score3_ins;
}

const char *
score3_rpop (rtx *ops)
{
  snprintf (score3_ins, INS_BUF_SZ, "rpop!\t%%1, %d", XVECLEN (ops[0], 0));
  return score3_ins;
}

/* Output asm code for ld/sw insn.  */
static int
score3_pr_addr_post (rtx *ops, int idata, int iaddr, char *ip,
                     enum score_mem_unit unit ATTRIBUTE_UNUSED)
{
  struct score3_address_info ai;

  gcc_assert (GET_CODE (ops[idata]) == REG);
  gcc_assert (score3_classify_address (&ai, SImode, XEXP (ops[iaddr], 0), true));

  if (ai.type == SCORE3_ADD_REG
      && ai.code == REG
      && GET_CODE (ai.offset) == CONST_INT
      && G16_REG_P (REGNO (ops[idata]))
      && G8_REG_P (REGNO (ai.reg))
      && ((INTVAL (ai.offset) & 3) == 0)
      && (IMM_IN_RANGE (INTVAL (ai.offset), 7, 0)))
    {
      ops[iaddr] = ai.reg;
      return snprintf (ip, INS_BUF_SZ, "!\t%%%d, [%%%d, "
                       HOST_WIDE_INT_PRINT_DEC "]",
                       idata, iaddr, INTVAL (ai.offset));
    }

  if (ai.type == SCORE3_ADD_SYMBOLIC)
    return snprintf (ip, INS_BUF_SZ, "48\t%%%d, %%a%d", idata, iaddr);

  return snprintf (ip, INS_BUF_SZ, "\t%%%d, %%a%d", idata, iaddr);
}

/* Output asm insn for load.  */
const char *
score3_linsn (rtx *ops, enum score_mem_unit unit, bool sign)
{
  const char *pre_ins[] =
    {"lbu", "lhu", "lw", "??", "lb", "lh", "lw", "??"};
  char *ip;

  strcpy (score3_ins, pre_ins[(sign ? 4 : 0) + unit]);
  ip = score3_ins + strlen (score3_ins);

  if (unit == SCORE_WORD)
    score3_pr_addr_post (ops, 0, 1, ip, unit);
  else
    snprintf (ip, INS_BUF_SZ, "\t%%0, %%a1");

  return score3_ins;
}

/* Output asm insn for store.  */
const char *
score3_sinsn (rtx *ops, enum score_mem_unit unit)
{
  const char *pre_ins[] = {"sb", "sh", "sw"};
  char *ip;

  strcpy (score3_ins, pre_ins[unit]);
  ip = score3_ins + strlen (score3_ins);

  if (unit == SCORE_WORD)
    score3_pr_addr_post (ops, 1, 0, ip, unit);
  else
    snprintf (ip, INS_BUF_SZ, "\t%%1, %%a0");

  return score3_ins;
}

/* Output asm insn for load immediate.  */
const char *
score3_limm (rtx *ops)
{
  HOST_WIDE_INT v;

  gcc_assert (GET_CODE (ops[0]) == REG);
  gcc_assert (GET_CODE (ops[1]) == CONST_INT);

  v = INTVAL (ops[1]);
  if (G16_REG_P (REGNO (ops[0])) && IMM_IN_RANGE (v, 5, 0))
    return "ldiu!\t%0, %c1";
  else if (IMM_IN_RANGE (v, 16, 1))
    return "ldi\t%0, %c1";
  else if ((v & 0xffff) == 0)
    return "ldis\t%0, %U1";
  else
    return "li\t%0, %c1";
}

/* Output asm insn for move.  */
const char *
score3_move (rtx *ops)
{
  gcc_assert (GET_CODE (ops[0]) == REG);
  gcc_assert (GET_CODE (ops[1]) == REG);

  return "mv!\t%0, %1";
}

/* Generate add insn.  */
const char *
score3_select_add_imm (rtx *ops, bool set_cc)
{
  HOST_WIDE_INT v = INTVAL (ops[2]);

  gcc_assert (GET_CODE (ops[2]) == CONST_INT);
  gcc_assert (REGNO (ops[0]) == REGNO (ops[1]));

  if (set_cc)
    return "addi.c\t%0, %c2";
  else
    if (IMM_IN_RANGE (v, 6, 1) && G16_REG_P (REGNO (ops[0])))
      return "addi!\t%0, %c2";
    else
      return "addi\t%0, %c2";
}

/* Output arith insn.  */
const char *
score3_select (rtx *ops, const char *inst_pre, bool commu ATTRIBUTE_UNUSED,
               const char *letter, bool set_cc)
{
  gcc_assert (GET_CODE (ops[0]) == REG);
  gcc_assert (GET_CODE (ops[1]) == REG);

  if (set_cc)
    snprintf (score3_ins, INS_BUF_SZ, "%s.c\t%%0, %%1, %%%s2", inst_pre, letter);
  else
    snprintf (score3_ins, INS_BUF_SZ, "%s\t%%0, %%1, %%%s2", inst_pre, letter);
  return score3_ins;
}

/* Output a Score3 casesi instruction.  */
const char *
score3_output_casesi (rtx *operands)
{
  rtx diff_vec = PATTERN (next_real_insn (operands[2]));
  gcc_assert (GET_CODE (diff_vec) == ADDR_DIFF_VEC);

  output_asm_insn ("cmpi.c\t%0, %1", operands);
  output_asm_insn ("bgtu\t%3", operands);
  switch (GET_MODE(diff_vec))
    {
    case QImode:
      output_asm_insn ("ldi48\t%4, %2", operands);
      output_asm_insn ("ltbb\t%4, [%4, %0]\n%2_tbb:", operands);
      return "brr!\t%4";
    case HImode:
      output_asm_insn ("ldi48\t%4, %2", operands);
      output_asm_insn ("ltbh\t%4, [%4, %0]\n%2_tbb:", operands);
      return "brr!\t%4";
    case SImode:
      output_asm_insn ("ldi48\t%4, %2", operands);
      output_asm_insn ("ltbw\t%4, [%4, %0]", operands);
      return "br!\t%4";
    default:
      gcc_unreachable ();
    }
}
