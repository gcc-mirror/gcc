/* score-mdaux.c for Sunplus S+CORE processor
   Copyright (C) 2005 Free Software Foundation, Inc.
   Contributed by Sunnorth

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
   along with GCC; see the file COPYING.  If not, write to
   the Free Software Foundation, 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include <signal.h>
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-attr.h"
#include "recog.h"
#include "toplev.h"
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
#include "score-mdaux.h"

#define BITSET_P(VALUE, BIT)      (((VALUE) & (1L << (BIT))) != 0)
#define INS_BUF_SZ                100

/* Define the information needed to generate branch insns.  This is
   stored from the compare operation.  */
rtx cmp_op0, cmp_op1;

static char ins[INS_BUF_SZ + 8];

/* Return true if SYMBOL is a SYMBOL_REF and OFFSET + SYMBOL points
   to the same object as SYMBOL.  */
static int
score_offset_within_object_p (rtx symbol, HOST_WIDE_INT offset)
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
score_split_const (rtx x, rtx *base, HOST_WIDE_INT *offset)
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
static enum
score_symbol_type score_classify_symbol (rtx x)
{
  if (GET_CODE (x) == LABEL_REF)
    return SYMBOL_GENERAL;

  if (GET_CODE (x) != SYMBOL_REF)
    gcc_unreachable ();

  if (CONSTANT_POOL_ADDRESS_P(x))
    {
      if (GET_MODE_SIZE (get_pool_mode (x)) <= SCORE_SDATA_MAX)
        return SYMBOL_SMALL_DATA;
      return SYMBOL_GENERAL;
    }
  if (SYMBOL_REF_SMALL_P (x))
    return SYMBOL_SMALL_DATA;
  return SYMBOL_GENERAL;
}

/* Return true if the current function must save REGNO.  */
static int
score_save_reg_p (unsigned int regno)
{
  /* Check call-saved registers.  */
  if (regs_ever_live[regno] && !call_used_regs[regno])
    return 1;

  /* We need to save the old frame pointer before setting up a new one.  */
  if (regno == HARD_FRAME_POINTER_REGNUM && frame_pointer_needed)
    return 1;

  /* We need to save the incoming return address if it is ever clobbered
     within the function.  */
  if (regno == RA_REGNUM && regs_ever_live[regno])
    return 1;

  return 0;
}

/* Return one word of double-word value OP, taking into account the fixed
   endianness of certain registers.  HIGH_P is true to select the high part,
   false to select the low part.  */
static rtx
subw (rtx op, int high_p)
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

struct score_frame_info *
mda_cached_frame (void)
{
  static struct score_frame_info _frame_info;
  return &_frame_info;
}

/* Return the bytes needed to compute the frame pointer from the current
   stack pointer.  SIZE is the size (in bytes) of the local variables.  */
struct score_frame_info *
mda_compute_frame_size (HOST_WIDE_INT size)
{
  unsigned int regno;
  struct score_frame_info *f = mda_cached_frame ();

  memset (f, 0, sizeof (struct score_frame_info));
  f->gp_reg_size = 0;
  f->mask = 0;
  f->var_size = SCORE_STACK_ALIGN (size);
  f->args_size = current_function_outgoing_args_size;
  f->cprestore_size = SCORE_STACK_ALIGN (STARTING_FRAME_OFFSET) - f->args_size;
  if (f->var_size == 0 && current_function_is_leaf)
    f->args_size = f->cprestore_size = 0;

  if (f->args_size == 0 && current_function_calls_alloca)
    f->args_size = UNITS_PER_WORD;

  f->total_size = f->var_size + f->args_size;
  for (regno = GP_REG_FIRST; regno <= GP_REG_LAST; regno++)
    {
      if (score_save_reg_p (regno))
        {
          f->gp_reg_size += GET_MODE_SIZE (SImode);
          f->mask |= 1 << (regno - GP_REG_FIRST);
        }
    }

  if (current_function_calls_eh_return)
    {
      unsigned int i;
      for (i = 0; ; ++i)
        {
          regno = EH_RETURN_DATA_REGNO (i);
          if (regno == INVALID_REGNUM)
            break;
          f->gp_reg_size += GET_MODE_SIZE (SImode);
          f->mask |= 1 << (regno - GP_REG_FIRST);
        }
    }

  f->total_size += SCORE_STACK_ALIGN (f->gp_reg_size);
  f->num_gp = f->gp_reg_size / UNITS_PER_WORD;

  if (f->mask)
    {
      HOST_WIDE_INT offset;
      offset = (f->args_size + f->cprestore_size + f->var_size
                + f->gp_reg_size - GET_MODE_SIZE (SImode));
      f->gp_sp_offset = offset;
    }
  else
    {
      f->gp_sp_offset = 0;
    }

  if ((f->total_size == f->gp_reg_size) && flag_pic)
    f->total_size += 8;

  return f;
}

/* Generate the prologue instructions for entry into a S+core function.  */
void
mdx_prologue (void)
{
#define EMIT_PL(_rtx)        RTX_FRAME_RELATED_P (_rtx) = 1

  struct score_frame_info *f = mda_compute_frame_size (get_frame_size ());
  HOST_WIDE_INT size;
  int regno;

  size = f->total_size - f->gp_reg_size;

  if (flag_pic)
    emit_insn (gen_cpload ());

  for (regno = (int) GP_REG_LAST; regno >= (int) GP_REG_FIRST; regno--)
    {
      if (BITSET_P (f->mask, regno - GP_REG_FIRST))
        {
          rtx mem = gen_rtx_MEM (SImode,
                                 gen_rtx_PRE_DEC (SImode, stack_pointer_rtx));
          rtx reg = gen_rtx_REG (SImode, regno);
          if (!current_function_calls_eh_return)
            MEM_READONLY_P (mem) = 1;
          EMIT_PL (emit_insn (gen_pushsi (mem, reg)));
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
          EMIT_PL (emit_move_insn (gen_rtx_REG (Pmode, PROLOGUE_TEMP_REGNUM),
                                   GEN_INT (size)));
          EMIT_PL (emit_insn
                   (gen_sub3_insn (stack_pointer_rtx,
                                   stack_pointer_rtx,
                                   gen_rtx_REG (Pmode,
                                                PROLOGUE_TEMP_REGNUM))));
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

  if (flag_pic)
    emit_insn (gen_cprestore (GEN_INT (size + 4)));

#undef EMIT_PL
}

/* Generate the epilogue instructions in a S+core function.  */
void
mdx_epilogue (int sibcall_p)
{
  struct score_frame_info *f = mda_compute_frame_size (get_frame_size ());
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
          emit_move_insn (gen_rtx_REG (Pmode, EPILOGUE_TEMP_REGNUM),
                          GEN_INT (size));
          emit_insn (gen_add3_insn (base, base,
                                    gen_rtx_REG (Pmode,
                                                 EPILOGUE_TEMP_REGNUM)));
        }
    }

  if (base != stack_pointer_rtx)
    emit_move_insn (stack_pointer_rtx, base);

  if (current_function_calls_eh_return)
    emit_insn (gen_add3_insn (stack_pointer_rtx,
                              stack_pointer_rtx,
                              EH_RETURN_STACKADJ_RTX));

  for (regno = (int) GP_REG_FIRST; regno <= (int) GP_REG_LAST; regno++)
    {
      if (BITSET_P (f->mask, regno - GP_REG_FIRST))
        {
          rtx mem = gen_rtx_MEM (SImode,
                                 gen_rtx_POST_INC (SImode, stack_pointer_rtx));
          rtx reg = gen_rtx_REG (SImode, regno);

          if (!current_function_calls_eh_return)
            MEM_READONLY_P (mem) = 1;

          emit_insn (gen_popsi (reg, mem));
        }
    }

  if (!sibcall_p)
    emit_jump_insn (gen_return_internal (gen_rtx_REG (Pmode, RA_REGNUM)));
}

/* Return true if X is a valid base register for the given mode.
   Allow only hard registers if STRICT.  */
int
mda_valid_base_register_p (rtx x, int strict)
{
  if (!strict && GET_CODE (x) == SUBREG)
    x = SUBREG_REG (x);

  return (GET_CODE (x) == REG
          && score_regno_mode_ok_for_base_p (REGNO (x), strict));
}

/* Return true if X is a valid address for machine mode MODE.  If it is,
   fill in INFO appropriately.  STRICT is true if we should only accept
   hard base registers.  */
int
mda_classify_address (struct score_address_info *info,
                      enum machine_mode mode, rtx x, int strict)
{
  info->code = GET_CODE (x);

  switch (info->code)
    {
    case REG:
    case SUBREG:
      info->type = ADD_REG;
      info->reg = x;
      info->offset = const0_rtx;
      return mda_valid_base_register_p (info->reg, strict);
    case PLUS:
      info->type = ADD_REG;
      info->reg = XEXP (x, 0);
      info->offset = XEXP (x, 1);
      return (mda_valid_base_register_p (info->reg, strict)
              && GET_CODE (info->offset) == CONST_INT
              && CONST_OK_FOR_LETTER_P (INTVAL (info->offset), 'O'));
    case PRE_DEC:
    case POST_DEC:
    case PRE_INC:
    case POST_INC:
      if (GET_MODE_SIZE (mode) > GET_MODE_SIZE (SImode))
        return false;
      info->type = ADD_REG;
      info->reg = XEXP (x, 0);
      info->offset = GEN_INT (GET_MODE_SIZE (mode));
      return mda_valid_base_register_p (info->reg, strict);
    case CONST_INT:
      info->type = ADD_CONST_INT;
      return CONST_OK_FOR_LETTER_P (INTVAL (x), 'O');
    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
      info->type = ADD_SYMBOLIC;
      return (mda_symbolic_constant_p (x, &info->symbol_type)
              && (info->symbol_type == SYMBOL_GENERAL
                  || info->symbol_type == SYMBOL_SMALL_DATA));
    default:
      return 0;
    }
}

void
mda_gen_cmp (enum machine_mode mode)
{
  emit_insn (gen_rtx_SET (VOIDmode, gen_rtx_REG (mode, CC_REGNUM),
                          gen_rtx_COMPARE (mode, cmp_op0, cmp_op1)));
}

/* Return true if X is a symbolic constant that can be calculated in
   the same way as a bare symbol.  If it is, store the type of the
   symbol in *SYMBOL_TYPE.  */
int
mda_symbolic_constant_p (rtx x, enum score_symbol_type *symbol_type)
{
  HOST_WIDE_INT offset;

  score_split_const (x, &x, &offset);
  if (GET_CODE (x) == SYMBOL_REF || GET_CODE (x) == LABEL_REF)
    *symbol_type = score_classify_symbol (x);
  else
    return 0;

  if (offset == 0)
    return 1;

  /* if offset > 15bit, must reload  */
  if (!CONST_OK_FOR_LETTER_P (offset, 'O'))
    return 0;

  switch (*symbol_type)
    {
    case SYMBOL_GENERAL:
      return 1;
    case SYMBOL_SMALL_DATA:
      return score_offset_within_object_p (x, offset);
    }
  gcc_unreachable ();
}

void
mdx_movsicc (rtx *ops)
{
  enum machine_mode mode = CCmode;

  if (GET_CODE (ops[1]) == EQ || GET_CODE (ops[1]) == NE)
    mode = CC_NZmode;

  emit_insn (gen_rtx_SET (VOIDmode, gen_rtx_REG (mode, CC_REGNUM),
                          gen_rtx_COMPARE (mode, cmp_op0, cmp_op1)));
}

/* Call and sibcall pattern all need call this function.  */
void
mdx_call (rtx *ops, bool sib)
{
  rtx addr = XEXP (ops[0], 0);
  if (!call_insn_operand (addr, VOIDmode))
    {
      rtx oaddr = addr;
      addr = gen_reg_rtx (Pmode);
      gen_move_insn (addr, oaddr);
    }

  if (sib)
    emit_call_insn (gen_sibcall_internal (addr, ops[1]));
  else
    emit_call_insn (gen_call_internal (addr, ops[1]));
}

/* Call value and sibcall value pattern all need call this function.  */
void
mdx_call_value (rtx *ops, bool sib)
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
    emit_call_insn (gen_sibcall_value_internal (result, addr, arg));
  else
    emit_call_insn (gen_call_value_internal (result, addr, arg));
}

/* Machine Split  */
void
mds_movdi (rtx *ops)
{
  rtx dst = ops[0];
  rtx src = ops[1];
  rtx dst0 = subw (dst, 0);
  rtx dst1 = subw (dst, 1);
  rtx src0 = subw (src, 0);
  rtx src1 = subw (src, 1);

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
mds_zero_extract_andi (rtx *ops)
{
  if (INTVAL (ops[1]) == 1 && const_bi_operand (ops[2], SImode))
    emit_insn (gen_zero_extract_bittst (ops[0], ops[2]));
  else
    {
      unsigned HOST_WIDE_INT mask;
      mask = (0xffffffffU & ((1U << INTVAL (ops[1])) - 1U));
      mask = mask << INTVAL (ops[2]);
      emit_insn (gen_andsi3_cmp (ops[0], gen_int_mode (mask, SImode)));
    }
}

/* Check addr could be present as PRE/POST mode.  */
static bool
mda_pindex_mem (rtx addr)
{
  if (GET_CODE (addr) == MEM)
    {
      switch (GET_CODE (XEXP (addr, 0)))
        {
        case PRE_DEC:
        case POST_DEC:
        case PRE_INC:
        case POST_INC:
          return true;
        default:
          break;
        }
    }
  return false;
}

/* Output asm code for ld/sw insn.  */
static int
pr_addr_post (rtx *ops, int idata, int iaddr, char *ip, enum mda_mem_unit unit)
{
  struct score_address_info ai;

  gcc_assert (GET_CODE (ops[idata]) == REG);
  gcc_assert (mda_classify_address (&ai, SImode, XEXP (ops[iaddr], 0), true));

  if (!mda_pindex_mem (ops[iaddr])
      && ai.type == ADD_REG
      && GET_CODE (ai.offset) == CONST_INT
      && G16_REG_P (REGNO (ops[idata]))
      && G16_REG_P (REGNO (ai.reg)))
    {
      if (INTVAL (ai.offset) == 0)
        {
          ops[iaddr] = ai.reg;
          return snprintf (ip, INS_BUF_SZ,
                           "!        %%%d, [%%%d]", idata, iaddr);
        }
      if (REGNO (ai.reg) == HARD_FRAME_POINTER_REGNUM)
        {
          HOST_WIDE_INT offset = INTVAL (ai.offset);
          if (MDA_ALIGN_UNIT (offset, unit)
              && CONST_OK_FOR_LETTER_P (offset >> unit, 'J'))
            {
              ops[iaddr] = ai.offset;
              return snprintf (ip, INS_BUF_SZ,
                               "p!        %%%d, %%c%d", idata, iaddr);
            }
        }
    }
  return snprintf (ip, INS_BUF_SZ, "        %%%d, %%a%d", idata, iaddr);
}

/* Output asm insn for load.  */
const char *
mdp_linsn (rtx *ops, enum mda_mem_unit unit, bool sign)
{
  const char *pre_ins[] =
    {"lbu", "lhu", "lw", "??", "lb", "lh", "lw", "??"};
  char *ip;

  strcpy (ins, pre_ins[(sign ? 4 : 0) + unit]);
  ip = ins + strlen (ins);

  if ((!sign && unit != MDA_HWORD)
      || (sign && unit != MDA_BYTE))
    pr_addr_post (ops, 0, 1, ip, unit);
  else
    snprintf (ip, INS_BUF_SZ, "        %%0, %%a1");

  return ins;
}

/* Output asm insn for store.  */
const char *
mdp_sinsn (rtx *ops, enum mda_mem_unit unit)
{
  const char *pre_ins[] = {"sb", "sh", "sw"};
  char *ip;

  strcpy (ins, pre_ins[unit]);
  ip = ins + strlen (ins);
  pr_addr_post (ops, 1, 0, ip, unit);
  return ins;
}

/* Output asm insn for load immediate.  */
const char *
mdp_limm (rtx *ops)
{
  gcc_assert (GET_CODE (ops[0]) == REG);

  if (G16_REG_P (REGNO (ops[0]))
      && CONST_OK_FOR_LETTER_P (INTVAL (ops[1]), 'I'))
    return "ldiu!   %0, %c1";
  else if (CONST_OK_FOR_LETTER_P (INTVAL (ops[1]), 'L'))
    return "ldi     %0, %c1";
  else if (EXTRA_CONSTRAINT (ops[1], 'Q'))
    return "ldis    %0, %U1";
  else
    return "li      %0, %D1";
}

/* Output asm insn for move.  */
const char *
mdp_move (rtx *ops)
{
  gcc_assert (GET_CODE (ops[0]) == REG);
  gcc_assert (GET_CODE (ops[1]) == REG);

  if (G16_REG_P (REGNO (ops[0])))
    {
      if (G16_REG_P (REGNO (ops[1])))
        return "mv!     %0, %1";
      else
        return "mlfh!   %0, %1";
    }
  else if (G16_REG_P (REGNO (ops[1])))
    return "mhfl!   %0, %1";
  else
    return "mv      %0, %1";
}

/* Score support add/sub with exponent immediate insn,
   use to judge imm condition.  */
static unsigned int
num_bits1 (unsigned HOST_WIDE_INT v)
{
  int i, n = 0;

  for (i = 0; i < BITS_PER_WORD; i++)
    n += BITSET_P (v, i) ? 1 : 0;
  return n;
}

/* Generate add insn, insn will affect condition flag. Optimize used.  */
const char *
mdp_add_imm_ucc (rtx *ops)
{
  HOST_WIDE_INT v = INTVAL (ops[2]);

  gcc_assert (GET_CODE (ops[2]) == CONST_INT);
  gcc_assert (REGNO (ops[0]) == REGNO (ops[1]));

  if (G16_REG_P (REGNO (ops[0])))
    {
      if (v > 0 && num_bits1 (v) == 1 && IMM_IN_RANGE (ffs (v) - 1, 4, 0))
        {
          ops[2] = GEN_INT (ffs (v) - 1);
          return "addei!  %0, %c2";
        }

      if (v < 0 && num_bits1 (-v) == 1 && IMM_IN_RANGE (ffs (-v) - 1, 4, 0))
        {
          ops[2] = GEN_INT (ffs (-v) - 1);
          return "subei!  %0, %c2";
        }
    }
    return "addi.c  %0, %c2";
}

/* Output arith insn, insn will update condition flag.  */
const char *
mdp_select (rtx *ops, const char *inst_pre, bool commu, const char *let)
{
  gcc_assert (GET_CODE (ops[0]) == REG);
  gcc_assert (GET_CODE (ops[1]) == REG);

  if (G16_REG_P (REGNO (ops[0]))
      && (GET_CODE (ops[2]) == REG ? G16_REG_P (REGNO (ops[2])) : 1)
      && REGNO (ops[0]) == REGNO (ops[1]))
    {
      snprintf (ins, INS_BUF_SZ, "%s!        %%0, %%%s2", inst_pre, let);
      return ins;
    }

  if (commu && G16_REG_P (REGNO (ops[0]))
      && G16_REG_P (REGNO (ops[1]))
      && REGNO (ops[0]) == REGNO (ops[2]))
    {
      gcc_assert (GET_CODE (ops[2]) == REG);
      snprintf (ins, INS_BUF_SZ, "%s!        %%0, %%%s1", inst_pre, let);
      return ins;
    }

  snprintf (ins, INS_BUF_SZ, "%s.c        %%0, %%1, %%%s2", inst_pre, let);
  return ins;
}

