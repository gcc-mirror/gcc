/* Subroutines for insn-output.c for HPPA.
   Copyright (C) 1992, 93, 94, 95, 96, 97, 1998 Free Software Foundation, Inc.
   Contributed by Tim Moore (moore@cs.utah.edu), based on sparc.c

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
#include <stdio.h>
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
#include "tree.h"
#include "reload.h"
#include "c-tree.h"
#include "expr.h"
#include "obstack.h"

static void restore_unscaled_index_insn_codes		PROTO((rtx));
static void record_unscaled_index_insn_codes		PROTO((rtx));

/* Save the operands last given to a compare for use when we
   generate a scc or bcc insn.  */

rtx hppa_compare_op0, hppa_compare_op1;
enum cmp_type hppa_branch_type;

/* Which cpu we are scheduling for.  */
enum processor_type pa_cpu;

/* String to hold which cpu we are scheduling for.  */
char *pa_cpu_string;

/* Set by the FUNCTION_PROFILER macro. */
int hp_profile_labelno;

/* Counts for the number of callee-saved general and floating point
   registers which were saved by the current function's prologue.  */
static int gr_saved, fr_saved;

/* Whether or not the current function uses an out-of-line prologue
   and epilogue.  */
static int out_of_line_prologue_epilogue;

static rtx find_addr_reg ();

/* Keep track of the number of bytes we have output in the CODE subspaces
   during this compilation so we'll know when to emit inline long-calls.  */

unsigned int total_code_bytes;

/* Variables to handle plabels that we discover are necessary at assembly
   output time.  They are output after the current function.  */

struct deferred_plabel
{
  rtx internal_label;
  char *name;
} *deferred_plabels = 0;
int n_deferred_plabels = 0;

/* Array indexed by INSN_UIDs holding the INSN_CODE of an insn which
   uses an unscaled indexed address before delay slot scheduling.  */
static int *unscaled_index_insn_codes;

/* Upper bound for the array.  */
static int max_unscaled_index_insn_codes_uid;

void
override_options ()
{
  /* Default to 7100 scheduling.  If the 7100LC scheduling ever
     gets reasonably tuned, it should be the default since that
     what most PAs sold now are.  */
  if (pa_cpu_string == NULL
      || ! strcmp (pa_cpu_string, "7100"))
    {
      pa_cpu_string = "7100";
      pa_cpu = PROCESSOR_7100;
    }
  else if (! strcmp (pa_cpu_string, "700"))
    {
      pa_cpu_string = "700";
      pa_cpu = PROCESSOR_700;
    }
  else if (! strcmp (pa_cpu_string, "7100LC"))
    {
      pa_cpu_string = "7100LC";
      pa_cpu = PROCESSOR_7100LC;
    }
  else
    {
      warning ("Unknown -mschedule= option (%s).\nValid options are 700, 7100 and 7100LC\n", pa_cpu_string);
    }

  if (flag_pic && TARGET_PORTABLE_RUNTIME)
    {
      warning ("PIC code generation is not supported in the portable runtime model\n");
    }

  if (flag_pic && (TARGET_NO_SPACE_REGS || TARGET_FAST_INDIRECT_CALLS))
   {
      warning ("PIC code generation is not compatible with fast indirect calls\n");
   }

  if (flag_pic && profile_flag)
    {
      warning ("PIC code generation is not compatible with profiling\n");
    }

  if (TARGET_SPACE && (flag_pic || profile_flag))
    {
      warning ("Out of line entry/exit sequences are not compatible\n");
      warning ("with PIC or profiling\n");
    }

  if (! TARGET_GAS && write_symbols != NO_DEBUG)
    {
      warning ("-g is only supported when using GAS on this processor,");
      warning ("-g option disabled.");
      write_symbols = NO_DEBUG;
    }
}


/* Return non-zero only if OP is a register of mode MODE,
   or CONST0_RTX.  */
int
reg_or_0_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (op == CONST0_RTX (mode) || register_operand (op, mode));
}

/* Return non-zero if OP is suitable for use in a call to a named
   function.

   (???) For 2.5 try to eliminate either call_operand_address or
   function_label_operand, they perform very similar functions.  */
int
call_operand_address (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (CONSTANT_P (op) && ! TARGET_PORTABLE_RUNTIME);
}

/* Return 1 if X contains a symbolic expression.  We know these
   expressions will have one of a few well defined forms, so
   we need only check those forms.  */
int
symbolic_expression_p (x)
     register rtx x;
{

  /* Strip off any HIGH. */
  if (GET_CODE (x) == HIGH)
    x = XEXP (x, 0);

  return (symbolic_operand (x, VOIDmode));
}

int
symbolic_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  switch (GET_CODE (op))
    {
    case SYMBOL_REF:
    case LABEL_REF:
      return 1;
    case CONST:
      op = XEXP (op, 0);
      return ((GET_CODE (XEXP (op, 0)) == SYMBOL_REF
	       || GET_CODE (XEXP (op, 0)) == LABEL_REF)
	      && GET_CODE (XEXP (op, 1)) == CONST_INT);
    default:
      return 0;
    }
}

/* Return truth value of statement that OP is a symbolic memory
   operand of mode MODE.  */

int
symbolic_memory_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  if (GET_CODE (op) != MEM)
    return 0;
  op = XEXP (op, 0);
  return (GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == CONST
	  || GET_CODE (op) == HIGH || GET_CODE (op) == LABEL_REF);
}

/* Return 1 if the operand is either a register or a memory operand that is
   not symbolic.  */

int
reg_or_nonsymb_mem_operand (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  if (register_operand (op, mode))
    return 1;

  if (memory_operand (op, mode) && ! symbolic_memory_operand (op, mode))
    return 1;

  return 0;
}

/* Return 1 if the operand is either a register, zero, or a memory operand
   that is not symbolic.  */

int
reg_or_0_or_nonsymb_mem_operand (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  if (register_operand (op, mode))
    return 1;

  if (op == CONST0_RTX (mode))
    return 1;

  if (memory_operand (op, mode) && ! symbolic_memory_operand (op, mode))
    return 1;

  return 0;
}

/* Accept any constant that can be moved in one instructions into a
   general register.  */
int
cint_ok_for_move (intval)
     HOST_WIDE_INT intval;
{
  /* OK if ldo, ldil, or zdepi, can be used.  */
  return (VAL_14_BITS_P (intval) || (intval & 0x7ff) == 0
	  || zdepi_cint_p (intval));
}

/* Accept anything that can be moved in one instruction into a general
   register.  */
int
move_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (register_operand (op, mode))
    return 1;

  if (GET_CODE (op) == CONST_INT)
    return cint_ok_for_move (INTVAL (op));

  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  if (GET_CODE (op) != MEM)
    return 0;

  op = XEXP (op, 0);
  if (GET_CODE (op) == LO_SUM)
    return (register_operand (XEXP (op, 0), Pmode)
	    && CONSTANT_P (XEXP (op, 1)));

  /* Since move_operand is only used for source operands, we can always
     allow scaled indexing!  */
  if (! TARGET_DISABLE_INDEXING
      && GET_CODE (op) == PLUS
      && ((GET_CODE (XEXP (op, 0)) == MULT
	   && GET_CODE (XEXP (XEXP (op, 0), 0)) == REG
	   && GET_CODE (XEXP (XEXP (op, 0), 1)) == CONST_INT
	   && INTVAL (XEXP (XEXP (op, 0), 1)) == GET_MODE_SIZE (mode)
	   && GET_CODE (XEXP (op, 1)) == REG)
	  || (GET_CODE (XEXP (op, 1)) == MULT
	      &&GET_CODE (XEXP (XEXP (op, 1), 0)) == REG
	      && GET_CODE (XEXP (XEXP (op, 1), 1)) == CONST_INT
	      && INTVAL (XEXP (XEXP (op, 1), 1)) == GET_MODE_SIZE (mode)
	      && GET_CODE (XEXP (op, 0)) == REG)))
    return 1;

  return memory_address_p (mode, op);
}

/* Accept REG and any CONST_INT that can be moved in one instruction into a
   general register.  */
int
reg_or_cint_move_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (register_operand (op, mode))
    return 1;

  if (GET_CODE (op) == CONST_INT)
    return cint_ok_for_move (INTVAL (op));

  return 0;
}

int
pic_label_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (!flag_pic)
    return 0;

  switch (GET_CODE (op))
    {
    case LABEL_REF:
      return 1;
    case CONST:
      op = XEXP (op, 0);
      return (GET_CODE (XEXP (op, 0)) == LABEL_REF
	      && GET_CODE (XEXP (op, 1)) == CONST_INT);
    default:
      return 0;
    }
}

int
fp_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return reg_renumber && FP_REG_P (op);
}



/* Return truth value of whether OP can be used as an operand in a
   three operand arithmetic insn that accepts registers of mode MODE
   or 14-bit signed integers.  */
int
arith_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT && INT_14_BITS (op)));
}

/* Return truth value of whether OP can be used as an operand in a
   three operand arithmetic insn that accepts registers of mode MODE
   or 11-bit signed integers.  */
int
arith11_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT && INT_11_BITS (op)));
}

/* A constant integer suitable for use in a PRE_MODIFY memory
   reference.  */
int
pre_cint_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT
	  && INTVAL (op) >= -0x2000 && INTVAL (op) < 0x10);
}

/* A constant integer suitable for use in a POST_MODIFY memory
   reference.  */
int
post_cint_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT
	  && INTVAL (op) < 0x2000 && INTVAL (op) >= -0x10);
}

int
arith_double_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_DOUBLE
	      && GET_MODE (op) == mode
	      && VAL_14_BITS_P (CONST_DOUBLE_LOW (op))
	      && (CONST_DOUBLE_HIGH (op) >= 0
		  == ((CONST_DOUBLE_LOW (op) & 0x1000) == 0))));
}

/* Return truth value of whether OP is a integer which fits the
   range constraining immediate operands in three-address insns, or
   is an integer register.  */

int
ireg_or_int5_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ((GET_CODE (op) == CONST_INT && INT_5_BITS (op))
	  || (GET_CODE (op) == REG && REGNO (op) > 0 && REGNO (op) < 32));
}

/* Return truth value of whether OP is a integer which fits the
   range constraining immediate operands in three-address insns.  */

int
int5_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT && INT_5_BITS (op));
}

int
uint5_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT && INT_U5_BITS (op));
}

int
int11_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT && INT_11_BITS (op));
}

int
uint32_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
#if HOST_BITS_PER_WIDE_INT > 32
  /* All allowed constants will fit a CONST_INT.  */
  return (GET_CODE (op) == CONST_INT
	  && (INTVAL (op) >= 0 && INTVAL (op) < 0x100000000L));
#else
  return (GET_CODE (op) == CONST_INT
	  || (GET_CODE (op) == CONST_DOUBLE
	      && CONST_DOUBLE_HIGH (op) == 0));
#endif
}

int
arith5_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return register_operand (op, mode) || int5_operand (op, mode);
}

/* True iff zdepi can be used to generate this CONST_INT.  */
int
zdepi_cint_p (x)
     unsigned HOST_WIDE_INT x;
{
  unsigned HOST_WIDE_INT lsb_mask, t;

  /* This might not be obvious, but it's at least fast.
     This function is critical; we don't have the time loops would take.  */
  lsb_mask = x & -x;
  t = ((x >> 4) + lsb_mask) & ~(lsb_mask - 1);
  /* Return true iff t is a power of two.  */
  return ((t & (t - 1)) == 0);
}

/* True iff depi or extru can be used to compute (reg & mask).
   Accept bit pattern like these:
   0....01....1
   1....10....0
   1..10..01..1  */
int
and_mask_p (mask)
     unsigned HOST_WIDE_INT mask;
{
  mask = ~mask;
  mask += mask & -mask;
  return (mask & (mask - 1)) == 0;
}

/* True iff depi or extru can be used to compute (reg & OP).  */
int
and_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT && and_mask_p (INTVAL (op))));
}

/* True iff depi can be used to compute (reg | MASK).  */
int
ior_mask_p (mask)
     unsigned HOST_WIDE_INT mask;
{
  mask += mask & -mask;
  return (mask & (mask - 1)) == 0;
}

/* True iff depi can be used to compute (reg | OP).  */
int
ior_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT && ior_mask_p (INTVAL (op)));
}

int
lhs_lshift_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return register_operand (op, mode) || lhs_lshift_cint_operand (op, mode);
}

/* True iff OP is a CONST_INT of the forms 0...0xxxx or 0...01...1xxxx.
   Such values can be the left hand side x in (x << r), using the zvdepi
   instruction.  */
int
lhs_lshift_cint_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  unsigned HOST_WIDE_INT x;
  if (GET_CODE (op) != CONST_INT)
    return 0;
  x = INTVAL (op) >> 4;
  return (x & (x + 1)) == 0;
}

int
arith32_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return register_operand (op, mode) || GET_CODE (op) == CONST_INT;
}

int
pc_or_label_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == PC || GET_CODE (op) == LABEL_REF);
}

/* Legitimize PIC addresses.  If the address is already
   position-independent, we return ORIG.  Newly generated
   position-independent addresses go to REG.  If we need more
   than one register, we lose.  */

rtx
legitimize_pic_address (orig, mode, reg)
     rtx orig, reg;
     enum machine_mode mode;
{
  rtx pic_ref = orig;

  /* Labels need special handling.  */
  if (pic_label_operand (orig))
    {
      emit_insn (gen_pic_load_label (reg, orig));
      current_function_uses_pic_offset_table = 1;
      return reg;
    }
  if (GET_CODE (orig) == SYMBOL_REF)
    {
      if (reg == 0)
	abort ();

      if (flag_pic == 2)
	{
	  emit_insn (gen_pic2_highpart (reg, pic_offset_table_rtx, orig));
	  pic_ref = gen_rtx (MEM, Pmode,
			     gen_rtx (LO_SUM, Pmode, reg,
				      gen_rtx (UNSPEC, SImode, gen_rtvec (1, orig), 0)));
	}
      else
	pic_ref = gen_rtx (MEM, Pmode,
			   gen_rtx (PLUS, Pmode, pic_offset_table_rtx, orig));
      current_function_uses_pic_offset_table = 1;
      RTX_UNCHANGING_P (pic_ref) = 1;
      emit_move_insn (reg, pic_ref);
      return reg;
    }
  else if (GET_CODE (orig) == CONST)
    {
      rtx base;

      if (GET_CODE (XEXP (orig, 0)) == PLUS
	  && XEXP (XEXP (orig, 0), 0) == pic_offset_table_rtx)
	return orig;

      if (reg == 0)
	abort ();

      if (GET_CODE (XEXP (orig, 0)) == PLUS)
	{
	  base = legitimize_pic_address (XEXP (XEXP (orig, 0), 0), Pmode, reg);
	  orig = legitimize_pic_address (XEXP (XEXP (orig, 0), 1), Pmode,
					 base == reg ? 0 : reg);
	}
      else abort ();
      if (GET_CODE (orig) == CONST_INT)
	{
	  if (INT_14_BITS (orig))
	    return plus_constant_for_output (base, INTVAL (orig));
	  orig = force_reg (Pmode, orig);
	}
      pic_ref = gen_rtx (PLUS, Pmode, base, orig);
      /* Likewise, should we set special REG_NOTEs here?  */
    }
  return pic_ref;
}

/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output.

   For the PA, transform:

	memory(X + <large int>)

   into:

	if (<large int> & mask) >= 16
	  Y = (<large int> & ~mask) + mask + 1	Round up.
	else
	  Y = (<large int> & ~mask)		Round down.
	Z = X + Y
	memory (Z + (<large int> - Y));

   This is for CSE to find several similar references, and only use one Z.

   X can either be a SYMBOL_REF or REG, but because combine can not
   perform a 4->2 combination we do nothing for SYMBOL_REF + D where
   D will not fit in 14 bits.

   MODE_FLOAT references allow displacements which fit in 5 bits, so use
   0x1f as the mask.

   MODE_INT references allow displacements which fit in 14 bits, so use
   0x3fff as the mask.

   This relies on the fact that most mode MODE_FLOAT references will use FP
   registers and most mode MODE_INT references will use integer registers.
   (In the rare case of an FP register used in an integer MODE, we depend
   on secondary reloads to clean things up.)


   It is also beneficial to handle (plus (mult (X) (Y)) (Z)) in a special
   manner if Y is 2, 4, or 8.  (allows more shadd insns and shifted indexed
   addressing modes to be used).

   Put X and Z into registers.  Then put the entire expression into
   a register.  */

rtx
hppa_legitimize_address (x, oldx, mode)
     rtx x, oldx;
     enum machine_mode mode;
{
  rtx orig = x;

  if (flag_pic)
    return legitimize_pic_address (x, mode, gen_reg_rtx (Pmode));

  /* Strip off CONST. */
  if (GET_CODE (x) == CONST)
    x = XEXP (x, 0);

  /* Special case.  Get the SYMBOL_REF into a register and use indexing.
     That should always be safe.  */
  if (GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x, 0)) == REG
      && GET_CODE (XEXP (x, 1)) == SYMBOL_REF)
    {
      rtx reg = force_reg (SImode, XEXP (x, 1));
      return force_reg (SImode, gen_rtx (PLUS, SImode, reg, XEXP (x, 0)));
    }

  /* Note we must reject symbols which represent function addresses
     since the assembler/linker can't handle arithmetic on plabels.  */
  if (GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x, 1)) == CONST_INT
      && ((GET_CODE (XEXP (x, 0)) == SYMBOL_REF
	   && !FUNCTION_NAME_P (XSTR (XEXP (x, 0), 0)))
	  || GET_CODE (XEXP (x, 0)) == REG))
    {
      rtx int_part, ptr_reg;
      int newoffset;
      int offset = INTVAL (XEXP (x, 1));
      int mask = GET_MODE_CLASS (mode) == MODE_FLOAT ? 0x1f : 0x3fff;

      /* Choose which way to round the offset.  Round up if we
	 are >= halfway to the next boundary.  */
      if ((offset & mask) >= ((mask + 1) / 2))
	newoffset = (offset & ~ mask) + mask + 1;
      else
	newoffset = (offset & ~ mask);

      /* If the newoffset will not fit in 14 bits (ldo), then
	 handling this would take 4 or 5 instructions (2 to load
	 the SYMBOL_REF + 1 or 2 to load the newoffset + 1 to
	 add the new offset and the SYMBOL_REF.)  Combine can
	 not handle 4->2 or 5->2 combinations, so do not create
	 them.  */
      if (! VAL_14_BITS_P (newoffset)
	  && GET_CODE (XEXP (x, 0)) == SYMBOL_REF)
	{
	  rtx const_part = gen_rtx (CONST, VOIDmode,
				    gen_rtx (PLUS, Pmode,
					     XEXP (x, 0),
					     GEN_INT (newoffset)));
	  rtx tmp_reg
	    = force_reg (Pmode,
			 gen_rtx (HIGH, Pmode, const_part));
	  ptr_reg
	    = force_reg (Pmode,
			 gen_rtx (LO_SUM, Pmode,
				  tmp_reg, const_part));
	}
      else
	{
	  if (! VAL_14_BITS_P (newoffset))
	    int_part = force_reg (Pmode, GEN_INT (newoffset));
	  else
	    int_part = GEN_INT (newoffset);

	  ptr_reg = force_reg (Pmode,
			       gen_rtx (PLUS, Pmode,
					force_reg (Pmode, XEXP (x, 0)),
					int_part));
	}
      return plus_constant (ptr_reg, offset - newoffset);
    }

  /* Handle (plus (mult (a) (shadd_constant)) (b)).  */

  if (GET_CODE (x) == PLUS && GET_CODE (XEXP (x, 0)) == MULT
      && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
      && shadd_constant_p (INTVAL (XEXP (XEXP (x, 0), 1)))
      && (GET_RTX_CLASS (GET_CODE (XEXP (x, 1))) == 'o'
	  || GET_CODE (XEXP (x, 1)) == SUBREG)
      && GET_CODE (XEXP (x, 1)) != CONST)
    {
      int val = INTVAL (XEXP (XEXP (x, 0), 1));
      rtx reg1, reg2;

      reg1 = XEXP (x, 1);
      if (GET_CODE (reg1) != REG)
	reg1 = force_reg (Pmode, force_operand (reg1, 0));

      reg2 = XEXP (XEXP (x, 0), 0);
      if (GET_CODE (reg2) != REG)
        reg2 = force_reg (Pmode, force_operand (reg2, 0));

      return force_reg (Pmode, gen_rtx (PLUS, Pmode,
					gen_rtx (MULT, Pmode,
						 reg2, GEN_INT (val)),
					reg1));
    }

  /* Similarly for (plus (plus (mult (a) (shadd_constant)) (b)) (c)).

     Only do so for floating point modes since this is more speculative
     and we lose if it's an integer store.  */
  if (GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x, 0)) == PLUS
      && GET_CODE (XEXP (XEXP (x, 0), 0)) == MULT
      && GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 1)) == CONST_INT
      && shadd_constant_p (INTVAL (XEXP (XEXP (XEXP (x, 0), 0), 1)))
      && (mode == SFmode || mode == DFmode))
    {

      /* First, try and figure out what to use as a base register.  */
      rtx reg1, reg2, base, idx, orig_base;

      reg1 = XEXP (XEXP (x, 0), 1);
      reg2 = XEXP (x, 1);
      base = NULL_RTX;
      idx = NULL_RTX;

      /* Make sure they're both regs.  If one was a SYMBOL_REF [+ const],
	 then emit_move_sequence will turn on REGNO_POINTER_FLAG so we'll
	 know it's a base register below.  */
      if (GET_CODE (reg1) != REG)
	reg1 = force_reg (Pmode, force_operand (reg1, 0));

      if (GET_CODE (reg2) != REG)
	reg2 = force_reg (Pmode, force_operand (reg2, 0));

      /* Figure out what the base and index are.  */
	 
      if (GET_CODE (reg1) == REG
	  && REGNO_POINTER_FLAG (REGNO (reg1)))
	{
	  base = reg1;
	  orig_base = XEXP (XEXP (x, 0), 1);
	  idx = gen_rtx (PLUS, Pmode,
			 gen_rtx (MULT, Pmode,
				  XEXP (XEXP (XEXP (x, 0), 0), 0),
				  XEXP (XEXP (XEXP (x, 0), 0), 1)),
			 XEXP (x, 1));
	}
      else if (GET_CODE (reg2) == REG
	       && REGNO_POINTER_FLAG (REGNO (reg2)))
	{
	  base = reg2;
	  orig_base = XEXP (x, 1);
	  idx = XEXP (x, 0);
	}

      if (base == 0)
	return orig;

      /* If the index adds a large constant, try to scale the
	 constant so that it can be loaded with only one insn.  */
      if (GET_CODE (XEXP (idx, 1)) == CONST_INT
	  && VAL_14_BITS_P (INTVAL (XEXP (idx, 1))
			    / INTVAL (XEXP (XEXP (idx, 0), 1)))
	  && INTVAL (XEXP (idx, 1)) % INTVAL (XEXP (XEXP (idx, 0), 1)) == 0)
	{
	  /* Divide the CONST_INT by the scale factor, then add it to A.  */
	  int val = INTVAL (XEXP (idx, 1));

	  val /= INTVAL (XEXP (XEXP (idx, 0), 1));
	  reg1 = XEXP (XEXP (idx, 0), 0);
	  if (GET_CODE (reg1) != REG)
	    reg1 = force_reg (Pmode, force_operand (reg1, 0));

	  reg1 = force_reg (Pmode, gen_rtx (PLUS, Pmode, reg1, GEN_INT (val)));

	  /* We can now generate a simple scaled indexed address.  */
	  return force_reg (Pmode, gen_rtx (PLUS, Pmode,
					    gen_rtx (MULT, Pmode, reg1,
						     XEXP (XEXP (idx, 0), 1)),
					    base));
	}

      /* If B + C is still a valid base register, then add them.  */
      if (GET_CODE (XEXP (idx, 1)) == CONST_INT
	  && INTVAL (XEXP (idx, 1)) <= 4096
	  && INTVAL (XEXP (idx, 1)) >= -4096)
	{
	  int val = INTVAL (XEXP (XEXP (idx, 0), 1));
	  rtx reg1, reg2;

	  reg1 = force_reg (Pmode, gen_rtx (PLUS, Pmode, base, XEXP (idx, 1)));

	  reg2 = XEXP (XEXP (idx, 0), 0);
	  if (GET_CODE (reg2) != CONST_INT)
	    reg2 = force_reg (Pmode, force_operand (reg2, 0));

	  return force_reg (Pmode, gen_rtx (PLUS, Pmode,
					    gen_rtx (MULT, Pmode,
						     reg2, GEN_INT (val)),
					    reg1));
	}

      /* Get the index into a register, then add the base + index and
	 return a register holding the result.  */

      /* First get A into a register.  */
      reg1 = XEXP (XEXP (idx, 0), 0);
      if (GET_CODE (reg1) != REG)
	reg1 = force_reg (Pmode, force_operand (reg1, 0));

      /* And get B into a register.  */
      reg2 = XEXP (idx, 1);
      if (GET_CODE (reg2) != REG)
	reg2 = force_reg (Pmode, force_operand (reg2, 0));

      reg1 = force_reg (Pmode, gen_rtx (PLUS, Pmode,
					gen_rtx (MULT, Pmode, reg1,
						 XEXP (XEXP (idx, 0), 1)),
					reg2));

      /* Add the result to our base register and return.  */
      return force_reg (Pmode, gen_rtx (PLUS, Pmode, base, reg1));
      
    }

  /* Uh-oh.  We might have an address for x[n-100000].  This needs
     special handling to avoid creating an indexed memory address
     with x-100000 as the base.
    
     If the constant part is small enough, then it's still safe because
     there is a guard page at the beginning and end of the data segment.

     Scaled references are common enough that we want to try and rearrange the
     terms so that we can use indexing for these addresses too.  Only
     do the optimization for floatint point modes.  */

  if (GET_CODE (x) == PLUS
      && symbolic_expression_p (XEXP (x, 1)))
    {
      /* Ugly.  We modify things here so that the address offset specified
	 by the index expression is computed first, then added to x to form
	 the entire address.  */

      rtx regx1, regx2, regy1, regy2, y;

      /* Strip off any CONST.  */
      y = XEXP (x, 1);
      if (GET_CODE (y) == CONST)
	y = XEXP (y, 0);

      if (GET_CODE (y) == PLUS || GET_CODE (y) == MINUS)
	{
	  /* See if this looks like
		(plus (mult (reg) (shadd_const))
		      (const (plus (symbol_ref) (const_int))))

	     Where const_int is small.  In that case the const
	     expression is a valid pointer for indexing. 

	     If const_int is big, but can be divided evenly by shadd_const
	     and added to (reg).  This allows more scaled indexed addresses.  */
	  if (GET_CODE (XEXP (y, 0)) == SYMBOL_REF
	      && GET_CODE (XEXP (x, 0)) == MULT
	      && GET_CODE (XEXP (y, 1)) == CONST_INT
	      && INTVAL (XEXP (y, 1)) >= -4096
	      && INTVAL (XEXP (y, 1)) <= 4095
	      && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
	      && shadd_constant_p (INTVAL (XEXP (XEXP (x, 0), 1))))
	    {
	      int val = INTVAL (XEXP (XEXP (x, 0), 1));
	      rtx reg1, reg2;

	      reg1 = XEXP (x, 1);
	      if (GET_CODE (reg1) != REG)
		reg1 = force_reg (Pmode, force_operand (reg1, 0));

	      reg2 = XEXP (XEXP (x, 0), 0);
	      if (GET_CODE (reg2) != REG)
	        reg2 = force_reg (Pmode, force_operand (reg2, 0));

	      return force_reg (Pmode, gen_rtx (PLUS, Pmode,
						gen_rtx (MULT, Pmode,
							 reg2, GEN_INT (val)),
						reg1));
	    }
	  else if ((mode == DFmode || mode == SFmode)
		   && GET_CODE (XEXP (y, 0)) == SYMBOL_REF
		   && GET_CODE (XEXP (x, 0)) == MULT
		   && GET_CODE (XEXP (y, 1)) == CONST_INT
		   && INTVAL (XEXP (y, 1)) % INTVAL (XEXP (XEXP (x, 0), 1)) == 0
		   && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
		   && shadd_constant_p (INTVAL (XEXP (XEXP (x, 0), 1))))
	    {
	      regx1
		= force_reg (Pmode, GEN_INT (INTVAL (XEXP (y, 1))
					     / INTVAL (XEXP (XEXP (x, 0), 1))));
	      regx2 = XEXP (XEXP (x, 0), 0);
	      if (GET_CODE (regx2) != REG)
		regx2 = force_reg (Pmode, force_operand (regx2, 0));
	      regx2 = force_reg (Pmode, gen_rtx (GET_CODE (y), Pmode,
						 regx2, regx1));
	      return force_reg (Pmode,
				gen_rtx (PLUS, Pmode,
					 gen_rtx (MULT, Pmode, regx2,
						  XEXP (XEXP (x, 0), 1)),
					 force_reg (Pmode, XEXP (y, 0))));
	    }
	  else if (GET_CODE (XEXP (y, 1)) == CONST_INT
		   && INTVAL (XEXP (y, 1)) >= -4096
		   && INTVAL (XEXP (y, 1)) <= 4095)
	    {
	      /* This is safe because of the guard page at the
		 beginning and end of the data space.  Just
		 return the original address.  */
	      return orig;
	    }
	  else
	    {
	      /* Doesn't look like one we can optimize.  */
	      regx1 = force_reg (Pmode, force_operand (XEXP (x, 0), 0));
	      regy1 = force_reg (Pmode, force_operand (XEXP (y, 0), 0));
	      regy2 = force_reg (Pmode, force_operand (XEXP (y, 1), 0));
	      regx1 = force_reg (Pmode,
				 gen_rtx (GET_CODE (y), Pmode, regx1, regy2));
	      return force_reg (Pmode, gen_rtx (PLUS, Pmode, regx1, regy1));
	    }
	}
    }

  return orig;
}

/* For the HPPA, REG and REG+CONST is cost 0
   and addresses involving symbolic constants are cost 2.

   PIC addresses are very expensive.

   It is no coincidence that this has the same structure
   as GO_IF_LEGITIMATE_ADDRESS.  */
int
hppa_address_cost (X)
     rtx X;
{
  if (GET_CODE (X) == PLUS)
      return 1;
  else if (GET_CODE (X) == LO_SUM)
    return 1;
  else if (GET_CODE (X) == HIGH)
    return 2;
  return 4;
}

/* Emit insns to move operands[1] into operands[0].

   Return 1 if we have written out everything that needs to be done to
   do the move.  Otherwise, return 0 and the caller will emit the move
   normally.  */

int
emit_move_sequence (operands, mode, scratch_reg)
     rtx *operands;
     enum machine_mode mode;
     rtx scratch_reg;
{
  register rtx operand0 = operands[0];
  register rtx operand1 = operands[1];
  register rtx tem;

  if (reload_in_progress && GET_CODE (operand0) == REG
      && REGNO (operand0) >= FIRST_PSEUDO_REGISTER)
    operand0 = reg_equiv_mem[REGNO (operand0)];
  else if (reload_in_progress && GET_CODE (operand0) == SUBREG
	   && GET_CODE (SUBREG_REG (operand0)) == REG
	   && REGNO (SUBREG_REG (operand0)) >= FIRST_PSEUDO_REGISTER)
    {
      SUBREG_REG (operand0) = reg_equiv_mem[REGNO (SUBREG_REG (operand0))];
      operand0 = alter_subreg (operand0);
    }

  if (reload_in_progress && GET_CODE (operand1) == REG
      && REGNO (operand1) >= FIRST_PSEUDO_REGISTER)
    operand1 = reg_equiv_mem[REGNO (operand1)];
  else if (reload_in_progress && GET_CODE (operand1) == SUBREG
	   && GET_CODE (SUBREG_REG (operand1)) == REG
	   && REGNO (SUBREG_REG (operand1)) >= FIRST_PSEUDO_REGISTER)
    {
      SUBREG_REG (operand1) = reg_equiv_mem[REGNO (SUBREG_REG (operand1))];
      operand1 = alter_subreg (operand1);
    }

  if (reload_in_progress && GET_CODE (operand0) == MEM
      && ((tem = find_replacement (&XEXP (operand0, 0)))
	  != XEXP (operand0, 0)))
    operand0 = gen_rtx (MEM, GET_MODE (operand0), tem);
  if (reload_in_progress && GET_CODE (operand1) == MEM
      && ((tem = find_replacement (&XEXP (operand1, 0)))
	  != XEXP (operand1, 0)))
    operand1 = gen_rtx (MEM, GET_MODE (operand1), tem);

  /* Handle secondary reloads for loads/stores of FP registers from
     REG+D addresses where D does not fit in 5 bits, including 
     (subreg (mem (addr))) cases.  */
  if (fp_reg_operand (operand0, mode)
      && ((GET_CODE (operand1) == MEM
	   && ! memory_address_p (DFmode, XEXP (operand1, 0)))
	  || ((GET_CODE (operand1) == SUBREG
	       && GET_CODE (XEXP (operand1, 0)) == MEM
	       && !memory_address_p (DFmode, XEXP (XEXP (operand1, 0), 0)))))
      && scratch_reg)
    {
      if (GET_CODE (operand1) == SUBREG)
	operand1 = XEXP (operand1, 0);

      scratch_reg = gen_rtx (REG, SImode, REGNO (scratch_reg));

      /* D might not fit in 14 bits either; for such cases load D into
	 scratch reg.  */
      if (!memory_address_p (SImode, XEXP (operand1, 0)))
	{
	  emit_move_insn (scratch_reg, XEXP (XEXP (operand1, 0), 1));
	  emit_move_insn (scratch_reg, gen_rtx (GET_CODE (XEXP (operand1, 0)),
						SImode,
						XEXP (XEXP (operand1, 0), 0),
						scratch_reg));
	}
      else
	emit_move_insn (scratch_reg, XEXP (operand1, 0));
      emit_insn (gen_rtx (SET, VOIDmode, operand0, gen_rtx (MEM, mode,
							    scratch_reg)));
      return 1;
    }
  else if (fp_reg_operand (operand1, mode)
	   && ((GET_CODE (operand0) == MEM
		&& ! memory_address_p (DFmode, XEXP (operand0, 0)))
	       || ((GET_CODE (operand0) == SUBREG)
		   && GET_CODE (XEXP (operand0, 0)) == MEM
		   && !memory_address_p (DFmode, XEXP (XEXP (operand0, 0), 0))))
	   && scratch_reg)
    {
      if (GET_CODE (operand0) == SUBREG)
	operand0 = XEXP (operand0, 0);

      scratch_reg = gen_rtx (REG, SImode, REGNO (scratch_reg));
      /* D might not fit in 14 bits either; for such cases load D into
	 scratch reg.  */
      if (!memory_address_p (SImode, XEXP (operand0, 0)))
	{
	  emit_move_insn (scratch_reg, XEXP (XEXP (operand0, 0), 1));
	  emit_move_insn (scratch_reg, gen_rtx (GET_CODE (XEXP (operand0, 0)),
						SImode,
						XEXP (XEXP (operand0, 0), 0),
						scratch_reg));
	}
      else
	emit_move_insn (scratch_reg, XEXP (operand0, 0));
      emit_insn (gen_rtx (SET, VOIDmode, gen_rtx (MEM, mode, scratch_reg),
			  operand1));
      return 1;
    }
  /* Handle secondary reloads for loads of FP registers from constant
     expressions by forcing the constant into memory.

     use scratch_reg to hold the address of the memory location.

     ??? The proper fix is to change PREFERRED_RELOAD_CLASS to return
     NO_REGS when presented with a const_int and an register class
     containing only FP registers.  Doing so unfortunately creates
     more problems than it solves.   Fix this for 2.5.  */
  else if (fp_reg_operand (operand0, mode)
	   && CONSTANT_P (operand1)
	   && scratch_reg)
    {
      rtx xoperands[2];

      /* Force the constant into memory and put the address of the
	 memory location into scratch_reg.  */
      xoperands[0] = scratch_reg;
      xoperands[1] = XEXP (force_const_mem (mode, operand1), 0);
      emit_move_sequence (xoperands, Pmode, 0);

      /* Now load the destination register.  */
      emit_insn (gen_rtx (SET, mode, operand0,
			  gen_rtx (MEM, mode, scratch_reg)));
      return 1;
    }
  /* Handle secondary reloads for SAR.  These occur when trying to load
     the SAR from memory a FP register, or with a constant.  */
  else if (GET_CODE (operand0) == REG
	   && REGNO_REG_CLASS (REGNO (operand0)) == SHIFT_REGS
	   && (GET_CODE (operand1) == MEM
	       || GET_CODE (operand1) == CONST_INT
	       || (GET_CODE (operand1) == REG
		   && FP_REG_CLASS_P (REGNO_REG_CLASS (REGNO (operand1)))))
	   && scratch_reg)
    {
      /* D might not fit in 14 bits either; for such cases load D into
	 scratch reg.  */
      if (GET_CODE (operand1) == MEM
	  && !memory_address_p (SImode, XEXP (operand1, 0)))
	{
	  emit_move_insn (scratch_reg, XEXP (XEXP (operand1, 0), 1));	
	  emit_move_insn (scratch_reg, gen_rtx (GET_CODE (XEXP (operand1, 0)),
						SImode,
						XEXP (XEXP (operand1, 0), 0),
						scratch_reg));
	  emit_move_insn (scratch_reg, gen_rtx (MEM, GET_MODE (operand1),
						scratch_reg));
	}
      else
	emit_move_insn (scratch_reg, operand1);
      emit_move_insn (operand0, scratch_reg);
      return 1;
    }
  /* Handle most common case: storing into a register.  */
  else if (register_operand (operand0, mode))
    {
      if (register_operand (operand1, mode)
	  || (GET_CODE (operand1) == CONST_INT && INT_14_BITS (operand1))
	  || (operand1 == CONST0_RTX (mode))
	  || (GET_CODE (operand1) == HIGH
	      && !symbolic_operand (XEXP (operand1, 0), VOIDmode))
	  /* Only `general_operands' can come here, so MEM is ok.  */
	  || GET_CODE (operand1) == MEM)
	{
	  /* Run this case quickly.  */
	  emit_insn (gen_rtx (SET, VOIDmode, operand0, operand1));
	  return 1;
	}
    }
  else if (GET_CODE (operand0) == MEM)
    {
      if (mode == DFmode && operand1 == CONST0_RTX (mode)
	  && !(reload_in_progress || reload_completed))
	{
	  rtx temp = gen_reg_rtx (DFmode);

	  emit_insn (gen_rtx (SET, VOIDmode, temp, operand1));
	  emit_insn (gen_rtx (SET, VOIDmode, operand0, temp));
	  return 1;
	}
      if (register_operand (operand1, mode) || operand1 == CONST0_RTX (mode))
	{
	  /* Run this case quickly.  */
	  emit_insn (gen_rtx (SET, VOIDmode, operand0, operand1));
	  return 1;
	}
      if (! (reload_in_progress || reload_completed))
	{
	  operands[0] = validize_mem (operand0);
	  operands[1] = operand1 = force_reg (mode, operand1);
	}
    }

  /* Simplify the source if we need to.  */
  if ((GET_CODE (operand1) != HIGH && immediate_operand (operand1, mode))
      || (GET_CODE (operand1) == HIGH
	  && symbolic_operand (XEXP (operand1, 0), mode)))
    {
      int ishighonly = 0;

      if (GET_CODE (operand1) == HIGH)
	{
	  ishighonly = 1;
	  operand1 = XEXP (operand1, 0);
	}
      if (symbolic_operand (operand1, mode))
	{
	  /* Argh.  The assembler and linker can't handle arithmetic
	     involving plabels.

	     So we force the plabel into memory, load operand0 from
	     the memory location, then add in the constant part.  */
	  if (GET_CODE (operand1) == CONST
	      && GET_CODE (XEXP (operand1, 0)) == PLUS
	      && function_label_operand (XEXP (XEXP (operand1, 0), 0), Pmode))
	    {
	      rtx temp, const_part;

	      /* Figure out what (if any) scratch register to use.  */
	      if (reload_in_progress || reload_completed)
		scratch_reg = scratch_reg ? scratch_reg : operand0;
	      else if (flag_pic)
		scratch_reg = gen_reg_rtx (Pmode);

	      /* Save away the constant part of the expression.  */
	      const_part = XEXP (XEXP (operand1, 0), 1);
	      if (GET_CODE (const_part) != CONST_INT)
		abort ();

	      /* Force the function label into memory.  */
	      temp = force_const_mem (mode, XEXP (XEXP (operand1, 0), 0));

	      /* Get the address of the memory location.  PIC-ify it if
		 necessary.  */
	      temp = XEXP (temp, 0);
	      if (flag_pic)
		temp = legitimize_pic_address (temp, mode, scratch_reg);

	      /* Put the address of the memory location into our destination
		 register.  */
	      operands[1] = temp;
	      emit_move_sequence (operands, mode, scratch_reg);

	      /* Now load from the memory location into our destination
		 register.  */
	      operands[1] = gen_rtx (MEM, Pmode, operands[0]);
	      emit_move_sequence (operands, mode, scratch_reg);

	      /* And add back in the constant part.  */
	      expand_inc (operand0, const_part);

	      return 1;
	    }

	  if (flag_pic)
	    {
	      rtx temp;

	      if (reload_in_progress || reload_completed)
		temp = scratch_reg ? scratch_reg : operand0;
	      else
		temp = gen_reg_rtx (Pmode);

	      /* (const (plus (symbol) (const_int))) must be forced to
		 memory during/after reload if the const_int will not fit
		 in 14 bits.  */
	      if (GET_CODE (operand1) == CONST
		       && GET_CODE (XEXP (operand1, 0)) == PLUS
		       && GET_CODE (XEXP (XEXP (operand1, 0), 1)) == CONST_INT
		       && !INT_14_BITS (XEXP (XEXP (operand1, 0), 1))
		       && (reload_completed || reload_in_progress)
		       && flag_pic)
		{
		  operands[1] = force_const_mem (mode, operand1);
		  operands[1] = legitimize_pic_address (XEXP (operands[1], 0),
							mode, temp);
		  emit_move_sequence (operands, mode, temp);
		}
	      else
		{
		  operands[1] = legitimize_pic_address (operand1, mode, temp);
		  emit_insn (gen_rtx (SET, VOIDmode, operand0, operands[1]));
		}
	    }
	  /* On the HPPA, references to data space are supposed to use dp,
	     register 27, but showing it in the RTL inhibits various cse
	     and loop optimizations.  */
	  else
	    {
	      rtx temp, set;

	      if (reload_in_progress || reload_completed)
		temp = scratch_reg ? scratch_reg : operand0;
	      else
		temp = gen_reg_rtx (mode);

	      /* Loading a SYMBOL_REF into a register makes that register
		 safe to be used as the base in an indexed address. 

		 Don't mark hard registers though.  That loses.  */
	      if (GET_CODE (operand0) == REG
		  && REGNO (operand0) >= FIRST_PSEUDO_REGISTER)
		REGNO_POINTER_FLAG (REGNO (operand0)) = 1;
	      if (REGNO (temp) >= FIRST_PSEUDO_REGISTER)
		REGNO_POINTER_FLAG (REGNO (temp)) = 1;
	      if (ishighonly)
		set = gen_rtx (SET, mode, operand0, temp);
	      else
		set = gen_rtx (SET, VOIDmode,
			       operand0,
			       gen_rtx (LO_SUM, mode, temp, operand1));

	      emit_insn (gen_rtx (SET, VOIDmode,
				  temp,
				  gen_rtx (HIGH, mode, operand1)));
	      emit_insn (set);

	    }
	  return 1;
	}
      else if (GET_CODE (operand1) != CONST_INT
	       || ! cint_ok_for_move (INTVAL (operand1)))
	{
	  rtx temp;

	  if (reload_in_progress || reload_completed)
	    temp = operand0;
	  else
	    temp = gen_reg_rtx (mode);

	  emit_insn (gen_rtx (SET, VOIDmode, temp,
			      gen_rtx (HIGH, mode, operand1)));
	  operands[1] = gen_rtx (LO_SUM, mode, temp, operand1);
	}
    }
  /* Now have insn-emit do whatever it normally does.  */
  return 0;
}

/* Examine EXP and return nonzero if it contains an ADDR_EXPR (meaning
   it will need a link/runtime reloc).  */

int
reloc_needed (exp)
     tree exp;
{
  int reloc = 0;

  switch (TREE_CODE (exp))
    {
    case ADDR_EXPR:
      return 1;

    case PLUS_EXPR:
    case MINUS_EXPR:
      reloc = reloc_needed (TREE_OPERAND (exp, 0));
      reloc |= reloc_needed (TREE_OPERAND (exp, 1));
      break;

    case NOP_EXPR:
    case CONVERT_EXPR:
    case NON_LVALUE_EXPR:
      reloc = reloc_needed (TREE_OPERAND (exp, 0));
      break;

    case CONSTRUCTOR:
      {
	register tree link;
	for (link = CONSTRUCTOR_ELTS (exp); link; link = TREE_CHAIN (link))
	  if (TREE_VALUE (link) != 0)
	    reloc |= reloc_needed (TREE_VALUE (link));
      }
      break;

    case ERROR_MARK:
      break;
    }
  return reloc;
}

/* Does operand (which is a symbolic_operand) live in text space? If
   so SYMBOL_REF_FLAG, which is set by ENCODE_SECTION_INFO, will be true.  */

int
read_only_operand (operand)
     rtx operand;
{
  if (GET_CODE (operand) == CONST)
    operand = XEXP (XEXP (operand, 0), 0);
  if (flag_pic)
    {
      if (GET_CODE (operand) == SYMBOL_REF)
	return SYMBOL_REF_FLAG (operand) && !CONSTANT_POOL_ADDRESS_P (operand);
    }
  else
    {
      if (GET_CODE (operand) == SYMBOL_REF)
	return SYMBOL_REF_FLAG (operand) || CONSTANT_POOL_ADDRESS_P (operand);
    }
  return 1;
}


/* Return the best assembler insn template
   for moving operands[1] into operands[0] as a fullword.   */
char *
singlemove_string (operands)
     rtx *operands;
{
  HOST_WIDE_INT intval;

  if (GET_CODE (operands[0]) == MEM)
    return "stw %r1,%0";
  if (GET_CODE (operands[1]) == MEM)
    return "ldw %1,%0";
  if (GET_CODE (operands[1]) == CONST_DOUBLE)
    {
      long i;
      REAL_VALUE_TYPE d;

      if (GET_MODE (operands[1]) != SFmode)
	abort ();

      /* Translate the CONST_DOUBLE to a CONST_INT with the same target
	 bit pattern.  */
      REAL_VALUE_FROM_CONST_DOUBLE (d, operands[1]);
      REAL_VALUE_TO_TARGET_SINGLE (d, i);

      operands[1] = GEN_INT (i);
      /* Fall through to CONST_INT case.  */
    }
  if (GET_CODE (operands[1]) == CONST_INT)
    {
      intval = INTVAL (operands[1]);

      if (VAL_14_BITS_P (intval))
	return "ldi %1,%0";
      else if ((intval & 0x7ff) == 0)
	return "ldil L'%1,%0";
      else if (zdepi_cint_p (intval))
	return "zdepi %Z1,%0";
      else
	return "ldil L'%1,%0\n\tldo R'%1(%0),%0";
    }
  return "copy %1,%0";
}


/* Compute position (in OP[1]) and width (in OP[2])
   useful for copying IMM to a register using the zdepi
   instructions.  Store the immediate value to insert in OP[0].  */
void
compute_zdepi_operands (imm, op)
     unsigned HOST_WIDE_INT imm;
     unsigned *op;
{
  int lsb, len;

  /* Find the least significant set bit in IMM.  */
  for (lsb = 0; lsb < 32; lsb++)
    {
      if ((imm & 1) != 0)
        break;
      imm >>= 1;
    }

  /* Choose variants based on *sign* of the 5-bit field.  */
  if ((imm & 0x10) == 0)
    len = (lsb <= 28) ? 4 : 32 - lsb;
  else
    {
      /* Find the width of the bitstring in IMM.  */
      for (len = 5; len < 32; len++)
	{
	  if ((imm & (1 << len)) == 0)
	    break;
	}

      /* Sign extend IMM as a 5-bit value.  */
      imm = (imm & 0xf) - 0x10;
    }

  op[0] = imm;
  op[1] = 31 - lsb;
  op[2] = len;
}

/* Output assembler code to perform a doubleword move insn
   with operands OPERANDS.  */

char *
output_move_double (operands)
     rtx *operands;
{
  enum { REGOP, OFFSOP, MEMOP, CNSTOP, RNDOP } optype0, optype1;
  rtx latehalf[2];
  rtx addreg0 = 0, addreg1 = 0;

  /* First classify both operands.  */

  if (REG_P (operands[0]))
    optype0 = REGOP;
  else if (offsettable_memref_p (operands[0]))
    optype0 = OFFSOP;
  else if (GET_CODE (operands[0]) == MEM)
    optype0 = MEMOP;
  else
    optype0 = RNDOP;

  if (REG_P (operands[1]))
    optype1 = REGOP;
  else if (CONSTANT_P (operands[1]))
    optype1 = CNSTOP;
  else if (offsettable_memref_p (operands[1]))
    optype1 = OFFSOP;
  else if (GET_CODE (operands[1]) == MEM)
    optype1 = MEMOP;
  else
    optype1 = RNDOP;

  /* Check for the cases that the operand constraints are not
     supposed to allow to happen.  Abort if we get one,
     because generating code for these cases is painful.  */

  if (optype0 != REGOP && optype1 != REGOP)
    abort ();

   /* Handle auto decrementing and incrementing loads and stores
     specifically, since the structure of the function doesn't work
     for them without major modification.  Do it better when we learn
     this port about the general inc/dec addressing of PA.
     (This was written by tege.  Chide him if it doesn't work.)  */

  if (optype0 == MEMOP)
    {
      /* We have to output the address syntax ourselves, since print_operand
	 doesn't deal with the addresses we want to use.  Fix this later.  */

      rtx addr = XEXP (operands[0], 0);
      if (GET_CODE (addr) == POST_INC || GET_CODE (addr) == POST_DEC)
	{
	  rtx high_reg = gen_rtx (SUBREG, SImode, operands[1], 0);

	  operands[0] = XEXP (addr, 0);
	  if (GET_CODE (operands[1]) != REG || GET_CODE (operands[0]) != REG)
	    abort ();

	  if (!reg_overlap_mentioned_p (high_reg, addr))
	    {
	      /* No overlap between high target register and address
		 register.  (We do this in a non-obvious way to
		 save a register file writeback)  */
	      if (GET_CODE (addr) == POST_INC)
		return "stws,ma %1,8(0,%0)\n\tstw %R1,-4(0,%0)";
	      return "stws,ma %1,-8(0,%0)\n\tstw %R1,12(0,%0)";
	    }
	  else
	    abort();
	}
      else if (GET_CODE (addr) == PRE_INC || GET_CODE (addr) == PRE_DEC)
	{
	  rtx high_reg = gen_rtx (SUBREG, SImode, operands[1], 0);

	  operands[0] = XEXP (addr, 0);
	  if (GET_CODE (operands[1]) != REG || GET_CODE (operands[0]) != REG)
	    abort ();

	  if (!reg_overlap_mentioned_p (high_reg, addr))
	    {
	      /* No overlap between high target register and address
		 register.  (We do this in a non-obvious way to
		 save a register file writeback)  */
	      if (GET_CODE (addr) == PRE_INC)
		return "stws,mb %1,8(0,%0)\n\tstw %R1,4(0,%0)";
	      return "stws,mb %1,-8(0,%0)\n\tstw %R1,4(0,%0)";
	    }
	  else
	    abort();
	}
    }
  if (optype1 == MEMOP)
    {
      /* We have to output the address syntax ourselves, since print_operand
	 doesn't deal with the addresses we want to use.  Fix this later.  */

      rtx addr = XEXP (operands[1], 0);
      if (GET_CODE (addr) == POST_INC || GET_CODE (addr) == POST_DEC)
	{
	  rtx high_reg = gen_rtx (SUBREG, SImode, operands[0], 0);

	  operands[1] = XEXP (addr, 0);
	  if (GET_CODE (operands[0]) != REG || GET_CODE (operands[1]) != REG)
	    abort ();

	  if (!reg_overlap_mentioned_p (high_reg, addr))
	    {
	      /* No overlap between high target register and address
		 register.  (We do this in a non-obvious way to
		 save a register file writeback)  */
	      if (GET_CODE (addr) == POST_INC)
		return "ldws,ma 8(0,%1),%0\n\tldw -4(0,%1),%R0";
	      return "ldws,ma -8(0,%1),%0\n\tldw 12(0,%1),%R0";
	    }
	  else
	    {
	      /* This is an undefined situation.  We should load into the
		 address register *and* update that register.  Probably
		 we don't need to handle this at all.  */
	      if (GET_CODE (addr) == POST_INC)
		return "ldw 4(0,%1),%R0\n\tldws,ma 8(0,%1),%0";
	      return "ldw 4(0,%1),%R0\n\tldws,ma -8(0,%1),%0";
	    }
	}
      else if (GET_CODE (addr) == PRE_INC || GET_CODE (addr) == PRE_DEC)
	{
	  rtx high_reg = gen_rtx (SUBREG, SImode, operands[0], 0);

	  operands[1] = XEXP (addr, 0);
	  if (GET_CODE (operands[0]) != REG || GET_CODE (operands[1]) != REG)
	    abort ();

	  if (!reg_overlap_mentioned_p (high_reg, addr))
	    {
	      /* No overlap between high target register and address
		 register.  (We do this in a non-obvious way to
		 save a register file writeback)  */
	      if (GET_CODE (addr) == PRE_INC)
		return "ldws,mb 8(0,%1),%0\n\tldw 4(0,%1),%R0";
	      return "ldws,mb -8(0,%1),%0\n\tldw 4(0,%1),%R0";
	    }
	  else
	    {
	      /* This is an undefined situation.  We should load into the
		 address register *and* update that register.  Probably
		 we don't need to handle this at all.  */
	      if (GET_CODE (addr) == PRE_INC)
		return "ldw 12(0,%1),%R0\n\tldws,mb 8(0,%1),%0";
	      return "ldw -4(0,%1),%R0\n\tldws,mb -8(0,%1),%0";
	    }
	}
      else if (GET_CODE (addr) == PLUS
	       && GET_CODE (XEXP (addr, 0)) == MULT)
	{
	  rtx high_reg = gen_rtx (SUBREG, SImode, operands[0], 0);

	  if (!reg_overlap_mentioned_p (high_reg, addr))
	    {
	      rtx xoperands[3];

	      xoperands[0] = high_reg;
	      xoperands[1] = XEXP (addr, 1);
	      xoperands[2] = XEXP (XEXP (addr, 0), 0);
	      xoperands[3] = XEXP (XEXP (addr, 0), 1);
	      output_asm_insn ("sh%O3addl %2,%1,%0", xoperands);
	      return "ldw 4(0,%0),%R0\n\tldw 0(0,%0),%0";
	    }
	  else
	    {
	      rtx xoperands[3];

	      xoperands[0] = high_reg;
	      xoperands[1] = XEXP (addr, 1);
	      xoperands[2] = XEXP (XEXP (addr, 0), 0);
	      xoperands[3] = XEXP (XEXP (addr, 0), 1);
	      output_asm_insn ("sh%O3addl %2,%1,%R0", xoperands);
	      return "ldw 0(0,%R0),%0\n\tldw 4(0,%R0),%R0";
	    }
	   
	}
    }

  /* If an operand is an unoffsettable memory ref, find a register
     we can increment temporarily to make it refer to the second word.  */

  if (optype0 == MEMOP)
    addreg0 = find_addr_reg (XEXP (operands[0], 0));

  if (optype1 == MEMOP)
    addreg1 = find_addr_reg (XEXP (operands[1], 0));

  /* Ok, we can do one word at a time.
     Normally we do the low-numbered word first.

     In either case, set up in LATEHALF the operands to use
     for the high-numbered word and in some cases alter the
     operands in OPERANDS to be suitable for the low-numbered word.  */

  if (optype0 == REGOP)
    latehalf[0] = gen_rtx (REG, SImode, REGNO (operands[0]) + 1);
  else if (optype0 == OFFSOP)
    latehalf[0] = adj_offsettable_operand (operands[0], 4);
  else
    latehalf[0] = operands[0];

  if (optype1 == REGOP)
    latehalf[1] = gen_rtx (REG, SImode, REGNO (operands[1]) + 1);
  else if (optype1 == OFFSOP)
    latehalf[1] = adj_offsettable_operand (operands[1], 4);
  else if (optype1 == CNSTOP)
    split_double (operands[1], &operands[1], &latehalf[1]);
  else
    latehalf[1] = operands[1];

  /* If the first move would clobber the source of the second one,
     do them in the other order.

     This can happen in two cases:

	mem -> register where the first half of the destination register
 	is the same register used in the memory's address.  Reload
	can create such insns.

	mem in this case will be either register indirect or register
	indirect plus a valid offset. 

	register -> register move where REGNO(dst) == REGNO(src + 1)
	someone (Tim/Tege?) claimed this can happen for parameter loads. 

     Handle mem -> register case first.  */
  if (optype0 == REGOP
      && (optype1 == MEMOP || optype1 == OFFSOP)
      && refers_to_regno_p (REGNO (operands[0]), REGNO (operands[0]) + 1,
			    operands[1], 0))
    {
      /* Do the late half first.  */
      if (addreg1)
	output_asm_insn ("ldo 4(%0),%0", &addreg1);
      output_asm_insn (singlemove_string (latehalf), latehalf);

      /* Then clobber.  */
      if (addreg1)
	output_asm_insn ("ldo -4(%0),%0", &addreg1);
      return singlemove_string (operands);
    }

  /* Now handle register -> register case.  */
  if (optype0 == REGOP && optype1 == REGOP
      && REGNO (operands[0]) == REGNO (operands[1]) + 1)
    {
      output_asm_insn (singlemove_string (latehalf), latehalf);
      return singlemove_string (operands);
    }

  /* Normal case: do the two words, low-numbered first.  */

  output_asm_insn (singlemove_string (operands), operands);

  /* Make any unoffsettable addresses point at high-numbered word.  */
  if (addreg0)
    output_asm_insn ("ldo 4(%0),%0", &addreg0);
  if (addreg1)
    output_asm_insn ("ldo 4(%0),%0", &addreg1);

  /* Do that word.  */
  output_asm_insn (singlemove_string (latehalf), latehalf);

  /* Undo the adds we just did.  */
  if (addreg0)
    output_asm_insn ("ldo -4(%0),%0", &addreg0);
  if (addreg1)
    output_asm_insn ("ldo -4(%0),%0", &addreg1);

  return "";
}

char *
output_fp_move_double (operands)
     rtx *operands;
{
  if (FP_REG_P (operands[0]))
    {
      if (FP_REG_P (operands[1])
	  || operands[1] == CONST0_RTX (GET_MODE (operands[0])))
	output_asm_insn ("fcpy,dbl %r1,%0", operands);
      else
	output_asm_insn ("fldd%F1 %1,%0", operands);
    }
  else if (FP_REG_P (operands[1]))
    {
      output_asm_insn ("fstd%F0 %1,%0", operands);
    }
  else if (operands[1] == CONST0_RTX (GET_MODE (operands[0])))
    {
      if (GET_CODE (operands[0]) == REG)
	{
	  rtx xoperands[2];
	  xoperands[1] = gen_rtx (REG, SImode, REGNO (operands[0]) + 1);
	  xoperands[0] = operands[0];
	  output_asm_insn ("copy %%r0,%0\n\tcopy %%r0,%1", xoperands);
	}
      /* This is a pain.  You have to be prepared to deal with an
	 arbitrary address here including pre/post increment/decrement.

	 so avoid this in the MD.  */
      else
	abort ();
    }
  else abort ();
  return "";
}

/* Return a REG that occurs in ADDR with coefficient 1.
   ADDR can be effectively incremented by incrementing REG.  */

static rtx
find_addr_reg (addr)
     rtx addr;
{
  while (GET_CODE (addr) == PLUS)
    {
      if (GET_CODE (XEXP (addr, 0)) == REG)
	addr = XEXP (addr, 0);
      else if (GET_CODE (XEXP (addr, 1)) == REG)
	addr = XEXP (addr, 1);
      else if (CONSTANT_P (XEXP (addr, 0)))
	addr = XEXP (addr, 1);
      else if (CONSTANT_P (XEXP (addr, 1)))
	addr = XEXP (addr, 0);
      else
	abort ();
    }
  if (GET_CODE (addr) == REG)
    return addr;
  abort ();
}

/* Emit code to perform a block move.

   OPERANDS[0] is the destination pointer as a REG, clobbered.
   OPERANDS[1] is the source pointer as a REG, clobbered.
   OPERANDS[2] is a register for temporary storage.
   OPERANDS[4] is the size as a CONST_INT
   OPERANDS[3] is a register for temporary storage.
   OPERANDS[5] is the alignment safe to use, as a CONST_INT. 
   OPERANDS[6] is another temporary register.   */

char *
output_block_move (operands, size_is_constant)
     rtx *operands;
     int size_is_constant;
{
  int align = INTVAL (operands[5]);
  unsigned long n_bytes = INTVAL (operands[4]);

  /* We can't move more than four bytes at a time because the PA
     has no longer integer move insns.  (Could use fp mem ops?)  */
  if (align > 4)
    align = 4;

  /* Note that we know each loop below will execute at least twice
     (else we would have open-coded the copy).  */
  switch (align)
    {
      case 4:
	/* Pre-adjust the loop counter.  */
	operands[4] = GEN_INT (n_bytes - 8);
	output_asm_insn ("ldi %4,%2", operands);

	/* Copying loop.  */
	output_asm_insn ("ldws,ma 4(0,%1),%3", operands);
	output_asm_insn ("ldws,ma 4(0,%1),%6", operands);
	output_asm_insn ("stws,ma %3,4(0,%0)", operands);
	output_asm_insn ("addib,>= -8,%2,.-12", operands);
	output_asm_insn ("stws,ma %6,4(0,%0)", operands);

	/* Handle the residual.  There could be up to 7 bytes of
	   residual to copy!  */
	if (n_bytes % 8 != 0)
	  {
	    operands[4] = GEN_INT (n_bytes % 4);
	    if (n_bytes % 8 >= 4)
	      output_asm_insn ("ldws,ma 4(0,%1),%3", operands);
	    if (n_bytes % 4 != 0)
	      output_asm_insn ("ldw 0(0,%1),%6", operands);
	    if (n_bytes % 8 >= 4)
	      output_asm_insn ("stws,ma %3,4(0,%0)", operands);
	    if (n_bytes % 4 != 0)
	      output_asm_insn ("stbys,e %6,%4(0,%0)", operands);
	  }
	return "";

      case 2:
	/* Pre-adjust the loop counter.  */
	operands[4] = GEN_INT (n_bytes - 4);
	output_asm_insn ("ldi %4,%2", operands);

	/* Copying loop.  */
	output_asm_insn ("ldhs,ma 2(0,%1),%3", operands);
	output_asm_insn ("ldhs,ma 2(0,%1),%6", operands);
	output_asm_insn ("sths,ma %3,2(0,%0)", operands);
	output_asm_insn ("addib,>= -4,%2,.-12", operands);
	output_asm_insn ("sths,ma %6,2(0,%0)", operands);

	/* Handle the residual.  */
	if (n_bytes % 4 != 0)
	  {
	    if (n_bytes % 4 >= 2)
	      output_asm_insn ("ldhs,ma 2(0,%1),%3", operands);
	    if (n_bytes % 2 != 0)
	      output_asm_insn ("ldb 0(0,%1),%6", operands);
	    if (n_bytes % 4 >= 2)
	      output_asm_insn ("sths,ma %3,2(0,%0)", operands);
	    if (n_bytes % 2 != 0)
	      output_asm_insn ("stb %6,0(0,%0)", operands);
	  }
	return "";

      case 1:
	/* Pre-adjust the loop counter.  */
	operands[4] = GEN_INT (n_bytes - 2);
	output_asm_insn ("ldi %4,%2", operands);

	/* Copying loop.  */
	output_asm_insn ("ldbs,ma 1(0,%1),%3", operands);
	output_asm_insn ("ldbs,ma 1(0,%1),%6", operands);
	output_asm_insn ("stbs,ma %3,1(0,%0)", operands);
	output_asm_insn ("addib,>= -2,%2,.-12", operands);
	output_asm_insn ("stbs,ma %6,1(0,%0)", operands);

	/* Handle the residual.  */
	if (n_bytes % 2 != 0)
	  {
	    output_asm_insn ("ldb 0(0,%1),%3", operands);
	    output_asm_insn ("stb %3,0(0,%0)", operands);
	  }
	return "";

      default:
	abort ();
    }
}

/* Count the number of insns necessary to handle this block move.

   Basic structure is the same as emit_block_move, except that we
   count insns rather than emit them.  */

int
compute_movstrsi_length (insn)
     rtx insn;
{
  rtx pat = PATTERN (insn);
  int align = INTVAL (XEXP (XVECEXP (pat, 0, 6), 0));
  unsigned long n_bytes = INTVAL (XEXP (XVECEXP (pat, 0, 5), 0));
  unsigned int n_insns = 0;

  /* We can't move more than four bytes at a time because the PA
     has no longer integer move insns.  (Could use fp mem ops?)  */
  if (align > 4)
    align = 4;

  /* The basic copying loop.  */
  n_insns = 6;

  /* Residuals.  */
  if (n_bytes % (2 * align) != 0)
    {
      if ((n_bytes % (2 * align)) >= align)
	n_insns += 2;

      if ((n_bytes % align) != 0)
	n_insns += 2;
    }

  /* Lengths are expressed in bytes now; each insn is 4 bytes.  */
  return n_insns * 4;
}


char *
output_and (operands)
     rtx *operands;
{
  if (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) != 0)
    {
      unsigned HOST_WIDE_INT mask = INTVAL (operands[2]);
      int ls0, ls1, ms0, p, len;

      for (ls0 = 0; ls0 < 32; ls0++)
	if ((mask & (1 << ls0)) == 0)
	  break;

      for (ls1 = ls0; ls1 < 32; ls1++)
	if ((mask & (1 << ls1)) != 0)
	  break;

      for (ms0 = ls1; ms0 < 32; ms0++)
	if ((mask & (1 << ms0)) == 0)
	  break;

      if (ms0 != 32)
	abort();

      if (ls1 == 32)
	{
	  len = ls0;

	  if (len == 0)
	    abort ();

	  operands[2] = GEN_INT (len);
	  return "extru %1,31,%2,%0";
	}
      else
	{
	  /* We could use this `depi' for the case above as well, but `depi'
	     requires one more register file access than an `extru'.  */

	  p = 31 - ls0;
	  len = ls1 - ls0;

	  operands[2] = GEN_INT (p);
	  operands[3] = GEN_INT (len);
	  return "depi 0,%2,%3,%0";
	}
    }
  else
    return "and %1,%2,%0";
}

char *
output_ior (operands)
     rtx *operands;
{
  unsigned HOST_WIDE_INT mask = INTVAL (operands[2]);
  int bs0, bs1, p, len;

  if (INTVAL (operands[2]) == 0)
    return "copy %1,%0";

  for (bs0 = 0; bs0 < 32; bs0++)
    if ((mask & (1 << bs0)) != 0)
      break;

  for (bs1 = bs0; bs1 < 32; bs1++)
    if ((mask & (1 << bs1)) == 0)
      break;

  if (bs1 != 32 && ((unsigned HOST_WIDE_INT) 1 << bs1) <= mask)
    abort();

  p = 31 - bs0;
  len = bs1 - bs0;

  operands[2] = GEN_INT (p);
  operands[3] = GEN_INT (len);
  return "depi -1,%2,%3,%0";
}

/* Output an ascii string.  */
void
output_ascii (file, p, size)
     FILE *file;
     unsigned char *p;
     int size;
{
  int i;
  int chars_output;
  unsigned char partial_output[16];	/* Max space 4 chars can occupy.   */

  /* The HP assembler can only take strings of 256 characters at one
     time.  This is a limitation on input line length, *not* the
     length of the string.  Sigh.  Even worse, it seems that the
     restriction is in number of input characters (see \xnn &
     \whatever).  So we have to do this very carefully.  */

  fputs ("\t.STRING \"", file);

  chars_output = 0;
  for (i = 0; i < size; i += 4)
    {
      int co = 0;
      int io = 0;
      for (io = 0, co = 0; io < MIN (4, size - i); io++)
	{
	  register unsigned int c = p[i + io];

	  if (c == '\"' || c == '\\')
	    partial_output[co++] = '\\';
	  if (c >= ' ' && c < 0177)
	    partial_output[co++] = c;
	  else
	    {
	      unsigned int hexd;
	      partial_output[co++] = '\\';
	      partial_output[co++] = 'x';
	      hexd =  c  / 16 - 0 + '0';
	      if (hexd > '9')
		hexd -= '9' - 'a' + 1;
	      partial_output[co++] = hexd;
	      hexd =  c % 16 - 0 + '0';
	      if (hexd > '9')
		hexd -= '9' - 'a' + 1;
	      partial_output[co++] = hexd;
	    }
	}
      if (chars_output + co > 243)
	{
	  fputs ("\"\n\t.STRING \"", file);
	  chars_output = 0;
	}
      fwrite (partial_output, 1, co, file);
      chars_output += co;
      co = 0;
    }
  fputs ("\"\n", file);
}

/* Try to rewrite floating point comparisons & branches to avoid
   useless add,tr insns.

   CHECK_NOTES is nonzero if we should examine REG_DEAD notes
   to see if FPCC is dead.  CHECK_NOTES is nonzero for the
   first attempt to remove useless add,tr insns.  It is zero
   for the second pass as reorg sometimes leaves bogus REG_DEAD
   notes lying around.

   When CHECK_NOTES is zero we can only eliminate add,tr insns
   when there's a 1:1 correspondence between fcmp and ftest/fbranch
   instructions.  */
void
remove_useless_addtr_insns (insns, check_notes)
     rtx insns;
     int check_notes;
{
  rtx insn;
  int all;
  static int pass = 0;

  /* This is fairly cheap, so always run it when optimizing.  */
  if (optimize > 0)
    {
      int fcmp_count = 0;
      int fbranch_count = 0;

      /* Walk all the insns in this function looking for fcmp & fbranch
	 instructions.  Keep track of how many of each we find.  */
      insns = get_insns ();
      for (insn = insns; insn; insn = next_insn (insn))
	{
	  rtx tmp;

	  /* Ignore anything that isn't an INSN or a JUMP_INSN.  */
	  if (GET_CODE (insn) != INSN && GET_CODE (insn) != JUMP_INSN)
	    continue;

	  tmp = PATTERN (insn);

	  /* It must be a set.  */
	  if (GET_CODE (tmp) != SET)
	    continue;

	  /* If the destination is CCFP, then we've found an fcmp insn.  */
	  tmp = SET_DEST (tmp);
	  if (GET_CODE (tmp) == REG && REGNO (tmp) == 0)
	    {
	      fcmp_count++;
	      continue;
	    }
	    
	  tmp = PATTERN (insn);
	  /* If this is an fbranch instruction, bump the fbranch counter.  */
	  if (GET_CODE (tmp) == SET
	      && SET_DEST (tmp) == pc_rtx
	      && GET_CODE (SET_SRC (tmp)) == IF_THEN_ELSE
	      && GET_CODE (XEXP (SET_SRC (tmp), 0)) == NE
	      && GET_CODE (XEXP (XEXP (SET_SRC (tmp), 0), 0)) == REG
	      && REGNO (XEXP (XEXP (SET_SRC (tmp), 0), 0)) == 0)
	    {
	      fbranch_count++;
	      continue;
	    }
	}


      /* Find all floating point compare + branch insns.  If possible,
	 reverse the comparison & the branch to avoid add,tr insns.  */
      for (insn = insns; insn; insn = next_insn (insn))
	{
	  rtx tmp, next;

	  /* Ignore anything that isn't an INSN.  */
	  if (GET_CODE (insn) != INSN)
	    continue;

	  tmp = PATTERN (insn);

	  /* It must be a set.  */
	  if (GET_CODE (tmp) != SET)
	    continue;

	  /* The destination must be CCFP, which is register zero.  */
	  tmp = SET_DEST (tmp);
	  if (GET_CODE (tmp) != REG || REGNO (tmp) != 0)
	    continue;

	  /* INSN should be a set of CCFP.

	     See if the result of this insn is used in a reversed FP
	     conditional branch.  If so, reverse our condition and
	     the branch.  Doing so avoids useless add,tr insns.  */
	  next = next_insn (insn);
	  while (next)
	    {
	      /* Jumps, calls and labels stop our search.  */
	      if (GET_CODE (next) == JUMP_INSN
		  || GET_CODE (next) == CALL_INSN
		  || GET_CODE (next) == CODE_LABEL)
		break;

	      /* As does another fcmp insn.  */
	      if (GET_CODE (next) == INSN
		  && GET_CODE (PATTERN (next)) == SET
		  && GET_CODE (SET_DEST (PATTERN (next))) == REG
		  && REGNO (SET_DEST (PATTERN (next))) == 0)
		break;

	      next = next_insn (next);
	    }

	  /* Is NEXT_INSN a branch?  */
	  if (next
	      && GET_CODE (next) == JUMP_INSN)
	    {
	      rtx pattern = PATTERN (next);

	      /* If it a reversed fp conditional branch (eg uses add,tr)
		 and CCFP dies, then reverse our conditional and the branch
		 to avoid the add,tr.  */
	      if (GET_CODE (pattern) == SET
		  && SET_DEST (pattern) == pc_rtx
		  && GET_CODE (SET_SRC (pattern)) == IF_THEN_ELSE
		  && GET_CODE (XEXP (SET_SRC (pattern), 0)) == NE
		  && GET_CODE (XEXP (XEXP (SET_SRC (pattern), 0), 0)) == REG
		  && REGNO (XEXP (XEXP (SET_SRC (pattern), 0), 0)) == 0
		  && GET_CODE (XEXP (SET_SRC (pattern), 1)) == PC
		  && (fcmp_count == fbranch_count
		      || (check_notes
			  && find_regno_note (next, REG_DEAD, 0))))
		{
		  /* Reverse the branch.  */
		  tmp = XEXP (SET_SRC (pattern), 1);
		  XEXP (SET_SRC (pattern), 1) = XEXP (SET_SRC (pattern), 2);
		  XEXP (SET_SRC (pattern), 2) = tmp;
		  INSN_CODE (next) = -1;

		  /* Reverse our condition.  */
		  tmp = PATTERN (insn);
		  PUT_CODE (XEXP (tmp, 1),
			    reverse_condition (GET_CODE (XEXP (tmp, 1))));
		}
	    }
	}
    }

  pass = !pass;

}

/* You may have trouble believing this, but this is the HP-PA stack
   layout.  Wow.

   Offset		Contents

   Variable arguments	(optional; any number may be allocated)

   SP-(4*(N+9))		arg word N
   	:		    :
      SP-56		arg word 5
      SP-52		arg word 4

   Fixed arguments	(must be allocated; may remain unused)

      SP-48		arg word 3
      SP-44		arg word 2
      SP-40		arg word 1
      SP-36		arg word 0

   Frame Marker

      SP-32		External Data Pointer (DP)
      SP-28		External sr4
      SP-24		External/stub RP (RP')
      SP-20		Current RP
      SP-16		Static Link
      SP-12		Clean up
      SP-8		Calling Stub RP (RP'')
      SP-4		Previous SP

   Top of Frame

      SP-0		Stack Pointer (points to next available address)

*/

/* This function saves registers as follows.  Registers marked with ' are
   this function's registers (as opposed to the previous function's).
   If a frame_pointer isn't needed, r4 is saved as a general register;
   the space for the frame pointer is still allocated, though, to keep
   things simple.


   Top of Frame

       SP (FP')		Previous FP
       SP + 4		Alignment filler (sigh)
       SP + 8		Space for locals reserved here.
       .
       .
       .
       SP + n		All call saved register used.
       .
       .
       .
       SP + o		All call saved fp registers used.
       .
       .
       .
       SP + p (SP')	points to next available address.

*/

/* Emit RTL to store REG at the memory location specified by BASE+DISP.
   Handle case where DISP > 8k by using the add_high_const pattern.

   Note in DISP > 8k case, we will leave the high part of the address
   in %r1.  There is code in expand_hppa_{prologue,epilogue} that knows this.*/
static void
store_reg (reg, disp, base)
     int reg, disp, base;
{
  if (VAL_14_BITS_P (disp))
    {
      emit_move_insn (gen_rtx (MEM, SImode,
			       gen_rtx (PLUS, SImode,
				        gen_rtx (REG, SImode, base),
				        GEN_INT (disp))),
		      gen_rtx (REG, SImode, reg));
    }
  else
    {
      emit_insn (gen_add_high_const (gen_rtx (REG, SImode, 1),
				     gen_rtx (REG, SImode, base),
				     GEN_INT (disp)));
      emit_move_insn (gen_rtx (MEM, SImode,
			       gen_rtx (LO_SUM, SImode,
					gen_rtx (REG, SImode, 1),
					GEN_INT (disp))),
		      gen_rtx (REG, SImode, reg));
    }
}

/* Emit RTL to load REG from the memory location specified by BASE+DISP.
   Handle case where DISP > 8k by using the add_high_const pattern.

   Note in DISP > 8k case, we will leave the high part of the address
   in %r1.  There is code in expand_hppa_{prologue,epilogue} that knows this.*/
static void
load_reg (reg, disp, base)
     int reg, disp, base;
{
  if (VAL_14_BITS_P (disp))
    {
      emit_move_insn (gen_rtx (REG, SImode, reg),
		      gen_rtx (MEM, SImode,
			       gen_rtx (PLUS, SImode,
				        gen_rtx (REG, SImode, base),
				        GEN_INT (disp))));
    }
  else
    {
      emit_insn (gen_add_high_const (gen_rtx (REG, SImode, 1),
				     gen_rtx (REG, SImode, base),
				     GEN_INT (disp)));
      emit_move_insn (gen_rtx (REG, SImode, reg),
		      gen_rtx (MEM, SImode,
			       gen_rtx (LO_SUM, SImode,
					gen_rtx (REG, SImode, 1),
					GEN_INT (disp))));
    }
}

/* Emit RTL to set REG to the value specified by BASE+DISP.
   Handle case where DISP > 8k by using the add_high_const pattern.

   Note in DISP > 8k case, we will leave the high part of the address
   in %r1.  There is code in expand_hppa_{prologue,epilogue} that knows this.*/
static void
set_reg_plus_d(reg, base, disp)
     int reg, base, disp;
{
  if (VAL_14_BITS_P (disp))
    {
      emit_move_insn (gen_rtx (REG, SImode, reg),
		      gen_rtx (PLUS, SImode,
			       gen_rtx (REG, SImode, base),
			       GEN_INT (disp)));
    }
  else
    {
      emit_insn (gen_add_high_const (gen_rtx (REG, SImode, 1),
				     gen_rtx (REG, SImode, base),
				     GEN_INT (disp)));
      emit_move_insn (gen_rtx (REG, SImode, reg),
		      gen_rtx (LO_SUM, SImode,
					gen_rtx (REG, SImode, 1),
					GEN_INT (disp)));
    }
}

/* Global variables set by FUNCTION_PROLOGUE.  */
/* Size of frame.  Need to know this to emit return insns from
   leaf procedures.  */
static int actual_fsize;
static int local_fsize, save_fregs;

int
compute_frame_size (size, fregs_live)
     int size;
     int *fregs_live;
{
  extern int current_function_outgoing_args_size;
  int i, fsize;

  /* 8 is space for frame pointer + filler. If any frame is allocated
     we need to add this in because of STARTING_FRAME_OFFSET. */
  fsize = size + (size || frame_pointer_needed ? 8 : 0);

  /* We must leave enough space for all the callee saved registers
     from 3 .. highest used callee save register since we don't
     know if we're going to have an inline or out of line prologue
     and epilogue.  */
  for (i = 18; i >= 3; i--)
    if (regs_ever_live[i])
      {
	fsize += 4 * (i - 2);
	break;
      }

  /* Round the stack.  */
  fsize = (fsize + 7) & ~7;

  /* We must leave enough space for all the callee saved registers
     from 3 .. highest used callee save register since we don't
     know if we're going to have an inline or out of line prologue
     and epilogue.  */
  for (i = 66; i >= 48; i -= 2)
    if (regs_ever_live[i] || regs_ever_live[i + 1])
      {
	if (fregs_live)
	  *fregs_live = 1;

	fsize += 4 * (i - 46);
	break;
      }

  fsize += current_function_outgoing_args_size;
  if (! leaf_function_p () || fsize)
    fsize += 32;
  return (fsize + 63) & ~63;
}

rtx hp_profile_label_rtx;
static char hp_profile_label_name[8];
void
output_function_prologue (file, size)
     FILE *file;
     int size;
{
  /* The function's label and associated .PROC must never be
     separated and must be output *after* any profiling declarations
     to avoid changing spaces/subspaces within a procedure.  */
  ASM_OUTPUT_LABEL (file, XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0));
  fputs ("\t.PROC\n", file);

  /* hppa_expand_prologue does the dirty work now.  We just need
     to output the assembler directives which denote the start
     of a function.  */
  fprintf (file, "\t.CALLINFO FRAME=%d", actual_fsize);
  if (regs_ever_live[2] || profile_flag)
    fputs (",CALLS,SAVE_RP", file);
  else
    fputs (",NO_CALLS", file);

  if (frame_pointer_needed)
    fputs (",SAVE_SP", file);

  /* Pass on information about the number of callee register saves
     performed in the prologue.

     The compiler is supposed to pass the highest register number
     saved, the assembler then has to adjust that number before
     entering it into the unwind descriptor (to account for any
     caller saved registers with lower register numbers than the
     first callee saved register).  */
  if (gr_saved)
    fprintf (file, ",ENTRY_GR=%d", gr_saved + 2);

  if (fr_saved)
    fprintf (file, ",ENTRY_FR=%d", fr_saved + 11);

  fputs ("\n\t.ENTRY\n", file);

  /* Horrid hack.  emit_function_prologue will modify this RTL in
     place to get the expected results.  */
  if (profile_flag)
    ASM_GENERATE_INTERNAL_LABEL (hp_profile_label_name, "LP",
				 hp_profile_labelno);

  /* If we're using GAS and not using the portable runtime model, then
     we don't need to accumulate the total number of code bytes.  */
  if (TARGET_GAS && ! TARGET_PORTABLE_RUNTIME)
    total_code_bytes = 0;
  else if (insn_addresses)
    {
      unsigned int old_total = total_code_bytes;

      total_code_bytes += insn_addresses[INSN_UID (get_last_insn())];
      total_code_bytes += FUNCTION_BOUNDARY / BITS_PER_UNIT;

      /* Be prepared to handle overflows.  */
      total_code_bytes = old_total > total_code_bytes ? -1 : total_code_bytes;
    }
  else
    total_code_bytes = -1;

  remove_useless_addtr_insns (get_insns (), 0);

  /* Restore INSN_CODEs for insn which use unscaled indexed addresses.  */
  restore_unscaled_index_insn_codes (get_insns ());
}

void
hppa_expand_prologue()
{
  extern char call_used_regs[];
  int size = get_frame_size ();
  int merge_sp_adjust_with_store = 0;
  int i, offset;
  rtx tmpreg, size_rtx;

  gr_saved = 0;
  fr_saved = 0;
  save_fregs = 0;
  local_fsize =  size + (size || frame_pointer_needed ? 8 : 0);
  actual_fsize = compute_frame_size (size, &save_fregs);

  /* Compute a few things we will use often.  */
  tmpreg = gen_rtx (REG, SImode, 1);
  size_rtx = GEN_INT (actual_fsize);

  /* Handle out of line prologues and epilogues.  */
  if (TARGET_SPACE)
    {
      rtx operands[2];
      int saves = 0;
      int outline_insn_count = 0;
      int inline_insn_count = 0;

      /* Count the number of insns for the inline and out of line
	 variants so we can choose one appropriately.

	 No need to screw with counting actual_fsize operations -- they're
	 done for both inline and out of line prologues.  */
      if (regs_ever_live[2])
	inline_insn_count += 1;

      if (! cint_ok_for_move (local_fsize))
	outline_insn_count += 2;
      else
	outline_insn_count += 1;

      /* Put the register save info into %r22.  */
      for (i = 18; i >= 3; i--)
	if (regs_ever_live[i] && ! call_used_regs[i])
	  {
	    /* -1 because the stack adjustment is normally done in
	       the same insn as a register save.  */
	    inline_insn_count += (i - 2) - 1;
	    saves = i;
            break;
	  }
  
      for (i = 66; i >= 48; i -= 2)
	if (regs_ever_live[i] || regs_ever_live[i + 1])
	  {
	    /* +1 needed as we load %r1 with the start of the freg
	       save area.  */
	    inline_insn_count += (i/2 - 23) + 1;
	    saves |= ((i/2 - 12 ) << 16);
	    break;
	  }

      if (frame_pointer_needed)
	inline_insn_count += 3;

      if (! cint_ok_for_move (saves))
	outline_insn_count += 2;
      else
	outline_insn_count += 1;

      if (TARGET_PORTABLE_RUNTIME)
	outline_insn_count += 2;
      else
	outline_insn_count += 1;
	
      /* If there's a lot of insns in the prologue, then do it as
	 an out-of-line sequence.  */
      if (inline_insn_count > outline_insn_count)
	{
	  /* Put the local_fisze into %r19.  */
	  operands[0] = gen_rtx (REG, SImode, 19);
	  operands[1] = GEN_INT (local_fsize);
	  emit_move_insn (operands[0], operands[1]);

	  /* Put the stack size into %r21.  */
	  operands[0] = gen_rtx (REG, SImode, 21);
	  operands[1] = size_rtx;
	  emit_move_insn (operands[0], operands[1]);

	  operands[0] = gen_rtx (REG, SImode, 22);
	  operands[1] = GEN_INT (saves);
	  emit_move_insn (operands[0], operands[1]);

	  /* Now call the out-of-line prologue.  */
	  emit_insn (gen_outline_prologue_call ());
	  emit_insn (gen_blockage ());

	  /* Note that we're using an out-of-line prologue.  */
	  out_of_line_prologue_epilogue = 1;
	  return;     
	}
    }

  out_of_line_prologue_epilogue = 0;

  /* Save RP first.  The calling conventions manual states RP will
     always be stored into the caller's frame at sp-20.  */
  if (regs_ever_live[2] || profile_flag)
    store_reg (2, -20, STACK_POINTER_REGNUM);

  /* Allocate the local frame and set up the frame pointer if needed.  */
  if (actual_fsize)
    if (frame_pointer_needed)
      {
	/* Copy the old frame pointer temporarily into %r1.  Set up the
	   new stack pointer, then store away the saved old frame pointer
	   into the stack at sp+actual_fsize and at the same time update
	   the stack pointer by actual_fsize bytes.  Two versions, first
	   handles small (<8k) frames.  The second handles large (>8k)
	   frames.  */
	emit_move_insn (tmpreg, frame_pointer_rtx);
	emit_move_insn (frame_pointer_rtx, stack_pointer_rtx);
	if (VAL_14_BITS_P (actual_fsize))
	  emit_insn (gen_post_stwm (stack_pointer_rtx, tmpreg, size_rtx));
	else
	  {
	    /* It is incorrect to store the saved frame pointer at *sp,
	       then increment sp (writes beyond the current stack boundary).

	       So instead use stwm to store at *sp and post-increment the
	       stack pointer as an atomic operation.  Then increment sp to
	       finish allocating the new frame.  */
	    emit_insn (gen_post_stwm (stack_pointer_rtx, tmpreg, GEN_INT (64)));
	    set_reg_plus_d (STACK_POINTER_REGNUM,
			    STACK_POINTER_REGNUM,
			    actual_fsize - 64);
	  }
      }
    /* no frame pointer needed.  */
    else
      {
	/* In some cases we can perform the first callee register save
	   and allocating the stack frame at the same time.   If so, just
	   make a note of it and defer allocating the frame until saving
	   the callee registers.  */
	if (VAL_14_BITS_P (-actual_fsize)
	    && local_fsize == 0
	    && ! profile_flag
	    && ! flag_pic)
	  merge_sp_adjust_with_store = 1;
	/* Can not optimize.  Adjust the stack frame by actual_fsize bytes.  */
	else if (actual_fsize != 0)
	  set_reg_plus_d (STACK_POINTER_REGNUM,
			  STACK_POINTER_REGNUM,
			  actual_fsize);
      }
  /* The hppa calling conventions say that that %r19, the pic offset
     register, is saved at sp - 32 (in this function's frame)  when
     generating PIC code.  FIXME:  What is the correct thing to do
     for functions which make no calls and allocate no frame?  Do
     we need to allocate a frame, or can we just omit the save?   For
     now we'll just omit the save.  */
  if (actual_fsize != 0 && flag_pic)
    store_reg (PIC_OFFSET_TABLE_REGNUM, -32, STACK_POINTER_REGNUM);

  /* Profiling code.

     Instead of taking one argument, the counter label, as most normal
     mcounts do, _mcount appears to behave differently on the HPPA.  It
     takes the return address of the caller, the address of this routine,
     and the address of the label.  Also, it isn't magic, so
     argument registers have to be preserved.  */
  if (profile_flag)
    {
      int pc_offset, i, arg_offset, basereg, offsetadj;

      pc_offset = 4 + (frame_pointer_needed
		       ? (VAL_14_BITS_P (actual_fsize) ? 12 : 20)
		       : (VAL_14_BITS_P (actual_fsize) ? 4 : 8));

      /* When the function has a frame pointer, use it as the base
	 register for saving/restore registers.  Else use the stack
	 pointer.  Adjust the offset according to the frame size if
	 this function does not have a frame pointer.  */

      basereg = frame_pointer_needed ? FRAME_POINTER_REGNUM
				     : STACK_POINTER_REGNUM;
      offsetadj = frame_pointer_needed ? 0 : actual_fsize;

      /* Horrid hack.  emit_function_prologue will modify this RTL in
	 place to get the expected results.   sprintf here is just to
	 put something in the name.  */
      sprintf(hp_profile_label_name, "LP$%04d", -1);
      hp_profile_label_rtx = gen_rtx (SYMBOL_REF, SImode,
				      hp_profile_label_name);
      if (current_function_returns_struct)
	store_reg (STRUCT_VALUE_REGNUM, - 12 - offsetadj, basereg);

      for (i = 26, arg_offset = -36 - offsetadj; i >= 23; i--, arg_offset -= 4)
	if (regs_ever_live [i])
	  {
	    store_reg (i, arg_offset, basereg);
	    /* Deal with arg_offset not fitting in 14 bits.  */
	    pc_offset += VAL_14_BITS_P (arg_offset) ? 4 : 8;
	  }

      emit_move_insn (gen_rtx (REG, SImode, 26), gen_rtx (REG, SImode, 2));
      emit_move_insn (tmpreg, gen_rtx (HIGH, SImode, hp_profile_label_rtx));
      emit_move_insn (gen_rtx (REG, SImode, 24),
		      gen_rtx (LO_SUM, SImode, tmpreg, hp_profile_label_rtx));
      /* %r25 is set from within the output pattern.  */
      emit_insn (gen_call_profiler (GEN_INT (- pc_offset - 20)));

      /* Restore argument registers.  */
      for (i = 26, arg_offset = -36 - offsetadj; i >= 23; i--, arg_offset -= 4)
	if (regs_ever_live [i])
	  load_reg (i, arg_offset, basereg);

      if (current_function_returns_struct)
	load_reg (STRUCT_VALUE_REGNUM, -12 - offsetadj, basereg);

    }

  /* Normal register save.

     Do not save the frame pointer in the frame_pointer_needed case.  It
     was done earlier.  */
  if (frame_pointer_needed)
    {
      for (i = 18, offset = local_fsize; i >= 4; i--)
	if (regs_ever_live[i] && ! call_used_regs[i])
	  {
	    store_reg (i, offset, FRAME_POINTER_REGNUM);
	    offset += 4;
	    gr_saved++;
	  }
      /* Account for %r3 which is saved in a special place.  */
      gr_saved++;
    }
  /* No frame pointer needed.  */
  else
    {
      for (i = 18, offset = local_fsize - actual_fsize; i >= 3; i--)
      	if (regs_ever_live[i] && ! call_used_regs[i])
	  {
	    /* If merge_sp_adjust_with_store is nonzero, then we can
	       optimize the first GR save.  */
	    if (merge_sp_adjust_with_store)
	      {
		merge_sp_adjust_with_store = 0;
	        emit_insn (gen_post_stwm (stack_pointer_rtx,
					  gen_rtx (REG, SImode, i),
					  GEN_INT (-offset)));
	      }
	    else
	      store_reg (i, offset, STACK_POINTER_REGNUM);
	    offset += 4;
	    gr_saved++;
	  }

      /* If we wanted to merge the SP adjustment with a GR save, but we never
	 did any GR saves, then just emit the adjustment here.  */
      if (merge_sp_adjust_with_store)
	set_reg_plus_d (STACK_POINTER_REGNUM,
			STACK_POINTER_REGNUM,
			actual_fsize);
    }

  /* Align pointer properly (doubleword boundary).  */
  offset = (offset + 7) & ~7;

  /* Floating point register store.  */
  if (save_fregs)
    {
      /* First get the frame or stack pointer to the start of the FP register
	 save area.  */
      if (frame_pointer_needed)
	set_reg_plus_d (1, FRAME_POINTER_REGNUM, offset);
      else
	set_reg_plus_d (1, STACK_POINTER_REGNUM, offset);

      /* Now actually save the FP registers.  */
      for (i = 66; i >= 48; i -= 2)
	{
	  if (regs_ever_live[i] || regs_ever_live[i + 1])
	    {
	      emit_move_insn (gen_rtx (MEM, DFmode,
				       gen_rtx (POST_INC, DFmode, tmpreg)),
			      gen_rtx (REG, DFmode, i));
	      fr_saved++;
	    }
	}
    }

  /* When generating PIC code it is necessary to save/restore the
     PIC register around each function call.  We used to do this
     in the call patterns themselves, but that implementation
     made incorrect assumptions about using global variables to hold
     per-function rtl code generated in the backend.

     So instead, we copy the PIC register into a reserved callee saved
     register in the prologue.  Then after each call we reload the PIC
     register from the callee saved register.  We also reload the PIC
     register from the callee saved register in the epilogue ensure the
     PIC register is valid at function exit.

     This may (depending on the exact characteristics of the function)
     even be more efficient. 

     Avoid this if the callee saved register wasn't used (these are
     leaf functions).  */
  if (flag_pic && regs_ever_live[PIC_OFFSET_TABLE_REGNUM_SAVED])
    emit_move_insn (gen_rtx (REG, SImode, PIC_OFFSET_TABLE_REGNUM_SAVED),
		    gen_rtx (REG, SImode, PIC_OFFSET_TABLE_REGNUM));
}


void
output_function_epilogue (file, size)
     FILE *file;
     int size;
{
  rtx insn = get_last_insn ();
  int i;

  /* hppa_expand_epilogue does the dirty work now.  We just need
     to output the assembler directives which denote the end
     of a function.

     To make debuggers happy, emit a nop if the epilogue was completely
     eliminated due to a volatile call as the last insn in the
     current function.  That way the return address (in %r2) will
     always point to a valid instruction in the current function.  */

  /* Get the last real insn.  */
  if (GET_CODE (insn) == NOTE)
    insn = prev_real_insn (insn);

  /* If it is a sequence, then look inside.  */
  if (insn && GET_CODE (insn) == INSN && GET_CODE (PATTERN (insn)) == SEQUENCE)
    insn = XVECEXP (PATTERN (insn), 0, 0);

  /* If insn is a CALL_INSN, then it must be a call to a volatile
     function (otherwise there would be epilogue insns).  */
  if (insn && GET_CODE (insn) == CALL_INSN)
    fputs ("\tnop\n", file);

  fputs ("\t.EXIT\n\t.PROCEND\n", file);

  /* Free up stuff we don't need anymore.  */
  if (unscaled_index_insn_codes)
    free (unscaled_index_insn_codes);
  max_unscaled_index_insn_codes_uid = 0;
}

void
hppa_expand_epilogue ()
{
  rtx tmpreg;
  int offset,i;
  int merge_sp_adjust_with_load  = 0;

  /* Handle out of line prologues and epilogues.  */
  if (TARGET_SPACE && out_of_line_prologue_epilogue)
    {
      int saves = 0;
      rtx operands[2];

      /* Put the register save info into %r22.  */
      for (i = 18; i >= 3; i--)
	if (regs_ever_live[i] && ! call_used_regs[i])
	  {
	    saves = i;
            break;
	  }
	  
      for (i = 66; i >= 48; i -= 2)
	if (regs_ever_live[i] || regs_ever_live[i + 1])
	  {
	    saves |= ((i/2 - 12 ) << 16);
	    break;
	  }

      emit_insn (gen_blockage ());

      /* Put the local_fisze into %r19.  */
      operands[0] = gen_rtx (REG, SImode, 19);
      operands[1] = GEN_INT (local_fsize);
      emit_move_insn (operands[0], operands[1]);

      /* Put the stack size into %r21.  */
      operands[0] = gen_rtx (REG, SImode, 21);
      operands[1] = GEN_INT (actual_fsize);
      emit_move_insn (operands[0], operands[1]);

      operands[0] = gen_rtx (REG, SImode, 22);
      operands[1] = GEN_INT (saves);
      emit_move_insn (operands[0], operands[1]);

      /* Now call the out-of-line epilogue.  */
      emit_insn (gen_outline_epilogue_call ());
      return;
    }

  /* We will use this often.  */
  tmpreg = gen_rtx (REG, SImode, 1);

  /* Try to restore RP early to avoid load/use interlocks when
     RP gets used in the return (bv) instruction.  This appears to still
     be necessary even when we schedule the prologue and epilogue. */
  if (frame_pointer_needed
      && (regs_ever_live [2] || profile_flag))
    load_reg (2, -20, FRAME_POINTER_REGNUM);

  /* No frame pointer, and stack is smaller than 8k.  */
  else if (! frame_pointer_needed
	   && VAL_14_BITS_P (actual_fsize + 20)
	   && (regs_ever_live[2] || profile_flag))
    load_reg (2, - (actual_fsize + 20), STACK_POINTER_REGNUM);

  /* General register restores.  */
  if (frame_pointer_needed)
    {
      for (i = 18, offset = local_fsize; i >= 4; i--)
	if (regs_ever_live[i] && ! call_used_regs[i])
	  {
	    load_reg (i, offset, FRAME_POINTER_REGNUM);
	    offset += 4;
	  }
    }
  else
    {
      for (i = 18, offset = local_fsize - actual_fsize; i >= 3; i--)
	{
	  if (regs_ever_live[i] && ! call_used_regs[i])
	    {
	      /* Only for the first load.
	         merge_sp_adjust_with_load holds the register load
	         with which we will merge the sp adjustment.  */
	      if (VAL_14_BITS_P (actual_fsize + 20)
		  && local_fsize == 0
		  && ! merge_sp_adjust_with_load)
	        merge_sp_adjust_with_load = i;
	      else
	        load_reg (i, offset, STACK_POINTER_REGNUM);
	      offset += 4;
	    }
	}
    }

  /* Align pointer properly (doubleword boundary).  */
  offset = (offset + 7) & ~7;

  /* FP register restores.  */
  if (save_fregs)
    {
      /* Adjust the register to index off of.  */
      if (frame_pointer_needed)
	set_reg_plus_d (1, FRAME_POINTER_REGNUM, offset);
      else
	set_reg_plus_d (1, STACK_POINTER_REGNUM, offset);

      /* Actually do the restores now.  */
      for (i = 66; i >= 48; i -= 2)
	{
	  if (regs_ever_live[i] || regs_ever_live[i + 1])
	    {
	      emit_move_insn (gen_rtx (REG, DFmode, i),
			      gen_rtx (MEM, DFmode,
				       gen_rtx (POST_INC, DFmode, tmpreg)));
	    }
	}
    }

  /* Emit a blockage insn here to keep these insns from being moved to
     an earlier spot in the epilogue, or into the main instruction stream.

     This is necessary as we must not cut the stack back before all the
     restores are finished.  */
  emit_insn (gen_blockage ());
  /* No frame pointer, but we have a stack greater than 8k.  We restore
     %r2 very late in this case.  (All other cases are restored as early
     as possible.)  */
  if (! frame_pointer_needed
      && ! VAL_14_BITS_P (actual_fsize + 20)
      && (regs_ever_live[2] || profile_flag))
    {
      set_reg_plus_d (STACK_POINTER_REGNUM,
		      STACK_POINTER_REGNUM,
		      - actual_fsize);

      /* This used to try and be clever by not depending on the value in
	 %r30 and instead use the value held in %r1 (so that the 2nd insn
	 which sets %r30 could be put in the delay slot of the return insn).
	
	 That won't work since if the stack is exactly 8k set_reg_plus_d
	 doesn't set %r1, just %r30.  */
      load_reg (2, - 20, STACK_POINTER_REGNUM);
    }

  /* Reset stack pointer (and possibly frame pointer).  The stack 
     pointer is initially set to fp + 64 to avoid a race condition.  */
  else if (frame_pointer_needed)
    {
      set_reg_plus_d (STACK_POINTER_REGNUM, FRAME_POINTER_REGNUM, 64);
      emit_insn (gen_pre_ldwm (frame_pointer_rtx, 
			       stack_pointer_rtx,
			       GEN_INT (-64)));
    }
  /* If we were deferring a callee register restore, do it now.  */
  else if (! frame_pointer_needed  && merge_sp_adjust_with_load)
    emit_insn (gen_pre_ldwm (gen_rtx (REG, SImode,
				      merge_sp_adjust_with_load),
			     stack_pointer_rtx,
			     GEN_INT (- actual_fsize)));
  else if (actual_fsize != 0)
    set_reg_plus_d (STACK_POINTER_REGNUM,
		    STACK_POINTER_REGNUM,
		    - actual_fsize);
}

/* Fetch the return address for the frame COUNT steps up from
   the current frame, after the prologue.  FRAMEADDR is the
   frame pointer of the COUNT frame.

   We want to ignore any export stub remnants here.

   The value returned is used in two different ways:

	1. To find a function's caller.

	2. To change the return address for a function.

   This function handles most instances of case 1; however, it will
   fail if there are two levels of stubs to execute on the return
   path.  The only way I believe that can happen is if the return value
   needs a parameter relocation, which never happens for C code.

   This function handles most instances of case 2; however, it will
   fail if we did not originally have stub code on the return path
   but will need code on the new return path.  This can happen if
   the caller & callee are both in the main program, but the new
   return location is in a shared library.

   To handle this correctly we need to set the return pointer at
   frame-20 to point to a return stub frame-24 to point to the
   location we wish to return to.  */

rtx
return_addr_rtx (count, frameaddr)
     int count;
     rtx frameaddr;
{
  rtx label;
  rtx saved_rp;
  rtx ins;

  saved_rp = gen_reg_rtx (Pmode);

  /* First, we start off with the normal return address pointer from
     -20[frameaddr].  */

  emit_move_insn (saved_rp, plus_constant (frameaddr, -5 * UNITS_PER_WORD));

  /* Get pointer to the instruction stream.  We have to mask out the
     privilege level from the two low order bits of the return address
     pointer here so that ins will point to the start of the first
     instruction that would have been executed if we returned.  */
  ins = copy_to_reg (gen_rtx (AND, Pmode,
			      copy_to_reg (gen_rtx (MEM, Pmode, saved_rp)),
			      MASK_RETURN_ADDR));
  label = gen_label_rtx ();

  /* Check the instruction stream at the normal return address for the
     export stub:

	0x4bc23fd1 | stub+8:   ldw -18(sr0,sp),rp
	0x004010a1 | stub+12:  ldsid (sr0,rp),r1
	0x00011820 | stub+16:  mtsp r1,sr0
	0xe0400002 | stub+20:  be,n 0(sr0,rp)

     If it is an export stub, than our return address is really in
     -24[frameaddr].  */

  emit_cmp_insn (gen_rtx (MEM, SImode, ins),
		 GEN_INT (0x4bc23fd1),
		 NE, NULL_RTX, SImode, 1, 0);
  emit_jump_insn (gen_bne (label));

  emit_cmp_insn (gen_rtx (MEM, SImode, plus_constant (ins, 4)),
		 GEN_INT (0x004010a1),
		 NE, NULL_RTX, SImode, 1, 0);
  emit_jump_insn (gen_bne (label));

  emit_cmp_insn (gen_rtx (MEM, SImode, plus_constant (ins, 8)),
		 GEN_INT (0x00011820),
		 NE, NULL_RTX, SImode, 1, 0);
  emit_jump_insn (gen_bne (label));

  emit_cmp_insn (gen_rtx (MEM, SImode, plus_constant (ins, 12)),
		 GEN_INT (0xe0400002),
		 NE, NULL_RTX, SImode, 1, 0);

  /* If there is no export stub then just use our initial guess of
     -20[frameaddr].  */

  emit_jump_insn (gen_bne (label));

  /* Here we know that our return address pointer points to an export
     stub.  We don't want to return the address of the export stub,
     but rather the return address that leads back into user code.
     That return address is stored at -24[frameaddr].  */

  emit_move_insn (saved_rp, plus_constant (frameaddr, -6 * UNITS_PER_WORD));

  emit_label (label);
  return gen_rtx (MEM, Pmode, memory_address (Pmode, saved_rp));
}

/* This is only valid once reload has completed because it depends on
   knowing exactly how much (if any) frame there is and...

   It's only valid if there is no frame marker to de-allocate and...

   It's only valid if %r2 hasn't been saved into the caller's frame
   (we're not profiling and %r2 isn't live anywhere).  */
int
hppa_can_use_return_insn_p ()
{
  return (reload_completed
	  && (compute_frame_size (get_frame_size (), 0) ? 0 : 1)
	  && ! profile_flag
	  && ! regs_ever_live[2]
	  && ! frame_pointer_needed);
}

void
emit_bcond_fp (code, operand0)
     enum rtx_code code;
     rtx operand0;
{
  emit_jump_insn (gen_rtx (SET, VOIDmode, pc_rtx,
			   gen_rtx (IF_THEN_ELSE, VOIDmode,
				    gen_rtx (code, VOIDmode,
					     gen_rtx (REG, CCFPmode, 0),
					     const0_rtx),
				    gen_rtx (LABEL_REF, VOIDmode, operand0),
				    pc_rtx)));

}

rtx
gen_cmp_fp (code, operand0, operand1)
     enum rtx_code code;
     rtx operand0, operand1;
{
  return gen_rtx (SET, VOIDmode, gen_rtx (REG, CCFPmode, 0),
		  gen_rtx (code, CCFPmode, operand0, operand1));
}

/* Adjust the cost of a scheduling dependency.  Return the new cost of
   a dependency LINK or INSN on DEP_INSN.  COST is the current cost.  */

int
pa_adjust_cost (insn, link, dep_insn, cost)
     rtx insn;
     rtx link;
     rtx dep_insn;
     int cost;
{
  if (! recog_memoized (insn))
    return 0;

  if (REG_NOTE_KIND (link) == 0)
    {
      /* Data dependency; DEP_INSN writes a register that INSN reads some
	 cycles later.  */

      if (get_attr_type (insn) == TYPE_FPSTORE)
	{
	  rtx pat = PATTERN (insn);
	  rtx dep_pat = PATTERN (dep_insn);
	  if (GET_CODE (pat) == PARALLEL)
	    {
	      /* This happens for the fstXs,mb patterns.  */
	      pat = XVECEXP (pat, 0, 0);
	    }
	  if (GET_CODE (pat) != SET || GET_CODE (dep_pat) != SET)
	    /* If this happens, we have to extend this to schedule
	       optimally.  Return 0 for now.  */
	  return 0;

	  if (rtx_equal_p (SET_DEST (dep_pat), SET_SRC (pat)))
	    {
	      if (! recog_memoized (dep_insn))
		return 0;
	      /* DEP_INSN is writing its result to the register
		 being stored in the fpstore INSN.  */
	      switch (get_attr_type (dep_insn))
		{
		case TYPE_FPLOAD:
		  /* This cost 3 cycles, not 2 as the md says for the
		     700 and 7100.  Note scaling of cost for 7100.  */
		  return cost + (pa_cpu == PROCESSOR_700) ? 1 : 2;

		case TYPE_FPALU:
		case TYPE_FPMULSGL:
		case TYPE_FPMULDBL:
		case TYPE_FPDIVSGL:
		case TYPE_FPDIVDBL:
		case TYPE_FPSQRTSGL:
		case TYPE_FPSQRTDBL:
		  /* In these important cases, we save one cycle compared to
		     when flop instruction feed each other.  */
		  return cost - (pa_cpu == PROCESSOR_700) ? 1 : 2;

		default:
		  return cost;
		}
	    }
	}

      /* For other data dependencies, the default cost specified in the
	 md is correct.  */
      return cost;
    }
  else if (REG_NOTE_KIND (link) == REG_DEP_ANTI)
    {
      /* Anti dependency; DEP_INSN reads a register that INSN writes some
	 cycles later.  */

      if (get_attr_type (insn) == TYPE_FPLOAD)
	{
	  rtx pat = PATTERN (insn);
	  rtx dep_pat = PATTERN (dep_insn);
	  if (GET_CODE (pat) == PARALLEL)
	    {
	      /* This happens for the fldXs,mb patterns.  */
	      pat = XVECEXP (pat, 0, 0);
	    }
	  if (GET_CODE (pat) != SET || GET_CODE (dep_pat) != SET)
	    /* If this happens, we have to extend this to schedule
	       optimally.  Return 0 for now.  */
	  return 0;

	  if (reg_mentioned_p (SET_DEST (pat), SET_SRC (dep_pat)))
	    {
	      if (! recog_memoized (dep_insn))
		return 0;
	      switch (get_attr_type (dep_insn))
		{
		case TYPE_FPALU:
		case TYPE_FPMULSGL:
		case TYPE_FPMULDBL:
		case TYPE_FPDIVSGL:
		case TYPE_FPDIVDBL:
		case TYPE_FPSQRTSGL:
		case TYPE_FPSQRTDBL:
		  /* A fpload can't be issued until one cycle before a
		     preceding arithmetic operation has finished if
		     the target of the fpload is any of the sources
		     (or destination) of the arithmetic operation.  */
		  return cost - (pa_cpu == PROCESSOR_700) ? 1 : 2;

		default:
		  return 0;
		}
	    }
	}
      else if (get_attr_type (insn) == TYPE_FPALU)
	{
	  rtx pat = PATTERN (insn);
	  rtx dep_pat = PATTERN (dep_insn);
	  if (GET_CODE (pat) == PARALLEL)
	    {
	      /* This happens for the fldXs,mb patterns.  */
	      pat = XVECEXP (pat, 0, 0);
	    }
	  if (GET_CODE (pat) != SET || GET_CODE (dep_pat) != SET)
	    /* If this happens, we have to extend this to schedule
	       optimally.  Return 0 for now.  */
	  return 0;

	  if (reg_mentioned_p (SET_DEST (pat), SET_SRC (dep_pat)))
	    {
	      if (! recog_memoized (dep_insn))
		return 0;
	      switch (get_attr_type (dep_insn))
		{
		case TYPE_FPDIVSGL:
		case TYPE_FPDIVDBL:
		case TYPE_FPSQRTSGL:
		case TYPE_FPSQRTDBL:
		  /* An ALU flop can't be issued until two cycles before a
		     preceding divide or sqrt operation has finished if
		     the target of the ALU flop is any of the sources
		     (or destination) of the divide or sqrt operation.  */
		  return cost - (pa_cpu == PROCESSOR_700) ? 2 : 4;

		default:
		  return 0;
		}
	    }
	}

      /* For other anti dependencies, the cost is 0.  */
      return 0;
    }
  else if (REG_NOTE_KIND (link) == REG_DEP_OUTPUT)
    {
      /* Output dependency; DEP_INSN writes a register that INSN writes some
	 cycles later.  */
      if (get_attr_type (insn) == TYPE_FPLOAD)
	{
	  rtx pat = PATTERN (insn);
	  rtx dep_pat = PATTERN (dep_insn);
	  if (GET_CODE (pat) == PARALLEL)
	    {
	      /* This happens for the fldXs,mb patterns.  */
	      pat = XVECEXP (pat, 0, 0);
	    }
	  if (GET_CODE (pat) != SET || GET_CODE (dep_pat) != SET)
	    /* If this happens, we have to extend this to schedule
	       optimally.  Return 0 for now.  */
	  return 0;

	  if (reg_mentioned_p (SET_DEST (pat), SET_DEST (dep_pat)))
	    {
	      if (! recog_memoized (dep_insn))
		return 0;
	      switch (get_attr_type (dep_insn))
		{
		case TYPE_FPALU:
		case TYPE_FPMULSGL:
		case TYPE_FPMULDBL:
		case TYPE_FPDIVSGL:
		case TYPE_FPDIVDBL:
		case TYPE_FPSQRTSGL:
		case TYPE_FPSQRTDBL:
		  /* A fpload can't be issued until one cycle before a
		     preceding arithmetic operation has finished if
		     the target of the fpload is the destination of the
		     arithmetic operation.  */
		  return cost - (pa_cpu == PROCESSOR_700) ? 1 : 2;

		default:
		  return 0;
		}
	    }
	}
      else if (get_attr_type (insn) == TYPE_FPALU)
	{
	  rtx pat = PATTERN (insn);
	  rtx dep_pat = PATTERN (dep_insn);
	  if (GET_CODE (pat) == PARALLEL)
	    {
	      /* This happens for the fldXs,mb patterns.  */
	      pat = XVECEXP (pat, 0, 0);
	    }
	  if (GET_CODE (pat) != SET || GET_CODE (dep_pat) != SET)
	    /* If this happens, we have to extend this to schedule
	       optimally.  Return 0 for now.  */
	  return 0;

	  if (reg_mentioned_p (SET_DEST (pat), SET_DEST (dep_pat)))
	    {
	      if (! recog_memoized (dep_insn))
		return 0;
	      switch (get_attr_type (dep_insn))
		{
		case TYPE_FPDIVSGL:
		case TYPE_FPDIVDBL:
		case TYPE_FPSQRTSGL:
		case TYPE_FPSQRTDBL:
		  /* An ALU flop can't be issued until two cycles before a
		     preceding divide or sqrt operation has finished if
		     the target of the ALU flop is also the target of
		     of the divide or sqrt operation.  */
		  return cost - (pa_cpu == PROCESSOR_700) ? 2 : 4;

		default:
		  return 0;
		}
	    }
	}

      /* For other output dependencies, the cost is 0.  */
      return 0;
    }
  else
    abort ();
}

/* Return any length adjustment needed by INSN which already has its length
   computed as LENGTH.   Return zero if no adjustment is necessary.

   For the PA: function calls, millicode calls, and backwards short
   conditional branches with unfilled delay slots need an adjustment by +1
   (to account for the NOP which will be inserted into the instruction stream).

   Also compute the length of an inline block move here as it is too
   complicated to express as a length attribute in pa.md.  */
int
pa_adjust_insn_length (insn, length)
    rtx insn;
    int length;
{
  rtx pat = PATTERN (insn);

  /* Call insns which are *not* indirect and have unfilled delay slots.  */
  if (GET_CODE (insn) == CALL_INSN)
    {

      if (GET_CODE (XVECEXP (pat, 0, 0)) == CALL
	  && GET_CODE (XEXP (XEXP (XVECEXP (pat, 0, 0), 0), 0)) == SYMBOL_REF)
	return 4;
      else if (GET_CODE (XVECEXP (pat, 0, 0)) == SET
	       && GET_CODE (XEXP (XEXP (XEXP (XVECEXP (pat, 0, 0), 1), 0), 0))
		  == SYMBOL_REF)
	return 4;
      else
	return 0;
    }
  /* Jumps inside switch tables which have unfilled delay slots 
     also need adjustment.  */
  else if (GET_CODE (insn) == JUMP_INSN
	   && simplejump_p (insn)
	   && GET_MODE (PATTERN (insn)) == DImode)
    return 4;
  /* Millicode insn with an unfilled delay slot.  */
  else if (GET_CODE (insn) == INSN
	   && GET_CODE (pat) != SEQUENCE
	   && GET_CODE (pat) != USE
	   && GET_CODE (pat) != CLOBBER
	   && get_attr_type (insn) == TYPE_MILLI)
    return 4;
  /* Block move pattern.  */
  else if (GET_CODE (insn) == INSN
	   && GET_CODE (pat) == PARALLEL
	   && GET_CODE (XEXP (XVECEXP (pat, 0, 0), 0)) == MEM
	   && GET_CODE (XEXP (XVECEXP (pat, 0, 0), 1)) == MEM
	   && GET_MODE (XEXP (XVECEXP (pat, 0, 0), 0)) == BLKmode
	   && GET_MODE (XEXP (XVECEXP (pat, 0, 0), 1)) == BLKmode)
    return compute_movstrsi_length (insn) - 4;
  /* Conditional branch with an unfilled delay slot.  */
  else if (GET_CODE (insn) == JUMP_INSN && ! simplejump_p (insn))
    {
      /* Adjust a short backwards conditional with an unfilled delay slot.  */
      if (GET_CODE (pat) == SET
	  && length == 4
	  && ! forward_branch_p (insn))
	return 4;
      else if (GET_CODE (pat) == PARALLEL
	       && get_attr_type (insn) == TYPE_PARALLEL_BRANCH
	       && length == 4)
	return 4;
      /* Adjust dbra insn with short backwards conditional branch with
	 unfilled delay slot -- only for case where counter is in a
	 general register register. */
      else if (GET_CODE (pat) == PARALLEL
	       && GET_CODE (XVECEXP (pat, 0, 1)) == SET
	       && GET_CODE (XEXP (XVECEXP (pat, 0, 1), 0)) == REG
 	       && ! FP_REG_P (XEXP (XVECEXP (pat, 0, 1), 0))
	       && length == 4
	       && ! forward_branch_p (insn))
	return 4;
      else
	return 0;
    }
  return 0;
}

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.  */

void
print_operand (file, x, code)
     FILE *file;
     rtx x;
     int code;
{
  switch (code)
    {
    case '#':
      /* Output a 'nop' if there's nothing for the delay slot.  */
      if (dbr_sequence_length () == 0)
	fputs ("\n\tnop", file);
      return;
    case '*':
      /* Output an nullification completer if there's nothing for the */
      /* delay slot or nullification is requested.  */
      if (dbr_sequence_length () == 0 ||
	  (final_sequence &&
	   INSN_ANNULLED_BRANCH_P (XVECEXP (final_sequence, 0, 0))))
        fputs (",n", file);
      return;
    case 'R':
      /* Print out the second register name of a register pair.
	 I.e., R (6) => 7.  */
      fputs (reg_names[REGNO (x)+1], file);
      return;
    case 'r':
      /* A register or zero. */
      if (x == const0_rtx
	  || (x == CONST0_RTX (DFmode))
	  || (x == CONST0_RTX (SFmode)))
	{
	  fputs ("0", file);
	  return;
	}
      else
	break;
    case 'C':			/* Plain (C)ondition */
    case 'X':
      switch (GET_CODE (x))
	{
	case EQ:
	  fputs ("=", file);  break;
	case NE:
	  fputs ("<>", file);  break;
	case GT:
	  fputs (">", file);  break;
	case GE:
	  fputs (">=", file);  break;
	case GEU:
	  fputs (">>=", file);  break;
	case GTU:
	  fputs (">>", file);  break;
	case LT:
	  fputs ("<", file);  break;
	case LE:
	  fputs ("<=", file);  break;
	case LEU:
	  fputs ("<<=", file);  break;
	case LTU:
	  fputs ("<<", file);  break;
	default:
	  abort ();
	}
      return;
    case 'N':			/* Condition, (N)egated */
      switch (GET_CODE (x))
	{
	case EQ:
	  fputs ("<>", file);  break;
	case NE:
	  fputs ("=", file);  break;
	case GT:
	  fputs ("<=", file);  break;
	case GE:
	  fputs ("<", file);  break;
	case GEU:
	  fputs ("<<", file);  break;
	case GTU:
	  fputs ("<<=", file);  break;
	case LT:
	  fputs (">=", file);  break;
	case LE:
	  fputs (">", file);  break;
	case LEU:
	  fputs (">>", file);  break;
	case LTU:
	  fputs (">>=", file);  break;
	default:
	  abort ();
	}
      return;
    /* For floating point comparisons.  Need special conditions to deal
       with NaNs properly.  */
    case 'Y':
      switch (GET_CODE (x))
	{
	case EQ:
	  fputs ("!=", file);  break;
	case NE:
	  fputs ("=", file);  break;
	case GT:
	  fputs ("<=", file);  break;
	case GE:
	  fputs ("<", file);  break;
	case LT:
	  fputs (">=", file);  break;
	case LE:
	  fputs (">", file);  break;
	default:
	  abort ();
	}
      return;
    case 'S':			/* Condition, operands are (S)wapped.  */
      switch (GET_CODE (x))
	{
	case EQ:
	  fputs ("=", file);  break;
	case NE:
	  fputs ("<>", file);  break;
	case GT:
	  fputs ("<", file);  break;
	case GE:
	  fputs ("<=", file);  break;
	case GEU:
	  fputs ("<<=", file);  break;
	case GTU:
	  fputs ("<<", file);  break;
	case LT:
	  fputs (">", file);  break;
	case LE:
	  fputs (">=", file);  break;
	case LEU:
	  fputs (">>=", file);  break;
	case LTU:
	  fputs (">>", file);  break;
	default:
	  abort ();
	}
      return;
    case 'B':			/* Condition, (B)oth swapped and negate.  */
      switch (GET_CODE (x))
	{
	case EQ:
	  fputs ("<>", file);  break;
	case NE:
	  fputs ("=", file);  break;
	case GT:
	  fputs (">=", file);  break;
	case GE:
	  fputs (">", file);  break;
	case GEU:
	  fputs (">>", file);  break;
	case GTU:
	  fputs (">>=", file);  break;
	case LT:
	  fputs ("<=", file);  break;
	case LE:
	  fputs ("<", file);  break;
	case LEU:
	  fputs ("<<", file);  break;
	case LTU:
	  fputs ("<<=", file);  break;
	default:
	  abort ();
	}
      return;
    case 'k':
      if (GET_CODE (x) == CONST_INT)
	{
	  fprintf (file, "%d", ~INTVAL (x));
	  return;
	}
      abort();
    case 'L':
      if (GET_CODE (x) == CONST_INT)
	{
	  fprintf (file, "%d", 32 - (INTVAL (x) & 31));
	  return;
	}
      abort();
    case 'O':
      if (GET_CODE (x) == CONST_INT && exact_log2 (INTVAL (x)) >= 0)
	{
	  fprintf (file, "%d", exact_log2 (INTVAL (x)));
	  return;
	}
      abort();
    case 'P':
      if (GET_CODE (x) == CONST_INT)
	{
	  fprintf (file, "%d", 31 - (INTVAL (x) & 31));
	  return;
	}
      abort();
    case 'I':
      if (GET_CODE (x) == CONST_INT)
	fputs ("i", file);
      return;
    case 'M':
    case 'F':
      switch (GET_CODE (XEXP (x, 0)))
	{
	case PRE_DEC:
	case PRE_INC:
	  fputs ("s,mb", file);
	  break;
	case POST_DEC:
	case POST_INC:
	  fputs ("s,ma", file);
	  break;
	case PLUS:
	  if (GET_CODE (XEXP (XEXP (x, 0), 0)) == MULT
	      || GET_CODE (XEXP (XEXP (x, 0), 1)) == MULT)
	    fputs ("x,s", file);
	  else if (code == 'F')
	    fputs ("s", file);
	  break;
	default:
	  if (code == 'F')
	    fputs ("s", file);
	  break;
	}
      return;
    case 'G':
      output_global_address (file, x, 0);
      return;
    case 'H':
      output_global_address (file, x, 1);
      return;
    case 0:			/* Don't do anything special */
      break;
    case 'Z':
      {
	unsigned op[3];
	compute_zdepi_operands (INTVAL (x), op);
	fprintf (file, "%d,%d,%d", op[0], op[1], op[2]);
	return;
      }
    default:
      abort ();
    }
  if (GET_CODE (x) == REG)
    {
      fputs (reg_names [REGNO (x)], file);
      if (FP_REG_P (x) && GET_MODE_SIZE (GET_MODE (x)) <= 4 && (REGNO (x) & 1) == 0)
	fputs ("L", file);
    }
  else if (GET_CODE (x) == MEM)
    {
      int size = GET_MODE_SIZE (GET_MODE (x));
      rtx base = XEXP (XEXP (x, 0), 0);
      switch (GET_CODE (XEXP (x, 0)))
	{
	case PRE_DEC:
	case POST_DEC:
	  fprintf (file, "-%d(0,%s)", size, reg_names [REGNO (base)]);
	  break;
	case PRE_INC:
	case POST_INC:
	  fprintf (file, "%d(0,%s)", size, reg_names [REGNO (base)]);
	  break;
	default:
	  if (GET_CODE (XEXP (x, 0)) == PLUS
	      && GET_CODE (XEXP (XEXP (x, 0), 0)) == MULT)
	    fprintf (file, "%s(0,%s)",
		     reg_names [REGNO (XEXP (XEXP (XEXP (x, 0), 0), 0))],
		     reg_names [REGNO (XEXP (XEXP (x, 0), 1))]);
	  else if (GET_CODE (XEXP (x, 0)) == PLUS
		   && GET_CODE (XEXP (XEXP (x, 0), 1)) == MULT)
	    fprintf (file, "%s(0,%s)",
		     reg_names [REGNO (XEXP (XEXP (XEXP (x, 0), 1), 0))],
		     reg_names [REGNO (XEXP (XEXP (x, 0), 0))]);
	  else
	    output_address (XEXP (x, 0));
	  break;
	}
    }
  else
    output_addr_const (file, x);
}

/* output a SYMBOL_REF or a CONST expression involving a SYMBOL_REF. */

void
output_global_address (file, x, round_constant)
     FILE *file;
     rtx x;
     int round_constant;
{

  /* Imagine  (high (const (plus ...))).  */
  if (GET_CODE (x) == HIGH)
    x = XEXP (x, 0);

  if (GET_CODE (x) == SYMBOL_REF && read_only_operand (x))
    assemble_name (file, XSTR (x, 0));
  else if (GET_CODE (x) == SYMBOL_REF && !flag_pic)
    {
      assemble_name (file, XSTR (x, 0));
      fputs ("-$global$", file);
    }
  else if (GET_CODE (x) == CONST)
    {
      char *sep = "";
      int offset = 0;		/* assembler wants -$global$ at end */
      rtx base;

      if (GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF)
	{
	  base = XEXP (XEXP (x, 0), 0);
	  output_addr_const (file, base);
	}
      else if (GET_CODE (XEXP (XEXP (x, 0), 0)) == CONST_INT)
	offset = INTVAL (XEXP (XEXP (x, 0), 0));
      else abort ();

      if (GET_CODE (XEXP (XEXP (x, 0), 1)) == SYMBOL_REF)
	{
	  base = XEXP (XEXP (x, 0), 1);
	  output_addr_const (file, base);
	}
      else if (GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT)
	offset = INTVAL (XEXP (XEXP (x, 0),1));
      else abort ();

      /* How bogus.  The compiler is apparently responsible for
	 rounding the constant if it uses an LR field selector.

	 The linker and/or assembler seem a better place since
	 they have to do this kind of thing already.

	 If we fail to do this, HP's optimizing linker may eliminate
	 an addil, but not update the ldw/stw/ldo instruction that
	 uses the result of the addil.  */
      if (round_constant)
	offset = ((offset + 0x1000) & ~0x1fff);

      if (GET_CODE (XEXP (x, 0)) == PLUS)
	{
	  if (offset < 0)
	    {
	      offset = -offset;
	      sep = "-";
	    }
	  else
	    sep = "+";
	}
      else if (GET_CODE (XEXP (x, 0)) == MINUS
	       && (GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF))
	sep = "-";
      else abort ();

      if (!read_only_operand (base) && !flag_pic)
	fputs ("-$global$", file);
      if (offset)
	fprintf (file,"%s%d", sep, offset);
    }
  else
    output_addr_const (file, x);
}

void
output_deferred_plabels (file)
     FILE *file;
{
  int i;
  /* If we have deferred plabels, then we need to switch into the data
     section and align it to a 4 byte boundary before we output the
     deferred plabels.  */
  if (n_deferred_plabels)
    {
      data_section ();
      ASM_OUTPUT_ALIGN (file, 2);
    }

  /* Now output the deferred plabels.  */
  for (i = 0; i < n_deferred_plabels; i++)
    {
      ASM_OUTPUT_INTERNAL_LABEL (file, "L", CODE_LABEL_NUMBER (deferred_plabels[i].internal_label));
      assemble_integer (gen_rtx (SYMBOL_REF, VOIDmode,
				 deferred_plabels[i].name), 4, 1);
    }
}

/* HP's millicode routines mean something special to the assembler.
   Keep track of which ones we have used.  */

enum millicodes { remI, remU, divI, divU, mulI, mulU, end1000 };
static char imported[(int)end1000];
static char *milli_names[] = {"remI", "remU", "divI", "divU", "mulI", "mulU"};
static char import_string[] = ".IMPORT $$....,MILLICODE";
#define MILLI_START 10

static void
import_milli (code)
     enum millicodes code;
{
  char str[sizeof (import_string)];

  if (!imported[(int)code])
    {
      imported[(int)code] = 1;
      strcpy (str, import_string);
      strncpy (str + MILLI_START, milli_names[(int)code], 4);
      output_asm_insn (str, 0);
    }
}

/* The register constraints have put the operands and return value in
   the proper registers. */

char *
output_mul_insn (unsignedp, insn)
     int unsignedp;
     rtx insn;
{
  import_milli (mulI);
  return output_millicode_call (insn, gen_rtx (SYMBOL_REF, SImode, "$$mulI"));
}

/* Emit the rtl for doing a division by a constant. */

/* Do magic division millicodes exist for this value? */
static int magic_milli[]= {0, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 0,
			     1, 1};

/* We'll use an array to keep track of the magic millicodes and
   whether or not we've used them already. [n][0] is signed, [n][1] is
   unsigned. */

static int div_milli[16][2];

int
div_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (mode == SImode
	  && ((GET_CODE (op) == REG && REGNO (op) == 25)
	      || (GET_CODE (op) == CONST_INT && INTVAL (op) > 0
		  && INTVAL (op) < 16 && magic_milli[INTVAL (op)])));
}

int
emit_hpdiv_const (operands, unsignedp)
     rtx *operands;
     int unsignedp;
{
  if (GET_CODE (operands[2]) == CONST_INT
      && INTVAL (operands[2]) > 0
      && INTVAL (operands[2]) < 16
      && magic_milli[INTVAL (operands[2])])
    {
      emit_move_insn ( gen_rtx (REG, SImode, 26), operands[1]);
      emit
	(gen_rtx
	 (PARALLEL, VOIDmode,
	  gen_rtvec (5, gen_rtx (SET, VOIDmode, gen_rtx (REG, SImode, 29),
				 gen_rtx (unsignedp ? UDIV : DIV, SImode,
					  gen_rtx (REG, SImode, 26),
					  operands[2])),
		     gen_rtx (CLOBBER, VOIDmode, operands[3]),
		     gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SImode, 26)),
		     gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SImode, 25)),
		     gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SImode, 31)))));
      emit_move_insn (operands[0], gen_rtx (REG, SImode, 29));
      return 1;
    }
  return 0;
}

char *
output_div_insn (operands, unsignedp, insn)
     rtx *operands;
     int unsignedp;
     rtx insn;
{
  int divisor;

  /* If the divisor is a constant, try to use one of the special
     opcodes .*/
  if (GET_CODE (operands[0]) == CONST_INT)
    {
      static char buf[100];
      divisor = INTVAL (operands[0]);
      if (!div_milli[divisor][unsignedp])
	{
	  div_milli[divisor][unsignedp] = 1;
	  if (unsignedp)
	    output_asm_insn (".IMPORT $$divU_%0,MILLICODE", operands);
	  else
	    output_asm_insn (".IMPORT $$divI_%0,MILLICODE", operands);
	}
      if (unsignedp)
	{
	  sprintf (buf, "$$divU_%d", INTVAL (operands[0]));
	  return output_millicode_call (insn,
					gen_rtx (SYMBOL_REF, SImode, buf));
	}
      else
	{
	  sprintf (buf, "$$divI_%d", INTVAL (operands[0]));
	  return output_millicode_call (insn,
					gen_rtx (SYMBOL_REF, SImode, buf));
	}
    }
  /* Divisor isn't a special constant. */
  else
    {
      if (unsignedp)
	{
	  import_milli (divU);
	  return output_millicode_call (insn,
					gen_rtx (SYMBOL_REF, SImode, "$$divU"));
	}
      else
	{
	  import_milli (divI);
	  return output_millicode_call (insn,
					gen_rtx (SYMBOL_REF, SImode, "$$divI"));
	}
    }
}

/* Output a $$rem millicode to do mod. */

char *
output_mod_insn (unsignedp, insn)
     int unsignedp;
     rtx insn;
{
  if (unsignedp)
    {
      import_milli (remU);
      return output_millicode_call (insn,
				    gen_rtx (SYMBOL_REF, SImode, "$$remU"));
    }
  else
    {
      import_milli (remI);
      return output_millicode_call (insn,
				    gen_rtx (SYMBOL_REF, SImode, "$$remI"));
    }
}

void
output_arg_descriptor (call_insn)
     rtx call_insn;
{
  char *arg_regs[4];
  enum machine_mode arg_mode;
  rtx link;
  int i, output_flag = 0;
  int regno;

  for (i = 0; i < 4; i++)
    arg_regs[i] = 0;

  /* Specify explicitly that no argument relocations should take place
     if using the portable runtime calling conventions.  */
  if (TARGET_PORTABLE_RUNTIME)
    {
      fputs ("\t.CALL ARGW0=NO,ARGW1=NO,ARGW2=NO,ARGW3=NO,RETVAL=NO\n",
	     asm_out_file);
      return;
    }

  if (GET_CODE (call_insn) != CALL_INSN)
    abort ();
  for (link = CALL_INSN_FUNCTION_USAGE (call_insn); link; link = XEXP (link, 1))
    {
      rtx use = XEXP (link, 0);

      if (! (GET_CODE (use) == USE
	     && GET_CODE (XEXP (use, 0)) == REG
	     && FUNCTION_ARG_REGNO_P (REGNO (XEXP (use, 0)))))
	continue;

      arg_mode = GET_MODE (XEXP (use, 0));
      regno = REGNO (XEXP (use, 0));
      if (regno >= 23 && regno <= 26)
	{
	  arg_regs[26 - regno] = "GR";
	  if (arg_mode == DImode)
	    arg_regs[25 - regno] = "GR";
	}
      else if (regno >= 32 && regno <= 39)
	{
	  if (arg_mode == SFmode)
	    arg_regs[(regno - 32) / 2] = "FR";
	  else
	    {
#ifndef HP_FP_ARG_DESCRIPTOR_REVERSED
	      arg_regs[(regno - 34) / 2] = "FR";
	      arg_regs[(regno - 34) / 2 + 1] = "FU";
#else
	      arg_regs[(regno - 34) / 2] = "FU";
	      arg_regs[(regno - 34) / 2 + 1] = "FR";
#endif
	    }
	}
    }
  fputs ("\t.CALL ", asm_out_file);
  for (i = 0; i < 4; i++)
    {
      if (arg_regs[i])
	{
	  if (output_flag++)
	    fputc (',', asm_out_file);
	  fprintf (asm_out_file, "ARGW%d=%s", i, arg_regs[i]);
	}
    }
  fputc ('\n', asm_out_file);
}

/* Return the class of any secondary reload register that is needed to
   move IN into a register in class CLASS using mode MODE.

   Profiling has showed this routine and its descendants account for
   a significant amount of compile time (~7%).  So it has been
   optimized to reduce redundant computations and eliminate useless
   function calls.

   It might be worthwhile to try and make this a leaf function too.  */

enum reg_class
secondary_reload_class (class, mode, in)
     enum reg_class class;
     enum machine_mode mode;
     rtx in;
{
  int regno, is_symbolic;

  /* Trying to load a constant into a FP register during PIC code
     generation will require %r1 as a scratch register.  */
  if (flag_pic == 2
      && GET_MODE_CLASS (mode) == MODE_INT
      && FP_REG_CLASS_P (class)
      && (GET_CODE (in) == CONST_INT || GET_CODE (in) == CONST_DOUBLE))
    return R1_REGS;

  /* Profiling showed the PA port spends about 1.3% of its compilation
     time in true_regnum from calls inside secondary_reload_class.  */

  if (GET_CODE (in) == REG)
    {
      regno = REGNO (in);
      if (regno >= FIRST_PSEUDO_REGISTER)
	regno = true_regnum (in);
    }
  else if (GET_CODE (in) == SUBREG)
    regno = true_regnum (in);
  else
    regno = -1;

  /* If we have something like (mem (mem (...)), we can safely assume the
     inner MEM will end up in a general register after reloading, so there's
     no need for a secondary reload.  */
  if (GET_CODE (in) == MEM
      && GET_CODE (XEXP (in, 0)) == MEM)
    return NO_REGS;

  /* Handle out of range displacement for integer mode loads/stores of
     FP registers.  */
  if (((regno >= FIRST_PSEUDO_REGISTER || regno == -1)
       && GET_MODE_CLASS (mode) == MODE_INT
       && FP_REG_CLASS_P (class))
      || (class == SHIFT_REGS && (regno <= 0 || regno >= 32)))
    return GENERAL_REGS;

  if (GET_CODE (in) == HIGH)
    in = XEXP (in, 0);

  /* Profiling has showed GCC spends about 2.6% of its compilation
     time in symbolic_operand from calls inside secondary_reload_class.

     We use an inline copy and only compute its return value once to avoid
     useless work.  */
  switch (GET_CODE (in))
    {
      rtx tmp;

      case SYMBOL_REF:
      case LABEL_REF:
        is_symbolic = 1;
        break;
      case CONST:
	tmp = XEXP (in, 0);
	is_symbolic = ((GET_CODE (XEXP (tmp, 0)) == SYMBOL_REF
			|| GET_CODE (XEXP (tmp, 0)) == LABEL_REF)
		       && GET_CODE (XEXP (tmp, 1)) == CONST_INT);
        break;

      default:
        is_symbolic = 0;
        break;
    }
  
  if (!flag_pic
      && is_symbolic
      && read_only_operand (in))
    return NO_REGS;

  if (class != R1_REGS && is_symbolic)
    return R1_REGS;

  return NO_REGS;
}

enum direction
function_arg_padding (mode, type)
     enum machine_mode mode;
     tree type;
{
  int size;

  if (mode == BLKmode)
    {
      if (type && TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST)
	size = int_size_in_bytes (type) * BITS_PER_UNIT;
      else
	return upward;		/* Don't know if this is right, but */
				/* same as old definition. */
    }
  else
    size = GET_MODE_BITSIZE (mode);
  if (size < PARM_BOUNDARY)
    return downward;
  else if (size % PARM_BOUNDARY)
    return upward;
  else
    return none;
}


/* Do what is necessary for `va_start'.  The argument is ignored;
   We look at the current function to determine if stdargs or varargs
   is used and fill in an initial va_list.  A pointer to this constructor
   is returned.  */

struct rtx_def *
hppa_builtin_saveregs (arglist)
     tree arglist;
{
  rtx offset, dest;
  tree fntype = TREE_TYPE (current_function_decl);
  int argadj = ((!(TYPE_ARG_TYPES (fntype) != 0
		   && (TREE_VALUE (tree_last (TYPE_ARG_TYPES (fntype)))
		       != void_type_node)))
		? UNITS_PER_WORD : 0);

  if (argadj)
    offset = plus_constant (current_function_arg_offset_rtx, argadj);
  else
    offset = current_function_arg_offset_rtx;

  /* Store general registers on the stack. */
  dest = gen_rtx (MEM, BLKmode,
		  plus_constant (current_function_internal_arg_pointer, -16));
  move_block_from_reg (23, dest, 4, 4 * UNITS_PER_WORD);

  /* move_block_from_reg will emit code to store the argument registers
     individually as scalar stores.

     However, other insns may later load from the same addresses for
     a structure load (passing a struct to a varargs routine).

     The alias code assumes that such aliasing can never happen, so we
     have to keep memory referencing insns from moving up beyond the
     last argument register store.  So we emit a blockage insn here.  */
  emit_insn (gen_blockage ());

  if (flag_check_memory_usage)
    emit_library_call (chkr_set_right_libfunc, 1, VOIDmode, 3,
		       dest, ptr_mode,
		       GEN_INT (4 * UNITS_PER_WORD), TYPE_MODE (sizetype),
		       GEN_INT (MEMORY_USE_RW),
		       TYPE_MODE (integer_type_node));

  return copy_to_reg (expand_binop (Pmode, add_optab,
				    current_function_internal_arg_pointer,
				    offset, 0, 0, OPTAB_LIB_WIDEN));
}

/* This routine handles all the normal conditional branch sequences we
   might need to generate.  It handles compare immediate vs compare
   register, nullification of delay slots, varying length branches,
   negated branches, and all combinations of the above.  It returns the
   output appropriate to emit the branch corresponding to all given
   parameters.  */

char *
output_cbranch (operands, nullify, length, negated, insn)
  rtx *operands;
  int nullify, length, negated;
  rtx insn;
{
  static char buf[100];
  int useskip = 0;

  /* A conditional branch to the following instruction (eg the delay slot) is
     asking for a disaster.  This can happen when not optimizing.

     In such cases it is safe to emit nothing.  */

  if (next_active_insn (JUMP_LABEL (insn)) == next_active_insn (insn))
    return "";

  /* If this is a long branch with its delay slot unfilled, set `nullify'
     as it can nullify the delay slot and save a nop.  */
  if (length == 8 && dbr_sequence_length () == 0)
    nullify = 1;

  /* If this is a short forward conditional branch which did not get
     its delay slot filled, the delay slot can still be nullified.  */
  if (! nullify && length == 4 && dbr_sequence_length () == 0)
    nullify = forward_branch_p (insn);

  /* A forward branch over a single nullified insn can be done with a
     comclr instruction.  This avoids a single cycle penalty due to
     mis-predicted branch if we fall through (branch not taken).  */
  if (length == 4
      && next_real_insn (insn) != 0
      && get_attr_length (next_real_insn (insn)) == 4
      && JUMP_LABEL (insn) == next_nonnote_insn (next_real_insn (insn))
      && nullify)
    useskip = 1;

  switch (length)
    {
      /* All short conditional branches except backwards with an unfilled
	 delay slot.  */
      case 4:
	if (useskip)
	  strcpy (buf, "com%I2clr,");
	else
	  strcpy (buf, "com%I2b,");
	if (negated)
	  strcat (buf, "%B3");
	else
	  strcat (buf, "%S3");
	if (useskip)
	  strcat (buf, " %2,%1,0");
	else if (nullify)
	  strcat (buf, ",n %2,%1,%0");
	else
	  strcat (buf, " %2,%1,%0");
	break;

     /* All long conditionals.  Note an short backward branch with an
	unfilled delay slot is treated just like a long backward branch
	with an unfilled delay slot.  */
      case 8:
	/* Handle weird backwards branch with a filled delay slot
	   with is nullified.  */
	if (dbr_sequence_length () != 0
	    && ! forward_branch_p (insn)
	    && nullify)
	  {
	    strcpy (buf, "com%I2b,");
	    if (negated)
	      strcat (buf, "%S3");
	    else
	      strcat (buf, "%B3");
	    strcat (buf, ",n %2,%1,.+12\n\tbl %0,0");
	  }
	/* Handle short backwards branch with an unfilled delay slot.
	   Using a comb;nop rather than comiclr;bl saves 1 cycle for both
	   taken and untaken branches.  */
	else if (dbr_sequence_length () == 0
		 && ! forward_branch_p (insn)
		 && insn_addresses
		 && VAL_14_BITS_P (insn_addresses[INSN_UID (JUMP_LABEL (insn))]
				    - insn_addresses[INSN_UID (insn)] - 8))
	  {
	    strcpy (buf, "com%I2b,");
	    if (negated)
	      strcat (buf, "%B3 %2,%1,%0%#");
	    else
	      strcat (buf, "%S3 %2,%1,%0%#");
	  }
	else
	  {
	    strcpy (buf, "com%I2clr,");
	    if (negated)
	      strcat (buf, "%S3");
	    else
	      strcat (buf, "%B3");
	    if (nullify)
	      strcat (buf, " %2,%1,0\n\tbl,n %0,0");
	    else
	      strcat (buf, " %2,%1,0\n\tbl %0,0");
	  }
	break;

      case 20:
	/* Very long branch.  Right now we only handle these when not
	   optimizing.  See "jump" pattern in pa.md for details.  */
	if (optimize)
	  abort ();

	/* Create a reversed conditional branch which branches around
	   the following insns.  */
	if (negated)
	  strcpy (buf, "com%I2b,%S3,n %2,%1,.+20");
	else
	  strcpy (buf, "com%I2b,%B3,n %2,%1,.+20");
	output_asm_insn (buf, operands);

	/* Output an insn to save %r1.  */
	output_asm_insn ("stw %%r1,-16(%%r30)", operands);

	/* Now output a very long branch to the original target.  */
	output_asm_insn ("ldil L'%l0,%%r1\n\tbe R'%l0(%%sr4,%%r1)", operands);

	/* Now restore the value of %r1 in the delay slot.  We're not
	   optimizing so we know nothing else can be in the delay slot.  */
	return "ldw -16(%%r30),%%r1";

      case 28:
	/* Very long branch when generating PIC code.  Right now we only
	   handle these when not optimizing.  See "jump" pattern in pa.md
	   for details.  */
	if (optimize)
	  abort ();

	/* Create a reversed conditional branch which branches around
	   the following insns.  */
	if (negated)
	  strcpy (buf, "com%I2b,%S3,n %2,%1,.+28");
	else
	  strcpy (buf, "com%I2b,%B3,n %2,%1,.+28");
	output_asm_insn (buf, operands);

	/* Output an insn to save %r1.  */
	output_asm_insn ("stw %%r1,-16(%%r30)", operands);

	/* Now output a very long PIC branch to the original target.  */
	{
	  rtx xoperands[5];

	  xoperands[0] = operands[0];
	  xoperands[1] = operands[1];
	  xoperands[2] = operands[2];
	  xoperands[3] = operands[3];
	  xoperands[4] = gen_label_rtx ();

	  output_asm_insn ("bl .+8,%%r1\n\taddil L'%l0-%l4,%%r1", xoperands);
	  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L",
				     CODE_LABEL_NUMBER (xoperands[4]));
	  output_asm_insn ("ldo R'%l0-%l4(%%r1),%%r1\n\tbv 0(%%r1)", xoperands);
	}

	/* Now restore the value of %r1 in the delay slot.  We're not
	   optimizing so we know nothing else can be in the delay slot.  */
	return "ldw -16(%%r30),%%r1";
	
      default:
	abort();
    }
  return buf;
}

/* This routine handles all the branch-on-bit conditional branch sequences we
   might need to generate.  It handles nullification of delay slots,
   varying length branches, negated branches and all combinations of the
   above.  it returns the appropriate output template to emit the branch.  */

char *
output_bb (operands, nullify, length, negated, insn, which)
  rtx *operands;
  int nullify, length, negated;
  rtx insn;
  int which;
{
  static char buf[100];
  int useskip = 0;

  /* A conditional branch to the following instruction (eg the delay slot) is
     asking for a disaster.  I do not think this can happen as this pattern
     is only used when optimizing; jump optimization should eliminate the
     jump.  But be prepared just in case.  */

  if (next_active_insn (JUMP_LABEL (insn)) == next_active_insn (insn))
    return "";

  /* If this is a long branch with its delay slot unfilled, set `nullify'
     as it can nullify the delay slot and save a nop.  */
  if (length == 8 && dbr_sequence_length () == 0)
    nullify = 1;

  /* If this is a short forward conditional branch which did not get
     its delay slot filled, the delay slot can still be nullified.  */
  if (! nullify && length == 4 && dbr_sequence_length () == 0)
    nullify = forward_branch_p (insn);

  /* A forward branch over a single nullified insn can be done with a
     extrs instruction.  This avoids a single cycle penalty due to
     mis-predicted branch if we fall through (branch not taken).  */

  if (length == 4
      && next_real_insn (insn) != 0
      && get_attr_length (next_real_insn (insn)) == 4
      && JUMP_LABEL (insn) == next_nonnote_insn (next_real_insn (insn))
      && nullify)
    useskip = 1;

  switch (length)
    {

      /* All short conditional branches except backwards with an unfilled
	 delay slot.  */
      case 4:
	if (useskip)
	  strcpy (buf, "extrs,");
	else
	  strcpy (buf, "bb,");
	if ((which == 0 && negated)
	     || (which == 1 && ! negated))
	  strcat (buf, ">=");
	else
	  strcat (buf, "<");
	if (useskip)
	  strcat (buf, " %0,%1,1,0");
	else if (nullify && negated)
	  strcat (buf, ",n %0,%1,%3");
	else if (nullify && ! negated)
	  strcat (buf, ",n %0,%1,%2");
	else if (! nullify && negated)
	  strcat (buf, "%0,%1,%3");
	else if (! nullify && ! negated)
	  strcat (buf, " %0,%1,%2");
	break;

     /* All long conditionals.  Note an short backward branch with an
	unfilled delay slot is treated just like a long backward branch
	with an unfilled delay slot.  */
      case 8:
	/* Handle weird backwards branch with a filled delay slot
	   with is nullified.  */
	if (dbr_sequence_length () != 0
	    && ! forward_branch_p (insn)
	    && nullify)
	  {
	    strcpy (buf, "bb,");
	    if ((which == 0 && negated)
		|| (which == 1 && ! negated))
	      strcat (buf, "<");
	    else
	      strcat (buf, ">=");
	    if (negated)
	      strcat (buf, ",n %0,%1,.+12\n\tbl %3,0");
	    else
	      strcat (buf, ",n %0,%1,.+12\n\tbl %2,0");
	  }
	/* Handle short backwards branch with an unfilled delay slot.
	   Using a bb;nop rather than extrs;bl saves 1 cycle for both
	   taken and untaken branches.  */
	else if (dbr_sequence_length () == 0
		 && ! forward_branch_p (insn)
		 && insn_addresses
		 && VAL_14_BITS_P (insn_addresses[INSN_UID (JUMP_LABEL (insn))]
				    - insn_addresses[INSN_UID (insn)] - 8))
	  {
	    strcpy (buf, "bb,");
	    if ((which == 0 && negated)
		|| (which == 1 && ! negated))
	      strcat (buf, ">=");
	    else
	      strcat (buf, "<");
	    if (negated)
	      strcat (buf, " %0,%1,%3%#");
	    else
	      strcat (buf, " %0,%1,%2%#");
	  }
	else
	  {
	    strcpy (buf, "extrs,");
	    if ((which == 0 && negated)
		|| (which == 1 && ! negated))
	      strcat (buf, "<");
	    else
	      strcat (buf, ">=");
	    if (nullify && negated)
	      strcat (buf, " %0,%1,1,0\n\tbl,n %3,0");
	    else if (nullify && ! negated)
	      strcat (buf, " %0,%1,1,0\n\tbl,n %2,0");
	    else if (negated)
	      strcat (buf, " %0,%1,1,0\n\tbl %3,0");
	    else
	      strcat (buf, " %0,%1,1,0\n\tbl %2,0");
	  }
	break;

      default:
	abort();
    }
  return buf;
}

/* This routine handles all the branch-on-variable-bit conditional branch
   sequences we might need to generate.  It handles nullification of delay
   slots, varying length branches, negated branches and all combinations
   of the above.  it returns the appropriate output template to emit the
   branch.  */

char *
output_bvb (operands, nullify, length, negated, insn, which)
  rtx *operands;
  int nullify, length, negated;
  rtx insn;
  int which;
{
  static char buf[100];
  int useskip = 0;

  /* A conditional branch to the following instruction (eg the delay slot) is
     asking for a disaster.  I do not think this can happen as this pattern
     is only used when optimizing; jump optimization should eliminate the
     jump.  But be prepared just in case.  */

  if (next_active_insn (JUMP_LABEL (insn)) == next_active_insn (insn))
    return "";

  /* If this is a long branch with its delay slot unfilled, set `nullify'
     as it can nullify the delay slot and save a nop.  */
  if (length == 8 && dbr_sequence_length () == 0)
    nullify = 1;

  /* If this is a short forward conditional branch which did not get
     its delay slot filled, the delay slot can still be nullified.  */
  if (! nullify && length == 4 && dbr_sequence_length () == 0)
    nullify = forward_branch_p (insn);

  /* A forward branch over a single nullified insn can be done with a
     extrs instruction.  This avoids a single cycle penalty due to
     mis-predicted branch if we fall through (branch not taken).  */

  if (length == 4
      && next_real_insn (insn) != 0
      && get_attr_length (next_real_insn (insn)) == 4
      && JUMP_LABEL (insn) == next_nonnote_insn (next_real_insn (insn))
      && nullify)
    useskip = 1;

  switch (length)
    {

      /* All short conditional branches except backwards with an unfilled
	 delay slot.  */
      case 4:
	if (useskip)
	  strcpy (buf, "vextrs,");
	else
	  strcpy (buf, "bvb,");
	if ((which == 0 && negated)
	     || (which == 1 && ! negated))
	  strcat (buf, ">=");
	else
	  strcat (buf, "<");
	if (useskip)
	  strcat (buf, " %0,1,0");
	else if (nullify && negated)
	  strcat (buf, ",n %0,%3");
	else if (nullify && ! negated)
	  strcat (buf, ",n %0,%2");
	else if (! nullify && negated)
	  strcat (buf, "%0,%3");
	else if (! nullify && ! negated)
	  strcat (buf, " %0,%2");
	break;

     /* All long conditionals.  Note an short backward branch with an
	unfilled delay slot is treated just like a long backward branch
	with an unfilled delay slot.  */
      case 8:
	/* Handle weird backwards branch with a filled delay slot
	   with is nullified.  */
	if (dbr_sequence_length () != 0
	    && ! forward_branch_p (insn)
	    && nullify)
	  {
	    strcpy (buf, "bvb,");
	    if ((which == 0 && negated)
		|| (which == 1 && ! negated))
	      strcat (buf, "<");
	    else
	      strcat (buf, ">=");
	    if (negated)
	      strcat (buf, ",n %0,.+12\n\tbl %3,0");
	    else
	      strcat (buf, ",n %0,.+12\n\tbl %2,0");
	  }
	/* Handle short backwards branch with an unfilled delay slot.
	   Using a bb;nop rather than extrs;bl saves 1 cycle for both
	   taken and untaken branches.  */
	else if (dbr_sequence_length () == 0
		 && ! forward_branch_p (insn)
		 && insn_addresses
		 && VAL_14_BITS_P (insn_addresses[INSN_UID (JUMP_LABEL (insn))]
				    - insn_addresses[INSN_UID (insn)] - 8))
	  {
	    strcpy (buf, "bvb,");
	    if ((which == 0 && negated)
		|| (which == 1 && ! negated))
	      strcat (buf, ">=");
	    else
	      strcat (buf, "<");
	    if (negated)
	      strcat (buf, " %0,%3%#");
	    else
	      strcat (buf, " %0,%2%#");
	  }
	else
	  {
	    strcpy (buf, "vextrs,");
	    if ((which == 0 && negated)
		|| (which == 1 && ! negated))
	      strcat (buf, "<");
	    else
	      strcat (buf, ">=");
	    if (nullify && negated)
	      strcat (buf, " %0,1,0\n\tbl,n %3,0");
	    else if (nullify && ! negated)
	      strcat (buf, " %0,1,0\n\tbl,n %2,0");
	    else if (negated)
	      strcat (buf, " %0,1,0\n\tbl %3,0");
	    else
	      strcat (buf, " %0,1,0\n\tbl %2,0");
	  }
	break;

      default:
	abort();
    }
  return buf;
}

/* Return the output template for emitting a dbra type insn.

   Note it may perform some output operations on its own before
   returning the final output string.  */
char *
output_dbra (operands, insn, which_alternative)
     rtx *operands;
     rtx insn;
     int which_alternative;
{

  /* A conditional branch to the following instruction (eg the delay slot) is
     asking for a disaster.  Be prepared!  */

  if (next_active_insn (JUMP_LABEL (insn)) == next_active_insn (insn))
    {
      if (which_alternative == 0)
	return "ldo %1(%0),%0";
      else if (which_alternative == 1)
	{
	  output_asm_insn ("fstws %0,-16(0,%%r30)",operands);
	  output_asm_insn ("ldw -16(0,%%r30),%4",operands);
	  output_asm_insn ("ldo %1(%4),%4\n\tstw %4,-16(0,%%r30)", operands);
	  return "fldws -16(0,%%r30),%0";
	}
      else
	{
	  output_asm_insn ("ldw %0,%4", operands);
	  return "ldo %1(%4),%4\n\tstw %4,%0";
	}
    }

  if (which_alternative == 0)
    {
      int nullify = INSN_ANNULLED_BRANCH_P (insn);
      int length = get_attr_length (insn);

      /* If this is a long branch with its delay slot unfilled, set `nullify'
	 as it can nullify the delay slot and save a nop.  */
      if (length == 8 && dbr_sequence_length () == 0)
	nullify = 1;

      /* If this is a short forward conditional branch which did not get
	 its delay slot filled, the delay slot can still be nullified.  */
      if (! nullify && length == 4 && dbr_sequence_length () == 0)
	nullify = forward_branch_p (insn);

      /* Handle short versions first.  */
      if (length == 4 && nullify)
	return "addib,%C2,n %1,%0,%3";
      else if (length == 4 && ! nullify)
	return "addib,%C2 %1,%0,%3";
      else if (length == 8)
	{
	  /* Handle weird backwards branch with a fulled delay slot
	     which is nullified.  */
	  if (dbr_sequence_length () != 0
	      && ! forward_branch_p (insn)
	      && nullify)
	    return "addib,%N2,n %1,%0,.+12\n\tbl %3,0";
	  /* Handle short backwards branch with an unfilled delay slot.
	     Using a addb;nop rather than addi;bl saves 1 cycle for both
	     taken and untaken branches.  */
	  else if (dbr_sequence_length () == 0
		   && ! forward_branch_p (insn)
		   && insn_addresses
		   && VAL_14_BITS_P (insn_addresses[INSN_UID (JUMP_LABEL (insn))]
				      - insn_addresses[INSN_UID (insn)] - 8))
	      return "addib,%C2 %1,%0,%3%#";

	  /* Handle normal cases.  */
	  if (nullify)
	    return "addi,%N2 %1,%0,%0\n\tbl,n %3,0";
	  else
	    return "addi,%N2 %1,%0,%0\n\tbl %3,0";
	}
      else
	abort();
    }
  /* Deal with gross reload from FP register case.  */
  else if (which_alternative == 1)
    {
      /* Move loop counter from FP register to MEM then into a GR,
	 increment the GR, store the GR into MEM, and finally reload
	 the FP register from MEM from within the branch's delay slot.  */
      output_asm_insn ("fstws %0,-16(0,%%r30)\n\tldw -16(0,%%r30),%4",operands);
      output_asm_insn ("ldo %1(%4),%4\n\tstw %4,-16(0,%%r30)", operands);
      if (get_attr_length (insn) == 24)
	return "comb,%S2 0,%4,%3\n\tfldws -16(0,%%r30),%0";
      else
	return "comclr,%B2 0,%4,0\n\tbl %3,0\n\tfldws -16(0,%%r30),%0";
    }
  /* Deal with gross reload from memory case.  */
  else
    {
      /* Reload loop counter from memory, the store back to memory
	 happens in the branch's delay slot.   */
      output_asm_insn ("ldw %0,%4", operands);
      if (get_attr_length (insn) == 12)
	return "addib,%C2 %1,%4,%3\n\tstw %4,%0";
      else
	return "addi,%N2 %1,%4,%4\n\tbl %3,0\n\tstw %4,%0";
    }
}

/* Return the output template for emitting a dbra type insn.

   Note it may perform some output operations on its own before
   returning the final output string.  */
char *
output_movb (operands, insn, which_alternative, reverse_comparison)
     rtx *operands;
     rtx insn;
     int which_alternative;
     int reverse_comparison;
{

  /* A conditional branch to the following instruction (eg the delay slot) is
     asking for a disaster.  Be prepared!  */

  if (next_active_insn (JUMP_LABEL (insn)) == next_active_insn (insn))
    {
      if (which_alternative == 0)
	return "copy %1,%0";
      else if (which_alternative == 1)
	{
	  output_asm_insn ("stw %1,-16(0,%%r30)",operands);
	  return "fldws -16(0,%%r30),%0";
	}
      else if (which_alternative == 2)
	return "stw %1,%0";
      else
	return "mtsar %r1";
    }

  /* Support the second variant.  */
  if (reverse_comparison)
    PUT_CODE (operands[2], reverse_condition (GET_CODE (operands[2])));

  if (which_alternative == 0)
    {
      int nullify = INSN_ANNULLED_BRANCH_P (insn);
      int length = get_attr_length (insn);

      /* If this is a long branch with its delay slot unfilled, set `nullify'
	 as it can nullify the delay slot and save a nop.  */
      if (length == 8 && dbr_sequence_length () == 0)
	nullify = 1;

      /* If this is a short forward conditional branch which did not get
	 its delay slot filled, the delay slot can still be nullified.  */
      if (! nullify && length == 4 && dbr_sequence_length () == 0)
	nullify = forward_branch_p (insn);

      /* Handle short versions first.  */
      if (length == 4 && nullify)
	return "movb,%C2,n %1,%0,%3";
      else if (length == 4 && ! nullify)
	return "movb,%C2 %1,%0,%3";
      else if (length == 8)
	{
	  /* Handle weird backwards branch with a filled delay slot
	     which is nullified.  */
	  if (dbr_sequence_length () != 0
	      && ! forward_branch_p (insn)
	      && nullify)
	    return "movb,%N2,n %1,%0,.+12\n\tbl %3,0";

	  /* Handle short backwards branch with an unfilled delay slot.
	     Using a movb;nop rather than or;bl saves 1 cycle for both
	     taken and untaken branches.  */
	  else if (dbr_sequence_length () == 0
		   && ! forward_branch_p (insn)
		   && insn_addresses
		   && VAL_14_BITS_P (insn_addresses[INSN_UID (JUMP_LABEL (insn))]
				      - insn_addresses[INSN_UID (insn)] - 8))
	    return "movb,%C2 %1,%0,%3%#";
	  /* Handle normal cases.  */
	  if (nullify)
	    return "or,%N2 %1,%%r0,%0\n\tbl,n %3,0";
	  else
	    return "or,%N2 %1,%%r0,%0\n\tbl %3,0";
	}
      else
	abort();
    }
  /* Deal with gross reload from FP register case.  */
  else if (which_alternative == 1)
    {
      /* Move loop counter from FP register to MEM then into a GR,
	 increment the GR, store the GR into MEM, and finally reload
	 the FP register from MEM from within the branch's delay slot.  */
      output_asm_insn ("stw %1,-16(0,%%r30)",operands);
      if (get_attr_length (insn) == 12)
	return "comb,%S2 0,%1,%3\n\tfldws -16(0,%%r30),%0";
      else
	return "comclr,%B2 0,%1,0\n\tbl %3,0\n\tfldws -16(0,%%r30),%0";
    }
  /* Deal with gross reload from memory case.  */
  else if (which_alternative == 2)
    {
      /* Reload loop counter from memory, the store back to memory
	 happens in the branch's delay slot.   */
      if (get_attr_length (insn) == 8)
	return "comb,%S2 0,%1,%3\n\tstw %1,%0";
      else
	return "comclr,%B2 0,%1,0\n\tbl %3,0\n\tstw %1,%0";
    }
  /* Handle SAR as a destination.  */
  else
    {
      if (get_attr_length (insn) == 8)
	return "comb,%S2 0,%1,%3\n\tmtsar %r1";
      else
	return "comclr,%B2 0,%1,0\n\tbl %3,0\n\tmtsar %r1";
    }
}


/* INSN is a millicode call.  It may have an unconditional jump in its delay
   slot.

   CALL_DEST is the routine we are calling.  */

char *
output_millicode_call (insn, call_dest)
  rtx insn;
  rtx call_dest;
{
  int distance;
  rtx xoperands[4];
  rtx seq_insn;

  /* Handle common case -- empty delay slot or no jump in the delay slot,
     and we're sure that the branch will reach the beginning of the $CODE$
     subspace.  */
  if ((dbr_sequence_length () == 0
       && (get_attr_length (insn) == 8 || get_attr_length (insn) == 28))
      || (dbr_sequence_length () != 0
	  && GET_CODE (NEXT_INSN (insn)) != JUMP_INSN
	  && get_attr_length (insn) == 4))
    {
      xoperands[0] = call_dest;
      output_asm_insn ("bl %0,%%r31%#", xoperands);
      return "";
    }

  /* This call may not reach the beginning of the $CODE$ subspace.  */
  if (get_attr_length (insn) > 4)
    {
      int delay_insn_deleted = 0;
      rtx xoperands[2];
      rtx link;

      /* We need to emit an inline long-call branch.  */
      if (dbr_sequence_length () != 0
	  && GET_CODE (NEXT_INSN (insn)) != JUMP_INSN)
	{
	  /* A non-jump insn in the delay slot.  By definition we can
	     emit this insn before the call.  */
	  final_scan_insn (NEXT_INSN (insn), asm_out_file, optimize, 0, 0);

	  /* Now delete the delay insn.  */
	  PUT_CODE (NEXT_INSN (insn), NOTE);
	  NOTE_LINE_NUMBER (NEXT_INSN (insn)) = NOTE_INSN_DELETED;
	  NOTE_SOURCE_FILE (NEXT_INSN (insn)) = 0;
	  delay_insn_deleted = 1;
	}

      /* If we're allowed to use be/ble instructions, then this is the
	 best sequence to use for a long millicode call.  */
      if (TARGET_NO_SPACE_REGS || TARGET_FAST_INDIRECT_CALLS
	  || ! (flag_pic  || TARGET_PORTABLE_RUNTIME))
	{
	  xoperands[0] = call_dest;
	  output_asm_insn ("ldil L%%%0,%%r31", xoperands);
	  output_asm_insn ("ble R%%%0(%%sr4,%%r31)", xoperands);
	  output_asm_insn ("nop", xoperands);
	}
      /* Pure portable runtime doesn't allow be/ble; we also don't have
	 PIC support int he assembler/linker, so this sequence is needed.  */
      else if (TARGET_PORTABLE_RUNTIME)
	{
	  xoperands[0] = call_dest;
	  /* Get the address of our target into %r29. */
	  output_asm_insn ("ldil L%%%0,%%r29", xoperands);
	  output_asm_insn ("ldo R%%%0(%%r29),%%r29", xoperands);

	  /* Get our return address into %r31.  */
	  output_asm_insn ("blr 0,%%r31", xoperands);

	  /* Jump to our target address in %r29.  */
	  output_asm_insn ("bv,n 0(%%r29)", xoperands);

	  /* Empty delay slot.  Note this insn gets fetched twice and
	     executed once.  To be safe we use a nop.  */
	  output_asm_insn ("nop", xoperands);
	  return "";
	}
      /* PIC long millicode call sequence.  */
      else
	{
	  xoperands[0] = call_dest;
	  xoperands[1] = gen_label_rtx ();
	  /* Get our address + 8 into %r1.  */
	  output_asm_insn ("bl .+8,%%r1", xoperands);

	  /* Add %r1 to the offset of our target from the next insn.  */
	  output_asm_insn ("addil L%%%0-%1,%%r1", xoperands);
	  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L",
				     CODE_LABEL_NUMBER (xoperands[1]));
	  output_asm_insn ("ldo R%%%0-%1(%%r1),%%r1", xoperands);

	  /* Get the return address into %r31.  */
	  output_asm_insn ("blr 0,%%r31", xoperands);

	  /* Branch to our target which is in %r1.  */
	  output_asm_insn ("bv,n 0(%%r1)", xoperands);

	  /* Empty delay slot.  Note this insn gets fetched twice and
	     executed once.  To be safe we use a nop.  */
	  output_asm_insn ("nop", xoperands);
	}

      /* If we had a jump in the call's delay slot, output it now.  */
      if (dbr_sequence_length () != 0
	  && !delay_insn_deleted)
	{
	  xoperands[0] = XEXP (PATTERN (NEXT_INSN (insn)), 1);
	  output_asm_insn ("b,n %0", xoperands);

	  /* Now delete the delay insn.  */
	  PUT_CODE (NEXT_INSN (insn), NOTE);
	  NOTE_LINE_NUMBER (NEXT_INSN (insn)) = NOTE_INSN_DELETED;
	  NOTE_SOURCE_FILE (NEXT_INSN (insn)) = 0;
	}
      return "";
    }

  /* This call has an unconditional jump in its delay slot and the
     call is known to reach its target or the beginning of the current
     subspace.  */

  /* Use the containing sequence insn's address.  */
  seq_insn = NEXT_INSN (PREV_INSN (XVECEXP (final_sequence, 0, 0)));

  distance = insn_addresses[INSN_UID (JUMP_LABEL (NEXT_INSN (insn)))]
	       - insn_addresses[INSN_UID (seq_insn)] - 8;

  /* If the branch was too far away, emit a normal call followed
     by a nop, followed by the unconditional branch.

     If the branch is close, then adjust %r2 from within the
     call's delay slot.  */

  xoperands[0] = call_dest;
  xoperands[1] = XEXP (PATTERN (NEXT_INSN (insn)), 1);
  if (! VAL_14_BITS_P (distance))
    output_asm_insn ("bl %0,%%r31\n\tnop\n\tbl,n %1,%%r0", xoperands);
  else
    {
      xoperands[3] = gen_label_rtx ();
      output_asm_insn ("\n\tbl %0,%%r31\n\tldo %1-%3(%%r31),%%r31", xoperands);
      ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L",
				 CODE_LABEL_NUMBER (xoperands[3]));
    }

  /* Delete the jump.  */
  PUT_CODE (NEXT_INSN (insn), NOTE);
  NOTE_LINE_NUMBER (NEXT_INSN (insn)) = NOTE_INSN_DELETED;
  NOTE_SOURCE_FILE (NEXT_INSN (insn)) = 0;
  return "";
}

extern struct obstack permanent_obstack;
extern struct obstack *saveable_obstack;
extern struct obstack *rtl_obstack;
extern struct obstack *current_obstack;

/* INSN is either a function call.  It may have an unconditional jump
   in its delay slot.

   CALL_DEST is the routine we are calling.  */

char *
output_call (insn, call_dest)
  rtx insn;
  rtx call_dest;
{
  int distance;
  rtx xoperands[4];
  rtx seq_insn;

  /* Handle common case -- empty delay slot or no jump in the delay slot,
     and we're sure that the branch will reach the beginning of the $CODE$
     subspace.  */
  if ((dbr_sequence_length () == 0
       && get_attr_length (insn) == 8)
      || (dbr_sequence_length () != 0
	  && GET_CODE (NEXT_INSN (insn)) != JUMP_INSN
	  && get_attr_length (insn) == 4))
    {
      xoperands[0] = call_dest;
      output_asm_insn ("bl %0,%%r2%#", xoperands);
      return "";
    }

  /* This call may not reach the beginning of the $CODE$ subspace.  */
  if (get_attr_length (insn) > 8)
    {
      int delay_insn_deleted = 0;
      rtx xoperands[2];
      rtx link;

      /* We need to emit an inline long-call branch.  Furthermore,
	 because we're changing a named function call into an indirect
	 function call well after the parameters have been set up, we
	 need to make sure any FP args appear in both the integer
	 and FP registers.  Also, we need move any delay slot insn
	 out of the delay slot.  And finally, we can't rely on the linker
	 being able to fix the call to $$dyncall!  -- Yuk!.  */
      if (dbr_sequence_length () != 0
	  && GET_CODE (NEXT_INSN (insn)) != JUMP_INSN)
	{
	  /* A non-jump insn in the delay slot.  By definition we can
	     emit this insn before the call (and in fact before argument
	     relocating.  */
	  final_scan_insn (NEXT_INSN (insn), asm_out_file, optimize, 0, 0);

	  /* Now delete the delay insn.  */
	  PUT_CODE (NEXT_INSN (insn), NOTE);
	  NOTE_LINE_NUMBER (NEXT_INSN (insn)) = NOTE_INSN_DELETED;
	  NOTE_SOURCE_FILE (NEXT_INSN (insn)) = 0;
	  delay_insn_deleted = 1;
	}

      /* Now copy any FP arguments into integer registers.  */
      for (link = CALL_INSN_FUNCTION_USAGE (insn); link; link = XEXP (link, 1))
	{
	  int arg_mode, regno;
	  rtx use = XEXP (link, 0);
	  if (! (GET_CODE (use) == USE
		 && GET_CODE (XEXP (use, 0)) == REG
		 && FUNCTION_ARG_REGNO_P (REGNO (XEXP (use, 0)))))
	    continue;

	  arg_mode = GET_MODE (XEXP (use, 0));
	  regno = REGNO (XEXP (use, 0));
	  /* Is it a floating point register?  */
	  if (regno >= 32 && regno <= 39)
	    {
	      /* Copy from the FP register into an integer register
		 (via memory).  */
	      if (arg_mode == SFmode)
		{
		  xoperands[0] = XEXP (use, 0);
		  xoperands[1] = gen_rtx (REG, SImode, 26 - (regno - 32) / 2);
		  output_asm_insn ("fstws %0,-16(%%sr0,%%r30)", xoperands);
		  output_asm_insn ("ldw -16(%%sr0,%%r30),%1", xoperands);
		}
	      else
		{
		  xoperands[0] = XEXP (use, 0);
		  xoperands[1] = gen_rtx (REG, DImode, 25 - (regno - 34) / 2);
		  output_asm_insn ("fstds %0,-16(%%sr0,%%r30)", xoperands);
		  output_asm_insn ("ldw -12(%%sr0,%%r30),%R1", xoperands);
		  output_asm_insn ("ldw -16(%%sr0,%%r30),%1", xoperands);
		}
	    }
	}

      /* Don't have to worry about TARGET_PORTABLE_RUNTIME here since
	 we don't have any direct calls in that case.  */
	{
	  int i;
	  char *name = XSTR (call_dest, 0);

	  /* See if we have already put this function on the list
	     of deferred plabels.  This list is generally small,
	     so a liner search is not too ugly.  If it proves too
	     slow replace it with something faster.  */
	  for (i = 0; i < n_deferred_plabels; i++)
	    if (strcmp (name, deferred_plabels[i].name) == 0)
	      break;

	  /* If the deferred plabel list is empty, or this entry was
	     not found on the list, create a new entry on the list.  */
	  if (deferred_plabels == NULL || i == n_deferred_plabels)
	    {
	      struct obstack *ambient_obstack = current_obstack;
	      struct obstack *ambient_rtl_obstack = rtl_obstack;
	      char *real_name;

	      /* Any RTL we create here needs to live until the end of
		 the compilation unit and therefore must live on the
		 permanent obstack.  */
	      current_obstack = &permanent_obstack;
	      rtl_obstack = &permanent_obstack;

	      if (deferred_plabels == 0)
		deferred_plabels = (struct deferred_plabel *)
		  xmalloc (1 * sizeof (struct deferred_plabel));
	      else
		deferred_plabels = (struct deferred_plabel *)
		  xrealloc (deferred_plabels,
			    ((n_deferred_plabels + 1)
			     * sizeof (struct deferred_plabel)));

	      i = n_deferred_plabels++;
	      deferred_plabels[i].internal_label = gen_label_rtx ();
	      deferred_plabels[i].name = obstack_alloc (&permanent_obstack,
							strlen (name) + 1);
	      strcpy (deferred_plabels[i].name, name);

	      /* Switch back to normal obstack allocation.  */
	      current_obstack = ambient_obstack;
	      rtl_obstack = ambient_rtl_obstack;

	      /* Gross.  We have just implicitly taken the address of this
		 function, mark it as such.  */
	      STRIP_NAME_ENCODING (real_name, name);
	      TREE_SYMBOL_REFERENCED (get_identifier (real_name)) = 1;
	    }

	  /* We have to load the address of the function using a procedure
	     label (plabel).  Inline plabels can lose for PIC and other
	     cases, so avoid them by creating a 32bit plabel in the data
	     segment.  */
	  if (flag_pic)
	    {
	      xoperands[0] = deferred_plabels[i].internal_label;
	      xoperands[1] = gen_label_rtx ();

	      output_asm_insn ("addil LT%%%0,%%r19", xoperands);
	      output_asm_insn ("ldw RT%%%0(%%r1),%%r22", xoperands);
	      output_asm_insn ("ldw 0(0,%%r22),%%r22", xoperands);

	      /* Get our address + 8 into %r1.  */
	      output_asm_insn ("bl .+8,%%r1", xoperands);

	      /* Add %r1 to the offset of dyncall from the next insn.  */
	      output_asm_insn ("addil L%%$$dyncall-%1,%%r1", xoperands);
	      ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L",
					 CODE_LABEL_NUMBER (xoperands[1]));
	      output_asm_insn ("ldo R%%$$dyncall-%1(%%r1),%%r1", xoperands);

	      /* Get the return address into %r31.  */
	      output_asm_insn ("blr 0,%%r31", xoperands);

	      /* Branch to our target which is in %r1.  */
	      output_asm_insn ("bv 0(%%r1)", xoperands);

	      /* Copy the return address into %r2 also.  */
	      output_asm_insn ("copy %%r31,%%r2", xoperands);
	    }
	  else
	    {
	      xoperands[0] = deferred_plabels[i].internal_label;

	      /* Get the address of our target into %r22.  */
	      output_asm_insn ("addil LR%%%0-$global$,%%r27", xoperands);
	      output_asm_insn ("ldw RR%%%0-$global$(%%r1),%%r22", xoperands);

	      /* Get the high part of the  address of $dyncall into %r2, then
		 add in the low part in the branch instruction.  */
	      output_asm_insn ("ldil L%%$$dyncall,%%r2", xoperands);
	      output_asm_insn ("ble  R%%$$dyncall(%%sr4,%%r2)", xoperands);

	      /* Copy the return pointer into both %r31 and %r2.  */
	      output_asm_insn ("copy %%r31,%%r2", xoperands);
	    }
	}

      /* If we had a jump in the call's delay slot, output it now.  */
      if (dbr_sequence_length () != 0
	  && !delay_insn_deleted)
	{
	  xoperands[0] = XEXP (PATTERN (NEXT_INSN (insn)), 1);
	  output_asm_insn ("b,n %0", xoperands);

	  /* Now delete the delay insn.  */
	  PUT_CODE (NEXT_INSN (insn), NOTE);
	  NOTE_LINE_NUMBER (NEXT_INSN (insn)) = NOTE_INSN_DELETED;
	  NOTE_SOURCE_FILE (NEXT_INSN (insn)) = 0;
	}
      return "";
    }

  /* This call has an unconditional jump in its delay slot and the
     call is known to reach its target or the beginning of the current
     subspace.  */

  /* Use the containing sequence insn's address.  */
  seq_insn = NEXT_INSN (PREV_INSN (XVECEXP (final_sequence, 0, 0)));

  distance = insn_addresses[INSN_UID (JUMP_LABEL (NEXT_INSN (insn)))]
	       - insn_addresses[INSN_UID (seq_insn)] - 8;

  /* If the branch was too far away, emit a normal call followed
     by a nop, followed by the unconditional branch.

     If the branch is close, then adjust %r2 from within the
     call's delay slot.  */

  xoperands[0] = call_dest;
  xoperands[1] = XEXP (PATTERN (NEXT_INSN (insn)), 1);
  if (! VAL_14_BITS_P (distance))
    output_asm_insn ("bl %0,%%r2\n\tnop\n\tbl,n %1,%%r0", xoperands);
  else
    {
      xoperands[3] = gen_label_rtx ();
      output_asm_insn ("\n\tbl %0,%%r2\n\tldo %1-%3(%%r2),%%r2", xoperands);
      ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L",
				 CODE_LABEL_NUMBER (xoperands[3]));
    }

  /* Delete the jump.  */
  PUT_CODE (NEXT_INSN (insn), NOTE);
  NOTE_LINE_NUMBER (NEXT_INSN (insn)) = NOTE_INSN_DELETED;
  NOTE_SOURCE_FILE (NEXT_INSN (insn)) = 0;
  return "";
}

/* In HPUX 8.0's shared library scheme, special relocations are needed
   for function labels if they might be passed to a function
   in a shared library (because shared libraries don't live in code
   space), and special magic is needed to construct their address.

   For reasons too disgusting to describe storage for the new name
   is allocated either on the saveable_obstack (released at function
   exit) or on the permanent_obstack for things that can never change
   (libcall names for example). */

void
hppa_encode_label (sym, permanent)
     rtx sym;
     int permanent;
{
  char *str = XSTR (sym, 0);
  int len = strlen (str);
  char *newstr;

  newstr = obstack_alloc ((permanent ? &permanent_obstack : saveable_obstack),
			  len + 2);

  if (str[0] == '*')
    *newstr++ = *str++;
  strcpy (newstr + 1, str);
  *newstr = '@';
  XSTR (sym,0) = newstr;
}

int
function_label_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return GET_CODE (op) == SYMBOL_REF && FUNCTION_NAME_P (XSTR (op, 0));
}

/* Returns 1 if OP is a function label involved in a simple addition
   with a constant.  Used to keep certain patterns from matching
   during instruction combination.  */
int
is_function_label_plus_const (op)
     rtx op;
{
  /* Strip off any CONST.  */
  if (GET_CODE (op) == CONST)
    op = XEXP (op, 0);

  return (GET_CODE (op) == PLUS
	  && function_label_operand (XEXP (op, 0), Pmode)
	  && GET_CODE (XEXP (op, 1)) == CONST_INT);
}

/* Returns 1 if the 6 operands specified in OPERANDS are suitable for
   use in fmpyadd instructions.  */
int
fmpyaddoperands (operands)
     rtx *operands;
{
  enum machine_mode mode = GET_MODE (operands[0]);

  /* Must be a floating point mode.  */
  if (mode != SFmode && mode != DFmode)
    return 0;

  /* All modes must be the same.  */
  if (! (mode == GET_MODE (operands[1])
	 && mode == GET_MODE (operands[2])
	 && mode == GET_MODE (operands[3])
	 && mode == GET_MODE (operands[4])
	 && mode == GET_MODE (operands[5])))
    return 0;

  /* All operands must be registers.  */
  if (! (GET_CODE (operands[1]) == REG
	 && GET_CODE (operands[2]) == REG
	 && GET_CODE (operands[3]) == REG
	 && GET_CODE (operands[4]) == REG
	 && GET_CODE (operands[5]) == REG))
    return 0;

  /* Only 2 real operands to the addition.  One of the input operands must
     be the same as the output operand.  */
  if (! rtx_equal_p (operands[3], operands[4])
      && ! rtx_equal_p (operands[3], operands[5]))
    return 0;

  /* Inout operand of add can not conflict with any operands from multiply.  */
  if (rtx_equal_p (operands[3], operands[0])
     || rtx_equal_p (operands[3], operands[1])
     || rtx_equal_p (operands[3], operands[2]))
    return 0;

  /* multiply can not feed into addition operands.  */
  if (rtx_equal_p (operands[4], operands[0])
      || rtx_equal_p (operands[5], operands[0]))
    return 0;

  /* SFmode limits the registers to the upper 32 of the 32bit FP regs.  */
  if (mode == SFmode
      && (REGNO (operands[0]) < 57
	  || REGNO (operands[1]) < 57
	  || REGNO (operands[2]) < 57
	  || REGNO (operands[3]) < 57
	  || REGNO (operands[4]) < 57
	  || REGNO (operands[5]) < 57))
    return 0;

  /* Passed.  Operands are suitable for fmpyadd.  */
  return 1;
}

/* Returns 1 if the 6 operands specified in OPERANDS are suitable for
   use in fmpysub instructions.  */
int
fmpysuboperands (operands)
     rtx *operands;
{
  enum machine_mode mode = GET_MODE (operands[0]);

  /* Must be a floating point mode.  */
  if (mode != SFmode && mode != DFmode)
    return 0;

  /* All modes must be the same.  */
  if (! (mode == GET_MODE (operands[1])
	 && mode == GET_MODE (operands[2])
	 && mode == GET_MODE (operands[3])
	 && mode == GET_MODE (operands[4])
	 && mode == GET_MODE (operands[5])))
    return 0;

  /* All operands must be registers.  */
  if (! (GET_CODE (operands[1]) == REG
	 && GET_CODE (operands[2]) == REG
	 && GET_CODE (operands[3]) == REG
	 && GET_CODE (operands[4]) == REG
	 && GET_CODE (operands[5]) == REG))
    return 0;

  /* Only 2 real operands to the subtraction.  Subtraction is not a commutative
     operation, so operands[4] must be the same as operand[3].  */
  if (! rtx_equal_p (operands[3], operands[4]))
    return 0;

  /* multiply can not feed into subtraction.  */
  if (rtx_equal_p (operands[5], operands[0]))
    return 0;

  /* Inout operand of sub can not conflict with any operands from multiply.  */
  if (rtx_equal_p (operands[3], operands[0])
     || rtx_equal_p (operands[3], operands[1])
     || rtx_equal_p (operands[3], operands[2]))
    return 0;

  /* SFmode limits the registers to the upper 32 of the 32bit FP regs.  */
  if (mode == SFmode
      && (REGNO (operands[0]) < 57
	  || REGNO (operands[1]) < 57
	  || REGNO (operands[2]) < 57
	  || REGNO (operands[3]) < 57
	  || REGNO (operands[4]) < 57
	  || REGNO (operands[5]) < 57))
    return 0;

  /* Passed.  Operands are suitable for fmpysub.  */
  return 1;
}

int
plus_xor_ior_operator (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == PLUS || GET_CODE (op) == XOR
	  || GET_CODE (op) == IOR);
}

/* Return 1 if the given constant is 2, 4, or 8.  These are the valid
   constants for shadd instructions.  */
int
shadd_constant_p (val)
     int val;
{
  if (val == 2 || val == 4 || val == 8)
    return 1;
  else
    return 0;
}

/* Return 1 if OP is a CONST_INT with the value 2, 4, or 8.  These are
   the valid constant for shadd instructions.  */
int
shadd_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT && shadd_constant_p (INTVAL (op)));
}

/* Return 1 if OP is valid as a base register in a reg + reg address.  */

int
basereg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  /* cse will create some unscaled indexed addresses, however; it
     generally isn't a win on the PA, so avoid creating unscaled
     indexed addresses until after cse is finished.  */
  if (!cse_not_expected)
    return 0;

  /* Once reload has started everything is considered valid.  Reload should
     only create indexed addresses using the stack/frame pointer, and any
     others were checked for validity when created by the combine pass. 

     Also allow any register when TARGET_NO_SPACE_REGS is in effect since
     we don't have to worry about the braindamaged implicit space register
     selection using the basereg only (rather than effective address)
     screwing us over.  */
  if (TARGET_NO_SPACE_REGS || reload_in_progress || reload_completed)
    return (GET_CODE (op) == REG);

  /* Stack is always OK for indexing.  */
  if (op == stack_pointer_rtx)
    return 1;

  /* While it's always safe to index off the frame pointer, it's not
     always profitable, particularly when the frame pointer is being
     eliminated.  */
  if (! flag_omit_frame_pointer && op == frame_pointer_rtx)
    return 1;

  /* The only other valid OPs are pseudo registers with
     REGNO_POINTER_FLAG set.  */
  if (GET_CODE (op) != REG
      || REGNO (op) < FIRST_PSEUDO_REGISTER
      || ! register_operand (op, mode))
    return 0;
    
  return REGNO_POINTER_FLAG (REGNO (op));
}

/* Return 1 if this operand is anything other than a hard register.  */

int
non_hard_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ! (GET_CODE (op) == REG && REGNO (op) < FIRST_PSEUDO_REGISTER);
}

/* Return 1 if INSN branches forward.  Should be using insn_addresses
   to avoid walking through all the insns... */
int
forward_branch_p (insn)
     rtx insn;
{
  rtx label = JUMP_LABEL (insn);

  while (insn)
    {
      if (insn == label)
	break;
      else
	insn = NEXT_INSN (insn);
    }

  return (insn == label);
}

/* Return 1 if OP is an equality comparison, else return 0.  */
int
eq_neq_comparison_operator (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == EQ || GET_CODE (op) == NE);
}

/* Return 1 if OP is an operator suitable for use in a movb instruction.  */
int
movb_comparison_operator (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == EQ || GET_CODE (op) == NE
	  || GET_CODE (op) == LT || GET_CODE (op) == GE);
}

/* Return 1 if INSN is in the delay slot of a call instruction.  */
int
jump_in_call_delay (insn)
     rtx insn;
{

  if (GET_CODE (insn) != JUMP_INSN)
    return 0;

  if (PREV_INSN (insn)
      && PREV_INSN (PREV_INSN (insn))
      && GET_CODE (next_active_insn (PREV_INSN (PREV_INSN (insn)))) == INSN)
    {
      rtx test_insn = next_active_insn (PREV_INSN (PREV_INSN (insn)));

      return (GET_CODE (PATTERN (test_insn)) == SEQUENCE
	      && XVECEXP (PATTERN (test_insn), 0, 1) == insn);

    }
  else
    return 0;
}

/* Output an unconditional move and branch insn.  */

char *
output_parallel_movb (operands, length)
     rtx *operands;
     int length;
{
  /* These are the cases in which we win.  */
  if (length == 4)
    return "mov%I1b,tr %1,%0,%2";

  /* None of these cases wins, but they don't lose either.  */
  if (dbr_sequence_length () == 0)
    {
      /* Nothing in the delay slot, fake it by putting the combined
	 insn (the copy or add) in the delay slot of a bl.  */
      if (GET_CODE (operands[1]) == CONST_INT)
	return "bl %2,0\n\tldi %1,%0";
      else
	return "bl %2,0\n\tcopy %1,%0";
    }
  else
    {
      /* Something in the delay slot, but we've got a long branch.  */
      if (GET_CODE (operands[1]) == CONST_INT)
	return "ldi %1,%0\n\tbl %2,0";
      else
	return "copy %1,%0\n\tbl %2,0";
    }
}

/* Output an unconditional add and branch insn.  */

char *
output_parallel_addb (operands, length)
     rtx *operands;
     int length;
{
  /* To make life easy we want operand0 to be the shared input/output
     operand and operand1 to be the readonly operand.  */
  if (operands[0] == operands[1])
    operands[1] = operands[2];

  /* These are the cases in which we win.  */
  if (length == 4)
    return "add%I1b,tr %1,%0,%3";

  /* None of these cases win, but they don't lose either.  */
  if (dbr_sequence_length () == 0)
    {
      /* Nothing in the delay slot, fake it by putting the combined
	 insn (the copy or add) in the delay slot of a bl.  */
      return "bl %3,0\n\tadd%I1 %1,%0,%0";
    }
  else
    {
      /* Something in the delay slot, but we've got a long branch.  */
      return "add%I1 %1,%0,%0\n\tbl %3,0";
    }
}

/* Return nonzero if INSN (a jump insn) immediately follows a call to
   a named function.  This is used to discourage creating parallel movb/addb
   insns since a jump which immediately follows a call can execute in the
   delay slot of the call.

   It is also used to avoid filling the delay slot of a jump which
   immediately follows a call since the jump can usually be eliminated
   completely by modifying RP in the delay slot of the call.  */
   
following_call (insn)
     rtx insn;
{
  /* Find the previous real insn, skipping NOTEs.  */
  insn = PREV_INSN (insn);
  while (insn && GET_CODE (insn) == NOTE)
    insn = PREV_INSN (insn);

  /* Check for CALL_INSNs and millicode calls.  */
  if (insn
      && ((GET_CODE (insn) == CALL_INSN
	   && get_attr_type (insn) != TYPE_DYNCALL)
	  || (GET_CODE (insn) == INSN
	      && GET_CODE (PATTERN (insn)) != SEQUENCE
	      && GET_CODE (PATTERN (insn)) != USE
	      && GET_CODE (PATTERN (insn)) != CLOBBER
	      && get_attr_type (insn) == TYPE_MILLI)))
    return 1;

  return 0;
}

/* Restore any INSN_CODEs for insns with unscaled indexed addresses since
   the INSN_CODE might be clobberd by rerecognition triggered by reorg.  */

static void
restore_unscaled_index_insn_codes (insns)
     rtx insns;
{
  rtx insn;

  for (insn = insns; insn; insn = NEXT_INSN (insn))
    {
      if (INSN_UID (insn) < max_unscaled_index_insn_codes_uid
	  && unscaled_index_insn_codes[INSN_UID (insn)] != -1)
	INSN_CODE (insn) = unscaled_index_insn_codes[INSN_UID (insn)];
    }
}

/* Severe braindamage:

   On the PA, address computations within MEM expressions are not
   commutative because of the implicit space register selection
   from the base register (instead of the entire effective address).

   Because of this mis-feature we have to know which register in a reg+reg
   address is the base and which is the index.

   Before reload, the base can be identified by REGNO_POINTER_FLAG.  We use
   this to force base + index addresses to match a different insn than
   index + base addresses.

   We assume that no pass during or after reload creates new unscaled indexed
   addresses, so any unscaled indexed address we find after reload must have
   at one time been recognized a base + index or index + base and we accept
   any register as a base register.

   This scheme assumes that no pass during/after reload will rerecognize an
   insn with an unscaled indexed address.  This failed due to a reorg call
   to rerecognize certain insns.

   So, we record if an insn uses an unscaled indexed address and which
   register is the base (via recording of the INSN_CODE for such insns).

   Just before we output code for the function, we make sure all the insns
   using unscaled indexed addresses have the same INSN_CODE as they did
   immediately before delay slot scheduling.

   This is extremely gross.  Long term, I'd like to be able to look at
   REG_POINTER_FLAG to handle these kinds of problems.  */
 
static void
record_unscaled_index_insn_codes (insns)
     rtx insns;
{
  rtx insn;

  max_unscaled_index_insn_codes_uid = get_max_uid ();
  unscaled_index_insn_codes
    = (int *)xmalloc (max_unscaled_index_insn_codes_uid * sizeof (int));
  memset (unscaled_index_insn_codes, -1,
	  max_unscaled_index_insn_codes_uid * sizeof (int));

  for (insn = insns; insn; insn = NEXT_INSN (insn))
    {
      rtx set = single_set (insn);
      rtx mem = NULL_RTX;

      /* Ignore anything that isn't a normal SET.  */
      if (set == NULL_RTX)
	continue;

      /* No insns can have more than one MEM.  */
      if (GET_CODE (SET_SRC (set)) == MEM)
	mem = SET_SRC (set);

      if (GET_CODE (SET_DEST (set)) == MEM)
	mem = SET_DEST (set);
	
      /* If neither operand is a mem, then there's nothing to do.  */
      if (mem == NULL_RTX)
	continue;

      if (GET_CODE (XEXP (mem, 0)) != PLUS)
	continue;

      /* If both are REGs (or SUBREGs), then record the insn code for
	 this insn.  */
      if (REG_P (XEXP (XEXP (mem, 0), 0)) && REG_P (XEXP (XEXP (mem, 0), 1)))
        unscaled_index_insn_codes[INSN_UID (insn)] = INSN_CODE (insn);
    }
}

/* We use this hook to perform a PA specific optimization which is difficult
   to do in earlier passes.

   We want the delay slots of branches within jump tables to be filled.
   None of the compiler passes at the moment even has the notion that a
   PA jump table doesn't contain addresses, but instead contains actual
   instructions!

   Because we actually jump into the table, the addresses of each entry
   must stay constant in relation to the beginning of the table (which
   itself must stay constant relative to the instruction to jump into
   it).  I don't believe we can guarantee earlier passes of the compiler
   will adhere to those rules.

   So, late in the compilation process we find all the jump tables, and
   expand them into real code -- eg each entry in the jump table vector
   will get an appropriate label followed by a jump to the final target.

   Reorg and the final jump pass can then optimize these branches and
   fill their delay slots.  We end up with smaller, more efficient code.

   The jump instructions within the table are special; we must be able 
   to identify them during assembly output (if the jumps don't get filled
   we need to emit a nop rather than nullifying the delay slot)).  We
   identify jumps in switch tables by marking the SET with DImode. 

   We also surround the jump table itself with BEGIN_BRTAB and END_BRTAB
   insns.  This serves two purposes, first it prevents jump.c from
   noticing that the last N entries in the table jump to the instruction
   immediately after the table and deleting the jumps.  Second, those
   insns mark where we should emit .begin_brtab and .end_brtab directives
   when using GAS (allows for better link time optimizations).  */

pa_reorg (insns)
     rtx insns;
{
  rtx insn;

  /* Keep track of which insns have unscaled indexed addresses, and which
     register is the base address in such insns.  */
  record_unscaled_index_insn_codes (insns);

  remove_useless_addtr_insns (insns, 1);

  pa_combine_instructions (get_insns ());

  /* This is fairly cheap, so always run it if optimizing.  */
  if (optimize > 0 && !TARGET_BIG_SWITCH)
    {
      /* Find and explode all ADDR_VEC or ADDR_DIFF_VEC insns.  */
      insns = get_insns ();
      for (insn = insns; insn; insn = NEXT_INSN (insn))
	{
	  rtx pattern, tmp, location;
	  unsigned int length, i;

	  /* Find an ADDR_VEC or ADDR_DIFF_VEC insn to explode.  */
	  if (GET_CODE (insn) != JUMP_INSN
	      || (GET_CODE (PATTERN (insn)) != ADDR_VEC
		  && GET_CODE (PATTERN (insn)) != ADDR_DIFF_VEC))
	    continue;

	  /* Emit marker for the beginning of the branch table.  */
	  emit_insn_before (gen_begin_brtab (), insn);

	  pattern = PATTERN (insn);
	  location = PREV_INSN (insn);
          length = XVECLEN (pattern, GET_CODE (pattern) == ADDR_DIFF_VEC);

	  for (i = 0; i < length; i++)
	    {
	      /* Emit a label before each jump to keep jump.c from
		 removing this code.  */
	      tmp = gen_label_rtx ();
	      LABEL_NUSES (tmp) = 1;
	      emit_label_after (tmp, location);
	      location = NEXT_INSN (location);

	      if (GET_CODE (pattern) == ADDR_VEC)
		{
		  /* Emit the jump itself.  */
		  tmp = gen_switch_jump (XEXP (XVECEXP (pattern, 0, i), 0));
		  tmp = emit_jump_insn_after (tmp, location);
		  JUMP_LABEL (tmp) = XEXP (XVECEXP (pattern, 0, i), 0);
		  LABEL_NUSES (JUMP_LABEL (tmp))++;
		  location = NEXT_INSN (location);
		}
	      else
		{
		  /* Emit the jump itself.  */
		  tmp = gen_switch_jump (XEXP (XVECEXP (pattern, 1, i), 0));
		  tmp = emit_jump_insn_after (tmp, location);
		  JUMP_LABEL (tmp) = XEXP (XVECEXP (pattern, 1, i), 0);
		  LABEL_NUSES (JUMP_LABEL (tmp))++;
		  location = NEXT_INSN (location);
		}

	      /* Emit a BARRIER after the jump.  */
	      emit_barrier_after (location);
	      location = NEXT_INSN (location);
	    }

	  /* Emit marker for the end of the branch table.  */
	  emit_insn_before (gen_end_brtab (), location);
	  location = NEXT_INSN (location);
	  emit_barrier_after (location);

	  /* Delete the ADDR_VEC or ADDR_DIFF_VEC.  */
	  delete_insn (insn);
	}
    }
  else
    {
      /* Sill need an end_brtab insn.  */
      insns = get_insns ();
      for (insn = insns; insn; insn = NEXT_INSN (insn))
	{
	  /* Find an ADDR_VEC insn.  */
	  if (GET_CODE (insn) != JUMP_INSN
	      || (GET_CODE (PATTERN (insn)) != ADDR_VEC
		  && GET_CODE (PATTERN (insn)) != ADDR_DIFF_VEC))
	    continue;

	  /* Now generate markers for the beginning and end of the
	     branch table.  */
	  emit_insn_before (gen_begin_brtab (), insn);
	  emit_insn_after (gen_end_brtab (), insn);
	}
    }
}

/* The PA has a number of odd instructions which can perform multiple
   tasks at once.  On first generation PA machines (PA1.0 and PA1.1)
   it may be profitable to combine two instructions into one instruction
   with two outputs.  It's not profitable PA2.0 machines because the
   two outputs would take two slots in the reorder buffers.

   This routine finds instructions which can be combined and combines
   them.  We only support some of the potential combinations, and we
   only try common ways to find suitable instructions.

      * addb can add two registers or a register and a small integer
      and jump to a nearby (+-8k) location.  Normally the jump to the
      nearby location is conditional on the result of the add, but by
      using the "true" condition we can make the jump unconditional.
      Thus addb can perform two independent operations in one insn.

      * movb is similar to addb in that it can perform a reg->reg
      or small immediate->reg copy and jump to a nearby (+-8k location).

      * fmpyadd and fmpysub can perform a FP multiply and either an
      FP add or FP sub if the operands of the multiply and add/sub are
      independent (there are other minor restrictions).  Note both
      the fmpy and fadd/fsub can in theory move to better spots according
      to data dependencies, but for now we require the fmpy stay at a
      fixed location.

      * Many of the memory operations can perform pre & post updates
      of index registers.  GCC's pre/post increment/decrement addressing
      is far too simple to take advantage of all the possibilities.  This
      pass may not be suitable since those insns may not be independent.

      * comclr can compare two ints or an int and a register, nullify
      the following instruction and zero some other register.  This
      is more difficult to use as it's harder to find an insn which
      will generate a comclr than finding something like an unconditional
      branch.  (conditional moves & long branches create comclr insns).

      * Most arithmetic operations can conditionally skip the next
      instruction.  They can be viewed as "perform this operation
      and conditionally jump to this nearby location" (where nearby
      is an insns away).  These are difficult to use due to the
      branch length restrictions.  */

pa_combine_instructions (insns)
     rtx insns;
{
  rtx anchor, new;

  /* This can get expensive since the basic algorithm is on the
     order of O(n^2) (or worse).  Only do it for -O2 or higher
     levels of optimization.  */
  if (optimize < 2)
    return;

  /* Walk down the list of insns looking for "anchor" insns which
     may be combined with "floating" insns.  As the name implies,
     "anchor" instructions don't move, while "floating" insns may
     move around.  */
  new = gen_rtx (PARALLEL, VOIDmode, gen_rtvec (2, NULL_RTX, NULL_RTX));
  new = make_insn_raw (new);

  for (anchor = get_insns (); anchor; anchor = NEXT_INSN (anchor))
    {
      enum attr_pa_combine_type anchor_attr;
      enum attr_pa_combine_type floater_attr;

      /* We only care about INSNs, JUMP_INSNs, and CALL_INSNs.
	 Also ignore any special USE insns.  */
      if (GET_CODE (anchor) != INSN
	  && GET_CODE (anchor) != JUMP_INSN
	  && GET_CODE (anchor) != CALL_INSN
	  || GET_CODE (PATTERN (anchor)) == USE
	  || GET_CODE (PATTERN (anchor)) == CLOBBER
	  || GET_CODE (PATTERN (anchor)) == ADDR_VEC
	  || GET_CODE (PATTERN (anchor)) == ADDR_DIFF_VEC)
	continue;

      anchor_attr = get_attr_pa_combine_type (anchor);
      /* See if anchor is an insn suitable for combination.  */
      if (anchor_attr == PA_COMBINE_TYPE_FMPY
	  || anchor_attr == PA_COMBINE_TYPE_FADDSUB
	  || (anchor_attr == PA_COMBINE_TYPE_UNCOND_BRANCH
	      && ! forward_branch_p (anchor)))
	{
	  rtx floater;

	  for (floater = PREV_INSN (anchor);
	       floater;
	       floater = PREV_INSN (floater))
	    {
	      if (GET_CODE (floater) == NOTE
		  || (GET_CODE (floater) == INSN
		      && (GET_CODE (PATTERN (floater)) == USE
			  || GET_CODE (PATTERN (floater)) == CLOBBER)))
		continue;

	      /* Anything except a regular INSN will stop our search.  */
	      if (GET_CODE (floater) != INSN
		  || GET_CODE (PATTERN (floater)) == ADDR_VEC
		  || GET_CODE (PATTERN (floater)) == ADDR_DIFF_VEC)
		{
		  floater = NULL_RTX;
		  break;
		}

	      /* See if FLOATER is suitable for combination with the
		 anchor.  */
	      floater_attr = get_attr_pa_combine_type (floater);
	      if ((anchor_attr == PA_COMBINE_TYPE_FMPY
		   && floater_attr == PA_COMBINE_TYPE_FADDSUB)
		  || (anchor_attr == PA_COMBINE_TYPE_FADDSUB
		      && floater_attr == PA_COMBINE_TYPE_FMPY))
		{
		  /* If ANCHOR and FLOATER can be combined, then we're
		     done with this pass.  */
		  if (pa_can_combine_p (new, anchor, floater, 0,
					SET_DEST (PATTERN (floater)),
					XEXP (SET_SRC (PATTERN (floater)), 0),
					XEXP (SET_SRC (PATTERN (floater)), 1)))
		    break;
		}

	      else if (anchor_attr == PA_COMBINE_TYPE_UNCOND_BRANCH
		       && floater_attr == PA_COMBINE_TYPE_ADDMOVE)
		{
		  if (GET_CODE (SET_SRC (PATTERN (floater))) == PLUS)
		    {
		      if (pa_can_combine_p (new, anchor, floater, 0,
					    SET_DEST (PATTERN (floater)),
					XEXP (SET_SRC (PATTERN (floater)), 0),
					XEXP (SET_SRC (PATTERN (floater)), 1)))
			break;
		    }
		  else
		    {
		      if (pa_can_combine_p (new, anchor, floater, 0,
					    SET_DEST (PATTERN (floater)),
					    SET_SRC (PATTERN (floater)),
					    SET_SRC (PATTERN (floater))))
			break;
		    }
		}
	    }

	  /* If we didn't find anything on the backwards scan try forwards.  */
	  if (!floater
	      && (anchor_attr == PA_COMBINE_TYPE_FMPY
		  || anchor_attr == PA_COMBINE_TYPE_FADDSUB))
	    {
	      for (floater = anchor; floater; floater = NEXT_INSN (floater))
		{
		  if (GET_CODE (floater) == NOTE
		      || (GET_CODE (floater) == INSN
			  && (GET_CODE (PATTERN (floater)) == USE
			      || GET_CODE (PATTERN (floater)) == CLOBBER)))
			
		    continue;

		  /* Anything except a regular INSN will stop our search.  */
		  if (GET_CODE (floater) != INSN
		      || GET_CODE (PATTERN (floater)) == ADDR_VEC
		      || GET_CODE (PATTERN (floater)) == ADDR_DIFF_VEC)
		    {
		      floater = NULL_RTX;
		      break;
		    }

		  /* See if FLOATER is suitable for combination with the
		     anchor.  */
		  floater_attr = get_attr_pa_combine_type (floater);
		  if ((anchor_attr == PA_COMBINE_TYPE_FMPY
		       && floater_attr == PA_COMBINE_TYPE_FADDSUB)
		      || (anchor_attr == PA_COMBINE_TYPE_FADDSUB
			  && floater_attr == PA_COMBINE_TYPE_FMPY))
		    {
		      /* If ANCHOR and FLOATER can be combined, then we're
			 done with this pass.  */
		      if (pa_can_combine_p (new, anchor, floater, 1,
					    SET_DEST (PATTERN (floater)),
					    XEXP (SET_SRC (PATTERN(floater)),0),
					    XEXP(SET_SRC(PATTERN(floater)),1)))
			break;
		    }
		}
	    }

	  /* FLOATER will be nonzero if we found a suitable floating
	     insn for combination with ANCHOR.  */
	  if (floater
	      && (anchor_attr == PA_COMBINE_TYPE_FADDSUB
		  || anchor_attr == PA_COMBINE_TYPE_FMPY))
	    {
	      /* Emit the new instruction and delete the old anchor.  */
	      emit_insn_before (gen_rtx (PARALLEL, VOIDmode,
					 gen_rtvec (2, PATTERN (anchor),
						    PATTERN (floater))),
				anchor);
	      PUT_CODE (anchor, NOTE);
	      NOTE_LINE_NUMBER (anchor) = NOTE_INSN_DELETED;
	      NOTE_SOURCE_FILE (anchor) = 0;

	      /* Emit a special USE insn for FLOATER, then delete
		 the floating insn.  */
	      emit_insn_before (gen_rtx (USE, VOIDmode, floater), floater);
	      delete_insn (floater);

	      continue;
	    }
	  else if (floater
		   && anchor_attr == PA_COMBINE_TYPE_UNCOND_BRANCH)
	    {
	      rtx temp;
	      /* Emit the new_jump instruction and delete the old anchor.  */
	      temp = emit_jump_insn_before (gen_rtx (PARALLEL, VOIDmode,
					      gen_rtvec (2, PATTERN (anchor),
							 PATTERN (floater))),
				anchor);
	      JUMP_LABEL (temp) = JUMP_LABEL (anchor);
	      PUT_CODE (anchor, NOTE);
	      NOTE_LINE_NUMBER (anchor) = NOTE_INSN_DELETED;
	      NOTE_SOURCE_FILE (anchor) = 0;

	      /* Emit a special USE insn for FLOATER, then delete
		 the floating insn.  */
	      emit_insn_before (gen_rtx (USE, VOIDmode, floater), floater);
	      delete_insn (floater);
	      continue;
	    }
	}
    }
}

int
pa_can_combine_p (new, anchor, floater, reversed, dest, src1, src2)
     rtx new, anchor, floater;
     int reversed;
     rtx dest, src1, src2;
{
  int insn_code_number;
  rtx start, end;

  /* Create a PARALLEL with the patterns of ANCHOR and
     FLOATER, try to recognize it, then test constraints
     for the resulting pattern.

     If the pattern doesn't match or the constraints
     aren't met keep searching for a suitable floater
     insn.  */
  XVECEXP (PATTERN (new), 0, 0) = PATTERN (anchor);
  XVECEXP (PATTERN (new), 0, 1) = PATTERN (floater);
  INSN_CODE (new) = -1;
  insn_code_number = recog_memoized (new);
  if (insn_code_number < 0
      || !constrain_operands (insn_code_number, 1))
    return 0;

  if (reversed)
    {
      start = anchor;
      end = floater;
    }
  else
    {
      start = floater;
      end = anchor;
    }

  /* There's up to three operands to consider.  One
     output and two inputs.

     The output must not be used between FLOATER & ANCHOR
     exclusive.  The inputs must not be set between
     FLOATER and ANCHOR exclusive.  */

  if (reg_used_between_p (dest, start, end))
    return 0;

  if (reg_set_between_p (src1, start, end))
    return 0;

  if (reg_set_between_p (src2, start, end))
    return 0;

  /* If we get here, then everything is good.  */
  return 1;
}
