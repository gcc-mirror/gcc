/* Subroutines used for code generation on Ubicom IP2022
   Communications Controller.
   Copyright (C) 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
   Contributed by Red Hat, Inc and Ubicom, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"
#include "insn-addr.h"
#include "flags.h"
#include "reload.h"
#include "tree.h"
#include "expr.h"
#include "optabs.h"
#include "toplev.h"
#include "obstack.h"
#include "function.h"
#include "recog.h"
#include "tm_p.h"
#include "target.h"
#include "target-def.h"
#include "basic-block.h"

/* There are problems with 'frame_pointer_needed'.  If we force it
   on, we either end up not eliminating uses of FP, which results in
   SPILL register failures or we may end up with calculation errors in
   the stack offsets.  Isolate the decision process into a simple macro.  */
#define CHAIN_FRAMES (frame_pointer_needed || FRAME_POINTER_REQUIRED)

static int ip2k_naked_function_p (tree);
#ifdef IP2K_MD_REORG_PASS
static void mdr_resequence_xy_yx (rtx);
static void mdr_pres_replace_and_recurse (rtx, rtx, rtx);
static void mdr_propagate_reg_equivs_sequence (rtx, rtx, rtx);
static void mdr_propagate_reg_equivs (rtx);
static int track_dp_reload (rtx , rtx *, int , int);
static void mdr_try_dp_reload_elim (rtx);
static void mdr_try_move_dp_reload (rtx);
static void mdr_try_move_pushes (rtx);
static void mdr_try_propagate_clr_sequence (rtx, unsigned int);
static void mdr_try_propagate_clr (rtx);
static void mdr_try_propagate_move_sequence (rtx, rtx, rtx);
static void mdr_try_propagate_move (rtx);
static void mdr_try_remove_redundant_insns (rtx);
static int track_w_reload (rtx, rtx *, int , int);
static void mdr_try_wreg_elim (rtx);
#endif /* IP2K_MD_REORG_PASS */
static void ip2k_reorg (void);
static int ip2k_check_can_adjust_stack_ref (rtx, int);
static void ip2k_adjust_stack_ref (rtx *, int);
static int ip2k_xexp_not_uses_reg_for_mem (rtx, unsigned int);
static tree ip2k_handle_progmem_attribute (tree *, tree, tree, int, bool *);
static tree ip2k_handle_fndecl_attribute (tree *, tree, tree, int, bool *);
static bool ip2k_rtx_costs (rtx, int, int, int *);
static int ip2k_address_cost (rtx);
static void ip2k_init_libfuncs (void);

const struct attribute_spec ip2k_attribute_table[];


/* Initialize the GCC target structure.  */
#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.word\t"

#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE function_prologue

#undef TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE function_epilogue

#undef TARGET_ASM_UNIQUE_SECTION
#define TARGET_ASM_UNIQUE_SECTION unique_section

#undef TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE ip2k_attribute_table

#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS ip2k_rtx_costs
#undef TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST ip2k_address_cost

#undef TARGET_MACHINE_DEPENDENT_REORG
#define TARGET_MACHINE_DEPENDENT_REORG ip2k_reorg

#undef TARGET_INIT_LIBFUNCS
#define TARGET_INIT_LIBFUNCS ip2k_init_libfuncs

struct gcc_target targetm = TARGET_INITIALIZER;

/* Prologue/Epilogue size in words.  */
static int prologue_size;
static int epilogue_size;

/* compare and test instructions for the IP2K are materialized by
   the conditional branch that uses them.  This is because conditional
   branches are skips over unconditional branches.  */
rtx ip2k_compare_operands[3];	/* Additional operands for condition code.  */
int ip2k_test_flag;		/* Indicates Z, WREG contain condition code
				   information.  */

/* Some ip2k patterns push a byte onto the stack and then access
   SP-relative addresses. Since reload doesn't know about these
   pushes, we must track them internally with a %< (push) or %> (pop)
   indicator.  */
static int ip2k_stack_delta;

/* Track if or how far our ip2k reorganization pass has run.  */
int ip2k_reorg_in_progress = 0;
int ip2k_reorg_completed = 0;
int ip2k_reorg_split_dimode = 0;
int ip2k_reorg_split_simode = 0;
int ip2k_reorg_split_himode = 0;
int ip2k_reorg_split_qimode = 0;
int ip2k_reorg_merge_qimode = 0;

/* Set up local allocation order.  */

void
ip2k_init_local_alloc (int *rao)
{
  static const int alloc_order[] = REG_ALLOC_ORDER;

  memcpy (rao, alloc_order, sizeof (alloc_order));
}

/* Returns the number of bytes of arguments automatically
   popped when returning from a subroutine call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack.  */

int
ip2k_return_pops_args (tree fundecl ATTRIBUTE_UNUSED, tree funtype, int size)
{
  if (TREE_CODE (funtype) == IDENTIFIER_NODE)
    return size;

  if (TYPE_ARG_TYPES (funtype) == NULL_TREE
      || (TREE_VALUE (tree_last (TYPE_ARG_TYPES (funtype))) == void_type_node))
    return size;

  return 0;
}

/* Return nonzero if FUNC is a naked function.  */

static int
ip2k_naked_function_p (tree func)
{
  tree a;

  if (TREE_CODE (func) != FUNCTION_DECL)
    abort ();
  
  a = lookup_attribute ("naked", DECL_ATTRIBUTES (func));
  return a != NULL_TREE;
}

/* Output function prologue.  */
void
function_prologue (FILE *file, HOST_WIDE_INT size)
{
  int leaf_func_p;
  int main_p;
  int reg;
  rtx operands[2];

  prologue_size = epilogue_size = 0;

  if (ip2k_naked_function_p (current_function_decl))
    {
      fprintf (file, "/* prologue: naked */\n");
      return;
    }

  leaf_func_p = leaf_function_p ();
  main_p = MAIN_NAME_P (DECL_NAME (current_function_decl));

  /* For now, we compute all these facts about the function, but don't
     take any action based on the information.  */

  prologue_size = 0;
  fprintf (file, "/* prologue: frame size=" HOST_WIDE_INT_PRINT_DEC " */\n",
	   size);
  
  /* Unless we're a leaf we need to save the return PC.  */

  if (! leaf_func_p)
    {
      OUT_AS1 (push, calll);
      OUT_AS1 (push, callh);
      prologue_size += 4;
    }
  
  /* We need to save the old FP and set the new FP pointing at the
     stack location where the old one is saved.  Note that because of
     post-decrement addressing, the SP is off-by-one after the
     push, so we harvest the SP address BEFORE we push the MSBs of
     the FP.  */
  if (CHAIN_FRAMES)
    {
      OUT_AS1 (push, REG_FP+1);	/* Save old LSBs.  */
      OUT_AS2 (mov, w, spl);
      OUT_AS2 (mov, REG_FP+1, w); /* SPL -> FPL  */

      OUT_AS2 (mov, w, sph);	/* Freeze SP MSBs  */
      OUT_AS1 (push, REG_FP);	/* Save old MSBs  */
      OUT_AS2 (mov, REG_FP, w);	/* SPH -> FPH  */
      prologue_size += 12;
    }

  for (reg = (CHAIN_FRAMES) ? (REG_FP - 1) : (REG_FP + 1);
       reg > 0; --reg)
    {
      if (regs_ever_live[reg] && ! call_used_regs[reg])
	{
	  fprintf (file, "\t" AS1 (push,%s) "\n", reg_names[reg]);
	  prologue_size += 2;
	}
    }

  if (size)
    {
      operands[0] = GEN_INT (size);

      switch (size & 0xff)
	{
	case 0:
	  break;
	case 1: 
	  OUT_AS1 (dec, spl);
	  prologue_size += 2;
	  break;
	default:
	  OUT_AS2 (mov, w, %L0);
	  OUT_AS2 (sub, spl, w);
	  prologue_size += 4;
	}

      switch (size & 0xff00)
	{
	case 0:
	  break;
	case 0x100:
	  OUT_AS1 (dec, sph);
	  prologue_size += 2;
	  break;
	default:
	  if ((size & 0xff) != ((size >> 8) & 0xff))
	    OUT_AS2 (mov, w, %H0); /* Otherwise W has value we want.  */
	  OUT_AS2 (sub, sph, w);
	  prologue_size += 4;
	}
    }

/* XXX - change this to use the carry-propagating subtract trick.  */
  if (flag_stack_check)
    {
      OUT_AS2 (mov, w, sph);
      OUT_AS2 (cmp, w, #%%hi8data(_end));
      OUT_AS1 (sc, );			/* C == 0 -> hi8(edata) < sph  */
      OUT_AS1 (page, 1f);
      OUT_AS1 (jmp, 1f);
      OUT_AS1 (sz, );			/* Z == 1 -> look at low byte  */
      OUT_AS1 (page,0f); 
      OUT_AS1 (jmp,0f);			/* sp < edata, so raise stack fault  */
      OUT_AS2 (mov, w, spl);
      OUT_AS2 (cmp, w, #%%lo8data(_end));
      OUT_AS1 (sc,);			/* C==1 ->  lo8(edata) >= spl  */
      OUT_AS1 (page,1f);
      OUT_AS1 (jmp,1f);
      OUT_AS1 (0:,);
      output_asm_insn ("push\t$ff", operands);
      OUT_AS1 (system,);
      OUT_AS1 (1:, );
      prologue_size += 30;
    }
}

/* Output function epilogue.  */
void
function_epilogue (FILE *file, HOST_WIDE_INT size)
{
  int leaf_func_p;
  int reg,savelimit;
  rtx operands[2];		/* Dummy used by OUT_ASn  */
  int args_locals_size = current_function_args_size;
  int saved_regs_p = 0;
  int need_ret = 1;

  /* Use this opportunity to reset the reorg flags!  */
  ip2k_reorg_in_progress = 0;
  ip2k_reorg_completed = 0;
  ip2k_reorg_split_dimode = 0;
  ip2k_reorg_split_simode = 0;
  ip2k_reorg_split_himode = 0;
  ip2k_reorg_split_qimode = 0;
  ip2k_reorg_merge_qimode = 0;

  if (ip2k_naked_function_p (current_function_decl))
    {
      fprintf (file, "/* epilogue: naked */\n");
      return;
    }

  leaf_func_p = leaf_function_p ();
  epilogue_size = 0;
  fprintf (file, "/* epilogue: frame size=" HOST_WIDE_INT_PRINT_DEC " */\n",
	   size);

  savelimit = (CHAIN_FRAMES) ? REG_FP : (REG_FP + 2);
  for (reg = 0; reg < savelimit; reg++)
    if (regs_ever_live[reg] && ! call_used_regs[reg])
      {
	saved_regs_p = 1;
	break;
      }

  if (size)
    {
      if (leaf_func_p && !CHAIN_FRAMES && !saved_regs_p
	  && current_function_pops_args)
	args_locals_size = current_function_args_size + size;
      else
	{
	  operands[0] = GEN_INT (size);

	  switch (size & 0xff)
	    {
	    default:
	      OUT_AS2 (mov, w, %L0);
	      OUT_AS2 (add, spl, w);
	      epilogue_size += 4;
	      /* fall-through  */
	    case 0:
	      break;
	    case 1:
	      OUT_AS1 (inc, spl);
	      epilogue_size += 2;
	    }

	  switch (size & 0xff00)
	    {
	    default:
	      if ((size & 0xff) != ((size >> 8) & 0xff))
		OUT_AS2 (mov, w, %H0);
	      OUT_AS2 (add, sph, w);
	      epilogue_size += 4;
	      /* fall-through  */
	    case 0:
	      break;
	    case 0x100:
	      OUT_AS1 (inc, sph);
	      epilogue_size += 2;
	    }
	}
    }

  for (reg = 0; reg < savelimit; reg++)
    {
      if (regs_ever_live[reg] && ! call_used_regs[reg])
	{
	  fprintf (file, "\t" AS1 (pop,%s) "\n", reg_names[reg]);
	  prologue_size += 2;
	}
    }

  if (CHAIN_FRAMES
      && ! (current_function_pops_args
	    && current_function_args_size >= 2
	    && current_function_args_size < 0x100))
    {
      OUT_AS1 (pop, REG_FP);
      OUT_AS1 (pop, REG_FP+1);
      epilogue_size += 4;
    }

  if (! leaf_func_p)
    {
      if (current_function_pops_args
          && current_function_args_size >= 2
          && current_function_args_size < 0x100)
        {
          if (current_function_args_size == 2)
	    {
	      if (CHAIN_FRAMES)
	        {
	          OUT_AS1 (page, __fp_pop2_args_ret);
	          OUT_AS1 (jmp, __fp_pop2_args_ret);
	        }
	      else
	        {
	          OUT_AS1 (page, __pop2_args_ret);
	          OUT_AS1 (jmp, __pop2_args_ret);
	        }
	      epilogue_size += 4;
	    }
          else
	    {
	      operands[0] = GEN_INT (current_function_args_size);
	      OUT_AS2 (mov, w, %L0);
	      if (CHAIN_FRAMES)
	        {
	          OUT_AS1 (page, __fp_pop_args_ret);
	          OUT_AS1 (jmp, __fp_pop_args_ret);
	        }
	      else
	        {
	          OUT_AS1 (page, __pop_args_ret);
	          OUT_AS1 (jmp, __pop_args_ret);
	        }
	      epilogue_size += 6;
	    }
          need_ret = 0;
        }
      else
        {
          OUT_AS1 (pop, callh);
          OUT_AS1 (pop, calll);
          epilogue_size += 4;
        }
    }
  else
    {
      if (current_function_pops_args
          && args_locals_size >= 2
          && args_locals_size < 0x100)
        {
          if (args_locals_size == 2)
	    {
	      if (CHAIN_FRAMES)
	        {
	          OUT_AS1 (page, __leaf_fp_pop2_args_ret);
	          OUT_AS1 (jmp, __leaf_fp_pop2_args_ret);
	          epilogue_size += 4;
		  need_ret = 0;
	        }
	    }
          else
	    {
	      operands[0] = GEN_INT (args_locals_size);
	      if (CHAIN_FRAMES)
	        {
                  OUT_AS2 (mov, w, %L0);
	          OUT_AS1 (page, __leaf_fp_pop_args_ret);
	          OUT_AS1 (jmp, __leaf_fp_pop_args_ret);
	          epilogue_size += 6;
		  need_ret = 0;
	        }
	    }
        }
    }
  
  if (current_function_pops_args && args_locals_size && need_ret)
    {
      operands[0] = GEN_INT (args_locals_size);

      switch (args_locals_size & 0xff)
        {
        default:
	  OUT_AS2 (mov, w, %L0);
	  OUT_AS2 (add, spl, w);
	  epilogue_size += 4;
	  /* fall-through  */

	case 0:
	  break;

	case 1:
	  OUT_AS1 (inc, spl);
	  epilogue_size += 2;
	}

      switch (args_locals_size & 0xff00)
	{
	default:
	  if ((args_locals_size & 0xff) != ((args_locals_size >> 8) & 0xff))
	    OUT_AS2 (mov, w, %H0);
	  OUT_AS2 (add, sph, w);
	  epilogue_size += 4;
	  /* fall-through  */

	case 0:
	  break;

	case 0x100:
	  OUT_AS1 (inc, sph);
	  epilogue_size += 2;
	}
    }

  if (need_ret)
    {
      OUT_AS1 (ret,);
      epilogue_size += 2;
    }
  
  fprintf (file, "/* epilogue end (size=%d) */\n", epilogue_size);
}

/* Return the difference between the registers after the function
   prologue.

   Stack Frame grows down:

   	ARGUMENTS
		<------ AP ($102:$103)
   	RETURN PC (unless leaf function)
	SAVEDFP (if needed)
		<------ FP [HARD_FRAME_POINTER] ($FD:$FE)
	SAVED REGS
		<------ VFP [$100:$101]
	STACK ALLOCATION
		<------ SP ($6:$7)  */
int
ip2k_init_elim_offset (int from, int to)
{
  int leaf_func_p = leaf_function_p ();
  int no_saved_pc = leaf_func_p
		    || ip2k_naked_function_p (current_function_decl);
  int offset;
  int reg;
  int reglimit;

  if (from == FRAME_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    return get_frame_size () + 1;

  if (from == ARG_POINTER_REGNUM && to == HARD_FRAME_POINTER_REGNUM)
    return (CHAIN_FRAMES ? 2 : 0) + (no_saved_pc ? 0 : 2);
      
  /* Count all the registers we had to preserve.  */

  reglimit = CHAIN_FRAMES ? REG_FP : (REG_FP + 2);
  for (offset = 0,reg = 0; reg < reglimit; ++reg)
    {
      if ((regs_ever_live[reg] && ! call_used_regs[reg]))
	{
	  ++offset;
	}
    }

  if (from == FRAME_POINTER_REGNUM && to == HARD_FRAME_POINTER_REGNUM)
    return -offset;

  if (from == HARD_FRAME_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    /* Add in the stack-local variables.  */
    return offset + get_frame_size () + 1;

  if (from == ARG_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    /* Add stack-locals plus saved FP and PC.  */
    return offset + get_frame_size () + 1
	   + (CHAIN_FRAMES ? 2 : 0) + (no_saved_pc ? 0 : 2);

  abort ();			/* Unanticipated elimination.  */
}

/* Return nonzero if X (an RTX) is a legitimate memory address on the target
   machine for a memory operand of mode MODE.  */

int
legitimate_address_p (enum machine_mode mode, rtx x, int strict)
{
  int off;

  if (GET_CODE (x) == SUBREG)
     x = SUBREG_REG (x);

  switch (GET_CODE (x))
    {
    case REG:
      /* IP allows indirection without offset - only okay if
         we don't require access to multiple bytes.  */
      if (REGNO (x) == REG_IP)
	return (GET_MODE_SIZE (mode) == 1) ? 'R' : 0;

      /* We can indirect through DP or SP register.  */
      if (strict ? REG_OK_FOR_BASE_STRICT_P (x)
	         : REG_OK_FOR_BASE_NOSTRICT_P (x))
	return 'S';
      break;

    case PLUS:
      /* Offsets from DP or SP are legal in the range 0..127  */
      {
	rtx op1, op2;

	op1 = XEXP (x, 0);
	op2 = XEXP (x, 1);

	if (REG_P (op2) && ! REG_P (op1))
	  {
	    rtx tmp = op1;
	    op1 = op2;
	    op2 = tmp;
	  }

	/* Don't let anything but R+I through..  */
	if (! REG_P (op1)
	    || REG_P (op2)
	    || GET_CODE (op2) != CONST_INT)
	  return 0;
	      
	switch (REGNO (op1))
	  {
	  case REG_DP:		/* only 0..127 displacement  */
	  case REG_SP:
	    off = 2 * GET_MODE_SIZE (mode);
	    if (! off)
	      off = 1;

	    if (INTVAL (op2) < 0 || INTVAL (op2) > (128 - off))
	      return 0;		/* Positive must be small enough that after
				   splitting all pieces are addressed.  */
	    return 'S';		/* Safe displacement.  */

	  case REG_IP:
	    if (GET_MODE_SIZE (mode) <= 1 && INTVAL (op2) == 0)
	      return (GET_MODE_SIZE (mode) == 1) ? 'R' : 0;
	    return 0;

	  case REG_AP:
	  case REG_FP:
	  case REG_VFP:
	  default:
	    if (strict || ! REG_OK_FOR_BASE_NOSTRICT_P (op1))
	      return 0;		/* Allow until reload.  */

	    return 'S';
	  }
      }
      break;

    case CONST:
    case SYMBOL_REF:
	 /* We always allow references to things in code space.  */
      return is_regfile_address (x) ? 0 : 'C';

    case LABEL_REF:
      return 'L';

    default:
      return 0;
    }

  return 0;
}

/* Is ADDR mode dependent?  */
int
ip2k_mode_dependent_address (rtx addr)
{
  switch (GET_CODE (addr))
    {
    case POST_INC:
    case POST_DEC:
    case PRE_INC:
    case PRE_DEC:
      return 1;

    case REG:
      return (REGNO (addr) == REG_IP); /* Can't do IP displaced addresses.  */

    default:
      return 0;			/* Assume no dependency.  */
    }
}

/* Attempts to replace X with a valid
   memory address for an operand of mode MODE.  */

rtx
legitimize_address (rtx x, rtx oldx ATTRIBUTE_UNUSED,
		    enum machine_mode mode ATTRIBUTE_UNUSED, rtx scratch)
{
  rtx reg;

  /* You might think that we could split up a symbolic address by
     adding the HIGH 8 bits and doing a displacement off the dp.  But
     because we only have 7 bits of offset, that doesn't actually
     help.  So only constant displacements are likely to obtain an
     advantage.  */
      
  if (GET_CODE (x) == PLUS && REG_P (XEXP (x, 0))
      && GET_CODE (XEXP (x, 1)) == CONST_INT
      && ! CONST_OK_FOR_LETTER_P (INTVAL (XEXP (x, 1)), 'K'))
    {
      int offset = INTVAL (XEXP (x, 1));

      reg = scratch ? scratch : gen_reg_rtx (Pmode);

      emit_insn (gen_rtx_SET (VOIDmode, reg,
			      gen_rtx_PLUS (Pmode, XEXP (x, 0),
					    GEN_INT (offset & 0xffc0))));
      x = gen_rtx_PLUS (Pmode, reg, GEN_INT (offset & 0x3f));
    }

  return x;			/* We don't have any other tricks.  */
}

/* Determine if X is a 'data' address or a code address.  All static
   data and stack variables reside in data memory.  Only code is believed
   to be in PRAM or FLASH.  */
int
is_regfile_address (rtx x)
{
  while (1)
    switch (GET_CODE (x))
      {
      case SYMBOL_REF:
	return ! SYMBOL_REF_FUNCTION_P (x); /* Declared as function.  */
      case CONST:
      case PLUS:
	x = XEXP (x, 0);
	break;
      case CONST_INT:
      case REG:
      case SUBREG:
	return 1;
      case LABEL_REF:
	return 0;
      default:
	return 0;
    }

  return 0;
}

/* Output ADDR to FILE as address.  */

void
print_operand_address (FILE *file, rtx addr)
{
  switch (GET_CODE (addr))
    {
    case SUBREG:
      addr = alter_subreg (&addr);
      /* fall-through  */

    case REG:
      fprintf (file, "(%s)",
	       REGNO (addr) == REG_DP ? "DP"
	       : REGNO (addr) == REG_SP ? "SP"
	       : REGNO (addr) == REG_IP ? "IP"
	       : REGNO (addr) == REG_VFP ? "VFP" /* Should never see this  */
	       : REGNO (addr) == REG_AP ? "AP" 	 /*  or this, either.  */
	       : reg_names[REGNO (addr)]);
      break;

    case PRE_DEC:
    case POST_INC:
      abort ();
      break;

    case CONST:
      addr = XEXP (addr, 0);
      print_operand_address (file, XEXP (addr, 0));
      fprintf (file, "+");
      print_operand_address (file, XEXP (addr, 1));
      return;

    case LO_SUM:
      if (is_regfile_address (XEXP (addr, 1)))
	fprintf (file, "%%lo8data(");
      else
	fprintf (file, "%%lo8insn(");
      print_operand_address (file, XEXP (addr, 1));
      fprintf (file, ")");
      print_operand_address (file, XEXP (addr, 0));
      break;
	       
    case PLUS:			/* Ought to be stack or dp references.  */
      if (XEXP (addr, 1) == const0_rtx
	  && GET_CODE (XEXP (addr, 0)) == PLUS)
	{
	  print_operand_address (file, XEXP (addr, 0));
	  return;
	}

      if (! REG_P (XEXP (addr, 0)) || REGNO (XEXP (addr, 0)) != REG_IP)
	print_operand_address (file, XEXP (addr, 1)); /* const  */
      print_operand_address (file, XEXP (addr, 0));   /* (reg)  */
      break;

    case HIGH:
      if (is_regfile_address (XEXP (addr, 0)))
	fprintf (file, "%%hi8data(");
      else
	fprintf (file, "%%hi8insn(");
      output_addr_const (file, XEXP (addr, 0));
      fprintf (file, ")");
      break;

    default:
      output_addr_const (file, addr);
    }
}


/* Output X as assembler operand to file FILE.  */
     
void
print_operand (FILE *file, rtx x, int code)
{
  int abcd = 0;
  unsigned long value;

  switch (code)
    {
    case '<':			/* Push */
      ip2k_stack_delta++;
      return;

    case '>':			/* Pop */
      ip2k_stack_delta--;
      return;

    case 'A':
    case 'B':
    case 'C':
    case 'D':
      abcd = code - 'A';
      break;

    case 'H':
      abcd = 0;
      break;

    case 'L':
      abcd = 1;
      break;

    case 'S':
    case 'T':
    case 'U':
    case 'V':
    case 'W':
    case 'X':
    case 'Y':
    case 'Z':
      abcd = code - 'S';

    default:
      break;
    }

  if (ip2k_short_operand (x, GET_MODE (x))
      && ip2k_address_uses_reg_p (x, REG_SP))
    /* An SP-relative address needs to account for interior stack
       pushes that reload didn't know about when it calculated the
       stack offset.  */
    abcd += ip2k_stack_delta;

  switch (GET_CODE (x))
    {
    case SUBREG:
      x = alter_subreg (&x);
      /* fall-through  */

    case REG:
      fprintf (file, reg_names[true_regnum (x) + abcd]);
      break;

    case CONST_INT:
      switch (code)
	{
        case 'x':
	  fprintf (file, "$%x", (int)(INTVAL (x) & 0xffff));
	  break;

	case 'b':
	  fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (x)); /* bit selector  */
	  break;

	case 'e':		/* "1 << n" - e.g. "exp"  */
	  fprintf (file, "#%d", 1 << INTVAL (x));
	  break;

	case 'A':
	case 'B':
	case 'C':
	case 'D':
	  value = INTVAL (x);
	  value >>= 8 * (3 - abcd);
	  value &= 0xff;

	  fprintf (file, "#%ld", value);
	  break;

	case 'H':
	  fprintf (file, "#%d", (int)((INTVAL (x) >> 8) & 0xff));
	  break;

	case 'L':
	  fprintf (file, "#%d", (int)(INTVAL (x) & 0xff));
	  break;

	case 'S':
	case 'T':
	case 'U':
	case 'V':
	case 'W':
	case 'X':
	case 'Y':
	case 'Z':
	  value = ((unsigned long long)INTVAL (x)) >> (8 * (7 - abcd)) & 0xff;
	  fprintf (file, "#%ld", value);
	  break;

	default:
	  fprintf (file, "#" HOST_WIDE_INT_PRINT_DEC, INTVAL (x));
	}
      break;

    case SYMBOL_REF:
    case LABEL_REF:
    case CODE_LABEL:
    case CONST:
      switch (code)
	{
	case 'A':
	case 'B':
	case 'C':
	case 'D':
	case 'S':
	case 'T':
	case 'U':
	case 'V':
	case 'W':
	case 'X':
	case 'Y':
	case 'Z':
	  abort ();		/* Probably an error.  */
	  break;

	case 'H':
	  fprintf (file, "#%s(",
		   is_regfile_address (x) ? "%hi8data"
		   			  : "%hi8insn");
	  print_operand_address (file, x);
	  fputc (')', file);
	  break;

	case 'L':
	  fprintf (file, "#%s(",
		   is_regfile_address (x) ? "%lo8data"
		   			  : "%lo8insn");
	  print_operand_address (file, x);
	  fputc (')', file);
	  break;

	default:
	  print_operand_address (file, x);
	}
      break;

    case MEM:
      {
	rtx addr = XEXP (x, 0);

	if (GET_CODE (addr) == SUBREG)
	  addr = alter_subreg (&x);

	if (CONSTANT_P (addr) && abcd)	
	  {
	    fputc ('(', file);
	    print_operand_address (file, addr);
	    fprintf (file, ")+%d", abcd);
	  }
	else if (abcd)
	  {
	    switch (GET_CODE (addr))
	      {
	      case PLUS:
		abcd += INTVAL (XEXP (addr, 1));

		/* Worry about (plus (plus (reg DP) (const_int 10))
					   (const_int 0))  */
		if (GET_CODE (XEXP (addr, 0)) == PLUS)
		  {
		    addr = XEXP (addr, 0);
		    abcd += INTVAL (XEXP (addr, 1));
		  }
		       
		fprintf (file, "%d", abcd);
		print_operand_address (file, XEXP (addr, 0));
		break;

	      case REG:
	      default:
		fprintf (file, "%d", abcd);
		print_operand_address (file, addr);
	      }
	  }
	else if (GET_CODE (addr) == REG
		 && (REGNO (addr) == REG_DP || REGNO (addr) == REG_SP))
	  {
	    fprintf (file, "0");
	    print_operand_address (file, addr);
	  }
	else
	  print_operand_address (file, addr);
      }
      break;

    case CONST_DOUBLE:
      /* Is this an integer or a floating point value?  */
      if (GET_MODE (x) == VOIDmode)
        {
          switch (code)
	    {
	    case 'S':
	    case 'T':
	    case 'U':
	    case 'V':
	      value = CONST_DOUBLE_HIGH (x);
	      value >>= 8 * (3 - abcd);
	      value &= 0xff;

	      fprintf (file, "#%ld", value);
	      break;

	    case 'W':
	    case 'X':
	    case 'Y':
	    case 'Z':
	      value = CONST_DOUBLE_LOW (x);
	      value >>= 8 * (7 - abcd);
	      value &= 0xff;

	      fprintf (file, "#%ld", value);
	      break;
	    }

	}
      else
        {
	  REAL_VALUE_TYPE rv;

	  REAL_VALUE_FROM_CONST_DOUBLE (rv, x);
	  REAL_VALUE_TO_TARGET_SINGLE (rv, value);
	  fprintf (file, "0x%lx", value);
        }
      break;

    default:
      fatal_insn ("bad operand", x);
    }
}

/* Remember the operands for the compare.  */
const char *
ip2k_set_compare (rtx x, rtx y)
{
  ip2k_compare_operands[0] = x;
  ip2k_compare_operands[1] = y;
  return "";
}

/* Emit the code for sCOND instructions.  */
const char *
ip2k_gen_sCOND (rtx insn ATTRIBUTE_UNUSED, enum rtx_code code, rtx dest)
{
#define operands ip2k_compare_operands
  enum machine_mode mode;

  operands[2] = dest;

  mode = GET_MODE (operands[0]);
  if ((mode != QImode) && (mode != HImode)
      && (mode != SImode) && (mode != DImode))
    mode = GET_MODE (operands[1]);

  /* We have a fast path for a specific type of QImode compare.  We ought
     to extend this for larger cases too but that wins less frequently and
     introduces a lot of complexity.  */
  if (mode == QImode
      && !rtx_equal_p (operands[0], operands[2])
      && !rtx_equal_p (operands[1], operands[2])
      && (! REG_P (operands[2])
	  || (ip2k_xexp_not_uses_reg_p (operands[0], REGNO (operands[2]), 1)
	      && ip2k_xexp_not_uses_reg_p (operands[1],
					   REGNO (operands[2]), 1))))
    {
      OUT_AS1 (clr, %2);
      if (immediate_operand (operands[1], QImode)
	  && ((INTVAL (operands[1]) & 0xff) == 0xff))
        {
	  if (code == EQ)
            OUT_AS2 (incsnz, w, %0);
          else
	    OUT_AS2 (incsz, w, %0);
	}
      else if (immediate_operand (operands[1], QImode)
	       && ((INTVAL (operands[1]) & 0xff) == 0x01))
	{
	  if (code == EQ)
            OUT_AS2 (decsnz, w, %0);
          else
	    OUT_AS2 (decsz, w, %0);
	}
      else if (ip2k_compare_operands[1] == const0_rtx)
	{
          OUT_AS2 (mov, w, %0);
	  if (code == EQ)
            OUT_AS1 (snz,);
          else
	    OUT_AS1 (sz,);
	}
      else
	{
          OUT_AS2 (mov, w, %0);
	  if (code == EQ)
            OUT_AS2 (csne, w, %1);
          else
	    OUT_AS2 (cse, w, %1);
	}
      OUT_AS1 (inc, %2);
    }
  else
    {
      if (ip2k_compare_operands[1] == const0_rtx)
	{
          switch (mode)
            {
            case QImode:
              OUT_AS2 (mov, w, %0);
              break;

            case HImode:
              OUT_AS2 (mov, w, %H0);
              OUT_AS2 (or, w, %L0);
              break;

	    case SImode:
              OUT_AS2 (mov, w, %A0);
              OUT_AS2 (or, w, %B0);
              OUT_AS2 (or, w, %C0);
              OUT_AS2 (or, w, %D0);
              break;

	    case DImode:
              OUT_AS2 (mov, w, %S0);
              OUT_AS2 (or, w, %T0);
              OUT_AS2 (or, w, %U0);
              OUT_AS2 (or, w, %V0);
              OUT_AS2 (or, w, %W0);
              OUT_AS2 (or, w, %X0);
              OUT_AS2 (or, w, %Y0);
              OUT_AS2 (or, w, %Z0);
              break;

            default:
   	      abort ();
            }
	}
      else
	{
	  switch (mode)
	    {
	    case QImode:
	      OUT_AS2 (mov, w, %1);
	      OUT_AS2 (cmp, w, %0);
	      break;

	    case HImode:
	      OUT_AS2 (mov, w, %H1);
	      OUT_AS2 (cmp, w, %H0);
	      OUT_AS1 (sz,);
	      OUT_AS1 (page, 2f);
	      OUT_AS1 (jmp, 2f);
	      OUT_AS2 (mov, w, %L1);
	      OUT_AS2 (cmp, w, %L0);
	      OUT_AS1 (2:,);
	      break;

	    case SImode:
	      if (code == EQ)
	        {
	          OUT_AS2 (mov, w, #1);
	          OUT_AS2 (mov, mulh, w);
		}
	      else
		OUT_AS1 (clr, mulh);
	      OUT_AS2 (mov, w, %A1);
	      OUT_AS2 (cse, w, %A0);
	      OUT_AS1 (page, 2f);
	      OUT_AS1 (jmp, 2f);
	      OUT_AS2 (mov, w, %B1);
	      OUT_AS2 (cse, w, %B0);
	      OUT_AS1 (page, 2f);
	      OUT_AS1 (jmp, 2f);
	      OUT_AS2 (mov, w, %C1);
	      OUT_AS2 (cse, w, %C0);
	      OUT_AS1 (page, 2f);
	      OUT_AS1 (jmp, 2f);
	      OUT_AS2 (mov, w, %D1);
	      OUT_AS2 (cse, w, %D0);
	      OUT_AS1 (2:,);
	      if (code == EQ)
	        OUT_AS1 (dec, mulh);
	      else
		OUT_AS1 (inc, mulh);
	      OUT_AS2 (mov, w, mulh);
	      OUT_AS2 (mov, %2, w);
	      return "";

	    case DImode:
	      if (code == EQ)
	        {
	          OUT_AS2 (mov, w, #1);
	          OUT_AS2 (mov, mulh, w);
		}
	      else
		OUT_AS1 (clr, mulh);
	      OUT_AS2 (mov, w, %S1);
	      OUT_AS2 (cse, w, %S0);
	      OUT_AS1 (page, 2f);
	      OUT_AS1 (jmp, 2f);
	      OUT_AS2 (mov, w, %T1);
	      OUT_AS2 (cse, w, %T0);
	      OUT_AS1 (page, 2f);
	      OUT_AS1 (jmp, 2f);
	      OUT_AS2 (mov, w, %U1);
	      OUT_AS2 (cse, w, %U0);
	      OUT_AS1 (page, 2f);
	      OUT_AS1 (jmp, 2f);
	      OUT_AS2 (mov, w, %V1);
	      OUT_AS2 (cse, w, %V0);
	      OUT_AS1 (page, 2f);
	      OUT_AS1 (jmp, 2f);
	      OUT_AS2 (mov, w, %W1);
	      OUT_AS2 (cse, w, %W0);
	      OUT_AS1 (page, 2f);
	      OUT_AS1 (jmp, 2f);
	      OUT_AS2 (mov, w, %X1);
	      OUT_AS2 (cse, w, %X0);
	      OUT_AS1 (page, 2f);
	      OUT_AS1 (jmp, 2f);
	      OUT_AS2 (mov, w, %Y1);
	      OUT_AS2 (cse, w, %Y0);
	      OUT_AS1 (page, 2f);
	      OUT_AS1 (jmp, 2f);
	      OUT_AS2 (mov, w, %Z1);
	      OUT_AS2 (cse, w, %Z0);
	      OUT_AS1 (2:,);
	      if (code == EQ)
	        OUT_AS1 (dec, mulh);
	      else
		OUT_AS1 (inc, mulh);
	      OUT_AS2 (mov, w, mulh);
	      OUT_AS2 (mov, %2, w);
	      return "";

            default:
	      abort ();
	    }
	}
      OUT_AS2 (mov, w, #0);
      if (code == EQ)
	OUT_AS1 (snz,);
      else
	OUT_AS1 (sz,);
      OUT_AS1 (inc, wreg);
      OUT_AS2 (mov, %2, w);
    }

  return "";
#undef operands
}

const char *
ip2k_gen_signed_comp_branch (rtx insn, enum rtx_code code, rtx label)
{
#define operands ip2k_compare_operands
  enum machine_mode mode;
  int can_use_skip = 0;
  rtx ninsn;

  operands[2] = label;

  mode = GET_MODE (operands[0]);
  if ((mode != QImode) && (mode != HImode)
      && (mode != SImode) && (mode != DImode))
    mode = GET_MODE (operands[1]);

  /* Look for situations where we can just skip the next instruction instead
     of skipping and then branching!  */
  ninsn = next_real_insn (insn);
  if (ninsn
      && (recog_memoized (ninsn) >= 0)
      && get_attr_skip (ninsn) == SKIP_YES)
    {
      rtx skip_tgt = next_nonnote_insn (next_real_insn (insn));

      /* The first situation is where the target of the jump is one insn
         after the jump insn and the insn being jumped is only one machine
	 opcode long.  */
      if (label == skip_tgt)
        can_use_skip = 1;
      else
	{
          /* If our skip target is in fact a code label then we ignore the
             label and move onto the next useful instruction.  Nothing we do
	     here has any effect on the use of skipping instructions.  */
          if (GET_CODE (skip_tgt) == CODE_LABEL)
	    skip_tgt = next_nonnote_insn (skip_tgt);

          /* The second situation is where we have something of the form:

               test_condition
               skip_conditional
               page/jump label

             optional_label (this may or may not exist):
               skippable_insn
               page/jump label

             In this case we can eliminate the first "page/jump label".  */
	  if (GET_CODE (skip_tgt) == JUMP_INSN)
	    {
	      rtx set = single_set (skip_tgt);
	      if (GET_CODE (XEXP (set, 0)) == PC
	          && GET_CODE (XEXP (set, 1)) == LABEL_REF
	          && label == JUMP_LABEL (skip_tgt))
	        can_use_skip = 2;
            }
	}
    }

  /* gcc is a little braindead and does some rather stateful things while
     inspecting attributes - we have to put this state back to what it's
     supposed to be.  */
  extract_constrain_insn_cached (insn);

  if (ip2k_compare_operands[1] == const0_rtx) /* These are easier.  */
    {
      switch (code)
        {
	case LT:
	  if (can_use_skip)
       	    {
	      OUT_AS2 (sb, %0, 7);
	    }
	  else
	    {
              OUT_AS2 (snb, %0, 7);
	      OUT_AS1 (page, %2);
	      OUT_AS1 (jmp, %2);
	    }
	  break;

	case GT:
          switch (mode)
	    {
            case DImode:
              OUT_AS2 (rl, w, %S0);
              OUT_AS2 (mov, w, %S0);
              OUT_AS2 (or, w, %T0);
              OUT_AS2 (or, w, %U0);
              OUT_AS2 (or, w, %V0);
              OUT_AS2 (or, w, %W0);
              OUT_AS2 (or, w, %X0);
              OUT_AS2 (or, w, %Y0);
              OUT_AS2 (or, w, %Z0);
 	      OUT_AS1 (snz, );
	      OUT_AS2 (setb, status, 0);
	      OUT_AS2 (sb, status, 0);
	      OUT_AS1 (page, %2);
	      OUT_AS1 (jmp, %2);
              break;

            case SImode:
              OUT_AS2 (rl, w, %A0);
              OUT_AS2 (mov, w, %A0);
              OUT_AS2 (or, w, %B0);
              OUT_AS2 (or, w, %C0);
              OUT_AS2 (or, w, %D0);
 	      OUT_AS1 (snz, );
	      OUT_AS2 (setb, status, 0);
	      OUT_AS2 (sb, status, 0);
	      OUT_AS1 (page, %2);
	      OUT_AS1 (jmp, %2);
              break;

            case HImode:
              OUT_AS2 (rl, w, %H0);
              OUT_AS2 (mov, w, %H0);
              OUT_AS2 (or, w, %L0);
 	      OUT_AS1 (snz, );
	      OUT_AS2 (setb, status, 0);
	      OUT_AS2 (sb, status, 0);
	      OUT_AS1 (page, %2);
	      OUT_AS1 (jmp, %2);
              break;

            case QImode:
              OUT_AS2 (mov, w, %0);	/* Will just do "sb w, 7".  */
 	      OUT_AS1 (snz, );
	      OUT_AS2 (setb, wreg, 7);
	      OUT_AS2 (sb, wreg, 7);
	      OUT_AS1 (page, %2);
	      OUT_AS1 (jmp, %2);
              break;

            default:
	      abort ();
            }
	  break;

	case LE:
          switch (mode)
	    {
            case DImode:
              OUT_AS2 (mov, w, %S0);
              OUT_AS2 (or, w, %T0);
              OUT_AS2 (or, w, %U0);
              OUT_AS2 (or, w, %V0);
              OUT_AS2 (or, w, %W0);
              OUT_AS2 (or, w, %X0);
              OUT_AS2 (or, w, %Y0);
              OUT_AS2 (or, w, %Z0);	/* Z is correct.  */
	      OUT_AS1 (sz, );
	      OUT_AS2 (snb, %S0, 7);
	      OUT_AS1 (page, %2);
	      OUT_AS1 (jmp, %2);
              break;

            case SImode:
              OUT_AS2 (mov, w, %A0);
              OUT_AS2 (or, w, %B0);
              OUT_AS2 (or, w, %C0);
              OUT_AS2 (or, w, %D0);	/* Z is correct.  */
	      OUT_AS1 (sz, );
	      OUT_AS2 (snb, %A0, 7);
	      OUT_AS1 (page, %2);
	      OUT_AS1 (jmp, %2);
              break;

            case HImode:
              OUT_AS2 (mov, w, %H0);
              OUT_AS2 (or, w, %L0);
	      OUT_AS1 (sz, );
	      OUT_AS2 (snb, %H0, 7);
	      OUT_AS1 (page, %2);
	      OUT_AS1 (jmp, %2);
              break;

            case QImode:
              OUT_AS2 (mov, w, %0);	/* Will just do "sb w, 7".  */
	      OUT_AS1 (sz, );
	      OUT_AS2 (snb, wreg, 7);
	      OUT_AS1 (page, %2);
	      OUT_AS1 (jmp, %2);
              break;

            default:
	      abort ();
            }
	  break;

	case GE:
	  if (can_use_skip)
            {
	      OUT_AS2 (snb, %0, 7);
	    }
	  else
            {
	      OUT_AS2 (sb, %0, 7);
	      OUT_AS1 (page, %2);
	      OUT_AS1 (jmp, %2);
	    }
	  break;

	default:
	  abort ();
        }
      return "";
    }

  /* signed compares are out of line because we can't get
     the hardware to compute the overflow for us.  */

  switch (mode)
    {
    case QImode:
      OUT_AS1 (push, %1%<);
      OUT_AS1 (push, %0%>);
      OUT_AS1 (page, __cmpqi2);
      OUT_AS1 (call, __cmpqi2);
      break;

    case HImode:
      OUT_AS1 (push, %L1%<);
      OUT_AS1 (push, %H1%<);
      OUT_AS1 (push, %L0%<);
      OUT_AS1 (push, %H0%>%>%>);
      OUT_AS1 (page, __cmphi2);
      OUT_AS1 (call, __cmphi2);
      break;

    case SImode:
      OUT_AS1 (push, %D1%<);
      OUT_AS1 (push, %C1%<);
      OUT_AS1 (push, %B1%<);
      OUT_AS1 (push, %A1%<);
      OUT_AS1 (push, %D0%<);
      OUT_AS1 (push, %C0%<);
      OUT_AS1 (push, %B0%<);
      OUT_AS1 (push, %A0%>%>%>%>%>%>%>);
      OUT_AS1 (page, __cmpsi2);
      OUT_AS1 (call, __cmpsi2);
      break;
  
    case DImode:
      if (GET_CODE (operands[0]) == MEM
	  && true_regnum (XEXP (operands[0], 0)) == REG_DP)
	{
	  OUT_AS1 (push, %Z1%<);
	  OUT_AS1 (push, %Y1%<);
	  OUT_AS1 (push, %X1%<);
	  OUT_AS1 (push, %W1%<);
	  OUT_AS1 (push, %V1%<);
	  OUT_AS1 (push, %U1%<);
	  OUT_AS1 (push, %T1%<);
	  OUT_AS1 (push, %S1%>%>%>%>%>%>%>);
	  OUT_AS1 (page, __cmpdi2_dp);
	  OUT_AS1 (call, __cmpdi2_dp);
	}
      else
	{
	  OUT_AS1 (push, %Z1%<);
	  OUT_AS1 (push, %Y1%<);
	  OUT_AS1 (push, %X1%<);
	  OUT_AS1 (push, %W1%<);
	  OUT_AS1 (push, %V1%<);
	  OUT_AS1 (push, %U1%<);
	  OUT_AS1 (push, %T1%<);
	  OUT_AS1 (push, %S1%<);
	  OUT_AS1 (push, %Z0%<);
	  OUT_AS1 (push, %Y0%<);
	  OUT_AS1 (push, %X0%<);
	  OUT_AS1 (push, %W0%<);
	  OUT_AS1 (push, %V0%<);
	  OUT_AS1 (push, %U0%<);
	  OUT_AS1 (push, %T0%<);
	  OUT_AS1 (push, %S0%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>);
	  OUT_AS1 (page, __cmpdi2);
	  OUT_AS1 (call, __cmpdi2);
	}
      break;

    default:
      abort ();
  }

  switch (code)
    {
    case LT:
      if (can_use_skip)
        {
	  OUT_AS2 (cse, w, #0);
	}
      else
	{
          OUT_AS2 (csne, w, #0);
          OUT_AS1 (page, %2);
          OUT_AS1 (jmp, %2);
	}
      break;
      
    case GT:
      if (can_use_skip)
	{
	  OUT_AS2 (cse, w, #2);
	}
      else
	{
          OUT_AS2 (csne, w, #2);
          OUT_AS1 (page, %2);
          OUT_AS1 (jmp, %2);
	}
      break;

    case LE:
      if (can_use_skip)
	{
	  OUT_AS2 (snb, wreg, 1);
	}
      else
	{
          OUT_AS2 (sb, wreg, 1);
          OUT_AS1 (page, %2);
          OUT_AS1 (jmp, %2);
	}
      break;

    case GE:
      if (can_use_skip)
	{
	  OUT_AS2 (csne, w, #0);
	}
      else
	{
          OUT_AS2 (cse, w, #0);
          OUT_AS1 (page, %2);
          OUT_AS1 (jmp, %2);
	}
      break;
      
    default:
      abort ();
    }
  return "";
#undef operands
}

const char *
ip2k_gen_unsigned_comp_branch (rtx insn, enum rtx_code code, rtx label)
{
#define operands ip2k_compare_operands
  enum machine_mode mode;
  int imm_sub = 0;
  int imm_cmp = 0;
  int can_use_skip = 0;
  rtx ninsn;
  HOST_WIDE_INT const_low;
  HOST_WIDE_INT const_high;

  operands[2] = label;

  mode = GET_MODE (operands[0]);
  if ((mode != QImode) && (mode != HImode) && (mode != SImode)
      && (mode != DImode))
    {
      mode = GET_MODE (operands[1]);
    }

  /* Look for situations where we can just skip the next instruction instead
     of skipping and then branching!  */
  ninsn = next_real_insn (insn);
  if (ninsn
      && (recog_memoized (ninsn) >= 0)
      && get_attr_skip (ninsn) == SKIP_YES)
    {
      rtx skip_tgt = next_nonnote_insn (next_real_insn (insn));

      /* The first situation is where the target of the jump is one insn
         after the jump insn and the insn being jumped is only one machine
	 opcode long.  */
      if (label == skip_tgt)
        can_use_skip = 1;
      else
	{
          /* If our skip target is in fact a code label then we ignore the
             label and move onto the next useful instruction.  Nothing we do
	     here has any effect on the use of skipping instructions.  */
          if (GET_CODE (skip_tgt) == CODE_LABEL)
	    skip_tgt = next_nonnote_insn (skip_tgt);

          /* The second situation is where we have something of the form:

               test_condition
               skip_conditional
               page/jump label

             optional_label (this may or may not exist):
               skippable_insn
               page/jump label

             In this case we can eliminate the first "page/jump label".  */
	  if (GET_CODE (skip_tgt) == JUMP_INSN)
	    {
	      rtx set = single_set (skip_tgt);
	      if (GET_CODE (XEXP (set, 0)) == PC
	          && GET_CODE (XEXP (set, 1)) == LABEL_REF
	          && label == JUMP_LABEL (skip_tgt))
	        can_use_skip = 2;
            }
	}
    }

  /* gcc is a little braindead and does some rather stateful things while
     inspecting attributes - we have to put this state back to what it's
     supposed to be.  */
  extract_constrain_insn_cached (insn);

  if (ip2k_compare_operands[1] == const0_rtx)
    {
      switch (code)
        {
        case LEU:
          code = EQ;			/* Nothing is LTU 0.  */
          goto zero;

        case GTU:			
          code = NE;			/* Anything nonzero is GTU.  */
          /* fall-through  */

        case EQ:
        case NE:			/* Test all the bits, result in
					   Z AND WREG.  */
        zero:
          switch (mode)
            {
	    case DImode:
              OUT_AS2 (mov, w, %S0);
              OUT_AS2 (or, w, %T0);
              OUT_AS2 (or, w, %U0);
              OUT_AS2 (or, w, %V0);
              OUT_AS2 (or, w, %W0);
              OUT_AS2 (or, w, %X0);
              OUT_AS2 (or, w, %Y0);
              OUT_AS2 (or, w, %Z0);
              break;

	    case SImode:
              OUT_AS2 (mov, w, %A0);
              OUT_AS2 (or, w, %B0);
              OUT_AS2 (or, w, %C0);
              OUT_AS2 (or, w, %D0);
              break;

            case HImode:
              OUT_AS2 (mov, w, %H0);
              OUT_AS2 (or, w, %L0);
              break;

            case QImode:
              OUT_AS2 (mov, w, %0);
              break;

            default:
   	      abort ();
            }

	  if (can_use_skip)
            {
	      if (code == EQ)
		OUT_AS1 (sz, );
	      else
		OUT_AS1 (snz, );
            }
	  else
            {
	      if (code == EQ)
                OUT_AS1 (snz,);
	      else
	        OUT_AS1 (sz,);
              OUT_AS1 (page, %2);
              OUT_AS1 (jmp, %2);
	    }
          break;

        case GEU:
          /* Always succeed.  */
          OUT_AS1 (page, %2);
          OUT_AS1 (jmp, %2);
          break;

        case LTU:			
          /* Always fail.  */
          break;

        default:
          abort ();
	}
      return "";
    }

  /* Look at whether we have a constant as one of our operands.  If we do
     and it's in the position that we use to subtract from during our
     normal optimized comparison concept then we have to shuffle things
     around!  */
  if (mode != QImode)
    {
      if ((immediate_operand (operands[1], GET_MODE (operands[1]))
	   && ((code == LEU) || (code == GTU)))
	  || (immediate_operand (operands[0], GET_MODE (operands[0]))
	      && ((code == LTU) || (code == GEU))))
        {
          imm_sub = 1;
        }
    }

  /* Same as above - look if we have a constant that we can compare
     for equality or non-equality.  If we know this then we can look
     for common value eliminations.  Note that we want to ensure that
     any immediate value is operand 1 to simplify the code later!  */
  if ((code == EQ) || (code == NE))
    {
      imm_cmp = immediate_operand (operands[1], GET_MODE (operands[1]));
      if (! imm_cmp)
	{
	  imm_cmp = immediate_operand (operands[0], GET_MODE (operands[0]));
	  if (imm_cmp)
 	    {
	      rtx tmp = operands[1];
	      operands[1] = operands[0];
	      operands[0] = tmp;
	    }
	}
    }
  
  switch (mode)
    {
    case QImode:
      switch (code)
        {
        case EQ:
	  if (imm_cmp && ((INTVAL (operands[1]) & 0xff) == 0xff))
	    OUT_AS2 (incsnz, w, %0);
	  else if (imm_cmp && ((INTVAL (operands[1]) & 0xff) == 0x01))
	    OUT_AS2 (decsnz, w, %0);
	  else
	    {
              OUT_AS2 (mov, w, %1);
	      OUT_AS2 (csne, w, %0);
	    }
	  OUT_AS1 (page, %2);
	  OUT_AS1 (jmp, %2);
	  break;

	case NE:
	  if (imm_cmp && ((INTVAL (operands[1]) & 0xff) == 0xff))
	    OUT_AS2 (incsz, w, %0);
	  else if (imm_cmp && ((INTVAL (operands[1]) & 0xff) == 0x01))
	    OUT_AS2 (decsz, w, %0);
	  else
	    {
              OUT_AS2 (mov, w, %1);
	      OUT_AS2 (cse, w, %0);
	    }
	  OUT_AS1 (page, %2);
	  OUT_AS1 (jmp, %2);
	  break;

	case GTU:
	  OUT_AS2 (mov, w, %0);
	  OUT_AS2 (cmp, w, %1);
	  OUT_AS1 (sc,);
	  OUT_AS1 (page, %2);
	  OUT_AS1 (jmp, %2);
	  break;

	case GEU:
	  OUT_AS2 (mov, w, %1);
	  OUT_AS2 (cmp, w, %0);
	  OUT_AS1 (snc,);
	  OUT_AS1 (page, %2);
	  OUT_AS1 (jmp, %2);
	  break;

	case LTU:
	  OUT_AS2 (mov, w, %1);
	  OUT_AS2 (cmp, w, %0);
	  OUT_AS1 (sc,);
	  OUT_AS1 (page, %2);
	  OUT_AS1 (jmp, %2);
	  break;

	case LEU:
	  OUT_AS2 (mov, w, %0);
	  OUT_AS2 (cmp, w, %1);
	  OUT_AS1 (snc,);
	  OUT_AS1 (page, %2);
	  OUT_AS1 (jmp, %2);
	  break;

	default:
	  abort ();
        }
      break;

    case HImode:
      switch (code)
        {
	case EQ:
	  {
	    unsigned char h = 0, l = 1;

	    if (imm_cmp)
	      {
	        h = (INTVAL (operands[1]) >> 8) & 0xff;
	        l = INTVAL (operands[1]) & 0xff;

		if ((h == 0xff) && (l == 0xff))
	          {
		    /* We should be able to do the following, but the
		       IP2k simulator doesn't like it and we get a load
		       of failures in gcc-c-torture.  */
		    OUT_AS2 (incsnz, w, %L0);
		    OUT_AS2 (incsz, w, %H0);
/*		    OUT_AS1 (skip,);		   Should have this  */
		    OUT_AS1 (page, 1f);/* Shouldn't need this!  */
	            OUT_AS1 (jmp, 1f); /* Shouldn't need this either.  */
		    OUT_AS1 (page, %2);
	            OUT_AS1 (jmp, %2);
	            OUT_AS1 (1:,);
		    break;
	          }
		else if (h == 0)
		  {
		    if (l == 1)
		      OUT_AS2 (dec, w, %L0);
		    else
		      {
		        OUT_AS2 (mov, w, %L0);
		        OUT_AS2 (sub, w, %L1);
		      }
		    OUT_AS2 (or, w, %H0);
		    OUT_AS1 (snz,);
		    OUT_AS1 (page, %2);
	            OUT_AS1 (jmp, %2);
		    break;
		  }
		else if (l == 0)
		  {
		    if (h == 1)
		      OUT_AS2 (dec, w, %H0);
		    else
		      {
		        OUT_AS2 (mov, w, %H0);
		        OUT_AS2 (sub, w, %H1);
		      }
		    OUT_AS2 (or, w, %L0);
		    OUT_AS1 (snz,);
		    OUT_AS1 (page, %2);
	            OUT_AS1 (jmp, %2);
		    break;
		  }
	      }

	    OUT_AS2 (mov, w, %H1);
	    OUT_AS2 (cse, w, %H0);
	    OUT_AS1 (page, 2f);
	    OUT_AS1 (jmp, 2f);
	    if (! imm_cmp || (h != l))
	      OUT_AS2 (mov, w, %L1);
	    OUT_AS2 (csne, w, %L0);
	    OUT_AS1 (page, %2);
	    OUT_AS1 (jmp, %2);
	    OUT_AS1 (2:,);
	  }
          break;

	case NE:
	  {
	    unsigned char h = 0, l = 1;

	    if (imm_cmp)
	      {
	        h = (INTVAL (operands[1]) >> 8) & 0xff;
	        l = INTVAL (operands[1]) & 0xff;

		if ((h == 0xff) && (l == 0xff))
	          {
		    OUT_AS2 (incsnz, w, %L0);
		    OUT_AS2 (incsz, w, %H0);
		    OUT_AS1 (page, %2);
	            OUT_AS1 (jmp, %2);
		    break;
	          }
	    	else if (h == 0)
		  {
		    if (l == 1)
		      OUT_AS2 (dec, w, %L0);
		    else
		      {
		        OUT_AS2 (mov, w, %L0);
		        OUT_AS2 (sub, w, %L1);
		      }
		    OUT_AS2 (or, w, %H0);
		    OUT_AS1 (sz,);
		    OUT_AS1 (page, %2);
	            OUT_AS1 (jmp, %2);
		    break;
		  }
		else if (l == 0)
		  {
		    if (h == 1)
		      OUT_AS2 (dec, w, %H0);
		    else
		      {
		        OUT_AS2 (mov, w, %H0);
		        OUT_AS2 (sub, w, %H1);
		      }
		    OUT_AS2 (or, w, %L0);
		    OUT_AS1 (sz,);
		    OUT_AS1 (page, %2);
	            OUT_AS1 (jmp, %2);
		    break;
		  }
	      }

	    OUT_AS2 (mov, w, %H1);
	    if (imm_cmp && (h == l))
	      {
	        OUT_AS2 (csne, w, %H0);
	        OUT_AS2 (cse, w, %L0);
	      }
	    else
	      {
	        OUT_AS2 (cse, w, %H0);
	        OUT_AS1 (page, %2);
	        OUT_AS1 (jmp, %2);
	        OUT_AS2 (mov, w, %L1);
	        OUT_AS2 (cse, w, %L0);
	      }
	    OUT_AS1 (page, %2);
	    OUT_AS1 (jmp, %2);
	  }
	  break;

	case GTU:
	  if (imm_sub)
	    {
	      /* > 0xffff never succeeds!  */
	      if ((INTVAL (operands[1]) & 0xffff) != 0xffff)
		{
	          operands[3] = GEN_INT (INTVAL (operands[1]) + 1);
	          OUT_AS2 (mov, w, %L3);
	          OUT_AS2 (sub, w, %L0);
	          OUT_AS2 (mov, w, %H3);
	          OUT_AS2 (subc, w, %H0);
	          OUT_AS1 (snc,);
	          OUT_AS1 (page, %2);
	          OUT_AS1 (jmp, %2);
		}
	    }
	  else
	    {
	      OUT_AS2 (mov, w, %L0);
	      OUT_AS2 (sub, w, %L1);
	      OUT_AS2 (mov, w, %H0);
	      OUT_AS2 (subc, w, %H1);
	      OUT_AS1 (sc,);
	      OUT_AS1 (page, %2);
	      OUT_AS1 (jmp, %2);
	    }
	  break;

	case GEU:
	  if (imm_sub)
	    {
	      if (INTVAL (operands[0]) == 0)
		{
                  OUT_AS2 (mov, w, %H1);
                  OUT_AS2 (or, w, %L1);
		  OUT_AS1 (snz,);
	          OUT_AS1 (page, %2);
	          OUT_AS1 (jmp, %2);
		}
	      else
	        {
	          operands[3] = GEN_INT (INTVAL (operands[0]) - 1);
	          OUT_AS2 (mov, w, %L3);
	          OUT_AS2 (sub, w, %L1);
	          OUT_AS2 (mov, w, %H3);
	          OUT_AS2 (subc, w, %H1);
	          OUT_AS1 (sc,);
	          OUT_AS1 (page, %2);
	          OUT_AS1 (jmp, %2);
		}
	    }
	  else
	    {
	      OUT_AS2 (mov, w, %L1);
	      OUT_AS2 (sub, w, %L0);
	      OUT_AS2 (mov, w, %H1);
	      OUT_AS2 (subc, w, %H0);
	      OUT_AS1 (snc,);
	      OUT_AS1 (page, %2);
	      OUT_AS1 (jmp, %2);
	    }
	  break;

	case LTU:
	  if (imm_sub)
	    {
	      if (INTVAL (operands[0]) == 0)
	        {
                  OUT_AS2 (mov, w, %H1);
                  OUT_AS2 (or, w, %L1);
		  OUT_AS1 (sz,);
	          OUT_AS1 (page, %2);
	          OUT_AS1 (jmp, %2);
		}
	      else
	        {
	          operands[3] = GEN_INT (INTVAL (operands[0]) - 1);
	          OUT_AS2 (mov, w, %L3);
	          OUT_AS2 (sub, w, %L1);
	          OUT_AS2 (mov, w, %H3);
	          OUT_AS2 (subc, w, %H1);
	          OUT_AS1 (snc,);
	          OUT_AS1 (page, %2);
	          OUT_AS1 (jmp, %2);
		}
	    }
	  else
	    {
	      OUT_AS2 (mov, w, %L1);
	      OUT_AS2 (sub, w, %L0);
	      OUT_AS2 (mov, w, %H1);
	      OUT_AS2 (subc, w, %H0);
	      OUT_AS1 (sc,);
	      OUT_AS1 (page, %2);
	      OUT_AS1 (jmp, %2);
            }
 	  break;

	case LEU:
	  if (imm_sub)
	    {
	      if ((INTVAL (operands[1]) & 0xffff) == 0xffff)
	        {
		  /* <= 0xffff always succeeds.  */
		  OUT_AS1 (page, %2);
	          OUT_AS1 (jmp, %2);
		}
	      else
		{
	          operands[3] = GEN_INT (INTVAL (operands[1]) + 1);
	          OUT_AS2 (mov, w, %L3);
	          OUT_AS2 (sub, w, %L0);
	          OUT_AS2 (mov, w, %H3);
	          OUT_AS2 (subc, w, %H0);
	          OUT_AS1 (sc,);
	          OUT_AS1 (page, %2);
	          OUT_AS1 (jmp, %2);
		}
	    }
	  else
	    {
	      OUT_AS2 (mov, w, %L0);
	      OUT_AS2 (sub, w, %L1);
	      OUT_AS2 (mov, w, %H0);
	      OUT_AS2 (subc, w, %H1);
	      OUT_AS1 (snc,);
	      OUT_AS1 (page, %2);
	      OUT_AS1 (jmp, %2);
	    }
	  break;

	default:
	  abort ();
        }
      break;

    case SImode:
      switch (code)
        {
	case EQ:
	  {
	    unsigned char a = 0, b = 1, c = 2, d = 3;

	    if (imm_cmp)
	      {
	        a = (INTVAL (operands[1]) >> 24) & 0xff;
	        b = (INTVAL (operands[1]) >> 16) & 0xff;
                c = (INTVAL (operands[1]) >> 8) & 0xff;
	        d = INTVAL (operands[1]) & 0xff;
	      }

            OUT_AS2 (mov, w, %A1);
	    if (imm_cmp && (b == a))
	      {
	        OUT_AS2 (csne, w, %A0);
	        OUT_AS2 (cse, w, %B0);
	      }
	    else
	      {
	        OUT_AS2 (cse, w, %A0);
	        OUT_AS1 (page, 2f);
	        OUT_AS1 (jmp, 2f);
	        OUT_AS2 (mov, w, %B1);
	        OUT_AS2 (cse, w, %B0);
	      }
	    OUT_AS1 (page, 2f);
	    OUT_AS1 (jmp, 2f);
	    if (! imm_cmp || (c != b))
	      OUT_AS2 (mov, w, %C1);
	    OUT_AS2 (cse, w, %C0);
	    OUT_AS1 (page, 2f);
	    OUT_AS1 (jmp, 2f);
	    if (! imm_cmp || (d != c))
	      OUT_AS2 (mov, w, %D1);
	    OUT_AS2 (csne, w, %D0);
	    OUT_AS1 (page, %2);
	    OUT_AS1 (jmp, %2);
	    OUT_AS1 (2:,);
	  }
	  break;

        case NE:
	  {
	    unsigned char a = 0, b = 1, c = 2, d = 3;

	    if (imm_cmp)
	      {
	        a = (INTVAL (operands[1]) >> 24) & 0xff;
	        b = (INTVAL (operands[1]) >> 16) & 0xff;
                c = (INTVAL (operands[1]) >> 8) & 0xff;
	        d = INTVAL (operands[1]) & 0xff;
	      }

	    OUT_AS2 (mov, w, %A1);
	    if (imm_cmp && (b == a))
	      {
	        OUT_AS2 (csne, w, %A0);
	        OUT_AS2 (cse, w, %B0);
	      }
	    else
	      {
	        OUT_AS2 (cse, w, %A0);
	        OUT_AS1 (page, %2);
	        OUT_AS1 (jmp, %2);
	        OUT_AS2 (mov, w, %B1);
	        OUT_AS2 (cse, w, %B0);
	      }
	    OUT_AS1 (page, %2);
	    OUT_AS1 (jmp, %2);
	    if (! imm_cmp || (c != b))
	      OUT_AS2 (mov, w, %C1);
	    if (imm_cmp && (d == c))
	      {
	        OUT_AS2 (csne, w, %C0);
	        OUT_AS2 (cse, w, %D0);
	      }
	    else
	      {
	        OUT_AS2 (cse, w, %C0);
	        OUT_AS1 (page, %2);
	        OUT_AS1 (jmp, %2);
	        OUT_AS2 (mov, w, %D1);
	        OUT_AS2 (cse, w, %D0);
	      }
	    OUT_AS1 (page, %2);
	    OUT_AS1 (jmp, %2);
	  }
	  break;

	case GTU:
	  if (imm_sub)
	    {
	      /* > 0xffffffff never succeeds!  */
	      if ((unsigned HOST_WIDE_INT)(INTVAL (operands[1]) & 0xffffffff)
		  != 0xffffffff)
		{
	          operands[3] = GEN_INT (INTVAL (operands[1]) + 1);
	          OUT_AS2 (mov, w, %D3);
	          OUT_AS2 (sub, w, %D0);
	          OUT_AS2 (mov, w, %C3);
	          OUT_AS2 (subc, w, %C0);
	          OUT_AS2 (mov, w, %B3);
	          OUT_AS2 (subc, w, %B0);
	          OUT_AS2 (mov, w, %A3);
	          OUT_AS2 (subc, w, %A0);
	          OUT_AS1 (snc,);
	          OUT_AS1 (page, %2);
	          OUT_AS1 (jmp, %2);
		}
	    }
	  else
	    {
	      OUT_AS2 (mov, w, %D0);
	      OUT_AS2 (sub, w, %D1);
	      OUT_AS2 (mov, w, %C0);
	      OUT_AS2 (subc, w, %C1);
	      OUT_AS2 (mov, w, %B0);
	      OUT_AS2 (subc, w, %B1);
	      OUT_AS2 (mov, w, %A0);
	      OUT_AS2 (subc, w, %A1);
	      OUT_AS1 (sc,);
	      OUT_AS1 (page, %2);
	      OUT_AS1 (jmp, %2);
	    }
	  break;

	case GEU:
	  if (imm_sub)
	    {
	      if (INTVAL (operands[0]) == 0)
		{
                  OUT_AS2 (mov, w, %A1);
                  OUT_AS2 (or, w, %B1);
                  OUT_AS2 (or, w, %C1);
                  OUT_AS2 (or, w, %D1);
		  OUT_AS1 (snz,);
	          OUT_AS1 (page, %2);
	          OUT_AS1 (jmp, %2);
		}
	      else
	        {
	          operands[3] = GEN_INT (INTVAL (operands[0]) - 1);
	          OUT_AS2 (mov, w, %D3);
	          OUT_AS2 (sub, w, %D1);
	          OUT_AS2 (mov, w, %C3);
	          OUT_AS2 (subc, w, %C1);
	          OUT_AS2 (mov, w, %B3);
	          OUT_AS2 (subc, w, %B1);
	          OUT_AS2 (mov, w, %A3);
	          OUT_AS2 (subc, w, %A1);
	          OUT_AS1 (sc,);
	          OUT_AS1 (page, %2);
	          OUT_AS1 (jmp, %2);
		}
	    }
	  else
	    {
	      OUT_AS2 (mov, w, %D1);
	      OUT_AS2 (sub, w, %D0);
	      OUT_AS2 (mov, w, %C1);
	      OUT_AS2 (subc, w, %C0);
	      OUT_AS2 (mov, w, %B1);
	      OUT_AS2 (subc, w, %B0);
	      OUT_AS2 (mov, w, %A1);
	      OUT_AS2 (subc, w, %A0);
	      OUT_AS1 (snc,);
	      OUT_AS1 (page, %2);
	      OUT_AS1 (jmp, %2);
	    }
	  break;

	case LTU:
	  if (imm_sub)
	    {
	      if (INTVAL (operands[0]) == 0)
	        {
                  OUT_AS2 (mov, w, %A1);
                  OUT_AS2 (or, w, %B1);
                  OUT_AS2 (or, w, %C1);
                  OUT_AS2 (or, w, %D1);
		  OUT_AS1 (sz,);
	          OUT_AS1 (page, %2);
	          OUT_AS1 (jmp, %2);
		}
	      else
	        {
	          operands[3] = GEN_INT (INTVAL (operands[0]) - 1);
	          OUT_AS2 (mov, w, %D3);
	          OUT_AS2 (sub, w, %D1);
	          OUT_AS2 (mov, w, %C3);
	          OUT_AS2 (subc, w, %C1);
	          OUT_AS2 (mov, w, %B3);
	          OUT_AS2 (subc, w, %B1);
	          OUT_AS2 (mov, w, %A3);
	          OUT_AS2 (subc, w, %A1);
	          OUT_AS1 (snc,);
	          OUT_AS1 (page, %2);
	          OUT_AS1 (jmp, %2);
	        }
	    }
	  else
	    {
	      OUT_AS2 (mov, w, %D1);
	      OUT_AS2 (sub, w, %D0);
	      OUT_AS2 (mov, w, %C1);
	      OUT_AS2 (subc, w, %C0);
	      OUT_AS2 (mov, w, %B1);
	      OUT_AS2 (subc, w, %B0);
	      OUT_AS2 (mov, w, %A1);
	      OUT_AS2 (subc, w, %A0);
	      OUT_AS1 (sc,);
	      OUT_AS1 (page, %2);
	      OUT_AS1 (jmp, %2);
	    }
	  break;

	case LEU:
	  if (imm_sub)
	    {
	      if ((unsigned HOST_WIDE_INT)(INTVAL (operands[1]) & 0xffffffff)
		  == 0xffffffff)
	        {
		  /* <= 0xffffffff always succeeds.  */
		  OUT_AS1 (page, %2);
	          OUT_AS1 (jmp, %2);
		}
	      else
		{
	          operands[3] = GEN_INT (INTVAL (operands[1]) + 1);
	          OUT_AS2 (mov, w, %D3);
	          OUT_AS2 (sub, w, %D0);
	          OUT_AS2 (mov, w, %C3);
	          OUT_AS2 (subc, w, %C0);
	          OUT_AS2 (mov, w, %B3);
	          OUT_AS2 (subc, w, %B0);
	          OUT_AS2 (mov, w, %A3);
	          OUT_AS2 (subc, w, %A0);
	          OUT_AS1 (sc,);
	          OUT_AS1 (page, %2);
	          OUT_AS1 (jmp, %2);
		}
	    }
	  else
	    {
	      OUT_AS2 (mov, w, %D0);
	      OUT_AS2 (sub, w, %D1);
	      OUT_AS2 (mov, w, %C0);
	      OUT_AS2 (subc, w, %C1);
	      OUT_AS2 (mov, w, %B0);
	      OUT_AS2 (subc, w, %B1);
	      OUT_AS2 (mov, w, %A0);
	      OUT_AS2 (subc, w, %A1);
	      OUT_AS1 (snc,);
	      OUT_AS1 (page, %2);
	      OUT_AS1 (jmp, %2);
	    }
	  break;

	default:
	  abort ();
        }
      break;

    case DImode:
      if (GET_CODE (operands[1]) == CONST_INT)
	{
	  const_low = INTVAL (operands[1]);
	  const_high = (const_low >= 0) - 1;
	}
      else if (GET_CODE (operands[1]) == CONST_DOUBLE)
	{
	  const_low = CONST_DOUBLE_LOW (operands[1]);
	  const_high = CONST_DOUBLE_HIGH (operands[1]);
	}
      switch (code)
        {
	case EQ:
	  {
	    unsigned char s = 0, t = 1, u = 2, v = 3;
	    unsigned char w = 4, x = 5, y = 6, z = 7;
	    if (optimize_size)
	      {
		if (GET_CODE (operands[0]) == MEM
		    && true_regnum (XEXP (operands[0], 0)) == REG_DP)
		  {
		    OUT_AS1 (push, %Z1%<);
		    OUT_AS1 (push, %Y1%<);
		    OUT_AS1 (push, %X1%<);
		    OUT_AS1 (push, %W1%<);
		    OUT_AS1 (push, %V1%<);
		    OUT_AS1 (push, %U1%<);
		    OUT_AS1 (push, %T1%<);
		    OUT_AS1 (push, %S1%>%>%>%>%>%>%>);
		    OUT_AS1 (page, __cmpdi2_dp);
		    OUT_AS1 (call, __cmpdi2_dp);
		    OUT_AS2 (csne, w, #1);
		    OUT_AS1 (page, %2);
		    OUT_AS1 (jmp, %2);
		  }
		else
		  {
		    OUT_AS1 (push, %Z1%<);
		    OUT_AS1 (push, %Y1%<);
		    OUT_AS1 (push, %X1%<);
		    OUT_AS1 (push, %W1%<);
		    OUT_AS1 (push, %V1%<);
		    OUT_AS1 (push, %U1%<);
		    OUT_AS1 (push, %T1%<);
		    OUT_AS1 (push, %S1%<);
		    OUT_AS1 (push, %Z0%<);
		    OUT_AS1 (push, %Y0%<);
		    OUT_AS1 (push, %X0%<);
		    OUT_AS1 (push, %W0%<);
		    OUT_AS1 (push, %V0%<);
		    OUT_AS1 (push, %U0%<);
		    OUT_AS1 (push, %T0%<);
		    OUT_AS1 (push, %S0%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>);
		    OUT_AS1 (page, __cmpdi2);
		    OUT_AS1 (call, __cmpdi2);
		    OUT_AS2 (csne, w, #1);
		    OUT_AS1 (page, %2);
		    OUT_AS1 (jmp, %2);
		  }
	      }
	    else
	      {
		if (imm_cmp)
		  {
		    s = (const_high >> 24) & 0xff;
		    t = (const_high >> 16) & 0xff;
		    u = (const_high >> 8) & 0xff;
		    v = const_high & 0xff;
		    w = (const_low >> 24) & 0xff;
		    x = (const_low >> 16) & 0xff;
		    y = (const_low >> 8) & 0xff;
		    z = const_low & 0xff;
		  }

		OUT_AS2 (mov, w, %S1);
		if (imm_cmp && (s == t))
		  {
		    OUT_AS2 (csne, w, %S0);
		    OUT_AS2 (cse, w, %T0);
		  }
		else
		  {
		    OUT_AS2 (cse, w, %S0);
		    OUT_AS1 (page, 2f);
		    OUT_AS1 (jmp, 2f);
		    OUT_AS2 (mov, w, %T1);
		    OUT_AS2 (cse, w, %T0);
		  }
		OUT_AS1 (page, 2f);
		OUT_AS1 (jmp, 2f);

		OUT_AS2 (mov, w, %U1);
		if (imm_cmp && (u == v))
		  {
		    OUT_AS2 (csne, w, %U0);
		    OUT_AS2 (cse, w, %V0);
		  }
		else
		  {
		    OUT_AS2 (cse, w, %U0);
		    OUT_AS1 (page, 2f);
		    OUT_AS1 (jmp, 2f);
		    OUT_AS2 (mov, w, %V1);
		    OUT_AS2 (cse, w, %V0);
		  }
		OUT_AS1 (page, 2f);
		OUT_AS1 (jmp, 2f);

		OUT_AS2 (mov, w, %W1);
		if (imm_cmp && (w == x))
		  {
		    OUT_AS2 (csne, w, %W0);
		    OUT_AS2 (cse, w, %X0);
		  }
		else
		  {
		    OUT_AS2 (cse, w, %W0);
		    OUT_AS1 (page, 2f);
		    OUT_AS1 (jmp, 2f);
		    OUT_AS2 (mov, w, %X1);
		    OUT_AS2 (cse, w, %X0);
		  }
		OUT_AS1 (page, 2f);
		OUT_AS1 (jmp, 2f);

		if (! imm_cmp || (x != y))
		  OUT_AS2 (mov, w, %Y1);
		OUT_AS2 (cse, w, %Y0);
		OUT_AS1 (page, 2f);
		OUT_AS1 (jmp, 2f);
		if (! imm_cmp || (z != y))
		  OUT_AS2 (mov, w, %Z1);
		OUT_AS2 (csne, w, %Z0);
		OUT_AS1 (page, %2);
		OUT_AS1 (jmp, %2);
		OUT_AS1 (2:,);
	      }
	  }
	  break;

	case NE:
	  {
	    unsigned char s = 0, t = 1, u = 2, v = 3;
	    unsigned char w = 4, x = 5, y = 6, z = 7;
	    
	    if (optimize_size)
	      {
		if (GET_CODE (operands[0]) == MEM
		    && true_regnum (XEXP (operands[0], 0)) == REG_DP)
		  {
		    OUT_AS1 (push, %Z1%<);
		    OUT_AS1 (push, %Y1%<);
		    OUT_AS1 (push, %X1%<);
		    OUT_AS1 (push, %W1%<);
		    OUT_AS1 (push, %V1%<);
		    OUT_AS1 (push, %U1%<);
		    OUT_AS1 (push, %T1%<);
		    OUT_AS1 (push, %S1%>%>%>%>%>%>%>);
		    OUT_AS1 (page, __cmpdi2_dp);
		    OUT_AS1 (call, __cmpdi2_dp);
		    OUT_AS2 (cse, w, #1);
		    OUT_AS1 (page, %2);
		    OUT_AS1 (jmp, %2);
		  }
		else
		  {
		    OUT_AS1 (push, %Z1%<);
		    OUT_AS1 (push, %Y1%<);
		    OUT_AS1 (push, %X1%<);
		    OUT_AS1 (push, %W1%<);
		    OUT_AS1 (push, %V1%<);
		    OUT_AS1 (push, %U1%<);
		    OUT_AS1 (push, %T1%<);
		    OUT_AS1 (push, %S1%<);
		    OUT_AS1 (push, %Z0%<);
		    OUT_AS1 (push, %Y0%<);
		    OUT_AS1 (push, %X0%<);
		    OUT_AS1 (push, %W0%<);
		    OUT_AS1 (push, %V0%<);
		    OUT_AS1 (push, %U0%<);
		    OUT_AS1 (push, %T0%<);
		    OUT_AS1 (push, %S0%>%>%>%>%>%>%>%>%>%>%>%>%>%>%>);
		    OUT_AS1 (page, __cmpdi2);
		    OUT_AS1 (call, __cmpdi2);
		    OUT_AS2 (cse, w, #1);
		    OUT_AS1 (page, %2);
		    OUT_AS1 (jmp, %2);
		  }
	      }
	    else
	      {
		if (imm_cmp)
		  {
		    s = (const_high >> 24) & 0xff;
		    t = (const_high >> 16) & 0xff;
		    u = (const_high >> 8) & 0xff;
		    v = const_high & 0xff;
		    w = (const_low >> 24) & 0xff;
		    x = (const_low >> 16) & 0xff;
		    y = (const_low >> 8) & 0xff;
		    z = const_low & 0xff;
		  }

		OUT_AS2 (mov, w, %S1);
		if (imm_cmp && (s == t))
		  {
		    OUT_AS2 (csne, w, %S0);
		    OUT_AS2 (cse, w, %T0);
		  }
		else
		  {
		    OUT_AS2 (cse, w, %S0);
		    OUT_AS1 (page, %2);
		    OUT_AS1 (jmp, %2);
		    OUT_AS2 (mov, w, %T1);
		    OUT_AS2 (cse, w, %T0);
		  }
		OUT_AS1 (page, %2);
		OUT_AS1 (jmp, %2);

		OUT_AS2 (mov, w, %U1);
		if (imm_cmp && (u == v))
		  {
		    OUT_AS2 (csne, w, %U0);
		    OUT_AS2 (cse, w, %V0);
		  }
		else
		  {
		    OUT_AS2 (cse, w, %U0);
		    OUT_AS1 (page, %2);
		    OUT_AS1 (jmp, %2);
		    OUT_AS2 (mov, w, %V1);
		    OUT_AS2 (cse, w, %V0);
		  }
		OUT_AS1 (page, %2);
		OUT_AS1 (jmp, %2);

		OUT_AS2 (mov, w, %W1);
		if (imm_cmp && (w == x))
		  {
		    OUT_AS2 (csne, w, %W0);
		    OUT_AS2 (cse, w, %X0);
		  }
		else
		  {
		    OUT_AS2 (cse, w, %W0);
		    OUT_AS1 (page, %2);
		    OUT_AS1 (jmp, %2);
		    OUT_AS2 (mov, w, %X1);
		    OUT_AS2 (cse, w, %X0);
		  }
		OUT_AS1 (page, %2);
		OUT_AS1 (jmp, %2);

		if (! imm_cmp || (y != x))
		  OUT_AS2 (mov, w, %Y1);
		if (imm_cmp && (z == y))
		  {
		    OUT_AS2 (csne, w, %Y0);
		    OUT_AS2 (cse, w, %Z0);
		  }
		else
		  {
		    OUT_AS2 (cse, w, %Y0);
		    OUT_AS1 (page, %2);
		    OUT_AS1 (jmp, %2);
		    OUT_AS2 (mov, w, %Z1);
		    OUT_AS2 (cse, w, %Z0);
		  }
		OUT_AS1 (page, %2);
		OUT_AS1 (jmp, %2);
	      }
	  }
	  break;

	case GTU:
	  if (imm_sub)
	    {
	      /* > 0xffffffffffffffff never suceeds!  */
	      if (((const_high & 0xffffffff) != 0xffffffff)
		  || ((const_low & 0xffffffff) != 0xffffffff))
		{
	          operands[3] = GEN_INT (const_low + 1);
		  operands[4] = GEN_INT (const_high
					 + (INTVAL (operands[3]) ? 0 : 1));
	          OUT_AS2 (mov, w, %D3);
	          OUT_AS2 (sub, w, %Z0);
	          OUT_AS2 (mov, w, %C3);
	          OUT_AS2 (subc, w, %Y0);
	          OUT_AS2 (mov, w, %B3);
	          OUT_AS2 (subc, w, %X0);
	          OUT_AS2 (mov, w, %A3);
	          OUT_AS2 (subc, w, %W0);
	          OUT_AS2 (mov, w, %D4);
	          OUT_AS2 (subc, w, %V0);
	          OUT_AS2 (mov, w, %C4);
	          OUT_AS2 (subc, w, %U0);
	          OUT_AS2 (mov, w, %B4);
	          OUT_AS2 (subc, w, %T0);
	          OUT_AS2 (mov, w, %A4);
	          OUT_AS2 (subc, w, %S0);
	          OUT_AS1 (snc,);
	          OUT_AS1 (page, %2);
	          OUT_AS1 (jmp, %2);
		}
	    }
	  else
	    {
	      OUT_AS2 (mov, w, %Z0);
	      OUT_AS2 (sub, w, %Z1);
	      OUT_AS2 (mov, w, %Y0);
	      OUT_AS2 (subc, w, %Y1);
	      OUT_AS2 (mov, w, %X0);
	      OUT_AS2 (subc, w, %X1);
	      OUT_AS2 (mov, w, %W0);
	      OUT_AS2 (subc, w, %W1);
	      OUT_AS2 (mov, w, %V0);
	      OUT_AS2 (subc, w, %V1);
	      OUT_AS2 (mov, w, %U0);
	      OUT_AS2 (subc, w, %U1);
	      OUT_AS2 (mov, w, %T0);
	      OUT_AS2 (subc, w, %T1);
	      OUT_AS2 (mov, w, %S0);
	      OUT_AS2 (subc, w, %S1);
	      OUT_AS1 (sc,);
	      OUT_AS1 (page, %2);
	      OUT_AS1 (jmp, %2);
	    }
	  break;

	case GEU:
	  if (imm_sub)
	    {
	      HOST_WIDE_INT const_low0;
	      HOST_WIDE_INT const_high0;
	      
	      if (GET_CODE (operands[0]) == CONST_INT)
		{
		  const_low0 = INTVAL (operands[0]);
		  const_high0 = (const_low >= 0) - 1;
		}
	      else if (GET_CODE (operands[0]) == CONST_DOUBLE)
		{
		  const_low0 = CONST_DOUBLE_LOW (operands[0]);
		  const_high0 = CONST_DOUBLE_HIGH (operands[0]);
		}
	      
	      if (const_high0 == 0 && const_low0 == 0)
		{
                  OUT_AS2 (mov, w, %S1);
                  OUT_AS2 (or, w, %T1);
                  OUT_AS2 (or, w, %U1);
                  OUT_AS2 (or, w, %V1);
                  OUT_AS2 (or, w, %W1);
                  OUT_AS2 (or, w, %X1);
                  OUT_AS2 (or, w, %Y1);
                  OUT_AS2 (or, w, %Z1);
		  OUT_AS1 (snz,);
	          OUT_AS1 (page, %2);
	          OUT_AS1 (jmp, %2);
		}
	      else
	        {
	          operands[3] = GEN_INT (const_low0 - 1);
		  operands[4] = GEN_INT (const_high0 - (const_low0 ? 1 : 0));
	          OUT_AS2 (mov, w, %D3);
	          OUT_AS2 (sub, w, %Z1);
	          OUT_AS2 (mov, w, %C3);
	          OUT_AS2 (subc, w, %Y1);
	          OUT_AS2 (mov, w, %B3);
	          OUT_AS2 (subc, w, %X1);
	          OUT_AS2 (mov, w, %A3);
	          OUT_AS2 (subc, w, %W1);
	          OUT_AS2 (mov, w, %D4);
	          OUT_AS2 (subc, w, %V1);
	          OUT_AS2 (mov, w, %C4);
	          OUT_AS2 (subc, w, %U1);
	          OUT_AS2 (mov, w, %B4);
	          OUT_AS2 (subc, w, %T1);
	          OUT_AS2 (mov, w, %A4);
	          OUT_AS2 (subc, w, %S1);
	          OUT_AS1 (sc,);
	          OUT_AS1 (page, %2);
	          OUT_AS1 (jmp, %2);
		}
	    }
	  else
	    {
	      OUT_AS2 (mov, w, %Z1);
	      OUT_AS2 (sub, w, %Z0);
	      OUT_AS2 (mov, w, %Y1);
	      OUT_AS2 (subc, w, %Y0);
	      OUT_AS2 (mov, w, %X1);
	      OUT_AS2 (subc, w, %X0);
	      OUT_AS2 (mov, w, %W1);
	      OUT_AS2 (subc, w, %W0);
	      OUT_AS2 (mov, w, %V1);
	      OUT_AS2 (subc, w, %V0);
	      OUT_AS2 (mov, w, %U1);
	      OUT_AS2 (subc, w, %U0);
	      OUT_AS2 (mov, w, %T1);
	      OUT_AS2 (subc, w, %T0);
	      OUT_AS2 (mov, w, %S1);
	      OUT_AS2 (subc, w, %S0);
	      OUT_AS1 (snc,);
	      OUT_AS1 (page, %2);
	      OUT_AS1 (jmp, %2);
	    }
	  break;

	case LTU:
	  if (imm_sub)
	    {
	      HOST_WIDE_INT const_low0;
	      HOST_WIDE_INT const_high0;
	      
	      if (GET_CODE (operands[0]) == CONST_INT)
		{
		  const_low0 = INTVAL (operands[0]);
		  const_high0 = (const_low >= 0) - 1;
		}
	      else if (GET_CODE (operands[0]) == CONST_DOUBLE)
		{
		  const_low0 = CONST_DOUBLE_LOW (operands[0]);
		  const_high0 = CONST_DOUBLE_HIGH (operands[0]);
		}
	      
	      if (const_high0 == 0 && const_low0 == 0)
		{
                  OUT_AS2 (mov, w, %S1);
                  OUT_AS2 (or, w, %T1);
                  OUT_AS2 (or, w, %U1);
                  OUT_AS2 (or, w, %V1);
                  OUT_AS2 (or, w, %W1);
                  OUT_AS2 (or, w, %X1);
                  OUT_AS2 (or, w, %Y1);
                  OUT_AS2 (or, w, %Z1);
		  OUT_AS1 (sz,);
	          OUT_AS1 (page, %2);
	          OUT_AS1 (jmp, %2);
		}
	      else
	        {
	          operands[3] = GEN_INT (const_low0 - 1);
		  operands[4] = GEN_INT (const_high0 - (const_low0 ? 1 : 0));
	          OUT_AS2 (mov, w, %D3);
	          OUT_AS2 (sub, w, %Z1);
	          OUT_AS2 (mov, w, %C3);
	          OUT_AS2 (subc, w, %Y1);
	          OUT_AS2 (mov, w, %B3);
	          OUT_AS2 (subc, w, %X1);
	          OUT_AS2 (mov, w, %A3);
	          OUT_AS2 (subc, w, %W1);
	          OUT_AS2 (mov, w, %D4);
	          OUT_AS2 (subc, w, %V1);
	          OUT_AS2 (mov, w, %C4);
	          OUT_AS2 (subc, w, %U1);
	          OUT_AS2 (mov, w, %B4);
	          OUT_AS2 (subc, w, %T1);
	          OUT_AS2 (mov, w, %A4);
	          OUT_AS2 (subc, w, %S1);
	          OUT_AS1 (snc,);
	          OUT_AS1 (page, %2);
	          OUT_AS1 (jmp, %2);
	        }
	    }
	  else
	    {
	      OUT_AS2 (mov, w, %Z1);
	      OUT_AS2 (sub, w, %Z0);
	      OUT_AS2 (mov, w, %Y1);
	      OUT_AS2 (subc, w, %Y0);
	      OUT_AS2 (mov, w, %X1);
	      OUT_AS2 (subc, w, %X0);
	      OUT_AS2 (mov, w, %W1);
	      OUT_AS2 (subc, w, %W0);
	      OUT_AS2 (mov, w, %V1);
	      OUT_AS2 (subc, w, %V0);
	      OUT_AS2 (mov, w, %U1);
	      OUT_AS2 (subc, w, %U0);
	      OUT_AS2 (mov, w, %T1);
	      OUT_AS2 (subc, w, %T0);
	      OUT_AS2 (mov, w, %S1);
	      OUT_AS2 (subc, w, %S0);
	      OUT_AS1 (sc,);
	      OUT_AS1 (page, %2);
	      OUT_AS1 (jmp, %2);
	    }
	  break;

	case LEU:
	  if (imm_sub)
	    {
	      if (((const_high & 0xffffffff) == 0xffffffff)
		  && ((const_low & 0xffffffff) == 0xffffffff))
	        {
		  /* <= 0xffffffffffffffff always suceeds.  */
		  OUT_AS1 (page, %2);
	          OUT_AS1 (jmp, %2);
		}
	      else
		{
	          operands[3] = GEN_INT (const_low + 1);
		  operands[4] = GEN_INT (const_high
					 + (INTVAL (operands[3]) ? 0 : 1));
	          OUT_AS2 (mov, w, %D3);
	          OUT_AS2 (sub, w, %Z0);
	          OUT_AS2 (mov, w, %C3);
	          OUT_AS2 (subc, w, %Y0);
	          OUT_AS2 (mov, w, %B3);
	          OUT_AS2 (subc, w, %X0);
	          OUT_AS2 (mov, w, %A3);
	          OUT_AS2 (subc, w, %W0);
	          OUT_AS2 (mov, w, %D4);
	          OUT_AS2 (subc, w, %V0);
	          OUT_AS2 (mov, w, %C4);
	          OUT_AS2 (subc, w, %U0);
	          OUT_AS2 (mov, w, %B4);
	          OUT_AS2 (subc, w, %T0);
	          OUT_AS2 (mov, w, %A4);
	          OUT_AS2 (subc, w, %S0);
	          OUT_AS1 (sc,);
	          OUT_AS1 (page, %2);
	          OUT_AS1 (jmp, %2);
		}
	    }
	  else
	    {
	      OUT_AS2 (mov, w, %Z0);
	      OUT_AS2 (sub, w, %Z1);
	      OUT_AS2 (mov, w, %Y0);
	      OUT_AS2 (subc, w, %Y1);
	      OUT_AS2 (mov, w, %X0);
	      OUT_AS2 (subc, w, %X1);
	      OUT_AS2 (mov, w, %W0);
	      OUT_AS2 (subc, w, %W1);
	      OUT_AS2 (mov, w, %V0);
	      OUT_AS2 (subc, w, %V1);
	      OUT_AS2 (mov, w, %U0);
	      OUT_AS2 (subc, w, %U1);
	      OUT_AS2 (mov, w, %T0);
	      OUT_AS2 (subc, w, %T1);
	      OUT_AS2 (mov, w, %S0);
	      OUT_AS2 (subc, w, %S1);
	      OUT_AS1 (snc,);
	      OUT_AS1 (page, %2);
	      OUT_AS1 (jmp, %2);
	    }
	  break;

	default:
	  abort ();
        }
      break;

    default:
      abort ();
  }
#undef operands
  return "";
}

/* Output rtx VALUE as .byte to file FILE.  */

void
asm_output_char (FILE *file, rtx value)
{
  fprintf (file, "\t.byte ");
  output_addr_const (file, value);
  fprintf (file, "\n");
}


/* Output VALUE as .byte to file FILE.  */

void
asm_output_byte (FILE *file, int value)
{
  fprintf (file, "\t.byte 0x%x\n",value & 0xff);
}


/* Output rtx VALUE as .word to file FILE.  */

void
asm_output_short (FILE *file, rtx value)
{
  fprintf (file, "\t.word ");
  output_addr_const (file, (value));
  fprintf (file, "\n");
}


/* Output real N to file FILE.  */

void
asm_output_float (FILE *file, REAL_VALUE_TYPE n)
{
  long val;
  char dstr[100];
  
  REAL_VALUE_TO_TARGET_SINGLE (n, val);
  real_to_decimal (dstr, &n, sizeof (dstr), 0, 1);

  fprintf (file, "\t.long 0x%08lx\t/* %s */\n", val, dstr);
}

/* Sets section name for declaration DECL.  */
  
void
unique_section (tree decl, int reloc ATTRIBUTE_UNUSED)
{
  int len;
  const char *name;
  char *string;
  const char *prefix;
  name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
  /* Strip off any encoding in name.  */
  name = (* targetm.strip_name_encoding) (name);

  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      if (flag_function_sections)
	prefix = ".text.";
      else
	prefix = ".text";
    }
  else 
    abort ();

  if (flag_function_sections)
    {
      len = strlen (name) + strlen (prefix);
      string = alloca (len + 1);
      sprintf (string, "%s%s", prefix, name);
      DECL_SECTION_NAME (decl) = build_string (len, string);
    }
}

/* Return value is nonzero if pseudos that have been
   assigned to registers of class CLASS would likely be spilled
   because registers of CLASS are needed for spill registers.  */

enum reg_class
class_likely_spilled_p (int c)
{
  return (c == IP_REGS
	  || c == IPL_REGS
	  || c == IPH_REGS
	  || c == DP_SP_REGS
	  || c == SP_REGS
	  || c == DP_REGS
	  || c == DPL_REGS
	  || c == DPH_REGS
	  || c == PTR_REGS);
}

/* Valid attributes:
   progmem - put data to program memory;
   naked     - don't generate function prologue/epilogue and `ret' command.

   Only `progmem' attribute valid for type.  */

const struct attribute_spec ip2k_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler } */
  { "progmem",   0, 0, false, false, false,  ip2k_handle_progmem_attribute },
  { "naked",     0, 0, true,  false, false,  ip2k_handle_fndecl_attribute },
  { NULL,        0, 0, false, false, false, NULL }
};

/* Handle a "progmem" attribute; arguments as in
   struct attribute_spec.handler.  */
static tree
ip2k_handle_progmem_attribute (tree *node, tree name,
			       tree args ATTRIBUTE_UNUSED,
			       int flags ATTRIBUTE_UNUSED,
			       bool *no_add_attrs)
{
  if (DECL_P (*node))
    {
      if (TREE_CODE (*node) == TYPE_DECL)
	{
	  /* This is really a decl attribute, not a type attribute,
	     but try to handle it for GCC 3.0 backwards compatibility.  */

	  tree type = TREE_TYPE (*node);
	  tree attr = tree_cons (name, args, TYPE_ATTRIBUTES (type));
	  tree newtype = build_type_attribute_variant (type, attr);

	  TYPE_MAIN_VARIANT (newtype) = TYPE_MAIN_VARIANT (type);
	  TREE_TYPE (*node) = newtype;
	  *no_add_attrs = true;
	}
      else if (TREE_STATIC (*node) || DECL_EXTERNAL (*node))
	{
	  if (DECL_INITIAL (*node) == NULL_TREE && !DECL_EXTERNAL (*node))
	    {
	      warning ("only initialized variables can be placed into "
		       "program memory area");
	      *no_add_attrs = true;
	    }
	}
      else
	{
	  warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
	  *no_add_attrs = true;
	}
    }

  return NULL_TREE;
}

/* Handle an attribute requiring a FUNCTION_DECL; arguments as in
   struct attribute_spec.handler.  */
static tree
ip2k_handle_fndecl_attribute (tree *node, tree name,
			      tree args ATTRIBUTE_UNUSED,
			      int flags ATTRIBUTE_UNUSED,
			      bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning ("`%s' attribute only applies to functions",
	       IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Cost functions.  */

/* Compute a (partial) cost for rtx X.  Return true if the complete
   cost has been computed, and false if subexpressions should be
   scanned.  In either case, *TOTAL contains the cost result.  */

static bool
ip2k_rtx_costs (rtx x, int code, int outer_code, int *total)
{
  enum machine_mode mode = GET_MODE (x);
  int extra_cost = 0;

  switch (code)
    {
    case CONST_INT:
    case CONST_DOUBLE:
    case LABEL_REF:
      *total = 0;
      return true;
    case CONST:
    case SYMBOL_REF:
      *total = 8;
      return true;

    case MEM:
      *total = ip2k_address_cost (XEXP (x, 0));
      return true;

    case ROTATE:
    case ROTATERT:
    case ASHIFT:
    case LSHIFTRT:
    case ASHIFTRT:
      if (GET_CODE (XEXP (x, 1)) == CONST_INT)
	{
	  int val = INTVAL (XEXP (x, 1));
	  int cost;

	  /* Shift by const instructions are proportional to
	     the shift count modulus 8.  Note that we increase the mode
	     size multiplier by 1 to account for clearing the carry flag.  */
	  cost = COSTS_N_INSNS (abs (val) % 8);
	  cost += rtx_cost (XEXP (x, 0), code);
	  cost *= (GET_MODE_SIZE (mode) + 1);

	  /* Sign-preserving shifts require 2 extra instructions.  */
	  if (code == ASHIFT)
            cost += COSTS_N_INSNS (2);

	  *total = cost;
	  return true;
	}
      *total = rtx_cost (XEXP (x, 0), code);
      *total += COSTS_N_INSNS (GET_MODE_SIZE (mode) * 8);
      return true;

    case MINUS:
    case PLUS:
    case AND:
    case XOR:
    case IOR:
      *total = COSTS_N_INSNS (GET_MODE_SIZE (mode) * 3);
      return false;

    case MOD:
    case DIV:
      if (mode == QImode)
	*total = COSTS_N_INSNS (20);
      else if (mode == HImode)
	*total = COSTS_N_INSNS (60);
      else if (mode == SImode)
	*total = COSTS_N_INSNS (180);
      else
	*total = COSTS_N_INSNS (540);
      return true;

    case MULT:
      /* These costs are OK, but should really handle subtle cases
         where we're using sign or zero extended args as these are
	 *much* cheaper than those given below!  */
      if (mode == QImode)
	*total = COSTS_N_INSNS (4);
      else if (mode == HImode)
	*total = COSTS_N_INSNS (12);
      else if (mode == SImode)
	*total = COSTS_N_INSNS (36);
      else
        *total = COSTS_N_INSNS (108);
      return true;

    case NEG:
    case SIGN_EXTEND:
      extra_cost = COSTS_N_INSNS (GET_MODE_SIZE (mode));
      
      /* Fall through.  */
    case NOT:
    case COMPARE:
    case ABS:
      *total = extra_cost + COSTS_N_INSNS (GET_MODE_SIZE (mode) * 2);
      return false;

    case TRUNCATE:
    case ZERO_EXTEND:
      if (outer_code == SET)
	{
	  *total = COSTS_N_INSNS (GET_MODE_SIZE (mode) * 3 / 2);
	  return false;
	}
      else
	{
	  *total = -(COSTS_N_INSNS (GET_MODE_SIZE (mode)) / 2);
	  return true;
	}

    case IF_THEN_ELSE:
      *total = rtx_cost (XEXP (x, 0), code) + COSTS_N_INSNS (2);
      return true;

    case EQ:
    case NE:
    case LTU:
    case GTU:
    case LEU:
    case GEU:
    case LT:
    case GT:
    case LE:
    case GE:
      *total = 0;
      return false;

    default:
      *total = COSTS_N_INSNS (4);
      return true;
    }
}

/* Calculate the cost of a memory address.  */

static int
ip2k_address_cost (rtx x)
{
  switch (legitimate_address_p (VOIDmode, x, 0))
    {
    case 'S':			/* Very low cost - (IP), (SP+N) or (DP+N)  */
      return 8;
      
    case 'R':			/* Indirected through IP.  */
      return 8;

    case 'L':			/* Label references.  */
      return 0;
      
    case 'C':			/* Constants and symbol references.  */
      return 4;

    default:
      return 1000;		/* Must reload.  */
    }
}

/* As part of the machine-dependent reorg we look for opcode sequences where
   we do some operation and then move the results back to one of the original
   source operands.  With working on the source operand directly is probably
   much cheaper and the move from this to the original source operand will be
   no more expensive than the original move.  */

#ifdef IP2K_MD_REORG_PASS
static void
mdr_resequence_xy_yx (first_insn)
     rtx first_insn;
{
  rtx insn;

  for (insn = first_insn; insn; insn = next_nonnote_insn (insn))
    {
      rtx set;

      if (GET_CODE (insn) != INSN)
	continue;

      set = (GET_CODE (PATTERN (insn)) == SET) ? PATTERN (insn) : NULL_RTX;
      if (set == NULL_RTX)
	continue;

      /* Look for operations that tend to be very cheap to run when the source
       * and dest args are the same because the IP2022 has opcodes that can
	 operate on the source directly.  If we have to spill through the W
	 register then we've possibly not got a good case for doing this.  */
      if ((GET_CODE (XEXP (set, 0)) == REG
	   || GET_CODE (XEXP (set, 0)) == MEM)
          && (GET_CODE (XEXP (set, 1)) == ASHIFT
	      || GET_CODE (XEXP (set, 1)) == ASHIFTRT
	      || GET_CODE (XEXP (set, 1)) == LSHIFTRT
	      || GET_CODE (XEXP (set, 1)) == XOR
	      || GET_CODE (XEXP (set, 1)) == IOR
	      || GET_CODE (XEXP (set, 1)) == AND
	      || GET_CODE (XEXP (set, 1)) == PLUS
	      || GET_CODE (XEXP (set, 1)) == MINUS
	      || GET_CODE (XEXP (set, 1)) == MULT))
	{
          rtx set2;
	  rtx next_insn;

	  next_insn = next_nonnote_insn (insn);
	  if (! next_insn)
	    continue;

          if (GET_CODE (next_insn) != INSN)
            continue;

          set2 = ((GET_CODE (PATTERN (next_insn)) == SET)
		  ? PATTERN (next_insn) : NULL_RTX);
          if (set2 == NULL_RTX)
            continue;

	  if ((GET_CODE (XEXP (XEXP (set, 1), 0)) == REG
	       || GET_CODE (XEXP (XEXP (set, 1), 0)) == MEM)
	      && rtx_equal_p (XEXP (set2, 0), XEXP (XEXP (set, 1), 0))
	      && rtx_equal_p (XEXP (set2, 1), XEXP (set, 0)))
	    {
	      rtx next2_insn;
	      rtx b_insn;

	      b_insn = gen_rtx_SET (VOIDmode,
				    XEXP (XEXP (set, 1), 0),
				    gen_rtx_fmt_ee (GET_CODE (XEXP (set, 1)),
						    GET_MODE (XEXP (set, 0)),
						    XEXP (XEXP (set, 1), 0),
						    XEXP (XEXP (set, 1), 1)));
		
	      emit_insn_before (b_insn, insn);
	      b_insn = gen_rtx_SET (GET_MODE (XEXP (set, 0)), XEXP (set, 0),
				    XEXP (XEXP (set, 1), 0));
	      next2_insn = emit_insn_before (b_insn, insn);
	      delete_insn (insn);
	      delete_insn (next_insn);
	      insn = next2_insn;
	      continue;
	    }

          /* Having tried with one operand of the expression, now, if
	     appropriate, try to do the same thing with the second operand.
	     Of course there are fewer operations that can match here
	     because they must be commutative.  */
          if (GET_RTX_CLASS (GET_CODE (XEXP (set, 1))) == 'c'
	      && (GET_CODE (XEXP (XEXP (set, 1), 1)) == REG
	          || GET_CODE (XEXP (XEXP (set, 1), 1)) == MEM)
	      && rtx_equal_p (XEXP (set2, 0), XEXP (XEXP (set, 1), 1))
	      && rtx_equal_p (XEXP (set2, 1), XEXP (set, 0)))
	    {
	      rtx rtx_ee;
	      rtx next2_insn;
	      int swap_args;

	      /* Try to ensure that we put things in a canonical form.  */
	      swap_args = (GET_CODE (XEXP (XEXP (set, 1), 0)) == REG
	      		   || GET_CODE (XEXP (XEXP (set, 1), 0)) == MEM);
	      rtx_ee = gen_rtx_fmt_ee (GET_CODE (XEXP (set, 1)),
	      			       GET_MODE (XEXP (set, 0)),
				       XEXP (XEXP (set, 1), swap_args ? 1 : 0),
				       XEXP (XEXP (set, 1),
					     swap_args ? 0 : 1));
	      
	      emit_insn_before (gen_rtx_SET (VOIDmode,
					     XEXP (XEXP (set, 1), 1),
					     rtx_ee),
			        insn);
	      next2_insn = emit_insn_before (gen_rtx_SET
					     (GET_MODE (XEXP (set, 0)),
					      XEXP (set, 0),
					      XEXP (XEXP (set, 1), 1)),
			                     insn);
	      delete_insn (insn);
	      delete_insn (next_insn);
	      insn = next2_insn;
	    }
	}
    }
}

/* Replace and recurse until we've tried QImode pieces!  */

static void
mdr_pres_replace_and_recurse (orig, with, insn)
     rtx orig;
     rtx with;
     rtx insn;
{
  enum machine_mode new_mode;

  validate_replace_rtx (orig, with, insn);

  switch (GET_MODE (orig))
    {
    case DImode:
    case DFmode:
      new_mode = SImode;
      break;

    case SImode:
    case SFmode:
      new_mode = HImode;
      break;

    case HImode:
      new_mode = QImode;
      break;

    default:
      return;
    }

  mdr_pres_replace_and_recurse (ip2k_get_low_half (orig, new_mode),
		  		ip2k_get_low_half (with, new_mode),
				insn);
  mdr_pres_replace_and_recurse (ip2k_get_high_half (orig, new_mode),
		  		ip2k_get_high_half (with, new_mode),
				insn);
}

/* Assist the following function, mdr_propagate_reg_equivs().  */

static void
mdr_propagate_reg_equivs_sequence (first_insn, orig, equiv)
     rtx first_insn;
     rtx orig;
     rtx equiv;
{
  rtx try_insn;
  rtx try_equiv = equiv;

  /* First scan the RTL looking for anything else that might clobber what
     we're doing.  If we find anything then we can't do the replacement.  */
  for (try_insn = next_nonnote_insn (first_insn);
       try_insn; try_insn = next_nonnote_insn (try_insn))
    {
      rtx pattern;

      if (GET_CODE (try_insn) != JUMP_INSN && GET_CODE (try_insn) != INSN)
	continue;

      pattern = PATTERN (try_insn);
      if (GET_CODE (pattern) == PARALLEL)
	{
          int j;

          for (j = 0; j < XVECLEN (pattern, 0); j++)
	    {
              rtx px = XVECEXP (pattern, 0, j);

	      if (GET_CODE (px) == SET)
	        if (! ip2k_composite_xexp_not_uses_reg_p (XEXP (px, 0),
							  REGNO (orig),
							  GET_MODE_SIZE (GET_MODE (orig))))
	          return;
	    }
        }
      else if (GET_CODE (pattern) == SET)
	{
          if (! ip2k_composite_xexp_not_uses_reg_p (XEXP (pattern, 0),
			      			    REGNO (orig),
						    GET_MODE_SIZE (GET_MODE (orig))))
	    return;
	}
    }

  /* Once we've decided that we're safe to do the replacement then make the
     changes.  */
  for (try_insn = next_nonnote_insn (first_insn); try_insn;
       try_insn = next_nonnote_insn (try_insn))
    {
      rtx set;
      rtx new_equiv = NULL_RTX;

      if (GET_CODE (try_insn) != JUMP_INSN && GET_CODE (try_insn) != INSN)
	{
	  try_equiv = equiv;
	  continue;
	}

      set = ((GET_CODE (PATTERN (try_insn)) == SET)
	     ? PATTERN (try_insn) : NULL_RTX);
      if (set == NULL_RTX)
	continue;

      /* We look for a special case of "push" operations screwing our
         register equivalence when it's based on a stack slot.  We can
         track this one and replace the old equivalence expression with
         a new one.  */
      if (GET_CODE (XEXP (set, 0)) == MEM
	  && GET_CODE (XEXP (XEXP (set, 0), 0)) == POST_DEC
	  && REG_P (XEXP (XEXP (XEXP (set, 0), 0), 0))
	  && REGNO (XEXP (XEXP (XEXP (set, 0), 0), 0)) == REG_SP)
        {
	  /* XXX - need to ensure that we can track this without going
	     out of range!  */
	  HOST_WIDE_INT disp = (INTVAL (XEXP (XEXP (try_equiv, 0), 1))
			       + GET_MODE_SIZE (GET_MODE (XEXP (set, 0))));
	  new_equiv = gen_rtx_MEM (GET_MODE (try_equiv),
		                   gen_rtx_PLUS (Pmode,
					         gen_rtx_REG (HImode, REG_SP),
				                 GEN_INT (disp)));
        }

      /* The replacement process is somewhat complicated by the fact that we
         might be dealing with what were originally subregs and thus we have
	 to replace parts of our original expression!  */
      mdr_pres_replace_and_recurse (orig, try_equiv, try_insn);

      if (new_equiv != NULL_RTX)
	try_equiv = new_equiv;
    }
}

/* Try propagating register equivalences forwards.  It may be that we can
   replace a register use with an equivalent expression that already
   holds the same value and thus allow one or more register loads to
   be eliminated.  */

static void
mdr_propagate_reg_equivs (first_insn)
     rtx first_insn;
{
  rtx insn;
  rtx set;

  for (insn = first_insn; insn; insn = next_nonnote_insn (insn))
    {
      if (GET_CODE (insn) != INSN)
	continue;

      set = (GET_CODE (PATTERN (insn)) == SET) ? PATTERN (insn) : NULL_RTX;
      if (set == NULL_RTX)
        continue;

      /* Have we found a stack slot equivalence for a register?  */
      if (REG_P (XEXP (set, 0))
	  && REGNO (XEXP (set, 0)) >= 0x88
	  && GET_CODE (XEXP (set, 1)) == MEM
	  && GET_CODE (XEXP (XEXP (set, 1), 0)) == PLUS
	  && REG_P (XEXP (XEXP (XEXP (set, 1), 0), 0))
	  && REGNO (XEXP (XEXP (XEXP (set, 1), 0), 0)) == REG_SP
	  && find_reg_note (insn, REG_EQUIV, NULL_RTX))
	{
	  mdr_propagate_reg_equivs_sequence (insn, XEXP (set, 0),
					     XEXP (set, 1));
	}
    }
}

/* Structure used to track jump targets.  */

struct dpre_jump_targets
{
  int target;			/* Is this a jump target?  */
  int reach_count;		/* Number of ways we can reach this insn.  */
  int touch_count;		/* Number of times we've touched this
				   insns during scanning.  */
  rtx dp_equiv;			/* DP-equivalence at this point.  */
};

struct dpre_jump_targets *ip2k_dpre_jump_targets;

/* DP equivalence tracking used within DP reload elimination.  */

static int
track_dp_reload (insn, dp_current, dp_current_ok, modifying)
     rtx insn;
     rtx *dp_current;
     int dp_current_ok;
     int modifying;
{
  rtx set;

  if (GET_CODE (insn) != INSN)
    {
      *dp_current = NULL_RTX;
      return 1;
    }

  set = (GET_CODE (PATTERN (insn)) == SET) ? PATTERN (insn) : NULL_RTX;
  if (set == NULL_RTX)
    {
      *dp_current = NULL_RTX;
      return 1;
    }

  /* If we're pushing a PLUS or MINUS then it's a win if we can replace
     an expression for which DP is equivalent with DP.  This happens
     surprisingly often when we pass a pointer to a structure embedded
     within another structure.  */
  if (*dp_current != NULL_RTX
      && GET_CODE (XEXP (set, 0)) == MEM
      && GET_CODE (XEXP (XEXP (set, 0), 0)) == POST_DEC
      && GET_CODE (XEXP (XEXP (XEXP (set, 0), 0), 0)) == REG
      && REGNO (XEXP (XEXP (XEXP (set, 0), 0), 0)) == REG_SP
      && (GET_CODE (XEXP (set, 1)) == PLUS
	  || GET_CODE (XEXP (set, 1)) == MINUS)
      && GET_CODE (*dp_current) != SYMBOL_REF
      && GET_CODE (*dp_current) != LABEL_REF
      && GET_CODE (*dp_current) != CONST)
    {
      if (modifying)
        validate_replace_rtx (*dp_current, gen_rtx_REG (HImode, REG_DP), insn);
    }

  /* Look for DP being modified.  If it is, see if it's being changed
     to what it already is!  */
  if (GET_CODE (XEXP (set, 0)) == REG
      && REGNO (XEXP (set, 0)) == REG_DP
      && GET_MODE (XEXP (set, 0)) == HImode)
    {
      /* If this is an equivalence we can delete the new set operation.  */
      if (*dp_current != NULL_RTX
          && rtx_equal_p (XEXP (set, 1), *dp_current))
        {
	  if (modifying)
            delete_insn (insn);
        }
      else
        {
          /* If we've not found an equivalence we can look for a special
	     case where an operand of the expression that sets DP is
	     already equivalent to DP and in that circumstance we simplify
	     by replacing that expression with DP.  */
	  if (*dp_current != NULL_RTX
	      && GET_CODE (*dp_current) != SYMBOL_REF
	      && GET_CODE (*dp_current) != LABEL_REF
	      && GET_CODE (*dp_current) != CONST
	      && modifying)
            validate_replace_rtx (*dp_current, XEXP (set, 0), insn);

          /* Assuming that we're not loading DP from something that uses DP
             itself then we mark the new equivalence for DP.  If we did match
             DP then we can't re-use this one.  */
	  if (ip2k_xexp_not_uses_reg_p (XEXP (set, 1), REG_DP, 2))
	    {
	      *dp_current = XEXP (set, 1);
	      return 1;
	    }
	  else
	    {
              *dp_current = NULL_RTX;
	      return 1;
	    }
	}
    }
  else if (GET_CODE (XEXP (set, 0)) == REG
           && (REGNO (XEXP (set, 0)) == REG_DPL
               || REGNO (XEXP (set, 0)) == REG_DPH))
    {
      /* If we clobber part of DP then we've clobbered any equivalences!  */
      *dp_current = NULL_RTX;
      return 1;
    }
  else if (! ip2k_xexp_not_uses_reg_p (XEXP (set, 0), REG_SP, 2)
	   && *dp_current != NULL_RTX
	   && !ip2k_xexp_not_uses_reg_p (*dp_current, REG_SP, 2))
    {
      /* We look for a special case of "push" operations screwing up the
         setting of DP when it's based on the stack.  We can track this one
         and replace the old expression for DP with a new one.  */
      if (GET_CODE (XEXP (set, 0)) == MEM
	  && GET_CODE (XEXP (XEXP (set, 0), 0)) == POST_DEC
	  && GET_CODE (XEXP (XEXP (XEXP (set, 0), 0), 0)) == REG
	  && REGNO (XEXP (XEXP (XEXP (set, 0), 0), 0)) == REG_SP
	  && GET_CODE (*dp_current) == MEM
	  && GET_CODE (XEXP (*dp_current, 0)) == PLUS)
        {
	  /* XXX - need to ensure that we can track this without going
	     out of range!   */
	  HOST_WIDE_INT disp = (INTVAL (XEXP (XEXP (*dp_current, 0), 1))
				+ GET_MODE_SIZE (GET_MODE (XEXP (set, 0))));
          *dp_current = gen_rtx_MEM (HImode,
				     gen_rtx_PLUS (Pmode,
				 	           gen_rtx_REG(HImode, REG_SP),
						   GEN_INT (disp)));
	  return 1;
	}

      /* Now we look for writes to the stack.  We can determine if these will
	 affect the equivalence we're tracking for DP and if not then we can
	 keep tracking it.  */
      if (GET_CODE (XEXP (set, 0)) == MEM
	  && GET_CODE (*dp_current) == MEM)
        {
	  /* Look at the SP offsets and look for any overlaps.  */
          int dp_cur_sp_offs = INTVAL (XEXP (XEXP (*dp_current, 0), 1));
	  int set_sp_offs = INTVAL (XEXP (XEXP (XEXP (set, 0), 0), 1));

	  if (abs (dp_cur_sp_offs - set_sp_offs) < 2)
            {
	      *dp_current = NULL_RTX;
	      return 1;
	    }
	}
    }
  else if (GET_CODE (XEXP (set, 0)) == REG
	   && *dp_current != NULL_RTX
	   && !ip2k_xexp_not_uses_reg_p (*dp_current, REGNO (XEXP (set, 0)),
				 	 GET_MODE_SIZE (GET_MODE (XEXP (set,
									0)))))
    {
      /* If we've just clobbered all or part of a register reference that we
         were sharing for DP then we can't share it any more!  */
      *dp_current = NULL_RTX;
    }

  return dp_current_ok;
}

/* As part of the machine-dependent reorg we scan loads and reloads of
   DP to see where any are redundant.  This does happens because we
   are able to subsequently transform things in interesting ways.  Sometimes
   gcc also does unnecessary reloads too so we try to eliminate these too.  */

static void
mdr_try_dp_reload_elim (first_insn)
     rtx first_insn;
{
  rtx insn;
  struct dpre_jump_targets *djt;
  rtx dp_current;
  int incomplete_scan;
  int last_incomplete_scan;

  ip2k_dpre_jump_targets
    = (struct dpre_jump_targets *) xcalloc (get_max_uid (),
					    sizeof (struct dpre_jump_targets));

  /* First we scan to build up a list of all CODE_LABEL insns and we work out
     how many different ways we can reach them.  */
  for (insn = first_insn; insn; insn = next_nonnote_insn (insn))
    {
      if (GET_CODE (insn) == CODE_LABEL)
	{
          djt = &ip2k_dpre_jump_targets[INSN_UID (insn)];
          djt->target = 1;
          djt->reach_count = LABEL_NUSES (insn);
	  djt->touch_count = 0;
	  djt->dp_equiv = NULL_RTX;
	  if (! prev_nonnote_insn (insn)
	      || (prev_nonnote_insn (insn)
	          && GET_CODE (prev_nonnote_insn (insn)) != BARRIER))
            djt->reach_count++;
	}
    }

  /* Next we scan all of the ways of reaching the code labels to see
     what the DP register is equivalent to as we reach them.  If we find
     that they're the same then we keep noting the matched value.  We
     iterate around this until we reach a convergence on DP equivalences
     at all code labels - we have to be very careful not to be too
     optimistic!  */
  incomplete_scan = -1;
  do
    {
      int dp_current_ok = 0;
      last_incomplete_scan = incomplete_scan;
      dp_current = NULL_RTX;

      for (insn = first_insn; insn; insn = next_nonnote_insn (insn))
        {
	  /* If we have a code label then we need to see if we already know
	     what the equivalence is at this point.  If we do then we use it
	     immediately, but if we don't then we have a special case to track
	     when we hit a fallthrough-edge (label with no barrier preceding
	     it).  Any other accesses to the label must be from jump insns
	     and so they're handled elsewhere.  */
          if (GET_CODE (insn) == CODE_LABEL)
            {
              djt = &ip2k_dpre_jump_targets[INSN_UID (insn)];

	      /* If we're fully characterized the use the equivalence.  */
	      if (djt->touch_count == djt->reach_count)
		{
		  dp_current = djt->dp_equiv;
		  dp_current_ok = 1;
		  continue;
		}

	      /* If we have a known equivalence for DP as we reach the
	         fallthrough-edge then track this into the code label.  */
	      if (dp_current_ok
		  && (! prev_nonnote_insn (insn)
	              || (prev_nonnote_insn (insn)
	                  && GET_CODE (prev_nonnote_insn (insn)) != BARRIER)))
	        {
	          if (djt->touch_count == 0)
                    djt->dp_equiv = dp_current;

	          if (djt->touch_count < djt->reach_count)
	            {
	              djt->touch_count++;
	              if (! rtx_equal_p (djt->dp_equiv, dp_current))
			{
			  /* When we definitely know that we can't form an
			     equivalence for DP here we must clobber anything
			     that we'd started to track too.  */
                          djt->dp_equiv = NULL_RTX;
			  dp_current = NULL_RTX;
			  dp_current_ok = 1;
			}
	            }
	        }

	      /* If we've not completely characterized this code label then
	         be cautious and assume that we don't know what DP is
		 equivalent to.  */
	      if (djt->touch_count < djt->reach_count)
                {
	          dp_current = NULL_RTX;
	          dp_current_ok = 0;
	        }

              continue;
            }

	  /* If we've hit a jump insn then we look for either an address
	     vector (jump table) or for jump label references.  */
          if (GET_CODE (insn) == JUMP_INSN)
	    {
	      /* Don't attempt to track here if we don't have a known
	         equivalence for DP at this point.  */
              if (dp_current_ok)
		{
	          rtx pat = PATTERN (insn);
	          if (GET_CODE (pat) == ADDR_VEC)
                    {
	              int i;
	              int len = XVECLEN (pat, 0);

	              for (i = 0; i < len; i++)
	                {
			  rtx vec_insn = XEXP (XVECEXP (pat, 0, i), 0);
	                  djt = &ip2k_dpre_jump_targets [INSN_UID (vec_insn)];

       	                  if (djt->touch_count == 0)
                            djt->dp_equiv = dp_current;

		          if (djt->touch_count < djt->reach_count)
	                    {
	                      djt->touch_count++;
	                      if (! rtx_equal_p (djt->dp_equiv, dp_current))
                                djt->dp_equiv = NULL_RTX;
	                    }
	                }
	            }
	          else if (JUMP_LABEL (insn))
	            {
		      rtx j_insn = JUMP_LABEL (insn);
	              djt = &ip2k_dpre_jump_targets[INSN_UID (j_insn)];

		      if (djt->touch_count == 0)
                        djt->dp_equiv = dp_current;

	              if (djt->touch_count < djt->reach_count)
	                {
	                  djt->touch_count++;
	                  if (! rtx_equal_p (djt->dp_equiv, dp_current))
                            djt->dp_equiv = NULL_RTX;
	                }
	            }
		}

              continue;
            }

	  /* Anything other than a code labal or jump arrives here.
	     We try and track DP, but sometimes we might not be able to.  */
          dp_current_ok = track_dp_reload (insn, &dp_current,
					   dp_current_ok, 0);
        }

      /* When we're looking to see if we've finished we count the number of
         paths through the code labels where we weren't able to definitively
	 track DP.
	 This number is used to see if we're converging on a solution.
	 If this hits zero then we've fully converged, but if this stays the
	 same as last time then we probably can't make any further
	 progress.  */
      incomplete_scan = 0;
      for (insn = first_insn; insn; insn = next_nonnote_insn (insn))
        {
          if (GET_CODE (insn) == CODE_LABEL)
            {
              djt = &ip2k_dpre_jump_targets[INSN_UID (insn)];
	      if (djt->touch_count != djt->reach_count)
		{
		  incomplete_scan += (djt->reach_count - djt->touch_count);
		  djt->dp_equiv = NULL_RTX;
		  djt->touch_count = 0;
		}
	    }
	}
    }
  while (incomplete_scan && incomplete_scan != last_incomplete_scan);

  /* Finally we scan the whole function and run DP elimination.  When we hit
     a CODE_LABEL we pick up any stored equivalence since we now know that
     every path to this point entered with DP holding the same thing!  If
     we subsequently have a reload that matches then we can eliminate it.  */
  dp_current = NULL_RTX;
  for (insn = first_insn; insn; insn = next_nonnote_insn (insn))
    {
      if (GET_CODE (insn) == JUMP_INSN)
        continue;

      if (GET_CODE (insn) == CODE_LABEL)
	{
          djt = &ip2k_dpre_jump_targets[INSN_UID (insn)];
	  dp_current = djt->dp_equiv;
          continue;
	}

      track_dp_reload (insn, &dp_current, 1, 1);
    }

  free (ip2k_dpre_jump_targets);
}

/* As part of the machine-dependent reorg we look for reloads of DP
   that we can move to earlier points within the file.
   Moving these out of the way allows more peepholes to match.  */

static void
mdr_try_move_dp_reload (first_insn)
     rtx first_insn;
{
  rtx insn;
  rtx set;
  rtx orig_first;

  /* Don't try to match the first instruction because we can't move it
     anyway.  */
  orig_first = first_insn;
  first_insn = next_nonnote_insn (first_insn);
  
  for (insn = first_insn; insn; insn = next_nonnote_insn (insn))
    {
      if (GET_CODE (insn) != INSN)
	continue;

      set = (GET_CODE (PATTERN (insn)) == SET) ? PATTERN (insn) : NULL_RTX;
      if (set == NULL_RTX)
	continue;

      /* Look for DP being loaded.  When we find this we start a rewind
         scan looking for possible positions to move this to.  */
      if (GET_CODE (XEXP (set, 0)) == REG
          && REGNO (XEXP (set, 0)) == REG_DP
	  && GET_MODE (XEXP (set, 0)) == HImode)
        {
	  int try_again;
	  rtx try_insn = insn;

	  do
	    {
              rtx rewind;
	      rtx check;

	      try_again = 0;
	      
	      /* For now we do the *really* simple version of things and only
	         attempt to move the load of DP if it's very safe to do so.  */
	      rewind = prev_nonnote_insn (try_insn);
	      if (rewind != orig_first && rewind != NULL_RTX
		  && GET_CODE (rewind) == INSN)
	        {
                  check = ((GET_CODE (PATTERN (rewind)) == SET)
			   ? PATTERN (rewind) : NULL_RTX);
		  if (check != NULL_RTX
		      && ip2k_composite_xexp_not_uses_cc0_p (XEXP (check, 0))
		      && ip2k_composite_xexp_not_uses_cc0_p (XEXP (check, 1)))
		    {
		      if (GET_CODE (XEXP (check, 0)) == REG
		          && REGNO (XEXP (check, 0)) != REG_DPH
		          && REGNO (XEXP (check, 0)) != REG_DPL
		          && (ip2k_composite_xexp_not_uses_reg_p
			      (XEXP (check, 1), REG_DP, 2))
		          && (ip2k_composite_xexp_not_uses_reg_p
			      (XEXP (set, 1),
			       REGNO (XEXP (check, 0)),
			       GET_MODE_SIZE (GET_MODE (XEXP (check, 0))))))
		        {
		          emit_insn_before (set, rewind);
			  if (try_insn == insn)
			    insn = prev_nonnote_insn (insn);
		          delete_insn (try_insn);
			  try_insn = prev_nonnote_insn (rewind);
			  try_again = 1;
		        }
		      else if (GET_CODE (XEXP (set, 1)) == REG
		               && ip2k_composite_xexp_not_uses_reg_p (XEXP (check, 1), REG_DP, 2)
		               && ip2k_composite_xexp_not_uses_reg_p (XEXP (check, 0), REG_DP, 2)
		               && ip2k_composite_xexp_not_uses_reg_p (XEXP (check, 0), REGNO (XEXP (set, 1)),
			      			                      GET_MODE_SIZE (GET_MODE (XEXP (set, 1)))))
		        {
		          emit_insn_before (set, rewind);
			  if (try_insn == insn)
			    insn = prev_nonnote_insn (insn);
		          delete_insn (try_insn);
			  try_insn = prev_nonnote_insn (rewind);
			  try_again = 1;
		        }
		    }
	        }
	    }
	  while (try_again && try_insn);
	}
    }
}
#endif /* IP2K_MD_REORG_PASS */

/* Look to see if the expression, x, can have any stack references offset by
   a fixed constant, offset.  If it definitely can then returns nonzero.  */

static int
ip2k_check_can_adjust_stack_ref (rtx x, int offset)
{
  if (GET_RTX_CLASS (GET_CODE (x)) == '2'
      || GET_RTX_CLASS (GET_CODE (x)) == 'c')
    return (ip2k_check_can_adjust_stack_ref (XEXP (x, 0), offset)
	    && ip2k_check_can_adjust_stack_ref (XEXP (x, 1), offset));

  if (GET_RTX_CLASS (GET_CODE (x)) == '1')
    return ip2k_check_can_adjust_stack_ref (XEXP (x, 0), offset);

  switch (GET_CODE (x))
    {
    case REG:
      return (REGNO (x) != REG_SPH && REGNO (x) != REG_SPL);

    case MEM:
      if (GET_CODE (XEXP (x, 0)) != PLUS)
	return 1;

      if (GET_CODE (XEXP (XEXP (x, 0), 0)) != REG)
	return 1;

      if (REGNO (XEXP (XEXP (x, 0), 0)) != REG_SP)
	return 1;

      /* We can't allow this if the adjustment will create an
         invalid address.  */
      return (INTVAL (XEXP (XEXP (x, 0), 1))
	      + offset <= (128 - 2 * GET_MODE_SIZE (GET_MODE (x))));

    case CONST:
    case CONST_INT:
    case CONST_DOUBLE:
    case SYMBOL_REF:
    case LABEL_REF:
      return 1;

    default:
      return 0;
    }
}

/* Adjusts all of the stack references in the expression pointed to by x by
   a fixed offset.  */

static void
ip2k_adjust_stack_ref (rtx *x, int offset)
{
  if (GET_RTX_CLASS (GET_CODE (*x)) == '2'
      || GET_RTX_CLASS (GET_CODE (*x)) == 'c')
    {
      ip2k_adjust_stack_ref (&XEXP (*x, 0), offset);
      ip2k_adjust_stack_ref (&XEXP (*x, 1), offset);
      return;
    }

  if (GET_RTX_CLASS (GET_CODE (*x)) == '1')
    {
      ip2k_adjust_stack_ref (&XEXP (*x, 0), offset);
      return;
    }

  switch (GET_CODE (*x))
    {
    case MEM:
      if (GET_CODE (XEXP (*x, 0)) != PLUS)
	return;

      if (GET_CODE (XEXP (XEXP (*x, 0), 0)) != REG)
	return;

      if (REGNO (XEXP (XEXP (*x, 0), 0)) != REG_SP)
	return;

      *x = copy_rtx (*x);
      XEXP (XEXP (*x, 0), 1) = GEN_INT (INTVAL (XEXP (XEXP (*x, 0), 1))
					+ offset);
      break;

    default:
      break;
    }
}

#ifdef IP2K_MD_REORG_PASS
/* As part of the machine-dependent reorg we look to move push instructions
   to earlier points within the file.  Moving these out of the way allows more
   peepholes to match.  */

static void
mdr_try_move_pushes (first_insn)
     rtx first_insn;
{
  rtx insn;
  rtx set;
  rtx orig_first;

  /* Don't try to match the first instruction because we can't move
     it anyway.  */
  orig_first = first_insn;
  first_insn = next_nonnote_insn (first_insn);
  
  for (insn = first_insn; insn; insn = next_nonnote_insn (insn))
    {
      if (GET_CODE (insn) != INSN)
	continue;

      set = (GET_CODE (PATTERN (insn)) == SET) ? PATTERN (insn) : NULL_RTX;
      if (set == NULL_RTX)
        continue;

      /* Have we found a push instruction?  */
      if (GET_CODE (XEXP (set, 0)) == MEM
	  && GET_CODE (XEXP (XEXP (set, 0), 0)) == POST_DEC
	  && GET_CODE (XEXP (XEXP (XEXP (set, 0), 0), 0)) == REG
	  && REGNO (XEXP (XEXP (XEXP (set, 0), 0), 0)) == REG_SP
	  && GET_CODE (XEXP (set, 1)) == REG)
	{
	  rtx try_insn = insn;
	  unsigned int regno = REGNO (XEXP (set, 1));
	  int reg_range = GET_MODE_SIZE (GET_MODE (XEXP (set, 1)));

	  while (1)
	    {
              rtx rewind;
	      rtx check;

	      rewind = prev_nonnote_insn (try_insn);
	      if (rewind == orig_first || rewind == NULL_RTX
		  || GET_CODE (rewind) != INSN)
		break;

              check = (GET_CODE (PATTERN (rewind)) == SET) ? PATTERN (rewind) : NULL_RTX;
	      if (check == NULL_RTX)
		break;

	      if (! ip2k_check_can_adjust_stack_ref (XEXP (check, 0),
						     reg_range)
	          || ! ip2k_check_can_adjust_stack_ref (XEXP (check, 1),
							reg_range))
		break;

	      /* If we've hit another push instruction we can't go any
		 further.  */
	      if (GET_CODE (XEXP (check, 0)) == MEM
	          && GET_CODE (XEXP (XEXP (check, 0), 0)) == POST_DEC
	          && GET_CODE (XEXP (XEXP (XEXP (check, 0), 0), 0)) == REG
	          && REGNO (XEXP (XEXP (XEXP (check, 0), 0), 0)) == REG_SP)
	        break;

	      /* If this is a register move then check that it doesn't clobber
	         SP or any part of the instruction we're trying to move.  */
	      if (GET_CODE (XEXP (check, 0)) == REG)
	        {
	          unsigned int check_reg = REGNO (XEXP (check, 0));
		  int check_reg_range = GET_MODE_SIZE (GET_MODE (XEXP (check,
								       0)));

		  /* If we have a special case where what we want to push is
		     being loaded by this "clobbering" insn then we can just
		     push what is being used to load us and then do the load.
		     This may seem a little odd, but we may subsequently be
		     able to merge the load with another instruction as it
		     may only be used once now!  Note though that we
		     specifically don't try this if the expression being
		     loaded is an HImode MEM using IP.  */
		  if (check_reg == regno
		      && check_reg_range == reg_range
		      && ((GET_CODE (XEXP (check, 1)) == REG
			  || (GET_CODE (XEXP (check, 1)) == MEM
			      && (GET_MODE (XEXP (check, 1)) != HImode
				  || ip2k_xexp_not_uses_reg_for_mem (XEXP (check, 1), REG_IP))))))
		    {
		      switch (check_reg_range)
			{
			case 1:
                          emit_insn_before (gen_movqi (XEXP (set, 0),
						       XEXP (check, 1)),
					    rewind);
		          delete_insn (try_insn);
			  break;

			case 2:
                          emit_insn_before (gen_movhi (XEXP (set, 0),
						       XEXP (check, 1)),
					    rewind);
		          delete_insn (try_insn);
			  break;

			case 4:
                          emit_insn_before (gen_movsi (XEXP (set, 0),
						       XEXP (check, 1)),
					    rewind);
		          delete_insn (try_insn);
			  break;

			case 8:
                          emit_insn_before (gen_movdi (XEXP (set, 0),
						       XEXP (check, 1)),
					    rewind);
		          delete_insn (try_insn);
			  break;
			}

		      ip2k_adjust_stack_ref (&XEXP (check, 0), reg_range);
		      ip2k_adjust_stack_ref (&XEXP (check, 1), reg_range);
	      	      try_insn = prev_nonnote_insn (rewind);
		      /* XXX - should be a continue?  */
		      break;
		    }
		  
		  if ((check_reg == REG_SPL)
		      || (check_reg == REG_SPH)
		      || (((regno <= check_reg)
			   && (regno + reg_range - 1) >= check_reg)
		      || ((regno <= (check_reg + check_reg_range - 1))
		          && ((regno + reg_range - 1)
			      >= (check_reg + check_reg_range - 1)))))
		    break;
		}

	      emit_insn_before (set, rewind);
	      delete_insn (try_insn);
	      ip2k_adjust_stack_ref (&XEXP (check, 0), reg_range);
	      ip2k_adjust_stack_ref (&XEXP (check, 1), reg_range);
	      try_insn = prev_nonnote_insn (rewind);
	    }
	}
    }
}

/* Assist the following function, mdr_try_propagate_clr().  */

static void
mdr_try_propagate_clr_sequence (first_insn, regno)
     rtx first_insn;
     unsigned int regno;
{
  rtx try_insn;

  for (try_insn = next_nonnote_insn (first_insn); try_insn;
       try_insn = next_nonnote_insn (try_insn))
    {
      rtx new_insn = NULL_RTX;
      rtx set2;

      if (GET_CODE (try_insn) == JUMP_INSN)
	continue;

      if (GET_CODE (try_insn) != INSN)
	break;

      set2 = ((GET_CODE (PATTERN (try_insn)) == SET)
	      ? PATTERN (try_insn) : NULL_RTX);
      if (set2 == NULL_RTX)
	continue;

      if (GET_CODE (XEXP (set2, 1)) == AND
	  && ((GET_CODE (XEXP (XEXP (set2, 1), 0)) == REG
	       && REGNO (XEXP (XEXP (set2, 1), 0)) == regno)
	      || (GET_CODE (XEXP (XEXP (set2, 1), 1)) == REG
	          && REGNO (XEXP (XEXP (set2, 1), 1)) == regno)))
	{
	  rtx remove_insn = try_insn;
	  try_insn = emit_insn_before (gen_rtx_SET (QImode, XEXP (set2, 0),
						    const0_rtx), try_insn);
	  delete_insn (remove_insn);
	}
      else if (GET_CODE (XEXP (set2, 1)) == IOR
	       && GET_CODE (XEXP (XEXP (set2, 1), 0)) == REG
	       && REGNO (XEXP (XEXP (set2, 1), 0)) == regno)
	{
	  rtx remove_insn = try_insn;
	  try_insn = emit_insn_before (gen_rtx_SET (QImode, XEXP (set2, 0),
						    XEXP (XEXP (set2, 1), 1)),
			               try_insn);
	  delete_insn (remove_insn);
	}
      else if (GET_CODE (XEXP (set2, 1)) == IOR
	       && GET_CODE (XEXP (XEXP (set2, 1), 1)) == REG
	       && REGNO (XEXP (XEXP (set2, 1), 1)) == regno)
	{
	  rtx remove_insn = try_insn;
	  try_insn = emit_insn_before (gen_rtx_SET (QImode, XEXP (set2, 0),
						    XEXP (XEXP (set2, 1), 0)),
			               try_insn);
	  delete_insn (remove_insn);
	}
      else if (GET_CODE (XEXP (set2, 1)) == XOR
	       && GET_CODE (XEXP (XEXP (set2, 1), 0)) == REG
	       && REGNO (XEXP (XEXP (set2, 1), 0)) == regno)
	{
	  rtx remove_insn = try_insn;
	  try_insn = emit_insn_before (gen_rtx_SET (QImode, XEXP (set2, 0),
						    XEXP (XEXP (set2, 1), 1)),
			               try_insn);
	  delete_insn (remove_insn);
	}
      else if (GET_CODE (XEXP (set2, 1)) == XOR
	       && GET_CODE (XEXP (XEXP (set2, 1), 1)) == REG
	       && REGNO (XEXP (XEXP (set2, 1), 1)) == regno)
	{
	  rtx remove_insn = try_insn;
	  try_insn = emit_insn_before (gen_rtx_SET (QImode, XEXP (set2, 0),
						    XEXP (XEXP (set2, 1), 0)),
			               try_insn);
	  delete_insn (remove_insn);
	}
      
      if (GET_CODE (XEXP (set2, 0)) == REG)
	{
          int reg2_range = GET_MODE_SIZE (GET_MODE (XEXP (set2, 0)));
	  unsigned int regno2 = REGNO (XEXP (set2, 0));

	  if (reg2_range == 1
	      && regno == regno2
	      && GET_CODE (XEXP (set2, 1)) == CONST_INT)
	    {
	      int iv = INTVAL (XEXP (set2, 1));
	      if (iv == 0xff)
		iv = -1;
	      if (iv == 1 || iv == -1)
	        {
		  new_insn = gen_rtx_SET (QImode, XEXP (set2, 0),
					  gen_rtx_PLUS (QImode, XEXP (set2, 0),
							GEN_INT (iv)));
		  new_insn = emit_insn_before (new_insn, try_insn);
		  delete_insn (try_insn);
		  try_insn = new_insn;
	        }
	      break;
	    }

	  if ((regno >= regno2) && (regno <= regno2 + reg2_range - 1))
            break;

          if (GET_CODE (XEXP (set2, 1)) == REG
	      && REGNO (XEXP (set2, 1)) == regno)
	    {
	      new_insn = emit_insn_before (gen_rtx_SET (QImode,
	      						XEXP (set2, 0),
							const0_rtx),
					   try_insn);
	      delete_insn (try_insn);
	      try_insn = new_insn;
	    }
	}

      if (GET_CODE (XEXP (set2, 0)) == CC0)
	{
          if (GET_CODE (XEXP (set2, 1)) == REG
	      && GET_MODE_SIZE (GET_MODE (XEXP (set2, 1))) == 2
	      && REGNO (XEXP (set2, 1)) == regno)
            {
	      new_insn = gen_rtx_SET (VOIDmode, gen_rtx (CC0, VOIDmode),
				      gen_rtx_REG(QImode, regno + 1));
              new_insn = emit_insn_before (new_insn, try_insn);
	    }
	  else if (GET_CODE (XEXP (set2, 1)) == COMPARE
	           && GET_CODE (XEXP (XEXP (set2, 1), 0)) == REG
		   && GET_MODE_SIZE (GET_MODE (XEXP (XEXP (set2, 1), 0))) == 2
		   && REGNO (XEXP (XEXP (set2, 1), 0)) == regno
		   && GET_CODE (XEXP (XEXP (set2, 1), 1)) == CONST_INT
		   && INTVAL (XEXP (XEXP (set2, 1), 1)) >= 0
		   && INTVAL (XEXP (XEXP (set2, 1), 1)) < 256)
	    {
	      new_insn = gen_rtx_SET (VOIDmode, cc0_rtx,
				      gen_rtx_COMPARE(QImode,
						      gen_rtx_REG (QImode,
								  regno + 1),
						      XEXP (XEXP (set2, 1),
							    1)));
              new_insn = emit_insn_before (new_insn, try_insn);
	    }

	  /* If we have inserted a replacement for a CC0 setter operation
	     then we need to delete the old one.  */
	  if (new_insn != NULL_RTX)
            {
              delete_insn (try_insn);
              try_insn = new_insn;

	      /* Now as we know that we have just done an unsigned compare
	         (remember we were zero-extended by the clr!) we also know
		 that we don't need a signed jump insn.  If we find that
		 our next isns is a signed jump then make it unsigned!  */
	      if (GET_CODE (next_nonnote_insn (try_insn)) == JUMP_INSN)
		{
	          rtx set3;

		  try_insn = next_nonnote_insn (try_insn);
                  set3 = ((GET_CODE (PATTERN (try_insn)) == SET)
			  ? PATTERN (try_insn) : NULL_RTX);
                  if (set3 == NULL_RTX)
		    continue;

		  /* If we discover that our jump target is only accessible
		     from here then we can continue our "clr" propagation to
		     it too!  */
		  if (LABEL_NUSES (JUMP_LABEL (try_insn)) == 1)
		    mdr_try_propagate_clr_sequence (JUMP_LABEL (try_insn),
						    regno);

		  if (GET_CODE (XEXP (set3, 0)) == PC
		      && GET_CODE (XEXP (set3, 1)) == IF_THEN_ELSE
		      && (GET_CODE (XEXP (XEXP (set3, 1), 0)) == GT
			  || GET_CODE (XEXP (XEXP (set3, 1), 0)) == GE
			  || GET_CODE (XEXP (XEXP (set3, 1), 0)) == LT
			  || GET_CODE (XEXP (XEXP (set3, 1), 0)) == LE)
		      && GET_CODE (XEXP (XEXP (XEXP (set3, 1), 0), 0)) == CC0
		      && (GET_CODE (XEXP (XEXP (XEXP (set3, 1), 0), 1))
			  == CONST_INT)
		      && GET_CODE (XEXP (XEXP (set3, 1), 1)) == LABEL_REF
		      && GET_CODE (XEXP (XEXP (set3, 1), 2)) == PC)
		    {
     		      enum rtx_code code;
		      rtx new_if;
		      rtx cmp;

		      /* Replace our old conditional jump with a new one that
			 does the unsigned form of what was previously a
			 signed comparison.  */
		      code = GET_CODE (XEXP (XEXP (set3, 1), 0));
		      cmp = gen_rtx_fmt_ee ((code == GT
					     ? GTU
					     : (code == GE
						? GEU
						: (code == LT ? LTU : LEU))),
				            VOIDmode,
					    XEXP (XEXP (XEXP (set3, 1), 0), 0),
					    XEXP (XEXP (XEXP (set3, 1), 0),
						  1));
	              new_if
			= gen_rtx_SET (GET_MODE (set3),
				       pc_rtx,
				       gen_rtx_IF_THEN_ELSE
				       (GET_MODE (XEXP (set3, 1)), cmp,
					XEXP (XEXP (set3, 1), 1),
					XEXP (XEXP (set3, 1), 2)));
		      new_insn = emit_jump_insn_before (new_if, try_insn);
		      LABEL_NUSES (JUMP_LABEL (try_insn))++;
		      delete_insn (try_insn);
		      try_insn = new_insn;
		    }
		}
	    }
	}
      else if (GET_CODE (XEXP (set2, 1)) == PLUS
	       && GET_CODE (XEXP (XEXP (set2, 1), 0)) == REG
	       && GET_MODE_SIZE (GET_MODE (XEXP (XEXP (set2, 1), 0))) == 2
	       && REGNO (XEXP (XEXP (set2, 1), 0)) == regno
	       && (GET_CODE (XEXP (XEXP (set2, 1), 1)) == REG
		   || GET_CODE (XEXP (XEXP (set2, 1), 1)) == MEM
		   || GET_CODE (XEXP (XEXP (set2, 1), 1)) == CONST_INT
		   || GET_CODE (XEXP (XEXP (set2, 1), 1)) == CONST
		   || GET_CODE (XEXP (XEXP (set2, 1), 1)) == SYMBOL_REF))
	{
	  rtx extend = gen_rtx_ZERO_EXTEND (HImode,
					    gen_rtx_REG (QImode, regno + 1));
	  new_insn = gen_rtx_SET (HImode, XEXP (set2, 0),
				  gen_rtx_PLUS (HImode, extend,
						XEXP (XEXP (set2, 1), 1)));
	  new_insn = emit_insn_before (new_insn, try_insn);
	  delete_insn (try_insn);
	  try_insn = new_insn;
	}
      else if (GET_CODE (XEXP (set2, 1)) == PLUS
	       && GET_CODE (XEXP (XEXP (set2, 1), 1)) == REG
	       && GET_MODE_SIZE (GET_MODE (XEXP (XEXP (set2, 1), 1))) == 2
	       && REGNO (XEXP (XEXP (set2, 1), 1)) == regno
	       && (GET_CODE (XEXP (XEXP (set2, 1), 0)) == REG
		   || GET_CODE (XEXP (XEXP (set2, 1), 0)) == MEM
		   || GET_CODE (XEXP (XEXP (set2, 1), 0)) == CONST_INT
		   || GET_CODE (XEXP (XEXP (set2, 1), 0)) == CONST
		   || GET_CODE (XEXP (XEXP (set2, 1), 0)) == SYMBOL_REF))
	{
	  rtx t_src = gen_rtx_PLUS (HImode,
				    gen_rtx_ZERO_EXTEND (HImode,
							 gen_rtx_REG (QImode,
								      regno
								      + 1)),
				    XEXP (XEXP (set2, 1), 0));
	  new_insn = emit_insn_before (gen_rtx_SET (HImode, XEXP (set2, 0),
						    t_src),
				       try_insn);
	  delete_insn (try_insn);
	  try_insn = new_insn;
	}
    }
}

/* One of the things that can quite often happen with an 8-bit CPU is that
   we end up clearing the MSByte of a 16-bit value.  Unfortunately, all too
   often gcc doesn't have any way to realize that only half of the value is
   useful and ends up doing more work than it should.  We scan for such
   occurrences here, track them and reduce compare operations to a smaller
   size where possible.
 
   Note that this is somewhat different to move propagation as we may
   actually change some instruction patterns when we're doing this whereas
   move propagation is just about doing a search and replace.  */

static void
mdr_try_propagate_clr (first_insn)
     rtx first_insn;
{
  rtx insn;
  rtx set;

  for (insn = first_insn; insn; insn = next_nonnote_insn (insn))
    {
      if (GET_CODE (insn) != INSN)
	continue;

      set = (GET_CODE (PATTERN (insn)) == SET) ? PATTERN (insn) : NULL_RTX;
      if (set == NULL_RTX)
        continue;

      /* Have we found a "clr" instruction?  */
      if (GET_CODE (XEXP (set, 0)) == REG
	  && GET_CODE (XEXP (set, 1)) == CONST_INT
	  && GET_MODE_SIZE (GET_MODE (XEXP (set, 0))) == 1
	  && INTVAL (XEXP (set, 1)) == 0)
	{
	  mdr_try_propagate_clr_sequence (insn, REGNO (XEXP (set, 0)));
	}
    }
}
#endif /* IP2K_MD_REORG_PASS */

/* Look to see if the expression, x, does not make any memory references
   via the specified register.  This is very conservative and only returns
   nonzero if we definitely don't have such a memory ref.  */

static int
ip2k_xexp_not_uses_reg_for_mem (rtx x, unsigned int regno)
{
  if (regno & 1)
    regno &= 0xfffffffe;

  if (GET_RTX_CLASS (GET_CODE (x)) == 'b')
    return (ip2k_xexp_not_uses_reg_for_mem (XEXP (x, 0), regno)
	    && ip2k_xexp_not_uses_reg_for_mem (XEXP (x, 1), regno)
	    && ip2k_xexp_not_uses_reg_for_mem (XEXP (x, 2), regno));

  if (GET_RTX_CLASS (GET_CODE (x)) == '2'
      || GET_RTX_CLASS (GET_CODE (x)) == 'c'
      || GET_RTX_CLASS (GET_CODE (x)) == '<')
    return (ip2k_xexp_not_uses_reg_for_mem (XEXP (x, 0), regno)
	    && ip2k_xexp_not_uses_reg_for_mem (XEXP (x, 1), regno));

  if (GET_RTX_CLASS (GET_CODE (x)) == '1'
      || GET_RTX_CLASS (GET_CODE (x)) == '3')
    return ip2k_xexp_not_uses_reg_for_mem (XEXP (x, 0), regno);

  switch (GET_CODE (x))
    {
    case REG:
      return 1;

    case MEM:
      if ((GET_CODE (XEXP (x, 0)) == PLUS
	   && GET_CODE (XEXP (XEXP (x, 0), 0)) == REG
	   && REGNO (XEXP (XEXP (x, 0), 0)) == regno)
	  || (GET_CODE (XEXP (x, 0)) == REG
	      && REGNO (XEXP (x, 0)) == regno))
	return 0;
      else
	return 1;

    case CONST:
    case CONST_INT:
    case CONST_DOUBLE:
    case SYMBOL_REF:
    case LABEL_REF:
    case CC0:
    case PC:
      return 1;

    default:
      return 0;
    }
}

#ifdef IP2K_MD_REORG_PASS
/* Assist the following function, mdr_try_propagate_move().  */

static void
mdr_try_propagate_move_sequence (first_insn, orig, equiv)
     rtx first_insn;
     rtx orig;
     rtx equiv;
{
  rtx try_insn;

  for (try_insn = next_nonnote_insn (first_insn); try_insn;
       try_insn = next_nonnote_insn (try_insn))
    {
      rtx set;
      int range;
      rtx new_equiv = NULL_RTX;

      if (GET_CODE (try_insn) != JUMP_INSN && GET_CODE (try_insn) != INSN)
	break;

      set = single_set (try_insn);
      if (set == NULL_RTX)
	break;

      range = MAX (GET_MODE_SIZE (GET_MODE (equiv)),
		   GET_MODE_SIZE (GET_MODE (XEXP (set, 0))));

      if (GET_CODE (equiv) == REG
	  && REGNO (equiv) == REG_W
	  && (recog_memoized (try_insn) < 0
	      || get_attr_clobberw (try_insn) != CLOBBERW_NO)
	  && (! (GET_CODE (XEXP (set, 0)) == REG
	         && REGNO (XEXP (set, 0)) == REG_W
	         && rtx_equal_p (XEXP (set, 1), orig))))
	break;
      else if (GET_CODE (XEXP (set, 0)) == REG
	  && (REGNO (XEXP (set, 0)) == REG_SP
	      || ! ip2k_xexp_not_uses_reg_p (equiv, REGNO (XEXP (set, 0)),
					     range)
	      || ! ip2k_xexp_not_uses_reg_p (orig, REGNO (XEXP (set, 0)),
					     range))
	  && ! rtx_equal_p (equiv, XEXP (set, 0))
	  && ! rtx_equal_p (orig, XEXP (set, 0)))
	break;
      else if (GET_CODE (orig) == REG
	       && (REGNO (orig) == REG_IPL
		   || REGNO (orig) == REG_IPH
		   || REGNO (orig) == REG_DPL
		   || REGNO (orig) == REG_DPH)
	       && (! ip2k_xexp_not_uses_reg_for_mem (XEXP (set, 0),
						     REGNO (orig))
	           || ! ip2k_xexp_not_uses_reg_for_mem (XEXP (set, 1),
							REGNO (orig))))
	break;
      else if (GET_CODE (XEXP (set, 0)) == MEM
	       && GET_CODE (equiv) == MEM)
	{
	  if (! ip2k_xexp_not_uses_reg_p (equiv, REG_SP, 2))
	    {
              if (! ip2k_xexp_not_uses_reg_p (XEXP (set, 0), REG_SP, 2))
		{
                  /* We look for a special case of "push" operations screwing
		     our register equivalence when it's based on a stack slot.
		     We can track this one and replace the old equivalence
		     expression with a new one.  */
	          if (GET_CODE (XEXP (XEXP (set, 0), 0)) == POST_DEC
	              && GET_CODE (XEXP (XEXP (XEXP (set, 0), 0), 0)) == REG
	              && REGNO (XEXP (XEXP (XEXP (set, 0), 0), 0)) == REG_SP
	              && GET_CODE (XEXP (equiv, 0)) == PLUS
	              && REGNO (XEXP (XEXP (equiv, 0), 0)) == REG_SP)
	            {
		      int md_size = GET_MODE_SIZE (GET_MODE (XEXP (set, 0)));
		      int new_sp_offs = INTVAL (XEXP (XEXP (equiv, 0), 1))
			+ md_size;

		      /* Don't allow an invalid stack pointer offset to be
			 created.  */
		      if (new_sp_offs > (128 - 2 * md_size))
			break;

	              new_equiv
			= gen_rtx_MEM (GET_MODE (equiv),
				       gen_rtx_PLUS (Pmode,
						     gen_rtx_REG (HImode ,
								  REG_SP),
						     GEN_INT (new_sp_offs)));
	            }
		  else if (! rtx_equal_p (equiv, XEXP (set, 0)))
	            {
	              /* Look at the SP offsets and look for any overlaps.  */
                      int equiv_offs = GET_CODE (XEXP (equiv, 0)) == PLUS
		              	       ? INTVAL (XEXP (XEXP (equiv, 0), 1))
			               : 0;
	              int set_offs
			= (GET_CODE (XEXP (XEXP (set, 0), 0)) == PLUS
			   ? INTVAL (XEXP (XEXP (XEXP (set, 0), 0), 1))
			   : 0);

	              if (abs (equiv_offs - set_offs) < range)
		        break;
		    }
		}
	    }

	  if (! ip2k_xexp_not_uses_reg_p (equiv, REG_IP, 2))
            break;

	  if (! ip2k_xexp_not_uses_reg_p (XEXP (set, 0), REG_DP, 2)
	      && ! ip2k_xexp_not_uses_reg_p (equiv, REG_DP, 2)
	      && ! rtx_equal_p (equiv, XEXP (set, 0)))
            {
	      /* Look at the DP offsets and look for any overlaps.  */
              int equiv_offs = GET_CODE (XEXP (equiv, 0)) == PLUS
		      	       ? INTVAL (XEXP (XEXP (equiv, 0), 1))
			       : 0;
	      int set_offs = GET_CODE (XEXP (XEXP (set, 0), 0)) == PLUS
		             ? INTVAL (XEXP (XEXP (XEXP (set, 0), 0), 1))
		             : 0;

	      if (abs (equiv_offs - set_offs) < range)
		break;
	    }
	}

      validate_replace_rtx_subexp (orig, equiv, try_insn, &XEXP (set, 1));

      if (rtx_equal_p (equiv, XEXP (set, 0))
	  || rtx_equal_p (orig, XEXP (set, 0)))
	break;

      if (new_equiv != NULL_RTX)
	equiv = new_equiv;
    }
}

/* Try propagating move instructions forwards.  It may be that we can
   replace a register use with an equivalent expression that already
   holds the same value and thus allow one or more register loads to
   be eliminated.  */

static void
mdr_try_propagate_move (first_insn)
     rtx first_insn;
{
  rtx insn;
  rtx set;

  for (insn = first_insn; insn; insn = next_nonnote_insn (insn))
    {
      if (GET_CODE (insn) != INSN)
	continue;

      set = (GET_CODE (PATTERN (insn)) == SET) ? PATTERN (insn) : NULL_RTX;
      if (set == NULL_RTX)
        continue;

      /* Have we found a simple move instruction?  */
      if (GET_CODE (XEXP (set, 0)) == REG
	  && (REGNO (XEXP (set, 0)) >= 0x80
	      || REGNO (XEXP (set, 0)) == REG_DPL
	      || REGNO (XEXP (set, 0)) == REG_DPH
	      || REGNO (XEXP (set, 0)) == REG_IPL
	      || REGNO (XEXP (set, 0)) == REG_IPH)
	  && ((GET_CODE (XEXP (set, 1)) == REG
	       && REGNO (XEXP (set, 1)) != REG_SP
	       && ip2k_xexp_not_uses_reg_p (XEXP (set, 0),
		       			    REGNO (XEXP (set, 1)),
					    GET_MODE_SIZE (GET_MODE (XEXP (set,
									   0)))))
	      || (GET_CODE (XEXP (set, 1)) == MEM
		  && (ip2k_xexp_not_uses_reg_p (XEXP (set, 1), REG_IP, 2)
		      || GET_MODE (XEXP (set, 1)) == QImode)
		  && ((REGNO (XEXP (set, 0)) != REG_DPH
		       && REGNO (XEXP (set, 0)) != REG_DPL)
		      || ip2k_xexp_not_uses_reg_p (XEXP (set, 1), REG_DP, 2)))
	      || (GET_CODE (XEXP (set, 1)) == CONST_INT
	          && (GET_MODE (XEXP (set, 0)) != QImode
		      || INTVAL (XEXP (set, 1)) != 0))
	      || GET_CODE (XEXP (set, 1)) == CONST_DOUBLE
	      || GET_CODE (XEXP (set, 1)) == CONST
	      || GET_CODE (XEXP (set, 1)) == SYMBOL_REF))
	{
	  mdr_try_propagate_move_sequence (insn, XEXP (set, 0), XEXP (set, 1));
	}
    }
}

/* Try to remove redundant instructions.  */

static void
mdr_try_remove_redundant_insns (first_insn)
     rtx first_insn;
{
  rtx insn;

  for (insn = first_insn; insn; insn = next_nonnote_insn (insn))
    {
      rtx set;
      enum machine_mode mode;
      int md_size;
      HOST_WIDE_INT pattern;
      int i;

      if (GET_CODE (insn) != INSN)
	continue;

      if (GET_CODE (PATTERN (insn)) == CONST_INT)
	{
	  /* We've found a dummy expression.  */
	  rtx remove_insn = insn;
	  insn = prev_nonnote_insn (insn);
	  delete_insn (remove_insn);
	  continue;
	}

      set = (GET_CODE (PATTERN (insn)) == SET) ? PATTERN (insn) : NULL_RTX;
      if (set == NULL_RTX)
        continue;

      mode = GET_MODE (XEXP (set, 0));
      md_size = GET_MODE_SIZE (mode);
      if ((md_size < 1) || (md_size > 4))
	continue;

      pattern = 0;
      for (i = 0; i < md_size; i++)
	{
          pattern <<= 8;
          pattern |= 0xff;
	}

      if ((GET_CODE (XEXP (set, 1)) == AND
	   && GET_CODE (XEXP (XEXP (set, 1), 1)) == CONST_INT
	   && INTVAL (XEXP (XEXP (set, 1), 1)) == pattern)
          || ((GET_CODE (XEXP (set, 1)) == IOR
	       || GET_CODE (XEXP (set, 1)) == XOR)
	      && GET_CODE (XEXP (XEXP (set, 1), 1)) == CONST_INT
	      && INTVAL (XEXP (XEXP (set, 1), 1)) == 0x00))
	{
          /* We've found an AND with all 1's, an XOR with all 0's or an
	     IOR with 0's.  */
	  rtx remove_insn = insn;

	  /* Is it completely redundant or should it become a move insn?  */
	  if (! rtx_equal_p (XEXP (set, 0), XEXP (XEXP (set, 1), 0)))
	    {
	      emit_insn_before (gen_rtx_SET (mode,
					     XEXP (set, 0),
					     XEXP (XEXP (set, 1), 0)),
				insn);
	    }

	  insn = prev_nonnote_insn(insn);
	  delete_insn (remove_insn);
	}
      else if (GET_CODE (XEXP (set, 1)) == AND
	  && GET_CODE (XEXP (XEXP (set, 1), 1)) == CONST_INT
	  && INTVAL (XEXP (XEXP (set, 1), 1)) == 0)
	{
	  /* We've found an AND with all 0's.  */
	  rtx remove_insn = insn;
	  insn = emit_insn_before (gen_rtx_SET (mode,
				                XEXP (set, 0),
						XEXP (XEXP (set, 1), 1)),
			           insn);
	  delete_insn (remove_insn);
	}
    }
}

/* Structure used to track jump targets.  */

struct we_jump_targets
{
  int target;			/* Is this a jump target?  */
  int reach_count;		/* Number of ways we can reach this insn.  */
  int touch_count;		/* Number of times we've touched this insn
				   during scanning.  */
  rtx w_equiv;			/* WREG-equivalence at this point.  */
};

struct we_jump_targets *ip2k_we_jump_targets;

/* WREG equivalence tracking used within DP reload elimination.  */

static int
track_w_reload (insn, w_current, w_current_ok, modifying)
     rtx insn;
     rtx *w_current;
     int w_current_ok;
     int modifying;
{
  rtx set;

  if (GET_CODE (insn) != INSN)
    {
      *w_current = NULL_RTX;
      return 1;
    }

  set = (GET_CODE (PATTERN (insn)) == SET) ? PATTERN (insn) : NULL_RTX;
  if (set == NULL_RTX)
    {
      *w_current = NULL_RTX;
      return 1;
    }

  /* Look for W being modified.  If it is, see if it's being changed
     to what it already is!  */
  if (GET_CODE (XEXP (set, 0)) == REG
      && REGNO (XEXP (set, 0)) == REG_W
      && GET_MODE (XEXP (set, 0)) == QImode)
    {
      /* If this is an equivalence we can delete the new set operation.  */
      if (*w_current != NULL_RTX
          && rtx_equal_p (XEXP (set, 1), *w_current))
        {
	  if (modifying)
            delete_insn (insn);
        }
      else
        {
	  *w_current = XEXP (set, 1);
	  return 1;
	}
    }
  else if (recog_memoized (insn) < 0
           || get_attr_clobberw (insn) != CLOBBERW_NO)
    {
      /* If we clobber W then we've clobbered any equivalences !  */
      *w_current = NULL_RTX;
      return 1;
    }
  else if (! ip2k_xexp_not_uses_reg_p (XEXP (set, 0), REG_SP, 2)
	   && *w_current != NULL_RTX
	   && !ip2k_xexp_not_uses_reg_p (*w_current, REG_SP, 2))
    {
      /* We look for a special case of "push" operations screwing up the
         setting of DP when it's based on the stack.  We can track this one
         and replace the old expression for DP with a new one.  */
      if (GET_CODE (XEXP (set, 0)) == MEM
	  && GET_CODE (XEXP (XEXP (set, 0), 0)) == POST_DEC
	  && GET_CODE (XEXP (XEXP (XEXP (set, 0), 0), 0)) == REG
	  && REGNO (XEXP (XEXP (XEXP (set, 0), 0), 0)) == REG_SP
	  && GET_CODE (*w_current) == MEM
	  && GET_CODE (XEXP (*w_current, 0)) == PLUS)
        {
	  /* XXX - need to ensure that we can track this without going
	     out of range!  */
	  rtx val = GEN_INT (INTVAL (XEXP (XEXP (*w_current, 0), 1))
			     + GET_MODE_SIZE (GET_MODE (XEXP (set, 0))));
          *w_current
	    = gen_rtx_MEM (HImode, gen_rtx_PLUS (Pmode,
						 gen_rtx_REG(HImode, REG_SP),
						 val));
	  return 1;
	}
    }
  else if (GET_CODE (XEXP (set, 0)) == REG
	   && *w_current != NULL_RTX
	   && !ip2k_xexp_not_uses_reg_p (*w_current, REGNO (XEXP (set, 0)),
				 	 GET_MODE_SIZE (GET_MODE (XEXP (set
									, 0)))))
    {
      /* If we've just clobbered all or part of a register reference that we
         were sharing for W then we can't share it any more!  */
      *w_current = NULL_RTX;
    }

  return w_current_ok;
}

/* As part of the machine-dependent reorg we scan moves into w and track them
   to see where any are redundant.  */

static void
mdr_try_wreg_elim (first_insn)
     rtx first_insn;
{
  rtx insn;
  struct we_jump_targets *wjt;
  rtx w_current;
  int incomplete_scan;
  int last_incomplete_scan;

  ip2k_we_jump_targets
    = (struct we_jump_targets *) xcalloc (get_max_uid (),
					  sizeof (struct we_jump_targets));

  /* First we scan to build up a list of all CODE_LABEL insns and we work out
     how many different ways we can reach them.  */
  for (insn = first_insn; insn; insn = next_nonnote_insn (insn))
    {
      if (GET_CODE (insn) == CODE_LABEL)
	{
          wjt = &ip2k_we_jump_targets[INSN_UID (insn)];
          wjt->target = 1;
          wjt->reach_count = LABEL_NUSES (insn);
	  wjt->touch_count = 0;
	  wjt->w_equiv = NULL_RTX;
	  if (! prev_nonnote_insn (insn)
	      || (prev_nonnote_insn (insn)
	          && GET_CODE (prev_nonnote_insn (insn)) != BARRIER))
            wjt->reach_count++;
	}
    }

  /* Next we scan all of the ways of reaching the code labels to see
     what the WREG register is equivalent to as we reach them.  If we find
     that they're the same then we keep noting the matched value.  We
     iterate around this until we reach a convergence on WREG equivalences
     at all code labels - we have to be very careful not to be too
     optimistic!  */
  incomplete_scan = -1;
  do
    {
      int w_current_ok = 0;
      last_incomplete_scan = incomplete_scan;
      w_current = NULL_RTX;

      for (insn = first_insn; insn; insn = next_nonnote_insn (insn))
        {
	  /* If we have a code label then we need to see if we already know
	     what the equivalence is at this point.  If we do then we use it
	     immediately, but if we don't then we have a special case to track
	     when we hit a fallthrough-edge (label with no barrier preceding
	     it).  Any other accesses to the label must be from jump insns
	     and so they're handled elsewhere.  */
          if (GET_CODE (insn) == CODE_LABEL)
            {
              wjt = &ip2k_we_jump_targets[INSN_UID (insn)];

	      /* If we're fully characterized the use the equivalence.  */
	      if (wjt->touch_count == wjt->reach_count)
		{
		  w_current = wjt->w_equiv;
		  w_current_ok = 1;
		  continue;
		}

	      /* If we have a known equivalence for WREG as we reach the
	         fallthrough-edge then track this into the code label.  */
	      if (w_current_ok
		  && (! prev_nonnote_insn (insn)
	              || (prev_nonnote_insn (insn)
	                  && GET_CODE (prev_nonnote_insn (insn)) != BARRIER)))
	        {
	          if (wjt->touch_count == 0)
                    wjt->w_equiv = w_current;

	          if (wjt->touch_count < wjt->reach_count)
	            {
	              wjt->touch_count++;
	              if (! rtx_equal_p (wjt->w_equiv, w_current))
			{
			  /* When we definitely know that we can't form an
			     equivalence for WREG here we must clobber anything
			     that we'd started to track too.  */
                          wjt->w_equiv = NULL_RTX;
			  w_current = NULL_RTX;
			  w_current_ok = 1;
			}
	            }
	        }

	      /* If we've not completely characterized this code label then
	         be cautious and assume that we don't know what WREG is
		 equivalent to.  */
	      if (wjt->touch_count < wjt->reach_count)
                {
	          w_current = NULL_RTX;
	          w_current_ok = 0;
	        }

              continue;
            }

	  /* If we've hit a jump insn then we look for either an address
	     vector (jump table) or for jump label references.  */
          if (GET_CODE (insn) == JUMP_INSN)
	    {
	      /* Don't attempt to track here if we don't have a known
	         equivalence for WREG at this point.  */
              if (w_current_ok)
		{
	          if (JUMP_LABEL (insn))
	            {
	              wjt
			= &ip2k_we_jump_targets[INSN_UID (JUMP_LABEL (insn))];

		      if (wjt->touch_count == 0)
                        wjt->w_equiv = w_current;

	              if (wjt->touch_count < wjt->reach_count)
	                {
	                  wjt->touch_count++;
	                  if (! rtx_equal_p (wjt->w_equiv, w_current))
                            wjt->w_equiv = NULL_RTX;
	                }
	            }
		}

              continue;
            }

	  /* Anything other than a code labal or jump arrives here.  We try and
	     track WREG, but sometimes we might not be able to.  */
          w_current_ok = track_w_reload (insn, &w_current, w_current_ok, 0);
        }

      /* When we're looking to see if we've finished we count the number of
         paths through the code labels where we weren't able to definitively
	 track WREG.  This number is used to see if we're converging on a
	 solution.
	 If this hits zero then we've fully converged, but if this stays the
	 same as last time then we probably can't make any further
	 progress.  */
      incomplete_scan = 0;
      for (insn = first_insn; insn; insn = next_nonnote_insn (insn))
        {
          if (GET_CODE (insn) == CODE_LABEL)
            {
              wjt = &ip2k_we_jump_targets[INSN_UID (insn)];
	      if (wjt->touch_count != wjt->reach_count)
		{
		  incomplete_scan += (wjt->reach_count - wjt->touch_count);
		  wjt->w_equiv = NULL_RTX;
		  wjt->touch_count = 0;
		}
	    }
	}
    }
  while (incomplete_scan && incomplete_scan != last_incomplete_scan);

  /* Finally we scan the whole function and run WREG elimination.  When we hit
     a CODE_LABEL we pick up any stored equivalence since we now know that
     every path to this point entered with WREG holding the same thing!  If
     we subsequently have a reload that matches then we can eliminate it.  */
  w_current = NULL_RTX;
  for (insn = first_insn; insn; insn = next_nonnote_insn (insn))
    {
      if (GET_CODE (insn) == JUMP_INSN)
        continue;

      if (GET_CODE (insn) == CODE_LABEL)
	{
          wjt = &ip2k_we_jump_targets[INSN_UID (insn)];
	  w_current = wjt->w_equiv;
          continue;
	}

      track_w_reload (insn, &w_current, 1, 1);
    }

  free (ip2k_we_jump_targets);
}
#endif /* IP2K_MD_REORG_PASS */

/* We perform a lot of untangling of the RTL within the reorg pass since
   the IP2k requires some really bizarre (and really undesireable) things
   to happen in order to guarantee not aborting.  This pass causes several
   earlier passes to be re-run as it progressively transforms things,
   making the subsequent runs continue to win.  */

static void
ip2k_reorg (void)
{
#ifdef IP2K_MD_REORG_PASS
  rtx first_insn, insn, set;
#endif

  CC_STATUS_INIT;

  if (optimize == 0)
    {
      ip2k_reorg_completed = 1;
      ip2k_reorg_split_dimode = 1;
      ip2k_reorg_split_simode = 1;
      ip2k_reorg_split_himode = 1;
      ip2k_reorg_split_qimode = 1;
      ip2k_reorg_merge_qimode = 1;
      return;
    }
#ifndef IP2K_MD_REORG_PASS
  ip2k_reorg_completed = 1;
  ip2k_reorg_split_dimode = 1;
  ip2k_reorg_split_simode = 1;
  ip2k_reorg_split_himode = 1;
  ip2k_reorg_split_qimode = 1;
  ip2k_reorg_merge_qimode = 1;
#else
  /* All optimizations below must be debugged and enabled one by one.
     All of them commented now because of abort in GCC core.  */
  
  ip2k_reorg_in_progress = 1;
  
  first_insn = get_insns ();

  /* Look for size effects of earlier optimizations - in particular look for
     situations where we're saying "use" a register on one hand but immediately
     tagging it as "REG_DEAD" at the same time!  Seems like a bug in core-gcc
     somewhere really but this is what we have to live with!  */
  for (insn = first_insn; insn; insn = NEXT_INSN (insn))
    {
      rtx body;

      if (GET_CODE (insn) == CODE_LABEL
	  || GET_CODE (insn) == NOTE
	  || GET_CODE (insn) == BARRIER)
	continue;

      if (!INSN_P (insn))
	continue;

      body = PATTERN (insn);
      if (GET_CODE (body) == USE)
	if (GET_CODE (XEXP (body, 0)) == REG)
	  {
	    int reg;

	    reg = REGNO (XEXP (body, 0));
	    if (find_regno_note (insn, REG_DEAD, reg))
	      {
		delete_insn (insn);
	      }
	  }
    }

  /* There's a good chance that since we last did CSE that we've rearranged
     things in such a way that another go will win.  Do so now!  */
  reload_cse_regs (first_insn);
  find_basic_blocks (first_insn, max_reg_num (), 0);
  life_analysis (first_insn, 0, PROP_REG_INFO | PROP_DEATH_NOTES);
  
  /* Look for where absurd things are happening with DP.  */
  mdr_try_dp_reload_elim (first_insn);

  ip2k_reorg_in_progress = 0;
  ip2k_reorg_completed = 1;

  split_all_insns (0);

  reload_cse_regs (first_insn);
  find_basic_blocks (first_insn, max_reg_num (), 0);
  life_analysis (first_insn, 0, PROP_REG_INFO | PROP_DEATH_NOTES);
  if (flag_peephole2)
    peephole2_optimize (NULL);

  mdr_resequence_xy_yx (first_insn);
  mdr_propagate_reg_equivs (first_insn);

  /* Look for redundant set instructions.  These can occur when we split
     instruction patterns and end up with the second half merging with
     or being replaced by something that clobbers the first half.  */
  for (insn = first_insn; insn; insn = next_nonnote_insn (insn))
    {
      if (GET_CODE (insn) == INSN)
        {
          set = (GET_CODE (PATTERN (insn)) == SET) ? PATTERN (insn) : NULL_RTX;
          if ((set != NULL_RTX)
              && (GET_CODE (XEXP (set, 0)) == REG)
	      && (GET_MODE (XEXP (set, 0)) == QImode)
	      && (find_regno_note (insn, REG_UNUSED, REGNO (XEXP (set, 0)))))
	    delete_insn (insn);
	}
    }

  mdr_try_move_dp_reload (first_insn);
  mdr_try_move_pushes (first_insn);

  find_basic_blocks (first_insn, max_reg_num (), 0);
  life_analysis (first_insn, 0, PROP_FINAL);

  mdr_try_propagate_move (first_insn);
  mdr_resequence_xy_yx (first_insn);

  ip2k_reorg_split_dimode = 1;
  split_all_insns (0);

  mdr_try_remove_redundant_insns (first_insn);

  mdr_try_propagate_move (first_insn);

  reload_cse_regs (first_insn);
  find_basic_blocks (first_insn, max_reg_num (), 0);
  life_analysis (first_insn, 0, PROP_FINAL);
  if (flag_peephole2)
    peephole2_optimize (NULL);

  mdr_try_propagate_move (first_insn);

  find_basic_blocks (first_insn, max_reg_num (), 0);
  life_analysis (first_insn, 0, PROP_FINAL);

  ip2k_reorg_split_simode = 1;
  split_all_insns (0);

  mdr_try_remove_redundant_insns (first_insn);

  mdr_try_propagate_move (first_insn);

  reload_cse_regs (first_insn);
  find_basic_blocks (first_insn, max_reg_num (), 0);
  life_analysis (first_insn, 0, PROP_FINAL);
  if (flag_peephole2)
    peephole2_optimize (NULL);

  mdr_try_propagate_move (first_insn);

  find_basic_blocks (first_insn, max_reg_num (), 0);
  life_analysis (first_insn, 0, PROP_FINAL);

  ip2k_reorg_split_himode = 1;
  ip2k_reorg_merge_qimode = 1;
  split_all_insns (0);

  mdr_try_remove_redundant_insns (first_insn);
  mdr_try_propagate_clr (first_insn);
  mdr_try_propagate_move (first_insn);

  mdr_try_dp_reload_elim (first_insn);
  mdr_try_move_dp_reload (first_insn);

  rebuild_jump_labels (first_insn);

  /* Call to  jump_optimize (...) was here, but now I removed it.  */
  
  find_basic_blocks (first_insn, max_reg_num (), 0);
  life_analysis (first_insn, 0, PROP_FINAL);
  if (flag_peephole2)
    peephole2_optimize (NULL);

  mdr_try_propagate_move (first_insn);

  find_basic_blocks (first_insn, max_reg_num (), 0);
  life_analysis (first_insn, 0, PROP_FINAL);
  mdr_try_remove_redundant_insns (first_insn);

  mdr_try_propagate_clr (first_insn);
  mdr_try_propagate_move (first_insn);

  find_basic_blocks (first_insn, max_reg_num (), 0);
  life_analysis (first_insn, 0, PROP_FINAL);

  ip2k_reorg_split_qimode = 1;
  split_all_insns (0);

  mdr_try_wreg_elim (first_insn);
  mdr_try_propagate_move (first_insn);

  find_basic_blocks (first_insn, max_reg_num (), 0);
  life_analysis (first_insn, 0, PROP_FINAL);
#endif
}

static void
ip2k_init_libfuncs (void)
{
  set_optab_libfunc (smul_optab, SImode, "_mulsi3");
  set_optab_libfunc (smul_optab, DImode, "_muldi3");
  set_optab_libfunc (cmp_optab,  HImode, "_cmphi2");
  set_optab_libfunc (cmp_optab,  SImode, "_cmpsi2");
}

/* Returns a bit position if mask contains only a single bit.  Returns -1 if
   there were zero or more than one set bits.  */
int
find_one_set_bit_p (HOST_WIDE_INT mask)
{
  int i;
  unsigned HOST_WIDE_INT n = mask;
  for (i = 0; i < 32; i++)
    {
      if (n & 0x80000000UL)
	{
	  if (n & 0x7fffffffUL)
	    return -1;
	  else
	    return 31 - i;
	}
      n <<= 1;
    }
  return -1;
}

/* Returns a bit position if mask contains only a single clear bit.
   Returns -1 if there were zero or more than one clear bits.  */
int
find_one_clear_bit_p (HOST_WIDE_INT mask)
{
  int i;
  unsigned HOST_WIDE_INT n = mask;
  for (i = 0; i < 32; i++)
    {
      if ((n & 0x80000000UL) == 0UL)
	{
	  if ((n & 0x7fffffffUL) != 0x7fffffffUL)
	    return -1;
	  else
	    return 31 - i;
	}
      n <<= 1;
      n |= 1;
    }
  return -1;
}


/* Split a move into two smaller pieces.
   MODE indicates the reduced mode.  OPERANDS[0] is the original destination
   OPERANDS[1] is the original src.  The new destinations are
   OPERANDS[2] and OPERANDS[4], while the new sources are OPERANDS[3]
   and OPERANDS[5].  */

void
ip2k_split_words (enum machine_mode nmode, enum machine_mode omode,
		  rtx *operands)
{
  rtx dl, dh;			/* src/dest pieces.  */
  rtx sl, sh;
  int move_high_first = 0;	/* Assume no overlap.  */
  int pushflag = 0;

  switch (GET_CODE (operands[0])) /* DEST */
    {
    case SUBREG:
    case REG:
      if ((GET_CODE (operands[1]) == REG
	   || GET_CODE (operands[1]) == SUBREG)
	  && (true_regnum (operands[0]) <= true_regnum (operands[1])
	      || (true_regnum (operands[1])
		  + GET_MODE_SIZE (omode) - 1 < true_regnum (operands[0]))))
	move_high_first = 1;

      if (GET_CODE (operands[0]) == SUBREG)
	{
	  dl = simplify_gen_subreg (nmode, operands[0], omode,
				    GET_MODE_SIZE (nmode));
	  dh = simplify_gen_subreg (nmode, operands[0], omode, 0);
	}
      else if (GET_CODE (operands[0]) == REG && ! IS_PSEUDO_P (operands[0]))
	{
	  int	r = REGNO (operands[0]);
	  dh = gen_rtx_REG (nmode, r);
	  dl = gen_rtx_REG (nmode, r + HARD_REGNO_NREGS (r, nmode));
	}
      else
	{
	  dh = gen_rtx_SUBREG (nmode, operands[0], 0);
	  dl = gen_rtx_SUBREG (nmode, operands[0], GET_MODE_SIZE (nmode));
	}
      break;

    case MEM:
      switch (GET_CODE (XEXP (operands[0], 0)))
	{
	case POST_INC:
	  abort ();
	case POST_DEC:
	  dl = dh = gen_rtx_MEM (nmode, XEXP (operands[0], 0));
	  pushflag = 1;
	  break;
	default:
	  dl = change_address (operands[0], nmode,
			       plus_constant (XEXP (operands[0], 0),
					      GET_MODE_SIZE (nmode)));
	  dh = gen_rtx_MEM (nmode, XEXP (operands[0], 0));
	}
      break;
    default:
      abort ();
    }

  switch (GET_CODE (operands[1]))
    {
    case REG:
      if (! IS_PSEUDO_P (operands[1]))
	{
	  int r = REGNO (operands[1]);

	  sh = gen_rtx_REG (nmode, r);
	  sl = gen_rtx_REG (nmode, r + HARD_REGNO_NREGS (r, nmode));
	}
      else
	{
	  sh = gen_rtx_SUBREG (nmode, operands[1], 0);
	  sl = gen_rtx_SUBREG (nmode, operands[1], GET_MODE_SIZE (nmode));
	}
      break;

    case CONST_DOUBLE:
      if (operands[1] == const0_rtx)
	sh = sl = const0_rtx;
      else
	{
	  if (GET_MODE (operands[0]) != DImode)
	    {
	      REAL_VALUE_TYPE rv;
	      long value;

	      REAL_VALUE_FROM_CONST_DOUBLE (rv, operands[1]);
	      REAL_VALUE_TO_TARGET_SINGLE (rv, value);

	      sh = gen_int_mode ((value >> 16) & 0xffff, nmode);
	      sl = gen_int_mode (value & 0xffff, nmode);
	    }
	  else
	    {
	      sh = gen_int_mode (CONST_DOUBLE_HIGH (operands[1]), nmode);
 	      sl = gen_int_mode (CONST_DOUBLE_LOW (operands[1]), nmode);
	    }
	}
      break;

    case CONST_INT:
      if (operands[1] == const0_rtx)
	sh = sl = const0_rtx;
      else
	{
	  int val = INTVAL (operands[1]);
	  int vl, vh;

	  switch (nmode)
	    {
	    case QImode:
	      vh = (val >> 8) & 0xff;
	      vl = val & 0xff;
	      break;

	    case HImode:
	      vh = (val >> 16) & 0xffff;
	      vl = val & 0xffff;
	      break;

	    case SImode:
	      if (val < 0)	/* sign extend  */
		vh = -1;
	      else
		vh = 0;
	      vl = val;		/* Give low 32 bits back.  */
	      break;

	    default:
	      abort ();
	    }
	    
	  sl = gen_int_mode (vl, nmode);
	  sh = gen_int_mode (vh, nmode);
	}
      break;

    case SUBREG:
      sl = simplify_gen_subreg (nmode, operands[1], omode,
				GET_MODE_SIZE (nmode));
      sh = simplify_gen_subreg (nmode, operands[1], omode, 0);
      break;

    case MEM:
      switch (GET_CODE (XEXP (operands[1], 0)))
	{
	case POST_DEC:
	case POST_INC:
	  abort ();
	  break;

	default:
	  /* Worry about splitting stack pushes.  */
	  if (pushflag && ip2k_address_uses_reg_p (operands[1], REG_SP))
	    sl = sh = change_address (operands[1], nmode,
				      plus_constant (XEXP (operands[1], 0),
						     GET_MODE_SIZE (nmode)));
	  else
	    {
	      sl = change_address (operands[1], nmode,
				   plus_constant (XEXP (operands[1], 0),
						  GET_MODE_SIZE (nmode)));
	      sh = gen_rtx_MEM (nmode, XEXP (operands[1], 0));
	    }
	}
      break;

    default:
      abort ();
    }

  if (move_high_first)
    {
      operands[2] = dh;
      operands[3] = sh;
      operands[4] = dl;
      operands[5] = sl;
    }
  else
    {
      operands[2] = dl;
      operands[3] = sl;
      operands[4] = dh;
      operands[5] = sh;
    }
  return;
}

/* Get the low half of an operand.  */
rtx
ip2k_get_low_half (rtx x, enum machine_mode mode)
{
  switch (GET_CODE (x))
    {
    case REG:
      if (! IS_PSEUDO_P (x))
	{
	  unsigned int r = REGNO (x);

	  return gen_rtx_REG (mode, r + HARD_REGNO_NREGS (r, mode));
	}
      else
	{
	  return gen_rtx_SUBREG (mode, x, GET_MODE_SIZE (mode));
	}
      break;

    case CONST_DOUBLE:
      if (x == const0_rtx)
	return const0_rtx;
      else
	{
	  if (mode != SImode)
	    {
	      REAL_VALUE_TYPE rv;
	      long value;

	      REAL_VALUE_FROM_CONST_DOUBLE (rv, x);
	      REAL_VALUE_TO_TARGET_SINGLE (rv, value);

	      return gen_int_mode (value & 0xffff, mode);
	    }
	  else
 	    return gen_int_mode (CONST_DOUBLE_LOW (x), mode);
	}
      break;

    case CONST_INT:
      if (x == const0_rtx)
	return const0_rtx;
      else
	{
	  int val = INTVAL (x);
	  int vl, vh;

	  switch (mode)
	    {
	    case QImode:
	      vh = (val >> 8) & 0xff;
	      vl = val & 0xff;
	      break;

	    case HImode:
	      vh = (val >> 16) & 0xffff;
	      vl = val & 0xffff;
	      break;

	    case SImode:
	      if (val < 0)	/* sign extend */
		vh = -1;
	      else
		vh = 0;
	      vl = val;		/* Give low 32 bits back.  */
	      break;

	    default:
	      abort ();
	    }
	    
	  return gen_int_mode (vl, mode);
	}
      break;

    case SUBREG:
      return simplify_gen_subreg (mode, x, GET_MODE (x), GET_MODE_SIZE (mode));

    case MEM:
      switch (GET_CODE (XEXP (x, 0)))
	{
	case POST_DEC:
	case POST_INC:
	  abort ();
	  break;

	default:
	  return change_address (x, mode,
				 plus_constant (XEXP (x, 0),
						GET_MODE_SIZE (mode)));
	}
      break;

    default:
      abort ();
    }
  return NULL_RTX;
}

/* Get the high half of an operand.  */
rtx
ip2k_get_high_half (rtx x, enum machine_mode mode)
{
  switch (GET_CODE (x))
    {
    case REG:
      if (! IS_PSEUDO_P (x))
	{
	  unsigned int r = REGNO (x);

	  return gen_rtx_REG (mode, r);
	}
      else
	{
	  return gen_rtx_SUBREG (mode, x, 0);
	}
      break;

    case CONST_DOUBLE:
      if (x == const0_rtx)
	return const0_rtx;
      else
	{
	  if (mode != SImode)
	    {
	      REAL_VALUE_TYPE rv;
	      long value;

	      REAL_VALUE_FROM_CONST_DOUBLE (rv, x);
	      REAL_VALUE_TO_TARGET_SINGLE (rv, value);

	      return gen_int_mode ((value >> 16) & 0xffff, mode);
	    }
	  else
	    return gen_int_mode (CONST_DOUBLE_HIGH (x), mode);
	}
      break;

    case CONST_INT:
      if (x == const0_rtx)
	return const0_rtx;
      else
	{
	  int val = INTVAL (x);
	  int vl, vh;

	  switch (mode)
	  {
	    case QImode:
	      vh = (val >> 8) & 0xff;
	      vl = val & 0xff;
	      break;

	    case HImode:
	      vh = (val >> 16) & 0xffff;
	      vl = val & 0xffff;
	      break;

	    case SImode:
	      if (val < 0)	/* sign extend */
		vh = -1;
	      else
		vh = 0;
	      vl = val;		/* Give low 32 bits back.  */
	      break;

	    default:
	      abort ();
	    }
	    
	  return gen_int_mode (vh, mode);
	}
      break;

    case SUBREG:
      return simplify_gen_subreg (mode, x, GET_MODE (x), 0);
      break;

    case MEM:
      switch (GET_CODE (XEXP (x, 0)))
	{
	case POST_DEC:
	case POST_INC:
	  abort ();
	  break;

	default:
	  return change_address (x, mode, plus_constant (XEXP (x, 0), 0));
	}
      break;

    default:
      abort ();
    }
  return NULL_RTX;
}

/* Does address X use register R. Only valid for REG_SP, REG_DP, REG_IP
   or REG_FP.  */

int
ip2k_address_uses_reg_p (rtx x, unsigned int r)
{
  if (GET_CODE (x) != MEM)
    return 0;

  x = XEXP (x, 0);

  while (1)
    switch (GET_CODE (x))
      {
      case POST_DEC:
      case POST_INC:
      case PRE_DEC:
      case PRE_INC:
	x = XEXP (x, 0);
	break;

      case PLUS:
	if (ip2k_address_uses_reg_p (XEXP (x, 1), r))
	  return 1;

	x = XEXP (x, 0);
	break;

      case SUBREG:
	/* Ignore subwords.  */
	x = SUBREG_REG (x);
	break;

      case REG:
	/* Have to consider that r might be LSB of a pointer reg.  */
	return ((REGNO (x) == r) || (REGNO (x) == (r - 1))) ? 1 : 0;

      case MEM:
	/* We might be looking at a (mem:BLK (mem (...)))  */
	x = XEXP (x, 0);
	break;

      default:
	return 0;
      };
}

/* Does the queried XEXP not use a particular register?  If we're certain
   that it doesn't then we return TRUE otherwise we assume FALSE.  */

int
ip2k_xexp_not_uses_reg_p (rtx x, unsigned int r, int rsz)
{
  switch (GET_CODE (x))
    {
    case REG:
      {
	int msz = GET_MODE_SIZE (GET_MODE (x));

        return (((REGNO (x) + msz - 1) < r)
		|| (REGNO (x) > (r + rsz - 1)));
      }

    case MEM:
      return !ip2k_address_uses_reg_p (x, r);

    case LABEL_REF:
    case SYMBOL_REF:
    case CONST:
    case CONST_INT:
    case CONST_DOUBLE:
    case CC0:
    case PC:
      return 1;

    default:
      return 0;
    }
}

/* Does the queried XEXP not use a particular register?  If we're certain
   that it doesn't then we return TRUE otherwise we assume FALSE.  */

int
ip2k_composite_xexp_not_uses_reg_p (rtx x, unsigned int r, int rsz)
{
  if (GET_RTX_CLASS (GET_CODE (x)) == 'b')
    return (ip2k_composite_xexp_not_uses_reg_p (XEXP (x, 0), r, rsz)
	    && ip2k_composite_xexp_not_uses_reg_p (XEXP (x, 1), r, rsz)
	    && ip2k_composite_xexp_not_uses_reg_p (XEXP (x, 2), r, rsz));

  if (GET_RTX_CLASS (GET_CODE (x)) == '2'
      || GET_RTX_CLASS (GET_CODE (x)) == 'c'
      || GET_RTX_CLASS (GET_CODE (x)) == '<')
    return (ip2k_composite_xexp_not_uses_reg_p (XEXP (x, 0), r, rsz)
	    && ip2k_composite_xexp_not_uses_reg_p (XEXP (x, 1), r, rsz));

  if (GET_RTX_CLASS (GET_CODE (x)) == '1'
      || GET_RTX_CLASS (GET_CODE (x)) == '3')
    return ip2k_composite_xexp_not_uses_reg_p (XEXP (x, 0), r, rsz);

  return ip2k_xexp_not_uses_reg_p (x, r, rsz);
}

/* Does the queried XEXP not use CC0?  If we're certain that
   it doesn't then we return TRUE otherwise we assume FALSE.  */

int
ip2k_composite_xexp_not_uses_cc0_p (rtx x)
{
  if (GET_RTX_CLASS (GET_CODE (x)) == 'b')
    return (ip2k_composite_xexp_not_uses_cc0_p (XEXP (x, 0))
	    && ip2k_composite_xexp_not_uses_cc0_p (XEXP (x, 1))
	    && ip2k_composite_xexp_not_uses_cc0_p (XEXP (x, 2)));

  if (GET_RTX_CLASS (GET_CODE (x)) == '2'
      || GET_RTX_CLASS (GET_CODE (x)) == 'c'
      || GET_RTX_CLASS (GET_CODE (x)) == '<')
    return (ip2k_composite_xexp_not_uses_cc0_p (XEXP (x, 0))
	    && ip2k_composite_xexp_not_uses_cc0_p (XEXP (x, 1)));

  if (GET_RTX_CLASS (GET_CODE (x)) == '1'
      || GET_RTX_CLASS (GET_CODE (x)) == '3')
    return ip2k_composite_xexp_not_uses_cc0_p (XEXP (x, 0));

  return GET_CODE (x) != CC0;
}

int
ip2k_split_dest_operand (rtx x, enum machine_mode mode)
{
  return nonimmediate_operand (x, mode) || push_operand (x, mode);
}

int
ip2k_nonptr_operand (rtx x, enum machine_mode mode)
{
  return register_operand (x, mode) && !ip2k_ptr_operand (x, mode);
}

/* Is X a reference to IP or DP or SP?  */

int
ip2k_ptr_operand (rtx x, enum machine_mode mode)

{
  if (GET_CODE (x) == SUBREG)
    x = SUBREG_REG (x);

  return (REG_P (x)
	  && (mode == HImode || mode == VOIDmode)
	  && (REGNO (x) == REG_IP
	      || REGNO (x) == REG_DP
	      || REGNO (x) == REG_SP));
}

int
ip2k_sp_operand (rtx x, enum machine_mode mode ATTRIBUTE_UNUSED)

{
  return REG_P (x) && REGNO (x) == REG_SP;
}

int
ip2k_ip_operand (rtx x, enum machine_mode mode)

{
  if (GET_CODE (x) != MEM)
    return 0;

  x = XEXP (x, 0);

  if (GET_CODE (x) == PLUS && XEXP (x, 1) == const0_rtx)
    x = XEXP (x, 0);

  if (! REG_P (x))
    return 0;

  if (GET_MODE_SIZE (mode) > 1)
    return 0;			/* Can't access offset bytes.  */

  return REGNO (x) == REG_IP;
}

/* Is X a memory address suitable for SP or DP relative addressing?  */
int
ip2k_short_operand (rtx x, enum machine_mode mode)
{
  int r;
  unsigned int offs = 0;

  if (! memory_operand (x, mode))
    return 0;			/* Got to be a memory address.  */

  x = XEXP (x, 0);
  switch (GET_CODE (x))
    {
    default:
      return 0;

    case PLUS:
      if (! REG_P (XEXP (x, 0))
	  || GET_CODE (XEXP (x, 1)) != CONST_INT)
	return 0;

      offs = INTVAL (XEXP (x, 1));

      if (128 <= offs)
	return 0;

      x = XEXP (x, 0);

      /* fall through  */

    case REG:
      if (IS_PSEUDO_P (x))
	return 0;		/* Optimistic - doesn't work.  */

      r = REGNO (x);

      /* For 'S' constraint, we presume that no IP adjustment
	 simulation is performed - so only QI mode allows IP to be a
	 short offset address.  All other IP references must be
	 handled by 'R' constraints.  */
      if (r == REG_IP && offs == 0 && GET_MODE_SIZE (mode) <= 1)
	return 1;

      return (r == REG_SP || r == REG_DP);
    }
}

int
ip2k_nonsp_reg_operand (rtx x, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  if (GET_CODE (x) == SUBREG)
    x = SUBREG_REG (x);

  return (REG_P (x) && REGNO (x) != REG_SP);
}

int
ip2k_gen_operand (rtx x, enum machine_mode mode)
{
  return ip2k_short_operand (x, mode)
    || (GET_CODE (x) == SUBREG
	&& REG_P (SUBREG_REG (x)))
    || (ip2k_nonsp_reg_operand (x, mode));
}

int
ip2k_extra_constraint (rtx x, int c)
{ 
  switch (c)
    {
    case 'S':			/* Allow offset in stack frame...  */
      return ip2k_short_operand (x, GET_MODE (x));

    case 'R':
      return ip2k_ip_operand (x, GET_MODE (x));

    case 'T':			/* Constant int or .data address.  */
      return CONSTANT_P (x) && is_regfile_address (x);

    default:
      return 0;
    }
}

int
ip2k_unary_operator (rtx op, enum machine_mode mode)
{
  return ((mode == VOIDmode || GET_MODE (op) == mode)
	  && GET_RTX_CLASS (GET_CODE (op)) == '1');
}

int
ip2k_binary_operator (rtx op, enum machine_mode mode)
{
  return ((mode == VOIDmode || GET_MODE (op) == mode)
	  && (GET_RTX_CLASS (GET_CODE (op)) == 'c'
	      || GET_RTX_CLASS (GET_CODE (op)) == '2'));
}

int
ip2k_symbol_ref_operand (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  /* We define an IP2k symbol ref to be either a direct reference or one
     with a constant offset.  */
  return (GET_CODE (op) == SYMBOL_REF)
	 || (GET_CODE (op) == CONST
	     && GET_CODE (XEXP (op, 0)) == PLUS
	     && GET_CODE (XEXP (XEXP (op, 0), 0)) == SYMBOL_REF);
}

int
ip2k_signed_comparison_operator (rtx op, enum machine_mode mode)
{
  return (comparison_operator (op, mode)
    && signed_condition (GET_CODE (op)) == GET_CODE (op));
}

int
ip2k_unsigned_comparison_operator (rtx op, enum machine_mode mode)
{
  return (comparison_operator (op, mode)
          && unsigned_condition (GET_CODE (op)) == GET_CODE (op));
}
