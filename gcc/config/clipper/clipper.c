/* Subroutines for insn-output.c for Clipper
   Copyright (C) 1987, 1988, 1991, 1997, 1998, 1999, 2000, 2001
   Free Software Foundation, Inc.
   Contributed by Holger Teutsch (holger@hotbso.rhein-main.de)

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
#include "output.h"
#include "insn-attr.h"
#include "tree.h"
#include "expr.h"
#include "optabs.h"
#include "libfuncs.h"
#include "c-tree.h"
#include "function.h"
#include "flags.h"
#include "recog.h"
#include "tm_p.h"
#include "target.h"
#include "target-def.h"

static void clipper_output_function_prologue PARAMS ((FILE *, HOST_WIDE_INT));
static void clipper_output_function_epilogue PARAMS ((FILE *, HOST_WIDE_INT));
static void clix_asm_out_constructor PARAMS ((rtx, int));
static void clix_asm_out_destructor PARAMS ((rtx, int));

extern char regs_ever_live[];

extern int frame_pointer_needed;

static int frame_size;

/* Initialize the GCC target structure.  */
#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.word\t"

#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE clipper_output_function_prologue
#undef TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE clipper_output_function_epilogue

struct gcc_target targetm = TARGET_INITIALIZER;

/* Compute size of a clipper stack frame where 'lsize' is the required
   space for local variables.  */

int
clipper_frame_size (lsize)
     int lsize;
{
  int i, size;				/* total size of frame */
  int save_size;
  save_size = 0;			/* compute size for reg saves */

  for (i = 16; i < 32; i++)
    if (regs_ever_live[i] && !call_used_regs[i])
      save_size += 8;

  for (i = 0; i < 16; i++)
    if (regs_ever_live[i] && !call_used_regs[i])
      save_size += 4;

  size = lsize + save_size;

  size = (size + 7) & ~7;		/* align to 64 Bit */
  return size;
}

/* Prologue and epilogue output
   Function is entered with pc pushed, i.e. stack is 32 bit aligned

   current_function_args_size == 0 means that the current function's args
   are passed totally in registers i.e fp is not used as ap.
   If frame_size is also 0 the current function does not push anything and
   can run with misaligned stack -> subq $4,sp / add $4,sp on entry and exit
   can be omitted.  */

static void
clipper_output_function_prologue (file, lsize)
     FILE *file;
     HOST_WIDE_INT lsize;			/* size for locals */
{
  int i, offset;
  int size;

  frame_size = size = clipper_frame_size (lsize);

  if (frame_pointer_needed)
    {
      fputs ("\tpushw  fp,sp\n", file);
      fputs ("\tmovw   sp,fp\n", file);
    }
  else if (size != 0 || current_function_args_size != 0)
    {
      size += 4;			/* keep stack aligned */
      frame_size = size;		/* must push data or access args */
    }

  if (size)
    {
      if (size < 16)
	fprintf (file, "\tsubq   $%d,sp\n", size);
      else
	fprintf (file, "\tsubi   $%d,sp\n", size);

      /* register save slots are relative to sp, because we have small positive
	 displacements and this works whether we have a frame pointer or not */

      offset = 0;
      for (i = 16; i < 32; i++)
	if (regs_ever_live[i] && !call_used_regs[i])
	  {
	    if (offset == 0)
	      fprintf (file, "\tstord  f%d,(sp)\n", i-16);
	    else
	      fprintf (file, "\tstord  f%d,%d(sp)\n", i-16, offset);
	    offset += 8;
	  }

      for (i = 0; i < 16; i++)
	if (regs_ever_live[i] && !call_used_regs[i])
	  {
	    if (offset == 0)
	      fprintf (file, "\tstorw  r%d,(sp)\n", i);
	    else
	      fprintf (file, "\tstorw  r%d,%d(sp)\n", i, offset);
	    offset += 4;
	  }
    }
}

static void
clipper_output_function_epilogue (file, size)
     FILE *file;
     HOST_WIDE_INT size ATTRIBUTE_UNUSED;
{
  int i, offset;

  if (frame_pointer_needed)
    {
      offset = -frame_size;

      for (i = 16; i < 32; i++)
	if (regs_ever_live[i] && !call_used_regs[i])
	  {
	    fprintf (file, "\tloadd  %d(fp),f%d\n", offset, i-16);
	    offset += 8;
	  }

      for (i = 0; i < 16; i++)
	if (regs_ever_live[i] && !call_used_regs[i])
	  {
	    fprintf (file, "\tloadw  %d(fp),r%d\n", offset, i);
	    offset += 4;
	  }

      fputs ("\tmovw   fp,sp\n\tpopw   sp,fp\n\tret    sp\n",
	     file);
    }

  else					/* no frame pointer */
    {
      offset = 0;

      for (i = 16; i < 32; i++)
	if (regs_ever_live[i] && !call_used_regs[i])
	  {
	    if (offset == 0)
	      fprintf (file, "\tloadd  (sp),f%d\n", i-16);
	    else
	      fprintf (file, "\tloadd  %d(sp),f%d\n", offset, i-16);
	    offset += 8;
	  }

      for (i = 0; i < 16; i++)
	if (regs_ever_live[i] && !call_used_regs[i])
	  {
	    if (offset == 0)
	      fprintf (file, "\tloadw  (sp),r%d\n", i);
	    else
	      fprintf (file, "\tloadw  %d(sp),r%d\n", offset, i);
	    offset += 4;
	  }

      if (frame_size > 0)
	{
	  if (frame_size < 16)
	    fprintf (file, "\taddq   $%d,sp\n", frame_size);
	  else
	    fprintf (file, "\taddi   $%d,sp\n", frame_size);
	}

      fputs ("\tret    sp\n", file);
    }
}

/*
 * blockmove
 *
 * clipper_movstr ()
 */
void
clipper_movstr (operands)
     rtx *operands;
{
  rtx dst,src,cnt,tmp,top,bottom=NULL_RTX,xops[3];
  int align;
  int fixed;

  extern FILE *asm_out_file;

  dst = operands[0];
  src = operands[1];
  /* don't change this operands[2]; gcc 2.3.3 doesn't honor clobber note */
  align = INTVAL (operands[3]);
  tmp = operands[4];
  cnt = operands[5];

  if (GET_CODE (operands[2]) == CONST_INT) /* fixed size move */
    {
      if ((fixed = INTVAL (operands[2])) <= 0)
	abort ();

      if (fixed <16)
	output_asm_insn ("loadq  %2,%5", operands);
      else
	output_asm_insn ("loadi  %2,%5", operands);
    }
  else
    {
      fixed = 0;
      bottom = (rtx)gen_label_rtx ();	/* need a bottom label */
      xops[0] = cnt; xops[1] = bottom;
      output_asm_insn ("movw   %2,%5", operands); /* count is scratch reg 5 */
      output_asm_insn ("brle   %l1", xops);
    }


  top = (rtx)gen_label_rtx ();		/* top of loop label */
  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L", CODE_LABEL_NUMBER (top));


  xops[0] = src; xops[1] = tmp; xops[2] = dst;

  if (fixed && (align & 0x3) == 0)	/* word aligned move with known size */
    {
      if (fixed >= 4)
	{
	  rtx xops1[2];
	  output_asm_insn(
	    "loadw  %a0,%1\n\taddq   $4,%0\n\tstorw  %1,%a2\n\taddq   $4,%2",
			  xops);

	  xops1[0] = cnt; xops1[1] = top;
	  output_asm_insn ("subq   $4,%0\n\tbrgt   %l1", xops1);
	}

      if (fixed & 0x2)
	{
	  output_asm_insn ("loadh  %a0,%1\n\tstorh  %1,%a2", xops);
	  if (fixed & 0x1)
	    output_asm_insn ("loadb  2%a0,%1\n\tstorb  %1,2%a2", xops);
	}
      else
	if (fixed & 0x1)
	  output_asm_insn ("loadb  %a0,%1\n\tstorb  %1,%a2", xops);
    }
  else
    {
      output_asm_insn(
	  "loadb  %a0,%1\n\taddq   $1,%0\n\tstorb  %1,%a2\n\taddq   $1,%2",
		      xops);

      xops[0] = cnt; xops[1] = top;
      output_asm_insn ("subq   $1,%0\n\tbrgt   %l1", xops);
    }

  if (fixed == 0)
    ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L", CODE_LABEL_NUMBER (bottom));
}


void
print_operand_address (file, addr)
     FILE *file;
     register rtx addr;
{
  rtx op0,op1;

  switch (GET_CODE (addr))
    {
    case REG:
      fprintf (file, "(%s)", reg_names[REGNO (addr)]);
      break;

    case PLUS:
      /* can be 'symbol + reg' or 'reg + reg' */

      op0 = XEXP (addr, 0);
      op1 = XEXP (addr, 1);

      if (GET_CODE (op0) == REG && GET_CODE (op1) == REG)
	{
	  fprintf (file, "[%s](%s)",
		   reg_names[REGNO (op0)], reg_names[REGNO (op1)]);
	  break;
	}

      if (GET_CODE (op0) == REG && CONSTANT_ADDRESS_P (op1))
	{
	  output_addr_const (file, op1);
	  fprintf (file, "(%s)", reg_names[REGNO (op0)]);
	  break;
	}

      if (GET_CODE (op1) == REG && CONSTANT_ADDRESS_P (op0))
	{
	  output_addr_const (file, op0);
	  fprintf (file, "(%s)", reg_names[REGNO (op1)]);
	  break;
	}
      abort ();				/* Oh no */

    default:
      output_addr_const (file, addr);
    }
}


const char *
rev_cond_name (op)
     rtx op;
{
  switch (GET_CODE (op))
    {
    case EQ:
      return "ne";
    case NE:
      return "eq";
    case LT:
      return "ge";
    case LE:
      return "gt";
    case GT:
      return "le";
    case GE:
      return "lt";
    case LTU:
      return "geu";
    case LEU:
      return "gtu";
    case GTU:
      return "leu";
    case GEU:
      return "ltu";

    default:
      abort ();
    }
}


/* Dump the argument register to the stack; return the location
   of the block.  */

struct rtx_def *
clipper_builtin_saveregs ()
{
  rtx block, addr, r0_addr, r1_addr, f0_addr, f1_addr, mem;
  int set = get_varargs_alias_set ();

  /* Allocate the save area for r0,r1,f0,f1 */

  block = assign_stack_local (BLKmode, 6 * UNITS_PER_WORD, 2 * BITS_PER_WORD);

  RTX_UNCHANGING_P (block) = 1;
  RTX_UNCHANGING_P (XEXP (block, 0)) = 1;

  addr = XEXP (block, 0);

  r0_addr = addr;
  r1_addr = plus_constant (addr, 4);
  f0_addr = plus_constant (addr, 8);
  f1_addr = plus_constant (addr, 16);

  /* Store int regs  */

  mem = gen_rtx_MEM (SImode, r0_addr);
  set_mem_alias_set (mem, set);
  emit_move_insn (mem, gen_rtx_REG (SImode, 0));

  mem = gen_rtx_MEM (SImode, r1_addr);
  set_mem_alias_set (mem, set);
  emit_move_insn (mem, gen_rtx_REG (SImode, 1));

  /* Store float regs  */

  mem = gen_rtx_MEM (DFmode, f0_addr);
  set_mem_alias_set (mem, set);
  emit_move_insn (mem, gen_rtx_REG (DFmode, 16));

  mem = gen_rtx_MEM (DFmode, f1_addr);
  set_mem_alias_set (mem, set);
  emit_move_insn (mem, gen_rtx_REG (DFmode, 17));

  return addr;
}

tree
clipper_build_va_list ()
{
  tree record, ap, reg, num;

  /*
    struct
    {
      int __va_ap;		// pointer to stack args
      void *__va_reg[4];	// pointer to r0,f0,r1,f1
      int __va_num;		// number of args processed
    };
  */

  record = make_node (RECORD_TYPE);

  num = build_decl (FIELD_DECL, get_identifier ("__va_num"),
		    integer_type_node);
  DECL_FIELD_CONTEXT (num) = record;

  reg = build_decl (FIELD_DECL, get_identifier ("__va_reg"),
		    build_array_type (ptr_type_node,
				      build_index_type (build_int_2 (3, 0))));
  DECL_FIELD_CONTEXT (reg) = record;
  TREE_CHAIN (reg) = num;

  ap = build_decl (FIELD_DECL, get_identifier ("__va_ap"),
		   integer_type_node);
  DECL_FIELD_CONTEXT (ap) = record;
  TREE_CHAIN (ap) = reg;

  TYPE_FIELDS (record) = ap;
  layout_type (record);

  return record;
}

void
clipper_va_start (stdarg_p, valist, nextarg)
     int stdarg_p;
     tree valist;
     rtx nextarg ATTRIBUTE_UNUSED;
{
  tree ap_field, reg_field, num_field;
  tree t, u, save_area;

  ap_field = TYPE_FIELDS (TREE_TYPE (valist));
  reg_field = TREE_CHAIN (ap_field);
  num_field = TREE_CHAIN (reg_field);

  ap_field = build (COMPONENT_REF, TREE_TYPE (ap_field), valist, ap_field);
  reg_field = build (COMPONENT_REF, TREE_TYPE (reg_field), valist, reg_field);
  num_field = build (COMPONENT_REF, TREE_TYPE (num_field), valist, num_field);

  /* Call __builtin_saveregs to save r0, r1, f0, and f1 in a block.  */

  save_area = make_tree (integer_type_node, expand_builtin_saveregs ());

  /* Set __va_ap.  */

  t = make_tree (ptr_type_node, virtual_incoming_args_rtx);
  if (stdarg_p && current_function_args_info.size != 0)
    t = build (PLUS_EXPR, ptr_type_node, t,
	       build_int_2 (current_function_args_info.size, 0));
  t = build (MODIFY_EXPR, TREE_TYPE (ap_field), ap_field, t);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  /* Set the four entries of __va_reg.  */

  t = build1 (NOP_EXPR, ptr_type_node, save_area);
  u = build (ARRAY_REF, ptr_type_node, reg_field, build_int_2 (0, 0));
  t = build (MODIFY_EXPR, ptr_type_node, u, t);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  t = fold (build (PLUS_EXPR, integer_type_node, save_area,
		   build_int_2 (8, 0)));
  t = build1 (NOP_EXPR, ptr_type_node, save_area);
  u = build (ARRAY_REF, ptr_type_node, reg_field, build_int_2 (1, 0));
  t = build (MODIFY_EXPR, ptr_type_node, u, t);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  t = fold (build (PLUS_EXPR, integer_type_node, save_area,
		   build_int_2 (4, 0)));
  t = build1 (NOP_EXPR, ptr_type_node, save_area);
  u = build (ARRAY_REF, ptr_type_node, reg_field, build_int_2 (2, 0));
  t = build (MODIFY_EXPR, ptr_type_node, u, t);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  t = fold (build (PLUS_EXPR, integer_type_node, save_area,
		   build_int_2 (16, 0)));
  t = build1 (NOP_EXPR, ptr_type_node, save_area);
  u = build (ARRAY_REF, ptr_type_node, reg_field, build_int_2 (3, 0));
  t = build (MODIFY_EXPR, ptr_type_node, u, t);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  /* Set __va_num.  */

  t = build_int_2 (current_function_args_info.num, 0);
  t = build (MODIFY_EXPR, TREE_TYPE (num_field), num_field, t);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
}

rtx
clipper_va_arg (valist, type)
     tree valist, type;
{
  tree ap_field, reg_field, num_field;
  tree addr, t;
  HOST_WIDE_INT align;
  rtx addr_rtx, over_label = NULL_RTX, tr;

  /*
    Integers:

    if (VA.__va_num < 2)
      addr = VA.__va_reg[2 * VA.__va_num];
    else
      addr = round(VA.__va_ap), VA.__va_ap = round(VA.__va_ap) + sizeof(TYPE);
    VA.__va_num++;

    Floats:

    if (VA.__va_num < 2)
      addr = VA.__va_reg[2 * VA.__va_num + 1];
    else
      addr = round(VA.__va_ap), VA.__va_ap = round(VA.__va_ap) + sizeof(TYPE);
    VA.__va_num++;

    Aggregates:

    addr = round(VA.__va_ap), VA.__va_ap = round(VA.__va_ap) + sizeof(TYPE);
    VA.__va_num++;
  */

  ap_field = TYPE_FIELDS (TREE_TYPE (valist));
  reg_field = TREE_CHAIN (ap_field);
  num_field = TREE_CHAIN (reg_field);

  ap_field = build (COMPONENT_REF, TREE_TYPE (ap_field), valist, ap_field);
  reg_field = build (COMPONENT_REF, TREE_TYPE (reg_field), valist, reg_field);
  num_field = build (COMPONENT_REF, TREE_TYPE (num_field), valist, num_field);

  addr_rtx = gen_reg_rtx (Pmode);

  if (! AGGREGATE_TYPE_P (type))
    {
      tree inreg;
      rtx false_label;

      over_label = gen_label_rtx ();
      false_label = gen_label_rtx ();

      emit_cmp_and_jump_insns (expand_expr (num_field, NULL_RTX, 0,
					    OPTAB_LIB_WIDEN),
			       GEN_INT (2), GE, const0_rtx,
			       TYPE_MODE (TREE_TYPE (num_field)),
			       TREE_UNSIGNED (num_field), false_label);

      inreg = fold (build (MULT_EXPR, integer_type_node, num_field,
			   build_int_2 (2, 0)));
      if (FLOAT_TYPE_P (type))
	inreg = fold (build (PLUS_EXPR, integer_type_node, inreg,
			     build_int_2 (1, 0)));
      inreg = fold (build (ARRAY_REF, ptr_type_node, reg_field, inreg));

      tr = expand_expr (inreg, addr_rtx, VOIDmode, EXPAND_NORMAL);
      if (tr != addr_rtx)
	emit_move_insn (addr_rtx, tr);

      emit_jump_insn (gen_jump (over_label));
      emit_barrier ();
      emit_label (false_label);
    }

  /* Round to alignment of `type', or at least integer alignment.  */

  align = TYPE_ALIGN (type);
  if (align < TYPE_ALIGN (integer_type_node))
    align = TYPE_ALIGN (integer_type_node);
  align /= BITS_PER_UNIT;

  addr = fold (build (PLUS_EXPR, ptr_type_node, ap_field,
		      build_int_2 (align-1, 0)));
  addr = fold (build (BIT_AND_EXPR, ptr_type_node, addr,
		      build_int_2 (-align, -1)));
  addr = save_expr (addr);

  tr = expand_expr (addr, addr_rtx, Pmode, EXPAND_NORMAL);
  if (tr != addr_rtx)
    emit_move_insn (addr_rtx, tr);
  
  t = build (MODIFY_EXPR, TREE_TYPE (ap_field), ap_field,
	     build (PLUS_EXPR, TREE_TYPE (ap_field), 
		    addr, build_int_2 (int_size_in_bytes (type), 0)));
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  if (over_label)
    emit_label (over_label);

  t = build (MODIFY_EXPR, TREE_TYPE (num_field), num_field,
	     build (PLUS_EXPR, TREE_TYPE (num_field), 
		    num_field, build_int_2 (1, 0)));
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  return addr_rtx;
}

/* Return truth value of whether OP can be used as an word register
   operand. Reject (SUBREG:SI (REG:SF )) */

int
int_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode) &&
	  (GET_CODE (op) != SUBREG ||
	   GET_MODE_CLASS (GET_MODE (SUBREG_REG (op))) == MODE_INT));
}

/* Return truth value of whether OP can be used as a float register
   operand. Reject (SUBREG:SF (REG:SI )) )) */

int
fp_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode) &&
	  (GET_CODE (op) != SUBREG ||
	   GET_MODE_CLASS (GET_MODE (SUBREG_REG (op))) == MODE_FLOAT));
}

static void
clix_asm_out_constructor (symbol, priority)
     rtx symbol;
     int priority ATTRIBUTE_UNUSED;
{
  init_section ();
  fputs ("\tloada  ", asm_out_file);
  assemble_name (asm_out_file, XSTR (symbol, 0));
  fputs (",r0\n\tsubq   $8,sp\n\tstorw   r0,(sp)\n", asm_out_file);
}

static void
clix_asm_out_destructor (symbol, priority)
     rtx symbol;
     int priority ATTRIBUTE_UNUSED;
{
  fini_section ();
  assemble_integer (symbol, POINTER_SIZE / BITS_PER_UNIT, POINTER_SIZE, 1);
  assemble_integer (const0_rtx, POINTER_SIZE / BITS_PER_UNIT, POINTER_SIZE, 1);
}
