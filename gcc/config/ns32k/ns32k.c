/* Subroutines for assembler code output on the NS32000.
   Copyright (C) 1988, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001,
   2004, 2005
   Free Software Foundation, Inc.

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
#include "output.h"
#include "insn-attr.h"
#include "tree.h"
#include "function.h"
#include "expr.h"
#include "flags.h"
#include "recog.h"
#include "tm_p.h"
#include "target.h"
#include "target-def.h"
#include "toplev.h"

#ifdef OSF_OS
int ns32k_num_files = 0;
#endif

/* This duplicates reg_class_contents in reg_class.c, but maybe that isn't
   initialized in time. Also this is more convenient as an array of ints.
   We know that HARD_REG_SET fits in an unsigned int */

const unsigned int ns32k_reg_class_contents[N_REG_CLASSES][1] = REG_CLASS_CONTENTS;

const enum reg_class regclass_map[FIRST_PSEUDO_REGISTER] =
{
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  FLOAT_REG0, LONG_FLOAT_REG0, FLOAT_REGS, FLOAT_REGS,
  FLOAT_REGS, FLOAT_REGS, FLOAT_REGS, FLOAT_REGS,
  LONG_REGS, LONG_REGS, LONG_REGS, LONG_REGS,
  LONG_REGS, LONG_REGS, LONG_REGS, LONG_REGS,
  FRAME_POINTER_REG, STACK_POINTER_REG
};

static const char *const ns32k_out_reg_names[] = OUTPUT_REGISTER_NAMES;

static bool ns32k_handle_option (size_t, const char *, int);
static rtx gen_indexed_expr (rtx, rtx, rtx);
static const char *singlemove_string (rtx *);
static void move_tail (rtx[], int, int);
static tree ns32k_handle_fntype_attribute (tree *, tree, tree, int, bool *);
const struct attribute_spec ns32k_attribute_table[];
static void ns32k_output_function_prologue (FILE *, HOST_WIDE_INT);
static void ns32k_output_function_epilogue (FILE *, HOST_WIDE_INT);
static bool ns32k_rtx_costs (rtx, int, int, int *);
static int ns32k_address_cost (rtx);
static rtx ns32k_struct_value_rtx (tree, int);
static int ns32k_arg_partial_bytes (CUMULATIVE_ARGS *, enum machine_mode,
				    tree, bool);

/* Initialize the GCC target structure.  */
#undef TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE ns32k_attribute_table

#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.word\t"

#ifdef ENCORE_ASM
#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\t.double\t"
#endif

#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE ns32k_output_function_prologue
#undef TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE ns32k_output_function_epilogue

#undef TARGET_DEFAULT_TARGET_FLAGS
#define TARGET_DEFAULT_TARGET_FLAGS TARGET_DEFAULT
#undef TARGET_HANDLE_OPTION
#define TARGET_HANDLE_OPTION ns32k_handle_option

#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS ns32k_rtx_costs
#undef TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST ns32k_address_cost

#undef TARGET_STRUCT_VALUE_RTX
#define TARGET_STRUCT_VALUE_RTX ns32k_struct_value_rtx

#undef TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES ns32k_arg_partial_bytes

#undef TARGET_ASM_FILE_START_APP_OFF
#define TARGET_ASM_FILE_START_APP_OFF true

struct gcc_target targetm = TARGET_INITIALIZER;

/* Implement TARGET_HANDLE_OPTION.  */

static bool
ns32k_handle_option (size_t code, const char *arg ATTRIBUTE_UNUSED,
		     int value ATTRIBUTE_UNUSED)
{
  switch (code)
    {
    case OPT_m32081:
      target_flags &= ~MASK_32381;
      return true;

    case OPT_msoft_float:
      target_flags &= ~(MASK_32081 | MASK_32381);
      return true;

    case OPT_m32332:
      target_flags &= ~MASK_32532;
      return true;

    case OPT_m32032:
      target_flags &= ~(MASK_32332 | MASK_32532);
      return true;

    default:
      return true;
    }
}

/* Generate the assembly code for function entry.  FILE is a stdio
   stream to output the code to.  SIZE is an int: how many units of
   temporary storage to allocate.

   Refer to the array `regs_ever_live' to determine which registers to
   save; `regs_ever_live[I]' is nonzero if register number I is ever
   used in the function.  This function is responsible for knowing
   which registers should not be saved even if used.  */

/*
 * The function prologue for the ns32k is fairly simple.
 * If a frame pointer is needed (decided in reload.c ?) then
 * we need assembler of the form
 *
 *  # Save the oldframe pointer, set the new frame pointer, make space
 *  # on the stack and save any general purpose registers necessary
 *
 *  enter [<general purpose regs to save>], <local stack space>
 *
 *  movf  fn, tos    # Save any floating point registers necessary
 *  .
 *  .
 *
 * If a frame pointer is not needed we need assembler of the form
 *
 *  # Make space on the stack
 *
 *  adjspd <local stack space + 4>
 *
 *  # Save any general purpose registers necessary
 *
 *  save [<general purpose regs to save>]
 *
 *  movf  fn, tos    # Save any floating point registers necessary
 *  .
 *  .
 */

#if !defined (MERLIN_TARGET) && !defined (UTEK_ASM)

#if defined(IMMEDIATE_PREFIX) && IMMEDIATE_PREFIX
#define ADJSP(FILE, N) \
        fprintf (FILE, "\tadjspd %c" HOST_WIDE_INT_PRINT_DEC "\n", IMMEDIATE_PREFIX, (N))
#else
#define ADJSP(FILE, N) \
        fprintf (FILE, "\tadjspd " HOST_WIDE_INT_PRINT_DEC "\n", (N))
#endif

static void
ns32k_output_function_prologue (FILE *file, HOST_WIDE_INT size)
{
  register int regno, g_regs_used = 0;
  int used_regs_buf[8], *bufp = used_regs_buf;
  int used_fregs_buf[17], *fbufp = used_fregs_buf;

  for (regno = R0_REGNUM; regno < F0_REGNUM; regno++)
    if (regs_ever_live[regno]
	&& ! call_used_regs[regno])
      {
        *bufp++ = regno; g_regs_used++;
      }
  *bufp = -1;

  for (; regno < FRAME_POINTER_REGNUM; regno++)
    if (regs_ever_live[regno] && !call_used_regs[regno])
      {
        *fbufp++ = regno;
      }
  *fbufp = -1;

  bufp = used_regs_buf;
  if (frame_pointer_needed)
    fprintf (file, "\tenter [");
  else
    {
      if (size)
        ADJSP (file, size + 4);
      if (g_regs_used && g_regs_used > 4)
        fprintf (file, "\tsave [");
      else
	{
	  while (*bufp >= 0)
            fprintf (file, "\tmovd r%d,tos\n", *bufp++);
	  g_regs_used = 0;
	}
    }

  while (*bufp >= 0)
    {
      fprintf (file, "r%d", *bufp++);
      if (*bufp >= 0)
	fputc (',', file);
    }

  if (frame_pointer_needed)
    fprintf (file, "]," HOST_WIDE_INT_PRINT_DEC "\n", size);
  else if (g_regs_used)
    fprintf (file, "]\n");

  fbufp = used_fregs_buf;
  while (*fbufp >= 0)
    {
      if ((*fbufp & 1) || (fbufp[0] != fbufp[1] - 1))
	fprintf (file, "\tmovf %s,tos\n", ns32k_out_reg_names[*fbufp++]);
      else
	{
	  fprintf (file, "\tmovl %s,tos\n",
		   ns32k_out_reg_names[fbufp[0]]);
	  fbufp += 2;
	}
    }

  if (flag_pic && current_function_uses_pic_offset_table)
    {
      fprintf (file, "\tsprd sb,tos\n");
      if (TARGET_REGPARM)
	{
	  fprintf (file, "\taddr __GLOBAL_OFFSET_TABLE_(pc),tos\n");
	  fprintf (file, "\tlprd sb,tos\n");
	}
      else
	{
	  fprintf (file, "\taddr __GLOBAL_OFFSET_TABLE_(pc),r0\n");
	  fprintf (file, "\tlprd sb,r0\n");
	}
    }
}

#else /* MERLIN_TARGET || UTEK_ASM  */

/* This differs from the standard one above in printing a bitmask
   rather than a register list in the enter or save instruction.  */

static void
ns32k_output_function_prologue (file, size)
     FILE *file;
     HOST_WIDE_INT size;
{
  register int regno, g_regs_used = 0;
  int used_regs_buf[8], *bufp = used_regs_buf;
  int used_fregs_buf[8], *fbufp = used_fregs_buf;

  for (regno = 0; regno < 8; regno++)
    if (regs_ever_live[regno]
	&& ! call_used_regs[regno])
      {
	*bufp++ = regno; g_regs_used++;
      }
  *bufp = -1;

  for (; regno < 16; regno++)
    if (regs_ever_live[regno] && !call_used_regs[regno]) {
      *fbufp++ = regno;
    }
  *fbufp = -1;

  bufp = used_regs_buf;
  if (frame_pointer_needed)
    fprintf (file, "\tenter ");
  else if (g_regs_used)
    fprintf (file, "\tsave ");

  if (frame_pointer_needed || g_regs_used)
    {
      char mask = 0;
      while (*bufp >= 0)
	mask |= 1 << *bufp++;
      fprintf (file, "$0x%x", (int) mask & 0xff);
    }

  if (frame_pointer_needed)
#ifdef UTEK_ASM
    fprintf (file, ",$%d\n", size);
#else
    fprintf (file, ",%d\n", size);
#endif
  else if (g_regs_used)
    fprintf (file, "\n");

  fbufp = used_fregs_buf;
  while (*fbufp >= 0)
    {
      if ((*fbufp & 1) || (fbufp[0] != fbufp[1] - 1))
	fprintf (file, "\tmovf f%d,tos\n", *fbufp++ - 8);
      else
	{
	  fprintf (file, "\tmovl f%d,tos\n", fbufp[0] - 8);
	  fbufp += 2;
	}
    }
}

#endif /* MERLIN_TARGET || UTEK_ASM  */

/* This function generates the assembly code for function exit,
   on machines that need it.

   The function epilogue should not depend on the current stack pointer,
   if EXIT_IGNORE_STACK is nonzero.  That doesn't apply here.

   If a frame pointer is needed (decided in reload.c ?) then
   we need assembler of the form

    movf  tos, fn	# Restore any saved floating point registers
    .
    .

    # Restore any saved general purpose registers, restore the stack
    # pointer from the frame pointer, restore the old frame pointer.
    exit [<general purpose regs to save>]

   If a frame pointer is not needed we need assembler of the form
    # Restore any general purpose registers saved

    movf  tos, fn	# Restore any saved floating point registers
    .
    .
    .
    restore [<general purpose regs to save>]

    # reclaim space allocated on stack

    adjspd <-(local stack space + 4)> */

#if !defined (MERLIN_TARGET) && !defined (UTEK_ASM)

static void
ns32k_output_function_epilogue (FILE *file, HOST_WIDE_INT size)
{
  register int regno, g_regs_used = 0, f_regs_used = 0;
  int used_regs_buf[8], *bufp = used_regs_buf;
  int used_fregs_buf[17], *fbufp = used_fregs_buf;

  if (flag_pic && current_function_uses_pic_offset_table)
    fprintf (file, "\tlprd sb,tos\n");

  *fbufp++ = -2;
  for (regno = F0_REGNUM; regno < FRAME_POINTER_REGNUM; regno++)
    if (regs_ever_live[regno] && !call_used_regs[regno])
      {
	*fbufp++ = regno; f_regs_used++;
      }
  fbufp--;

  for (regno = 0; regno < F0_REGNUM; regno++)
    if (regs_ever_live[regno]
	&& ! call_used_regs[regno])
      {
        *bufp++ = regno; g_regs_used++;
      }

  while (fbufp > used_fregs_buf)
    {
      if ((*fbufp & 1) && fbufp[0] == fbufp[-1] + 1)
	{
	  fprintf (file, "\tmovl tos,%s\n",
		   ns32k_out_reg_names[fbufp[-1]]);
	  fbufp -= 2;
	}
      else fprintf (file, "\tmovf tos,%s\n", ns32k_out_reg_names[*fbufp--]);
    }

  if (frame_pointer_needed)
    fprintf (file, "\texit [");
  else
    {
      if (g_regs_used && g_regs_used > 4)
        fprintf (file, "\trestore [");
      else
        {
	  while (bufp > used_regs_buf)
            fprintf (file, "\tmovd tos,r%d\n", *--bufp);
	  g_regs_used = 0;
        }
    }

  while (bufp > used_regs_buf)
    {
      fprintf (file, "r%d", *--bufp);
      if (bufp > used_regs_buf)
	fputc (',', file);
    }

  if (g_regs_used || frame_pointer_needed)
    fprintf (file, "]\n");

  if (size && !frame_pointer_needed)
    ADJSP (file, -(size + 4));

  if (current_function_pops_args)
    fprintf (file, "\tret %d\n", current_function_pops_args);
  else
    fprintf (file, "\tret 0\n");
}

#else /* MERLIN_TARGET || UTEK_ASM  */

/* This differs from the standard one above in printing a bitmask
   rather than a register list in the exit or restore instruction.  */

static void
ns32k_output_function_epilogue (file, size)
     FILE *file;
     HOST_WIDE_INT size ATTRIBUTE_UNUSED;
{
  register int regno, g_regs_used = 0, f_regs_used = 0;
  int used_regs_buf[8], *bufp = used_regs_buf;
  int used_fregs_buf[8], *fbufp = used_fregs_buf;

  *fbufp++ = -2;
  for (regno = 8; regno < 16; regno++)
    if (regs_ever_live[regno] && !call_used_regs[regno]) {
      *fbufp++ = regno; f_regs_used++;
    }
  fbufp--;

  for (regno = 0; regno < 8; regno++)
    if (regs_ever_live[regno]
	&& ! call_used_regs[regno])
      {
	*bufp++ = regno; g_regs_used++;
      }

  while (fbufp > used_fregs_buf)
    {
      if ((*fbufp & 1) && fbufp[0] == fbufp[-1] + 1)
	{
	  fprintf (file, "\tmovl tos,f%d\n", fbufp[-1] - 8);
	  fbufp -= 2;
	}
      else fprintf (file, "\tmovf tos,f%d\n", *fbufp-- - 8);
    }

  if (frame_pointer_needed)
    fprintf (file, "\texit ");
  else if (g_regs_used)
    fprintf (file, "\trestore ");

  if (g_regs_used || frame_pointer_needed)
    {
      char mask = 0;

      while (bufp > used_regs_buf)
	{
	  /* Utek assembler takes care of reversing this */
	  mask |= 1 << *--bufp;
	}
      fprintf (file, "$0x%x\n", (int) mask & 0xff);
    }

#ifdef UTEK_ASM
  if (current_function_pops_args)
    fprintf (file, "\tret $%d\n", current_function_pops_args);
  else
    fprintf (file, "\tret $0\n");
#else
  if (current_function_pops_args)
    fprintf (file, "\tret %d\n", current_function_pops_args);
  else
    fprintf (file, "\tret 0\n");
#endif
}

#endif /* MERLIN_TARGET || UTEK_ASM  */

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE. */ 
int
hard_regno_mode_ok (int regno, enum machine_mode mode)
{
  int size = GET_MODE_UNIT_SIZE (mode);

  if (FLOAT_MODE_P (mode))
    {
      if (size == UNITS_PER_WORD && regno < L1_REGNUM)
	return 1;
      if (size == UNITS_PER_WORD * 2
	  && (((regno & 1) == 0 && regno < FRAME_POINTER_REGNUM)))
	return 1;
      return 0;
    }
  if (size == UNITS_PER_WORD * 2
      && (regno & 1) == 0 && regno < F0_REGNUM)
    return 1;
  if (size <= UNITS_PER_WORD
      && (regno < F0_REGNUM || regno == FRAME_POINTER_REGNUM
	  || regno == STACK_POINTER_REGNUM))
    return 1;
  return 0;
}

static bool
ns32k_rtx_costs (rtx x, int code, int outer_code ATTRIBUTE_UNUSED, int *total)
{
  switch (code)
    {
    case CONST_INT:
      if (INTVAL (x) <= 7 && INTVAL (x) >= -8)
	*total = 0;
      else if (INTVAL (x) < 0x2000 && INTVAL (x) >= -0x2000)
        *total = 1;
      else
	*total = 3;
      return true;

    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
      *total = 3;
      return true;

    case CONST_DOUBLE:
      *total = 5;
      return true;

    default:
      return false;
    }
}

int
register_move_cost (enum reg_class CLASS1, enum reg_class CLASS2)
{
  if (CLASS1 == NO_REGS || CLASS2 == NO_REGS)
    return 2;
  if ((SUBSET_P (CLASS1, FP_REGS) && !SUBSET_P (CLASS2, FP_REGS))
   || (!SUBSET_P (CLASS1, FP_REGS) && SUBSET_P (CLASS2, FP_REGS)))
    return 8;
  if (((CLASS1) == STACK_POINTER_REG && !SUBSET_P (CLASS2,GENERAL_REGS))
      || ((CLASS2) == STACK_POINTER_REG && !SUBSET_P (CLASS1,GENERAL_REGS)))
    return 6;
  if (((CLASS1) == FRAME_POINTER_REG && !SUBSET_P (CLASS2,GENERAL_REGS))
      || ((CLASS2) == FRAME_POINTER_REG && !SUBSET_P (CLASS1,GENERAL_REGS)))
    return 6;
  return 2;
}

#if 0
/* We made the insn definitions copy from floating point to general
  registers via the stack.  */
int
secondary_memory_needed (enum reg_class CLASS1,
			 enum reg_class CLASS2,
			 enum machine_mode M)
{
  int ret = ((SUBSET_P (CLASS1, FP_REGS) && !SUBSET_P (CLASS2, FP_REGS))
   || (!SUBSET_P (CLASS1, FP_REGS) && SUBSET_P (CLASS2, FP_REGS)));
  return ret;
}
#endif
    

/* TARGET_ADDRESS_COST calls this.  This function is not optimal
   for the 32032 & 32332, but it probably is better than
   the default.  */

static int
ns32k_address_cost (rtx operand)
{
  int cost = 0;

  switch (GET_CODE (operand))
    {
    case REG:
      cost += 1;
      break;

    case POST_DEC:
    case PRE_DEC:
      break;

    case CONST_INT:
      if (INTVAL (operand) <= 7 && INTVAL (operand) >= -8)
	break;
      if (INTVAL (operand) < 0x2000 && INTVAL (operand) >= -0x2000)
	{
	  cost +=1;
	  break;
	}
    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
      cost +=3;
      break;
    case CONST_DOUBLE:
      cost += 5;
      break;

    case MEM:
      cost += ns32k_address_cost (XEXP (operand, 0)) + 3;
      break;

    case MULT:
      cost += 2;
      /* FALLTHRU */
    case PLUS:
      cost += ns32k_address_cost (XEXP (operand, 0));
      cost += ns32k_address_cost (XEXP (operand, 1));
      break;

    default:
      break;
    }

  return cost;
}

/* Return the register class of a scratch register needed to copy IN into
   or out of a register in CLASS in MODE.  If it can be done directly,
   NO_REGS is returned.  */

enum reg_class
secondary_reload_class (enum reg_class class,
			enum machine_mode mode ATTRIBUTE_UNUSED,
			rtx in)
{
  int regno = true_regnum (in);

  if (regno >= FIRST_PSEUDO_REGISTER)
    regno = -1;

  if ((class == FRAME_POINTER_REG && regno == STACK_POINTER_REGNUM)
      || ( class == STACK_POINTER_REG && regno == FRAME_POINTER_REGNUM))
    return GENERAL_REGS;
  else
    return NO_REGS;
}

/* Generate the rtx that comes from an address expression in the md file */
/* The expression to be build is BASE[INDEX:SCALE].  To recognize this,
   scale must be converted from an exponent (from ASHIFT) to a
   multiplier (for MULT). */

static rtx
gen_indexed_expr (rtx base, rtx index, rtx scale)
{
  rtx addr;

  /* This generates an invalid addressing mode, if BASE is
     fp or sp.  This is handled by PRINT_OPERAND_ADDRESS.  */
  if (GET_CODE (base) != REG && GET_CODE (base) != CONST_INT)
    base = gen_rtx_MEM (SImode, base);
  addr = gen_rtx_MULT (SImode, index,
		       GEN_INT (1 << INTVAL (scale)));
  addr = gen_rtx_PLUS (SImode, base, addr);
  return addr;
}


/* Split one or more DImode RTL references into pairs of SImode
   references.  The RTL can be REG, offsettable MEM, integer constant, or
   CONST_DOUBLE.  "operands" is a pointer to an array of DImode RTL to
   split and "num" is its length.  lo_half and hi_half are output arrays
   that parallel "operands". */

void
split_di (rtx operands[], int num, rtx lo_half[], rtx hi_half[])
{
  while (num--)
    {
      if (GET_CODE (operands[num]) == REG)
	{
	  lo_half[num] = gen_rtx_REG (SImode, REGNO (operands[num]));
	  hi_half[num] = gen_rtx_REG (SImode, REGNO (operands[num]) + 1);
	}
      else if (CONSTANT_P (operands[num]))
	{
	  split_double (operands[num], &lo_half[num], &hi_half[num]);
	}
      else if (offsettable_memref_p (operands[num]))
	{
	  lo_half[num] = operands[num];
	  hi_half[num] = adjust_address (operands[num], SImode, 4);
	}
      else
	abort ();
    }
}

/* Return the best assembler insn template
   for moving operands[1] into operands[0] as a fullword.  */

static const char *
singlemove_string (rtx *operands)
{
  if (GET_CODE (operands[1]) == CONST_INT
      && INTVAL (operands[1]) <= 7
      && INTVAL (operands[1]) >= -8)
    return "movqd %1,%0";
  return "movd %1,%0";
}

const char *
output_move_double (rtx *operands)
{
  enum anon1 { REGOP, OFFSOP, PUSHOP, CNSTOP, RNDOP } optype0, optype1;
  rtx latehalf[2];

  /* First classify both operands.  */

  if (REG_P (operands[0]))
    optype0 = REGOP;
  else if (offsettable_memref_p (operands[0]))
    optype0 = OFFSOP;
  else if (GET_CODE (XEXP (operands[0], 0)) == PRE_DEC)
    optype0 = PUSHOP;
  else
    optype0 = RNDOP;

  if (REG_P (operands[1]))
    optype1 = REGOP;
  else if (CONSTANT_P (operands[1])
	   || GET_CODE (operands[1]) == CONST_DOUBLE)
    optype1 = CNSTOP;
  else if (offsettable_memref_p (operands[1]))
    optype1 = OFFSOP;
  else if (GET_CODE (XEXP (operands[1], 0)) == PRE_DEC)
    optype1 = PUSHOP;
  else
    optype1 = RNDOP;

  /* Check for the cases that the operand constraints are not
     supposed to allow to happen.  Abort if we get one,
     because generating code for these cases is painful.  */

  if (optype0 == RNDOP || optype1 == RNDOP)
    abort ();

  /* Ok, we can do one word at a time.
     Normally we do the low-numbered word first,
     but if either operand is autodecrementing then we
     do the high-numbered word first.

     In either case, set up in LATEHALF the operands to use
     for the high-numbered word and in some cases alter the
     operands in OPERANDS to be suitable for the low-numbered word.  */

  if (optype0 == REGOP)
    latehalf[0] = gen_rtx_REG (SImode, REGNO (operands[0]) + 1);
  else if (optype0 == OFFSOP)
    latehalf[0] = adjust_address (operands[0], SImode, 4);
  else
    latehalf[0] = operands[0];

  if (optype1 == REGOP)
    latehalf[1] = gen_rtx_REG (SImode, REGNO (operands[1]) + 1);
  else if (optype1 == OFFSOP)
    latehalf[1] = adjust_address (operands[1], SImode, 4);
  else if (optype1 == CNSTOP)
    split_double (operands[1], &operands[1], &latehalf[1]);
  else
    latehalf[1] = operands[1];

  /* If insn is effectively movd N(sp),tos then we will do the
     high word first.  We should use the adjusted operand 1 (which is N+4(sp))
     for the low word as well, to compensate for the first decrement of sp.
     Given this, it doesn't matter which half we do "first".  */
  if (optype0 == PUSHOP
      && REGNO (XEXP (XEXP (operands[0], 0), 0)) == STACK_POINTER_REGNUM
      && reg_overlap_mentioned_p (stack_pointer_rtx, operands[1]))
    operands[1] = latehalf[1];

  /* If one or both operands autodecrementing,
     do the two words, high-numbered first.  */
  else if (optype0 == PUSHOP || optype1 == PUSHOP)
    {
      output_asm_insn (singlemove_string (latehalf), latehalf);
      return singlemove_string (operands);
    }

  /* If the first move would clobber the source of the second one,
     do them in the other order.  */

  /* Overlapping registers.  */
  if (optype0 == REGOP && optype1 == REGOP
      && REGNO (operands[0]) == REGNO (latehalf[1]))
    {
      /* Do that word.  */
      output_asm_insn (singlemove_string (latehalf), latehalf);
      /* Do low-numbered word.  */
      return singlemove_string (operands);
    }
  /* Loading into a register which overlaps a register used in the address.  */
  else if (optype0 == REGOP && optype1 != REGOP
	   && reg_overlap_mentioned_p (operands[0], operands[1]))
    {
      if (reg_mentioned_p (operands[0], XEXP (operands[1], 0))
	  && reg_mentioned_p (latehalf[0], XEXP (operands[1], 0)))
	{
	  /* If both halves of dest are used in the src memory address,
	     load the destination address into the low reg (operands[0]).
	     Then it works to load latehalf first.  */
	  rtx xops[2];
	  xops[0] = XEXP (operands[1], 0);
	  xops[1] = operands[0];
	  output_asm_insn ("addr %a0,%1", xops);
	  operands[1] = gen_rtx_MEM (DImode, operands[0]);
	  latehalf[1] = adjust_address (operands[1], SImode, 4);
	  /* The first half has the overlap, Do the late half first.  */
	  output_asm_insn (singlemove_string (latehalf), latehalf);
	  /* Then clobber.  */
	  return singlemove_string (operands);
	}
      if (reg_mentioned_p (operands[0], XEXP (operands[1], 0)))
	{
	  /* The first half has the overlap, Do the late half first.  */
	  output_asm_insn (singlemove_string (latehalf), latehalf);
	  /* Then clobber.  */
	  return singlemove_string (operands);
	}
    }

  /* Normal case.  Do the two words, low-numbered first.  */

  output_asm_insn (singlemove_string (operands), operands);

  operands[0] = latehalf[0];
  operands[1] = latehalf[1];
  return singlemove_string (operands);
}


#define MAX_UNALIGNED_COPY (32)
/* Expand string/block move operations.

   operands[0] is the pointer to the destination.
   operands[1] is the pointer to the source.
   operands[2] is the number of bytes to move.
   operands[3] is the alignment.  */

static void
move_tail (rtx operands[], int bytes, int offset)
{
  if (bytes & 2)
    {
      emit_move_insn (adjust_address (operands[0], HImode, offset),
		      adjust_address (operands[1], HImode, offset));
      offset += 2;
    }
  if (bytes & 1)
    emit_move_insn (adjust_address (operands[0], QImode, offset),
		    adjust_address (operands[1], QImode, offset));
}

void
expand_block_move (rtx operands[])
{
  rtx bytes_rtx	= operands[2];
  rtx align_rtx = operands[3];
  int constp	= (GET_CODE (bytes_rtx) == CONST_INT);
  int bytes	= (constp ? INTVAL (bytes_rtx) : 0);
  int align	= INTVAL (align_rtx);
  rtx src_reg = gen_rtx_REG (Pmode, 1);
  rtx dest_reg = gen_rtx_REG (Pmode, 2);
  rtx count_reg = gen_rtx_REG (SImode, 0);

  if (constp && bytes <= 0)
    return;

  if (constp && bytes < 20)
    {
      int words = bytes >> 2;

      if (words)
	{
	  if (words < 3)
	    {
	      int offset = 0;

	      for (; words; words--, offset += 4)
		emit_move_insn (adjust_address (operands[0], SImode, offset),
				adjust_address (operands[1], SImode, offset));
	    }
	  else
	    {
	      /* Use movmd. It is slower than multiple movd's but more
		 compact. It is also slower than movsd for large copies
		 but causes less registers reloading so is better than movsd
		 for small copies.  */
	      rtx src, dest;
	      dest = copy_addr_to_reg (XEXP (operands[0], 0));
	      src = copy_addr_to_reg (XEXP (operands[1], 0));
	    
	      emit_insn (gen_movmemsi2(dest, src, GEN_INT (words)));
	    }
	}
      move_tail (operands, bytes & 3, bytes & ~3);
      return;
    }

  if (align > UNITS_PER_WORD)
    align = UNITS_PER_WORD;

  /* Move the address into scratch registers.  */
  emit_insn (gen_rtx_CLOBBER (VOIDmode, dest_reg));
  emit_move_insn (dest_reg, XEXP (operands[0], 0));
  operands[0] = gen_rtx_MEM (SImode, dest_reg);
  emit_insn (gen_rtx_CLOBBER (VOIDmode, src_reg));
  emit_move_insn (src_reg, XEXP (operands[1], 0));
  operands[1] = gen_rtx_MEM (SImode, src_reg);
  emit_insn (gen_rtx_CLOBBER (VOIDmode, count_reg));

  if (constp && (align == UNITS_PER_WORD || bytes < MAX_UNALIGNED_COPY))
    {
      /* constant no of bytes and aligned or small enough copy to not bother
       * aligning. Emit insns to copy by words.
       */
      if (bytes >> 2)
	{
	  emit_move_insn (count_reg, GEN_INT (bytes >> 2));
	  emit_insn (gen_movmemsi1 (GEN_INT (4)));
	}
      /* insns to copy rest */
      move_tail (operands, bytes & 3, 0);
    }
  else if (align == UNITS_PER_WORD)
    {
      /* insns to copy by words */
      emit_insn (gen_lshrsi3 (count_reg, bytes_rtx, const2_rtx));
      emit_insn (gen_movmemsi1 (GEN_INT (4)));
      if (constp)
	{
	  move_tail (operands, bytes & 3, 0);
	}
      else
	{
	  /* insns to copy rest */
	  emit_insn (gen_andsi3 (count_reg, bytes_rtx, GEN_INT (3)));
	  emit_insn (gen_movmemsi1 (const1_rtx));
	}
    }
  else
    {
      /* Not aligned and we may have a lot to copy so it is worth
       * aligning.
       */
      rtx aligned_label = gen_label_rtx ();
      rtx bytes_reg;

      bytes_reg = copy_to_mode_reg (SImode, bytes_rtx);
      if (!constp)
	{
	  /* Emit insns to test and skip over the alignment if it is
	   * not worth it. This doubles as a test to ensure that the alignment
	   * operation can't copy too many bytes
	   */
	  emit_insn (gen_cmpsi (bytes_reg, GEN_INT (MAX_UNALIGNED_COPY)));
	  emit_jump_insn (gen_blt (aligned_label));
	}

      /* Emit insns to do alignment at run time */
      emit_insn (gen_negsi2 (count_reg, src_reg));
      emit_insn (gen_andsi3 (count_reg, count_reg, GEN_INT (3)));
      emit_insn (gen_subsi3 (bytes_reg, bytes_reg, count_reg));
      emit_insn (gen_movmemsi1 (const1_rtx));
      if (!constp)
	emit_label (aligned_label);

      /* insns to copy by words */
      emit_insn (gen_lshrsi3 (count_reg, bytes_reg, const2_rtx));
      emit_insn (gen_movmemsi1 (GEN_INT (4)));

      /* insns to copy rest */
      emit_insn (gen_andsi3 (count_reg, bytes_reg, GEN_INT (3)));
      emit_insn (gen_movmemsi1 (const1_rtx));
    }
}


/* Returns 1 if OP contains a global symbol reference */

int
global_symbolic_reference_mentioned_p (rtx op, int f)
{
  register const char *fmt;
  register int i;

  if (GET_CODE (op) == SYMBOL_REF)
    {
      if (! SYMBOL_REF_LOCAL_P (op))
	return 1;
      else
        return 0;
    }
  else if (f && GET_CODE (op) != CONST)
    return 0;

  fmt = GET_RTX_FORMAT (GET_CODE (op));
  for (i = GET_RTX_LENGTH (GET_CODE (op)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  register int j;

	  for (j = XVECLEN (op, i) - 1; j >= 0; j--)
	    if (global_symbolic_reference_mentioned_p (XVECEXP (op, i, j), 0))
	      return 1;
	}
      else if (fmt[i] == 'e' 
	       && global_symbolic_reference_mentioned_p (XEXP (op, i), 0))
	return 1;
    }

  return 0;
}


/* Returns 1 if OP contains a symbol reference */

int
symbolic_reference_mentioned_p (rtx op)
{
  register const char *fmt;
  register int i;

  if (GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == LABEL_REF)
    return 1;

  fmt = GET_RTX_FORMAT (GET_CODE (op));
  for (i = GET_RTX_LENGTH (GET_CODE (op)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  register int j;

	  for (j = XVECLEN (op, i) - 1; j >= 0; j--)
	    if (symbolic_reference_mentioned_p (XVECEXP (op, i, j)))
	      return 1;
	}
      else if (fmt[i] == 'e' && symbolic_reference_mentioned_p (XEXP (op, i)))
	return 1;
    }

  return 0;
}

/* Table of machine-specific attributes.  */

const struct attribute_spec ns32k_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler } */
  /* Stdcall attribute says callee is responsible for popping arguments
     if they are not variable.  */
  { "stdcall", 0, 0, false, true,  true,  ns32k_handle_fntype_attribute },
  /* Cdecl attribute says the callee is a normal C declaration */
  { "cdecl",   0, 0, false, true,  true,  ns32k_handle_fntype_attribute },
  { NULL,      0, 0, false, false, false, NULL }
};

/* Handle an attribute requiring a FUNCTION_TYPE, FIELD_DECL or TYPE_DECL;
   arguments as in struct attribute_spec.handler.  */
static tree
ns32k_handle_fntype_attribute (tree *node, tree name,
			       tree args ATTRIBUTE_UNUSED,
			       int flags ATTRIBUTE_UNUSED,
			       bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_TYPE
      && TREE_CODE (*node) != FIELD_DECL
      && TREE_CODE (*node) != TYPE_DECL)
    {
      warning (OPT_Wattributes, "%qs attribute only applies to functions",
	       IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}


/* Value is the number of bytes of arguments automatically
   popped when returning from a subroutine call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack.

   On the ns32k, the RET insn may be used to pop them if the number
     of args is fixed, but if the number is variable then the caller
     must pop them all.  RET can't be used for library calls now
     because the library is compiled with the Unix compiler.
   Use of RET is a selectable option, since it is incompatible with
   standard Unix calling sequences.  If the option is not selected,
   the caller must always pop the args.

   The attribute stdcall is equivalent to RET on a per module basis.  */

int
ns32k_return_pops_args (tree fundecl ATTRIBUTE_UNUSED, tree funtype, int size)
{
  int rtd = TARGET_RTD;

  if (TREE_CODE (funtype) == IDENTIFIER_NODE)
    return rtd ? size : 0;

  /* Cdecl functions override -mrtd, and never pop the stack */
  if (lookup_attribute ("cdecl", TYPE_ATTRIBUTES (funtype)))
    return 0;

  /* Stdcall functions will pop the stack if not variable args */
  if (lookup_attribute ("stdcall", TYPE_ATTRIBUTES (funtype)))
    rtd = 1;

  if (rtd)
    {
      if (TYPE_ARG_TYPES (funtype) == NULL_TREE
	  || (TREE_VALUE (tree_last (TYPE_ARG_TYPES (funtype))) == void_type_node))
	return size;
    }

  return 0;
}

/* PRINT_OPERAND is defined to call this function,
   which is easier to debug than putting all the code in
   a macro definition in ns32k.h.  */

/* XXX time 12% of cpu time is in fprintf for non optimizing */
void
print_operand (FILE *file, rtx x, int code)
{
  if (code == '$')
    PUT_IMMEDIATE_PREFIX (file);
  else if (code == '?')
    PUT_EXTERNAL_PREFIX (file);
  else if (GET_CODE (x) == REG)
    fprintf (file, "%s", ns32k_out_reg_names[REGNO (x)]);
  else if (GET_CODE (x) == MEM)
    {
      output_address (XEXP (x, 0));
    }
  else if (GET_CODE (x) == CONST_DOUBLE && GET_MODE (x) != VOIDmode)
    {
      REAL_VALUE_TYPE r;

      REAL_VALUE_FROM_CONST_DOUBLE (r, x);
      PUT_IMMEDIATE_PREFIX (file);
      if (GET_MODE (x) == DFmode)
	{ 
#ifdef SEQUENT_ASM
	  /* Sequent likes its floating point constants as integers */
	  long l[2];
	  REAL_VALUE_TO_TARGET_DOUBLE (r, l);
	  fprintf (file, "0Dx%08x%08x",
		   l[!WORDS_BIG_ENDIAN], l[WORDS_BIG_ENDIAN]);
#else
	  char s[30];
	  real_to_decimal (s, &r, sizeof (s), 0, 1);
#ifdef ENCORE_ASM
	  fprintf (file, "0f%s", s);
#else
	  fprintf (file, "0d%s", s);
#endif
#endif
	}
      else
	{
#ifdef SEQUENT_ASM
	  long l;
	  REAL_VALUE_TO_TARGET_SINGLE (r, l);
	  fprintf (file, "0Fx%08lx", l);
#else
	  char s[30];
	  real_to_decimal (s, &r, sizeof (s), 0, 1);
	  fprintf (file, "0f%s", s);
#endif
	}
    }
  else
    {
      if (flag_pic
          && GET_CODE (x) == CONST
          && symbolic_reference_mentioned_p (x))
        {
	  fprintf (stderr, "illegal constant for pic-mode: \n");
	  print_rtl (stderr, x);
          fprintf (stderr, "\nGET_CODE (x) == %d, CONST == %d, symbolic_reference_mentioned_p (x) == %d\n",
		  GET_CODE (x), CONST, symbolic_reference_mentioned_p (x));
	  abort ();
	}
      else if (flag_pic
               && (GET_CODE (x) == SYMBOL_REF || GET_CODE (x) == LABEL_REF))
	{
	  output_addr_const (file, x);
	  fprintf (file, "(sb)");
	}
      else
        {
#ifdef NO_IMMEDIATE_PREFIX_IF_SYMBOLIC
          if (GET_CODE (x) == CONST_INT)
#endif
	    PUT_IMMEDIATE_PREFIX (file);
          output_addr_const (file, x);
	}
    }
}

/* PRINT_OPERAND_ADDRESS is defined to call this function,
   which is easier to debug than putting all the code in
   a macro definition in ns32k.h .  */

/* Completely rewritten to get this to work with Gas for PC532 Mach.
   This function didn't work and I just wasn't able (nor very willing) to
   figure out how it worked.
   90-11-25 Tatu Yl|nen <ylo@cs.hut.fi> */

void
print_operand_address (register FILE *file, register rtx addr)
{
  static const char scales[] = { 'b', 'w', 'd', 0, 'q', };
  rtx offset, base, indexexp, tmp;
  int scale;
  extern int flag_pic;

  if (GET_CODE (addr) == PRE_DEC || GET_CODE (addr) == POST_DEC)
    {
      fprintf (file, "tos");
      return;
    }

  offset = NULL;
  base = NULL;
  indexexp = NULL;
  while (addr != NULL)
    {
      if (GET_CODE (addr) == PLUS)
	{
	  if (GET_CODE (XEXP (addr, 0)) == PLUS)
	    {
	      tmp = XEXP (addr, 1);
	      addr = XEXP (addr, 0);
	    }
	  else
	    {
	      tmp = XEXP (addr,0);
	      addr = XEXP (addr,1);
	    }
	}
      else
	{
	  tmp = addr;
	  addr = NULL;
	}
      switch (GET_CODE (tmp))
	{
	case PLUS:
	  abort ();
	case MEM:
	  if (base)
	    {
	      indexexp = base;
	      base = tmp;
	    }
	  else
	    base = tmp;
	  break;
	case REG:
	  if (REGNO (tmp) < F0_REGNUM)
	    if (base)
	      {
		indexexp = tmp;
	      }
	    else
	      base = tmp;
	  else
	    if (base)
	      {
		indexexp = base;
		base = tmp;
	      }
	    else
	      base = tmp;
	  break;
	case MULT:
	  indexexp = tmp;
	  break;
	case SYMBOL_REF:
	  if (flag_pic && ! SYMBOL_REF_LOCAL_P (tmp))
	    {
	      if (base)
		{
		  if (indexexp)
		    abort ();
		  indexexp = base;
		}
	      base = tmp;
	      break;
	    }
	case CONST:
	  if (flag_pic && GET_CODE (tmp) == CONST)
	    {
	      rtx sym, off, tmp1;
	      tmp1 = XEXP (tmp,0);
	      if (GET_CODE (tmp1)  != PLUS)
		abort ();

	      sym = XEXP (tmp1,0);
	      if (GET_CODE (sym) != SYMBOL_REF)
	        {
	          off = sym;
		  sym = XEXP (tmp1,1);
		}
	      else
	        off = XEXP (tmp1,1);
	      if (GET_CODE (sym) == SYMBOL_REF)
		{
		  if (GET_CODE (off) != CONST_INT)
		    abort ();

		  if (! SYMBOL_REF_LOCAL_P (sym))
		    {
		      if (base)
			{
			  if (indexexp)
			    abort ();

			  indexexp = base;
			}

		      if (offset != 0)
			abort ();

		      base = sym;
		      offset = off;
		      break;
		    }
		}
	    }
	case CONST_INT:
	case LABEL_REF:
	  if (offset)
	    offset = gen_rtx_PLUS (SImode, tmp, offset);
	  else
	    offset = tmp;
	  break;
	default:
	  abort ();
	}
    }
  if (! offset)
    offset = const0_rtx;

  if (base
#ifndef INDEX_RATHER_THAN_BASE
      && (flag_pic || TARGET_HIMEM)
      && GET_CODE (base) != SYMBOL_REF 
      && GET_CODE (offset) != CONST_INT
#else
  /* This is a re-implementation of the SEQUENT_ADDRESS_BUG fix.  */
#endif
      && !indexexp && GET_CODE (base) == REG
      && REG_OK_FOR_INDEX_P (base))
    {
      indexexp = base;
      base = NULL;
    }

  /* now, offset, base and indexexp are set */
#ifndef BASE_REG_NEEDED
  if (! base)
    {
#if defined (PC_RELATIVE) || defined (NO_ABSOLUTE_PREFIX_IF_SYMBOLIC)
      if (GET_CODE (offset) == CONST_INT)
#endif
	PUT_ABSOLUTE_PREFIX (file);
    }
#endif

  output_addr_const (file, offset);
  if (base) /* base can be (REG ...) or (MEM ...) */
    switch (GET_CODE (base))
      {
	/* now we must output base.  Possible alternatives are:
	   (rN)       (REG ...)
	   (sp)	      (REG ...)
	   (fp)       (REG ...)
	   (pc)       (REG ...)  used for SYMBOL_REF and LABEL_REF, output
	   (disp(fp)) (MEM ...)       just before possible [rX:y]
	   (disp(sp)) (MEM ...)
	   (disp(sb)) (MEM ...)
	   */
      case REG:
	fprintf (file, "(%s)", ns32k_out_reg_names[REGNO (base)]);
	break;
      case SYMBOL_REF:
	if (! flag_pic)
	  abort ();

        fprintf (file, "(");
	output_addr_const (file, base);
	fprintf (file, "(sb))");
        break;
      case MEM:
	addr = XEXP (base,0);
	base = NULL;
	offset = NULL;
	while (addr != NULL)
	  {
	    if (GET_CODE (addr) == PLUS)
	      {
		if (GET_CODE (XEXP (addr, 0)) == PLUS)
		  {
		    tmp = XEXP (addr, 1);
		    addr = XEXP (addr, 0);
		  }
		else
		  {
		    tmp = XEXP (addr, 0);
		    addr = XEXP (addr, 1);
		  }
	      }
	    else
	      {
		tmp = addr;
		addr = NULL;
	      }
	    switch (GET_CODE (tmp))
	      {
	      case REG:
		base = tmp;
		break;
	      case CONST:
	      case CONST_INT:
	      case SYMBOL_REF:
	      case LABEL_REF:
		if (offset)
		  offset = gen_rtx_PLUS (SImode, tmp, offset);
		else
		  offset = tmp;
		break;
	      default:
		abort ();
	      }
	  }
	if (! offset)
	  offset = const0_rtx;
	fprintf (file, "(");
	output_addr_const (file, offset);
	if (base)
	  fprintf (file, "(%s)", ns32k_out_reg_names[REGNO (base)]);
	else if (TARGET_SB)
	  fprintf (file, "(sb)");
	else
	  abort ();
	fprintf (file, ")");
	break;
      default:
	abort ();
      }
#ifdef PC_RELATIVE
  else if (GET_CODE (offset) != CONST_INT)
    fprintf (file, "(pc)");
#ifdef BASE_REG_NEEDED
  else if (TARGET_SB)
    fprintf (file, "(sb)");
  else
    abort ();
#endif
#endif /* PC_RELATIVE */

  /* now print index if we have one */
  if (indexexp)
    {
      if (GET_CODE (indexexp) == MULT)
	{
	  scale = INTVAL (XEXP (indexexp, 1)) >> 1;
	  indexexp = XEXP (indexexp, 0);
	}
      else
	scale = 0;
      if (GET_CODE (indexexp) != REG || REGNO (indexexp) >= F0_REGNUM)
	abort ();

#ifdef UTEK_ASM
      fprintf (file, "[%c`%s]",
	       scales[scale],
	       ns32k_out_reg_names[REGNO (indexexp)]);
#else
      fprintf (file, "[%s:%c]",
	       ns32k_out_reg_names[REGNO (indexexp)],
	       scales[scale]);
#endif
    }
}

/* National 32032 shifting is so bad that we can get
   better performance in many common cases by using other
   techniques.  */
const char *
output_shift_insn (rtx *operands)
{
  if (GET_CODE (operands[2]) == CONST_INT
      && INTVAL (operands[2]) > 0
      && INTVAL (operands[2]) <= 3)
    {
      if (GET_CODE (operands[0]) == REG)
	{
	  if (GET_CODE (operands[1]) == REG)
	    {
	      if (REGNO (operands[0]) == REGNO (operands[1]))
		{
		  if (operands[2] == const1_rtx)
		    return "addd %0,%0";
		  else if (INTVAL (operands[2]) == 2)
		    return "addd %0,%0\n\taddd %0,%0";
		}
	      if (operands[2] == const1_rtx)
		return "movd %1,%0\n\taddd %0,%0";
	    
	      operands[1] = gen_indexed_expr (const0_rtx, operands[1], operands[2]);
	      return "addr %a1,%0";
	    }
	  if (operands[2] == const1_rtx)
	    return "movd %1,%0\n\taddd %0,%0";
	}
      else if (GET_CODE (operands[1]) == REG)
	{
	  operands[1] = gen_indexed_expr (const0_rtx, operands[1], operands[2]);
	  return "addr %a1,%0";
	}
      else if (INTVAL (operands[2]) == 1
	       && GET_CODE (operands[1]) == MEM
	       && rtx_equal_p (operands [0], operands[1]))
	{
	  rtx temp = XEXP (operands[1], 0);
	
	  if (GET_CODE (temp) == REG
	      || (GET_CODE (temp) == PLUS
		  && GET_CODE (XEXP (temp, 0)) == REG
		  && GET_CODE (XEXP (temp, 1)) == CONST_INT))
	    return "addd %0,%0";
	}
      else return "ashd %2,%0";
    }
  return "ashd %2,%0";
}

const char *
output_move_dconst (int n, const char *s)
{
  static char r[32];

  if (n > -9 && n < 8)
    strcpy (r, "movqd ");
  else if (n > 0 && n < 256)
    strcpy (r, "movzbd ");
  else if (n > 0 && n < 65536)
    strcpy (r, "movzwd ");
  else if (n < 0 && n > -129)
    strcpy (r, "movxbd ");
  else if (n < 0 && n > -32769)
    strcpy (r, "movxwd ");
  else
    strcpy (r, "movd ");
  strcat (r, s);
  return r;
}

static rtx
ns32k_struct_value_rtx (tree fntype ATTRIBUTE_UNUSED,
			int incoming ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (Pmode, NS32K_STRUCT_VALUE_REGNUM);
}

/* Worker function for NOTICE_UPDATE_CC.  */

void
ns32k_notice_update_cc (rtx exp, rtx insn ATTRIBUTE_UNUSED)
{
  if (GET_CODE (exp) == SET)
    {
      if (GET_CODE (SET_DEST (exp)) == CC0)
	{
	  cc_status.flags = 0;
	  cc_status.value1 = SET_DEST (exp);
	  cc_status.value2 = SET_SRC (exp);
	}
      else if (GET_CODE (SET_SRC (exp)) == CALL)
	{
	  CC_STATUS_INIT;
	}
      else if (GET_CODE (SET_DEST (exp)) == REG)
	{
	  if (cc_status.value1
	      && reg_overlap_mentioned_p (SET_DEST (exp), cc_status.value1))
	    cc_status.value1 = 0;
	  if (cc_status.value2
	      && reg_overlap_mentioned_p (SET_DEST (exp), cc_status.value2))
	    cc_status.value2 = 0;
	}
      else if (GET_CODE (SET_DEST (exp)) == MEM)
	{
	  CC_STATUS_INIT;
	}
    }
  else if (GET_CODE (exp) == PARALLEL
	   && GET_CODE (XVECEXP (exp, 0, 0)) == SET)
    {
      if (GET_CODE (SET_DEST (XVECEXP (exp, 0, 0))) == CC0)
	{
	  cc_status.flags = 0;
	  cc_status.value1 = SET_DEST (XVECEXP (exp, 0, 0));
	  cc_status.value2 = SET_SRC (XVECEXP (exp, 0, 0));
	}
      else if (GET_CODE (SET_DEST (XVECEXP (exp, 0, 0))) == REG)
	{
	  if (cc_status.value1
	      && reg_overlap_mentioned_p (SET_DEST (XVECEXP (exp, 0, 0)),
					  cc_status.value1))
	    cc_status.value1 = 0;
	  if (cc_status.value2
	      && reg_overlap_mentioned_p (SET_DEST (XVECEXP (exp, 0, 0)),
					  cc_status.value2))
	    cc_status.value2 = 0;
	}
      else if (GET_CODE (SET_DEST (XVECEXP (exp, 0, 0))) == MEM)
	{
	  CC_STATUS_INIT;
	}
    }
  else if (GET_CODE (exp) == CALL)
    {
      /* all bets are off */
      CC_STATUS_INIT;
    }
  else
    {
      /* nothing happens? CC_STATUS_INIT; */
    }
  if (cc_status.value1 && GET_CODE (cc_status.value1) == REG
      && cc_status.value2
      && reg_overlap_mentioned_p (cc_status.value1, cc_status.value2))
    abort ();
}

/* Implement TARGET_ARG_PARTIAL_BYTES.  */

static int
ns32k_arg_partial_bytes (CUMULATIVE_ARGS *pcum, enum machine_mode mode,
			 tree type, bool named ATTRIBUTE_UNUSED)
{
  int cum = *pcum;

  if (TARGET_REGPARM && cum < 8)
    {
      HOST_WIDE_INT size;

      if (mode == BLKmode)
	size = int_size_in_bytes (type);
      else
	size = GET_MODE_SIZE (mode);

      if (8 < cum + size)
	return 8 - cum;
    }

  return 0;
}
