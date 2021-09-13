/* Scheduler hooks for IA-32 which implement CPU specific logic.
   Copyright (C) 1988-2021 Free Software Foundation, Inc.

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

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "cfghooks.h"
#include "tm_p.h"
#include "target.h"
#include "insn-config.h"
#include "insn-attr.h"
#include "insn-opinit.h"
#include "recog.h"

/* Return the maximum number of instructions a cpu can issue.  */

int
ix86_issue_rate (void)
{
  switch (ix86_tune)
    {
    case PROCESSOR_PENTIUM:
    case PROCESSOR_LAKEMONT:
    case PROCESSOR_BONNELL:
    case PROCESSOR_SILVERMONT:
    case PROCESSOR_KNL:
    case PROCESSOR_KNM:
    case PROCESSOR_INTEL:
    case PROCESSOR_K6:
    case PROCESSOR_BTVER2:
    case PROCESSOR_PENTIUM4:
    case PROCESSOR_NOCONA:
      return 2;

    case PROCESSOR_PENTIUMPRO:
    case PROCESSOR_ATHLON:
    case PROCESSOR_K8:
    case PROCESSOR_AMDFAM10:
    case PROCESSOR_BTVER1:
      return 3;

    case PROCESSOR_BDVER1:
    case PROCESSOR_BDVER2:
    case PROCESSOR_BDVER3:
    case PROCESSOR_BDVER4:
    case PROCESSOR_ZNVER1:
    case PROCESSOR_ZNVER2:
    case PROCESSOR_ZNVER3:
    case PROCESSOR_CORE2:
    case PROCESSOR_NEHALEM:
    case PROCESSOR_SANDYBRIDGE:
    case PROCESSOR_HASWELL:
    case PROCESSOR_GENERIC:
      return 4;

    default:
      return 1;
    }
}

/* Return true iff USE_INSN has a memory address with operands set by
   SET_INSN.  */

bool
ix86_agi_dependent (rtx_insn *set_insn, rtx_insn *use_insn)
{
  int i;
  extract_insn_cached (use_insn);
  for (i = recog_data.n_operands - 1; i >= 0; --i)
    if (MEM_P (recog_data.operand[i]))
      {
	rtx addr = XEXP (recog_data.operand[i], 0);
	if (modified_in_p (addr, set_insn) != 0)
	  {
	    /* No AGI stall if SET_INSN is a push or pop and USE_INSN
	       has SP based memory (unless index reg is modified in a pop).  */
	    rtx set = single_set (set_insn);
	    if (set
		&& (push_operand (SET_DEST (set), GET_MODE (SET_DEST (set)))
		    || pop_operand (SET_SRC (set), GET_MODE (SET_SRC (set)))))
	      {
		struct ix86_address parts;
		if (ix86_decompose_address (addr, &parts)
		    && parts.base == stack_pointer_rtx
		    && (parts.index == NULL_RTX
			|| MEM_P (SET_DEST (set))
			|| !modified_in_p (parts.index, set_insn)))
		  return false;
	      }
	    return true;
	  }
	return false;
      }
  return false;
}

/* A subroutine of ix86_adjust_cost -- return TRUE iff INSN reads flags set
   by DEP_INSN and nothing set by DEP_INSN.  */

static bool
ix86_flags_dependent (rtx_insn *insn, rtx_insn *dep_insn, enum attr_type insn_type)
{
  rtx set, set2;

  /* Simplify the test for uninteresting insns.  */
  if (insn_type != TYPE_SETCC
      && insn_type != TYPE_ICMOV
      && insn_type != TYPE_FCMOV
      && insn_type != TYPE_IBR)
    return false;

  if ((set = single_set (dep_insn)) != 0)
    {
      set = SET_DEST (set);
      set2 = NULL_RTX;
    }
  else if (GET_CODE (PATTERN (dep_insn)) == PARALLEL
	   && XVECLEN (PATTERN (dep_insn), 0) == 2
	   && GET_CODE (XVECEXP (PATTERN (dep_insn), 0, 0)) == SET
	   && GET_CODE (XVECEXP (PATTERN (dep_insn), 0, 1)) == SET)
    {
      set = SET_DEST (XVECEXP (PATTERN (dep_insn), 0, 0));
      set2 = SET_DEST (XVECEXP (PATTERN (dep_insn), 0, 0));
    }
  else
    return false;

  if (!REG_P (set) || REGNO (set) != FLAGS_REG)
    return false;

  /* This test is true if the dependent insn reads the flags but
     not any other potentially set register.  */
  if (!reg_overlap_mentioned_p (set, PATTERN (insn)))
    return false;

  if (set2 && reg_overlap_mentioned_p (set2, PATTERN (insn)))
    return false;

  return true;
}

/* Helper function for exact_store_load_dependency.
   Return true if addr is found in insn.  */
static bool
exact_dependency_1 (rtx addr, rtx insn)
{
  enum rtx_code code;
  const char *format_ptr;
  int i, j;

  code = GET_CODE (insn);
  switch (code)
    {
    case MEM:
      if (rtx_equal_p (addr, insn))
	return true;
      break;
    case REG:
    CASE_CONST_ANY:
    case SYMBOL_REF:
    case CODE_LABEL:
    case PC:
    case EXPR_LIST:
      return false;
    default:
      break;
    }

  format_ptr = GET_RTX_FORMAT (code);
  for (i = 0; i < GET_RTX_LENGTH (code); i++)
    {
      switch (*format_ptr++)
	{
	case 'e':
	  if (exact_dependency_1 (addr, XEXP (insn, i)))
	    return true;
	  break;
	case 'E':
	  for (j = 0; j < XVECLEN (insn, i); j++)
	    if (exact_dependency_1 (addr, XVECEXP (insn, i, j)))
	      return true;
	  break;
	}
    }
  return false;
}

/* Return true if there exists exact dependency for store & load, i.e.
   the same memory address is used in them.  */
static bool
exact_store_load_dependency (rtx_insn *store, rtx_insn *load)
{
  rtx set1, set2;

  set1 = single_set (store);
  if (!set1)
    return false;
  if (!MEM_P (SET_DEST (set1)))
    return false;
  set2 = single_set (load);
  if (!set2)
    return false;
  if (exact_dependency_1 (SET_DEST (set1), SET_SRC (set2)))
    return true;
  return false;
}


/* This function corrects the value of COST (latency) based on the relationship
   between INSN and DEP_INSN through a dependence of type DEP_TYPE, and strength
   DW.  It should return the new value.

   On x86 CPUs this is most commonly used to model the fact that valus of
   registers used to compute address of memory operand  needs to be ready
   earlier than values of registers used in the actual operation.  */

int
ix86_adjust_cost (rtx_insn *insn, int dep_type, rtx_insn *dep_insn, int cost,
		  unsigned int)
{
  enum attr_type insn_type, dep_insn_type;
  enum attr_memory memory;
  rtx set, set2;
  int dep_insn_code_number;

  /* Anti and output dependencies have zero cost on all CPUs.  */
  if (dep_type != 0)
    return 0;

  dep_insn_code_number = recog_memoized (dep_insn);

  /* If we can't recognize the insns, we can't really do anything.  */
  if (dep_insn_code_number < 0 || recog_memoized (insn) < 0)
    return cost;

  insn_type = get_attr_type (insn);
  dep_insn_type = get_attr_type (dep_insn);

  switch (ix86_tune)
    {
    case PROCESSOR_PENTIUM:
    case PROCESSOR_LAKEMONT:
      /* Address Generation Interlock adds a cycle of latency.  */
      if (insn_type == TYPE_LEA)
	{
	  rtx addr = PATTERN (insn);

	  if (GET_CODE (addr) == PARALLEL)
	    addr = XVECEXP (addr, 0, 0);

	  gcc_assert (GET_CODE (addr) == SET);

	  addr = SET_SRC (addr);
	  if (modified_in_p (addr, dep_insn))
	    cost += 1;
	}
      else if (ix86_agi_dependent (dep_insn, insn))
	cost += 1;

      /* ??? Compares pair with jump/setcc.  */
      if (ix86_flags_dependent (insn, dep_insn, insn_type))
	cost = 0;

      /* Floating point stores require value to be ready one cycle earlier.  */
      if (insn_type == TYPE_FMOV
	  && get_attr_memory (insn) == MEMORY_STORE
	  && !ix86_agi_dependent (dep_insn, insn))
	cost += 1;
      break;

    case PROCESSOR_PENTIUMPRO:
      /* INT->FP conversion is expensive.  */
      if (get_attr_fp_int_src (dep_insn))
	cost += 5;

      /* There is one cycle extra latency between an FP op and a store.  */
      if (insn_type == TYPE_FMOV
	  && (set = single_set (dep_insn)) != NULL_RTX
	  && (set2 = single_set (insn)) != NULL_RTX
	  && rtx_equal_p (SET_DEST (set), SET_SRC (set2))
	  && MEM_P (SET_DEST (set2)))
	cost += 1;

      memory = get_attr_memory (insn);

      /* Show ability of reorder buffer to hide latency of load by executing
	 in parallel with previous instruction in case
	 previous instruction is not needed to compute the address.  */
      if ((memory == MEMORY_LOAD || memory == MEMORY_BOTH)
	  && !ix86_agi_dependent (dep_insn, insn))
	{
	  /* Claim moves to take one cycle, as core can issue one load
	     at time and the next load can start cycle later.  */
	  if (dep_insn_type == TYPE_IMOV
	      || dep_insn_type == TYPE_FMOV)
	    cost = 1;
	  else if (cost > 1)
	    cost--;
	}
      break;

    case PROCESSOR_K6:
     /* The esp dependency is resolved before
	the instruction is really finished.  */
      if ((insn_type == TYPE_PUSH || insn_type == TYPE_POP)
	  && (dep_insn_type == TYPE_PUSH || dep_insn_type == TYPE_POP))
	return 1;

      /* INT->FP conversion is expensive.  */
      if (get_attr_fp_int_src (dep_insn))
	cost += 5;

      memory = get_attr_memory (insn);

      /* Show ability of reorder buffer to hide latency of load by executing
	 in parallel with previous instruction in case
	 previous instruction is not needed to compute the address.  */
      if ((memory == MEMORY_LOAD || memory == MEMORY_BOTH)
	  && !ix86_agi_dependent (dep_insn, insn))
	{
	  /* Claim moves to take one cycle, as core can issue one load
	     at time and the next load can start cycle later.  */
	  if (dep_insn_type == TYPE_IMOV
	      || dep_insn_type == TYPE_FMOV)
	    cost = 1;
	  else if (cost > 2)
	    cost -= 2;
	  else
	    cost = 1;
	}
      break;

    case PROCESSOR_AMDFAM10:
    case PROCESSOR_BDVER1:
    case PROCESSOR_BDVER2:
    case PROCESSOR_BDVER3:
    case PROCESSOR_BDVER4:
    case PROCESSOR_BTVER1:
    case PROCESSOR_BTVER2:
      /* Stack engine allows to execute push&pop instructions in parall.  */
      if ((insn_type == TYPE_PUSH || insn_type == TYPE_POP)
	  && (dep_insn_type == TYPE_PUSH || dep_insn_type == TYPE_POP))
	return 0;
      /* FALLTHRU */

    case PROCESSOR_ATHLON:
    case PROCESSOR_K8:
      memory = get_attr_memory (insn);

      /* Show ability of reorder buffer to hide latency of load by executing
	 in parallel with previous instruction in case
	 previous instruction is not needed to compute the address.  */
      if ((memory == MEMORY_LOAD || memory == MEMORY_BOTH)
	  && !ix86_agi_dependent (dep_insn, insn))
	{
	  enum attr_unit unit = get_attr_unit (insn);
	  int loadcost = 3;

	  /* Because of the difference between the length of integer and
	     floating unit pipeline preparation stages, the memory operands
	     for floating point are cheaper.

	     ??? For Athlon it the difference is most probably 2.  */
	  if (unit == UNIT_INTEGER || unit == UNIT_UNKNOWN)
	    loadcost = 3;
	  else
	    loadcost = TARGET_CPU_P (ATHLON) ? 2 : 0;

	  if (cost >= loadcost)
	    cost -= loadcost;
	  else
	    cost = 0;
	}
      break;

    case PROCESSOR_ZNVER1:
    case PROCESSOR_ZNVER2:
    case PROCESSOR_ZNVER3:
      /* Stack engine allows to execute push&pop instructions in parall.  */
      if ((insn_type == TYPE_PUSH || insn_type == TYPE_POP)
	  && (dep_insn_type == TYPE_PUSH || dep_insn_type == TYPE_POP))
	return 0;

      memory = get_attr_memory (insn);

      /* Show ability of reorder buffer to hide latency of load by executing
	 in parallel with previous instruction in case
	 previous instruction is not needed to compute the address.  */
      if ((memory == MEMORY_LOAD || memory == MEMORY_BOTH)
	  && !ix86_agi_dependent (dep_insn, insn))
	{
	  enum attr_unit unit = get_attr_unit (insn);
	  int loadcost;

	  if (unit == UNIT_INTEGER || unit == UNIT_UNKNOWN)
	    loadcost = 4;
	  else
	    loadcost = 7;

	  if (cost >= loadcost)
	    cost -= loadcost;
	  else
	    cost = 0;
	}
      break;

    case PROCESSOR_CORE2:
    case PROCESSOR_NEHALEM:
    case PROCESSOR_SANDYBRIDGE:
    case PROCESSOR_HASWELL:
    case PROCESSOR_GENERIC:
      /* Stack engine allows to execute push&pop instructions in parall.  */
      if ((insn_type == TYPE_PUSH || insn_type == TYPE_POP)
	  && (dep_insn_type == TYPE_PUSH || dep_insn_type == TYPE_POP))
	return 0;

      memory = get_attr_memory (insn);

      /* Show ability of reorder buffer to hide latency of load by executing
	 in parallel with previous instruction in case
	 previous instruction is not needed to compute the address.  */
      if ((memory == MEMORY_LOAD || memory == MEMORY_BOTH)
	  && !ix86_agi_dependent (dep_insn, insn))
	{
	  if (cost >= 4)
	    cost -= 4;
	  else
	    cost = 0;
	}
      break;

    case PROCESSOR_SILVERMONT:
    case PROCESSOR_KNL:
    case PROCESSOR_KNM:
    case PROCESSOR_INTEL:
      if (!reload_completed)
	return cost;

      /* Increase cost of integer loads.  */
      memory = get_attr_memory (dep_insn);
      if (memory == MEMORY_LOAD || memory == MEMORY_BOTH)
	{
	  enum attr_unit unit = get_attr_unit (dep_insn);
	  if (unit == UNIT_INTEGER && cost == 1)
	    {
	      if (memory == MEMORY_LOAD)
		cost = 3;
	      else
		{
		  /* Increase cost of ld/st for short int types only
		     because of store forwarding issue.  */
		  rtx set = single_set (dep_insn);
		  if (set && (GET_MODE (SET_DEST (set)) == QImode
			      || GET_MODE (SET_DEST (set)) == HImode))
		    {
		      /* Increase cost of store/load insn if exact
			 dependence exists and it is load insn.  */
		      enum attr_memory insn_memory = get_attr_memory (insn);
		      if (insn_memory == MEMORY_LOAD
			  && exact_store_load_dependency (dep_insn, insn))
			cost = 3;
		    }
		}
	    }
	}

    default:
      break;
    }

  return cost;
}

/* How many alternative schedules to try.  This should be as wide as the
   scheduling freedom in the DFA, but no wider.  Making this value too
   large results extra work for the scheduler.  */

int
ia32_multipass_dfa_lookahead (void)
{
  /* Generally, we want haifa-sched:max_issue() to look ahead as far
     as many instructions can be executed on a cycle, i.e.,
     issue_rate.  */
  if (reload_completed)
    return ix86_issue_rate ();
  /* Don't use lookahead for pre-reload schedule to save compile time.  */
  return 0;
}

/* Return true if target platform supports macro-fusion.  */

bool
ix86_macro_fusion_p ()
{
  return TARGET_FUSE_CMP_AND_BRANCH;
}

/* Check whether current microarchitecture support macro fusion
   for insn pair "CONDGEN + CONDJMP". Refer to
   "Intel Architectures Optimization Reference Manual". */

bool
ix86_macro_fusion_pair_p (rtx_insn *condgen, rtx_insn *condjmp)
{
  rtx src, dest;
  enum rtx_code ccode;
  rtx compare_set = NULL_RTX, test_if, cond;
  rtx alu_set = NULL_RTX, addr = NULL_RTX;
  enum attr_type condgen_type;

  if (!any_condjump_p (condjmp))
    return false;

  unsigned int condreg1, condreg2;
  rtx cc_reg_1;
  targetm.fixed_condition_code_regs (&condreg1, &condreg2);
  cc_reg_1 = gen_rtx_REG (CCmode, condreg1);
  if (!reg_referenced_p (cc_reg_1, PATTERN (condjmp))
      || !condgen
      || !modified_in_p (cc_reg_1, condgen))
    return false;

  condgen_type = get_attr_type (condgen);
  if (condgen_type == TYPE_MULTI
      && INSN_CODE (condgen) == code_for_stack_protect_test_1 (ptr_mode)
      && TARGET_FUSE_ALU_AND_BRANCH)
    {
      /* stack_protect_test_<mode> ends with a sub, which subtracts
	 a non-rip special memory operand from a GPR.  */
      src = NULL_RTX;
      alu_set = XVECEXP (PATTERN (condgen), 0, 1);
      goto handle_stack_protect_test;
    }
  else if (condgen_type != TYPE_TEST
	   && condgen_type != TYPE_ICMP
	   && condgen_type != TYPE_INCDEC
	   && condgen_type != TYPE_ALU)
    return false;

  compare_set = single_set (condgen);
  if (compare_set == NULL_RTX && !TARGET_FUSE_ALU_AND_BRANCH)
    return false;

  if (compare_set == NULL_RTX)
    {
      int i;
      rtx pat = PATTERN (condgen);
      for (i = 0; i < XVECLEN (pat, 0); i++)
	if (GET_CODE (XVECEXP (pat, 0, i)) == SET)
	  {
	    rtx set_src = SET_SRC (XVECEXP (pat, 0, i));
	    if (GET_CODE (set_src) == COMPARE)
	      compare_set = XVECEXP (pat, 0, i);
	    else
	      alu_set = XVECEXP (pat, 0, i);
	  }
    }
  if (compare_set == NULL_RTX)
    return false;
  src = SET_SRC (compare_set);
  if (GET_CODE (src) != COMPARE)
    return false;

  /* Macro-fusion for cmp/test MEM-IMM + conditional jmp is not
     supported.  */
  if ((MEM_P (XEXP (src, 0)) && CONST_INT_P (XEXP (src, 1)))
      || (MEM_P (XEXP (src, 1)) && CONST_INT_P (XEXP (src, 0))))
    return false;

  /* No fusion for RIP-relative address.  */
  if (MEM_P (XEXP (src, 0)))
    addr = XEXP (XEXP (src, 0), 0);
  else if (MEM_P (XEXP (src, 1)))
    addr = XEXP (XEXP (src, 1), 0);

  if (addr)
    {
      ix86_address parts;
      int ok = ix86_decompose_address (addr, &parts);
      gcc_assert (ok);

      if (ix86_rip_relative_addr_p (&parts))
	return false;
    }

 handle_stack_protect_test:
  test_if = SET_SRC (pc_set (condjmp));
  cond = XEXP (test_if, 0);
  ccode = GET_CODE (cond);
  /* Check whether conditional jump use Sign or Overflow Flags.  */
  if (!TARGET_FUSE_CMP_AND_BRANCH_SOFLAGS
      && (ccode == GE || ccode == GT || ccode == LE || ccode == LT))
    return false;

  /* Return true for TYPE_TEST and TYPE_ICMP.  */
  if (condgen_type == TYPE_TEST || condgen_type == TYPE_ICMP)
    return true;

  /* The following is the case that macro-fusion for alu + jmp.  */
  if (!TARGET_FUSE_ALU_AND_BRANCH || !alu_set)
    return false;

  /* No fusion for alu op with memory destination operand.  */
  dest = SET_DEST (alu_set);
  if (MEM_P (dest))
    return false;

  /* Macro-fusion for inc/dec + unsigned conditional jump is not
     supported.  */
  if (condgen_type == TYPE_INCDEC
      && (ccode == GEU || ccode == GTU || ccode == LEU || ccode == LTU))
    return false;

  return true;
}

