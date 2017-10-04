/* Subroutines used for calculate rtx costs of Andes NDS32 cpu for GNU compiler
   Copyright (C) 2012-2017 Free Software Foundation, Inc.
   Contributed by Andes Technology Corporation.

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

/* ------------------------------------------------------------------------ */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "tm_p.h"
#include "optabs.h"		/* For GEN_FCN.  */
#include "recog.h"
#include "tm-constrs.h"

/* ------------------------------------------------------------------------ */

bool
nds32_rtx_costs_impl (rtx x,
		      machine_mode mode ATTRIBUTE_UNUSED,
		      int outer_code,
		      int opno ATTRIBUTE_UNUSED,
		      int *total,
		      bool speed)
{
  int code = GET_CODE (x);

  /* According to 'speed', goto suitable cost model section.  */
  if (speed)
    goto performance_cost;
  else
    goto size_cost;


performance_cost:
  /* This is section for performance cost model.  */

  /* In gcc/rtl.h, the default value of COSTS_N_INSNS(N) is N*4.
     We treat it as 4-cycle cost for each instruction
     under performance consideration.  */
  switch (code)
    {
    case SET:
      /* For 'SET' rtx, we need to return false
         so that it can recursively calculate costs.  */
      return false;

    case USE:
      /* Used in combine.c as a marker.  */
      *total = 0;
      break;

    case MULT:
      *total = COSTS_N_INSNS (1);
      break;

    case DIV:
    case UDIV:
    case MOD:
    case UMOD:
      *total = COSTS_N_INSNS (7);
      break;

    default:
      *total = COSTS_N_INSNS (1);
      break;
    }

  return true;


size_cost:
  /* This is section for size cost model.  */

  /* In gcc/rtl.h, the default value of COSTS_N_INSNS(N) is N*4.
     We treat it as 4-byte cost for each instruction
     under code size consideration.  */
  switch (code)
    {
    case SET:
      /* For 'SET' rtx, we need to return false
	 so that it can recursively calculate costs.  */
      return false;

    case USE:
      /* Used in combine.c as a marker.  */
      *total = 0;
      break;

    case CONST_INT:
      /* All instructions involving constant operation
	 need to be considered for cost evaluation.  */
      if (outer_code == SET)
	{
	  /* (set X imm5s), use movi55, 2-byte cost.
	     (set X imm20s), use movi, 4-byte cost.
	     (set X BIG_INT), use sethi/ori, 8-byte cost.  */
	  if (satisfies_constraint_Is05 (x))
	    *total = COSTS_N_INSNS (1) - 2;
	  else if (satisfies_constraint_Is20 (x))
	    *total = COSTS_N_INSNS (1);
	  else
	    *total = COSTS_N_INSNS (2);
	}
      else if (outer_code == PLUS || outer_code == MINUS)
	{
	  /* Possible addi333/subi333 or subi45/addi45, 2-byte cost.
	     General case, cost 1 instruction with 4-byte.  */
	  if (satisfies_constraint_Iu05 (x))
	    *total = COSTS_N_INSNS (1) - 2;
	  else
	    *total = COSTS_N_INSNS (1);
	}
      else if (outer_code == ASHIFT)
	{
	  /* Possible slli333, 2-byte cost.
	     General case, cost 1 instruction with 4-byte.  */
	  if (satisfies_constraint_Iu03 (x))
	    *total = COSTS_N_INSNS (1) - 2;
	  else
	    *total = COSTS_N_INSNS (1);
	}
      else if (outer_code == ASHIFTRT || outer_code == LSHIFTRT)
	{
	  /* Possible srai45 or srli45, 2-byte cost.
	     General case, cost 1 instruction with 4-byte.  */
	  if (satisfies_constraint_Iu05 (x))
	    *total = COSTS_N_INSNS (1) - 2;
	  else
	    *total = COSTS_N_INSNS (1);
	}
      else
	{
	  /* For other cases, simply set it 4-byte cost.  */
	  *total = COSTS_N_INSNS (1);
	}
      break;

    case CONST_DOUBLE:
      /* It requires high part and low part processing, set it 8-byte cost.  */
      *total = COSTS_N_INSNS (2);
      break;

    default:
      /* For other cases, generally we set it 4-byte cost
         and stop resurively traversing.  */
      *total = COSTS_N_INSNS (1);
      break;
    }

  return true;
}

int
nds32_address_cost_impl (rtx address,
			 machine_mode mode ATTRIBUTE_UNUSED,
			 addr_space_t as ATTRIBUTE_UNUSED,
			 bool speed)
{
  rtx plus0, plus1;
  enum rtx_code code;

  code = GET_CODE (address);

  /* According to 'speed', goto suitable cost model section.  */
  if (speed)
    goto performance_cost;
  else
    goto size_cost;

performance_cost:
  /* This is section for performance cost model.  */

  /* FALLTHRU, currently we use same cost model as size_cost.  */

size_cost:
  /* This is section for size cost model.  */

  switch (code)
    {
    case POST_MODIFY:
    case POST_INC:
    case POST_DEC:
      /* We encourage that rtx contains
	 POST_MODIFY/POST_INC/POST_DEC behavior.  */
      return 0;

    case SYMBOL_REF:
      /* We can have gp-relative load/store for symbol_ref.
	 Have it 4-byte cost.  */
      return COSTS_N_INSNS (1);

    case CONST:
      /* It is supposed to be the pattern (const (plus symbol_ref const_int)).
	 Have it 4-byte cost.  */
      return COSTS_N_INSNS (1);

    case REG:
      /* Simply return 4-byte costs.  */
      return COSTS_N_INSNS (1);

    case PLUS:
      /* We do not need to check if the address is a legitimate address,
	 because this hook is never called with an invalid address.
	 But we better check the range of
	 const_int value for cost, if it exists.  */
      plus0 = XEXP (address, 0);
      plus1 = XEXP (address, 1);

      if (REG_P (plus0) && CONST_INT_P (plus1))
	{
	  /* If it is possible to be lwi333/swi333 form,
	     make it 2-byte cost.  */
	  if (satisfies_constraint_Iu05 (plus1))
	    return (COSTS_N_INSNS (1) - 2);
	  else
	    return COSTS_N_INSNS (1);
	}

      /* For other 'plus' situation, make it cost 4-byte.  */
      return COSTS_N_INSNS (1);

    default:
      break;
    }

  return COSTS_N_INSNS (4);
}

/* ------------------------------------------------------------------------ */
