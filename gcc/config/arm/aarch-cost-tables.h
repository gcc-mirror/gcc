/* RTX cost tables shared between arm and aarch64.

   Copyright (C) 2013 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

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

#ifndef GCC_AARCH_COST_TABLES_H
#define GCC_AARCH_COST_TABLES_H

const struct cpu_cost_table generic_extra_costs =
{
  /* ALU */
  {
    0,			/* Arith.  */
    0,			/* Logical.  */
    0,			/* Shift.  */
    COSTS_N_INSNS (1),	/* Shift_reg.  */
    0,			/* Arith_shift.  */
    COSTS_N_INSNS (1),	/* Arith_shift_reg.  */
    0,			/* Log_shift.  */
    COSTS_N_INSNS (1),	/* Log_shift_reg.  */
    0,			/* Extend.  */
    COSTS_N_INSNS (1),	/* Extend_arith.  */
    0,			/* Bfi.  */
    0,			/* Bfx.  */
    0,			/* Clz.  */
    COSTS_N_INSNS (1),	/* non_exec.  */
    false		/* non_exec_costs_exec.  */
  },
  {
    /* MULT SImode */
    {
      COSTS_N_INSNS (2),	/* Simple.  */
      COSTS_N_INSNS (1),	/* Flag_setting.  */
      COSTS_N_INSNS (2),	/* Extend.  */
      COSTS_N_INSNS (3),	/* Add.  */
      COSTS_N_INSNS (3),	/* Extend_add.  */
      COSTS_N_INSNS (8)		/* Idiv.  */
    },
    /* MULT DImode */
    {
      0,			/* Simple (N/A).  */
      0,			/* Flag_setting (N/A).  */
      COSTS_N_INSNS (2),	/* Extend.  */
      0,			/* Add (N/A).  */
      COSTS_N_INSNS (3),	/* Extend_add.  */
      0				/* Idiv (N/A).  */
    }
  },
  /* LD/ST */
  {
    COSTS_N_INSNS (2),	/* Load.  */
    COSTS_N_INSNS (2),	/* Load_sign_extend.  */
    COSTS_N_INSNS (3),	/* Ldrd.  */
    COSTS_N_INSNS (2),	/* Ldm_1st.  */
    1,			/* Ldm_regs_per_insn_1st.  */
    1,			/* Ldm_regs_per_insn_subsequent.  */
    COSTS_N_INSNS (2),	/* Loadf.  */
    COSTS_N_INSNS (3),	/* Loadd.  */
    COSTS_N_INSNS (1),  /* Load_unaligned.  */
    COSTS_N_INSNS (2),	/* Store.  */
    COSTS_N_INSNS (3),	/* Strd.  */
    COSTS_N_INSNS (2),	/* Stm_1st.  */
    1,			/* Stm_regs_per_insn_1st.  */
    1,			/* Stm_regs_per_insn_subsequent.  */
    COSTS_N_INSNS (2),	/* Storef.  */
    COSTS_N_INSNS (3),	/* Stored.  */
    COSTS_N_INSNS (1)  /* Store_unaligned.  */
  },
  {
    /* FP SFmode */
    {
      COSTS_N_INSNS (7),	/* Div.  */
      COSTS_N_INSNS (2),	/* Mult.  */
      COSTS_N_INSNS (3),	/* Mult_addsub.  */
      COSTS_N_INSNS (3),	/* Fma.  */
      COSTS_N_INSNS (1),	/* Addsub.  */
      0,			/* Fpconst.  */
      0,			/* Neg.  */
      0,			/* Compare.  */
      0,			/* Widen.  */
      0,			/* Narrow.  */
      0,			/* Toint.  */
      0,			/* Fromint.  */
      0				/* Roundint.  */
    },
    /* FP DFmode */
    {
      COSTS_N_INSNS (15),	/* Div.  */
      COSTS_N_INSNS (5),	/* Mult.  */
      COSTS_N_INSNS (7),	/* Mult_addsub.  */
      COSTS_N_INSNS (7),	/* Fma.  */
      COSTS_N_INSNS (3),	/* Addsub.  */
      0,			/* Fpconst.  */
      0,			/* Neg.  */
      0,			/* Compare.  */
      0,			/* Widen.  */
      0,			/* Narrow.  */
      0,			/* Toint.  */
      0,			/* Fromint.  */
      0				/* Roundint.  */
    }
  },
  /* Vector */
  {
    COSTS_N_INSNS (1)	/* Alu.  */
  }
};

#endif /* GCC_AARCH_COST_TABLES_H */

