/* RTX cost tables for AArch64.

   Copyright (C) 2014-2016 Free Software Foundation, Inc.

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

#ifndef GCC_AARCH64_COST_TABLES_H
#define GCC_AARCH64_COST_TABLES_H

#include "config/arm/aarch-cost-tables.h"

/* ThunderX does not have implement AArch32.  */
const struct cpu_cost_table thunderx_extra_costs =
{
  /* ALU */
  {
    0,			/* Arith.  */
    0,			/* Logical.  */
    0,			/* Shift.  */
    0,			/* Shift_reg.  */
    COSTS_N_INSNS (1),	/* Arith_shift.  */
    COSTS_N_INSNS (1),	/* Arith_shift_reg.  */
    COSTS_N_INSNS (1),	/* UNUSED: Log_shift.  */
    COSTS_N_INSNS (1),	/* UNUSED: Log_shift_reg.  */
    0,			/* Extend.  */
    COSTS_N_INSNS (1),	/* Extend_arith.  */
    0,			/* Bfi.  */
    0,			/* Bfx.  */
    COSTS_N_INSNS (5),	/* Clz.  */
    0,			/* rev.  */
    0,			/* UNUSED: non_exec.  */
    false		/* UNUSED: non_exec_costs_exec.  */
  },
  {
    /* MULT SImode */
    {
      COSTS_N_INSNS (3),	/* Simple.  */
      0,			/* Flag_setting.  */
      0,			/* Extend.  */
      0,			/* Add.  */
      COSTS_N_INSNS (1),	/* Extend_add.  */
      COSTS_N_INSNS (21)	/* Idiv.  */
    },
    /* MULT DImode */
    {
      COSTS_N_INSNS (3),	/* Simple.  */
      0,			/* Flag_setting.  */
      0,			/* Extend.  */
      0,			/* Add.  */
      COSTS_N_INSNS (1),	/* Extend_add.  */
      COSTS_N_INSNS (37)	/* Idiv.  */
    },
  },
  /* LD/ST */
  {
    COSTS_N_INSNS (2),	/* Load.  */
    COSTS_N_INSNS (2),	/* Load_sign_extend.  */
    COSTS_N_INSNS (2),	/* Ldrd.  */
    0,			/* N/A: Ldm_1st.  */
    0,			/* N/A: Ldm_regs_per_insn_1st.  */
    0,			/* N/A: Ldm_regs_per_insn_subsequent.  */
    COSTS_N_INSNS (3),	/* Loadf.  */
    COSTS_N_INSNS (3),	/* Loadd.  */
    0,  		/* N/A: Load_unaligned.  */
    0,			/* Store.  */
    0,			/* Strd.  */
    0,			/* N/A: Stm_1st.  */
    0,			/* N/A: Stm_regs_per_insn_1st.  */
    0,			/* N/A: Stm_regs_per_insn_subsequent.  */
    0,			/* Storef.  */
    0,			/* Stored.  */
    COSTS_N_INSNS (1),	/* Store_unaligned.  */
    COSTS_N_INSNS (1),	/* Loadv.  */
    COSTS_N_INSNS (1)	/* Storev.  */
  },
  {
    /* FP SFmode */
    {
      COSTS_N_INSNS (11),	/* Div.  */
      COSTS_N_INSNS (5),	/* Mult.  */
      COSTS_N_INSNS (5),	/* Mult_addsub.  */
      COSTS_N_INSNS (5),	/* Fma.  */
      COSTS_N_INSNS (3),	/* Addsub.  */
      0,			/* Fpconst.  */
      COSTS_N_INSNS (1),	/* Neg.  */
      0,			/* Compare.  */
      COSTS_N_INSNS (5),	/* Widen.  */
      COSTS_N_INSNS (5),	/* Narrow.  */
      COSTS_N_INSNS (5),	/* Toint.  */
      COSTS_N_INSNS (5),	/* Fromint.  */
      COSTS_N_INSNS (1)		/* Roundint.  */
    },
    /* FP DFmode */
    {
      COSTS_N_INSNS (21),	/* Div.  */
      COSTS_N_INSNS (5),	/* Mult.  */
      COSTS_N_INSNS (5),	/* Mult_addsub.  */
      COSTS_N_INSNS (5),	/* Fma.  */
      COSTS_N_INSNS (3),	/* Addsub.  */
      0,			/* Fpconst.  */
      COSTS_N_INSNS (1),	/* Neg.  */
      0,			/* Compare.  */
      COSTS_N_INSNS (5),	/* Widen.  */
      COSTS_N_INSNS (5),	/* Narrow.  */
      COSTS_N_INSNS (5),	/* Toint.  */
      COSTS_N_INSNS (5),	/* Fromint.  */
      COSTS_N_INSNS (1)		/* Roundint.  */
    }
  },
  /* Vector */
  {
    COSTS_N_INSNS (1)	/* Alu.  */
  }
};

const struct cpu_cost_table vulcan_extra_costs =
{
  /* ALU */
  {
    0,			/* Arith.  */
    0,			/* Logical.  */
    0,			/* Shift.  */
    0,			/* Shift_reg.  */
    COSTS_N_INSNS (1),	/* Arith_shift.  */
    COSTS_N_INSNS (1),	/* Arith_shift_reg.  */
    COSTS_N_INSNS (1),	/* Log_shift.  */
    COSTS_N_INSNS (1),	/* Log_shift_reg.  */
    0,			/* Extend.  */
    COSTS_N_INSNS (1),	/* Extend_arith.  */
    0,			/* Bfi.  */
    0,			/* Bfx.  */
    COSTS_N_INSNS (3),	/* Clz.  */
    0,			/* Rev.  */
    0,			/* Non_exec.  */
    true		/* Non_exec_costs_exec.  */
  },
  {
    /* MULT SImode */
    {
      COSTS_N_INSNS (4),	/* Simple.  */
      COSTS_N_INSNS (4),	/* Flag_setting.  */
      COSTS_N_INSNS (4),	/* Extend.  */
      COSTS_N_INSNS (5),	/* Add.  */
      COSTS_N_INSNS (5),	/* Extend_add.  */
      COSTS_N_INSNS (18)	/* Idiv.  */
    },
    /* MULT DImode */
    {
      COSTS_N_INSNS (4),       /* Simple.  */
      0,                       /* Flag_setting.  */
      COSTS_N_INSNS (4),       /* Extend.  */
      COSTS_N_INSNS (5),       /* Add.  */
      COSTS_N_INSNS (5),       /* Extend_add.  */
      COSTS_N_INSNS (26)       /* Idiv.  */
    }
  },
  /* LD/ST */
  {
    COSTS_N_INSNS (4),	/* Load.  */
    COSTS_N_INSNS (4),	/* Load_sign_extend.  */
    COSTS_N_INSNS (5),	/* Ldrd.  */
    COSTS_N_INSNS (4),	/* Ldm_1st.  */
    1,			/* Ldm_regs_per_insn_1st.  */
    1,			/* Ldm_regs_per_insn_subsequent.  */
    COSTS_N_INSNS (4),	/* Loadf.  */
    COSTS_N_INSNS (4),	/* Loadd.  */
    COSTS_N_INSNS (4),	/* Load_unaligned.  */
    0,			/* Store.  */
    0,			/* Strd.  */
    0,			/* Stm_1st.  */
    1,			/* Stm_regs_per_insn_1st.  */
    1,			/* Stm_regs_per_insn_subsequent.  */
    0,			/* Storef.  */
    0,			/* Stored.  */
    0,			/* Store_unaligned.  */
    COSTS_N_INSNS (1),	/* Loadv.  */
    COSTS_N_INSNS (1)	/* Storev.  */
  },
  {
    /* FP SFmode */
    {
      COSTS_N_INSNS (4),	/* Div.  */
      COSTS_N_INSNS (1),	/* Mult.  */
      COSTS_N_INSNS (1),	/* Mult_addsub. */
      COSTS_N_INSNS (1),	/* Fma.  */
      COSTS_N_INSNS (1),	/* Addsub.  */
      COSTS_N_INSNS (1),	/* Fpconst. */
      COSTS_N_INSNS (1),	/* Neg.  */
      COSTS_N_INSNS (1),	/* Compare.  */
      COSTS_N_INSNS (2),	/* Widen.  */
      COSTS_N_INSNS (2),	/* Narrow.  */
      COSTS_N_INSNS (2),	/* Toint.  */
      COSTS_N_INSNS (2),	/* Fromint.  */
      COSTS_N_INSNS (2) 	/* Roundint.  */
    },
    /* FP DFmode */
    {
      COSTS_N_INSNS (6),	/* Div.  */
      COSTS_N_INSNS (1),	/* Mult.  */
      COSTS_N_INSNS (1),	/* Mult_addsub.  */
      COSTS_N_INSNS (1),	/* Fma.  */
      COSTS_N_INSNS (1),	/* Addsub.  */
      COSTS_N_INSNS (1),	/* Fpconst.  */
      COSTS_N_INSNS (1),	/* Neg.  */
      COSTS_N_INSNS (1),	/* Compare.  */
      COSTS_N_INSNS (2),	/* Widen.  */
      COSTS_N_INSNS (2),	/* Narrow.  */
      COSTS_N_INSNS (2),	/* Toint.  */
      COSTS_N_INSNS (2),	/* Fromint.  */
      COSTS_N_INSNS (2) 	/* Roundint.  */
    }
  },
  /* Vector */
  {
    COSTS_N_INSNS (1)	/* Alu.  */
  }
};


#endif

