/* RTX cost tables shared between arm and aarch64.

   Copyright (C) 2013-2014 Free Software Foundation, Inc.
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

const struct cpu_cost_table cortexa53_extra_costs =
{
  /* ALU */
  {
    0,			/* Arith.  */
    0,			/* Logical.  */
    COSTS_N_INSNS (1),	/* Shift.  */
    COSTS_N_INSNS (2),	/* Shift_reg.  */
    COSTS_N_INSNS (1),	/* Arith_shift.  */
    COSTS_N_INSNS (2),	/* Arith_shift_reg.  */
    COSTS_N_INSNS (1),	/* Log_shift.  */
    COSTS_N_INSNS (2),	/* Log_shift_reg.  */
    0,			/* Extend.  */
    COSTS_N_INSNS (1),	/* Extend_arith.  */
    COSTS_N_INSNS (1),	/* Bfi.  */
    COSTS_N_INSNS (1),	/* Bfx.  */
    0,			/* Clz.  */
    0,			/* non_exec.  */
    true		/* non_exec_costs_exec.  */
  },
  {
    /* MULT SImode */
    {
      COSTS_N_INSNS (1),	/* Simple.  */
      COSTS_N_INSNS (2),	/* Flag_setting.  */
      COSTS_N_INSNS (1),	/* Extend.  */
      COSTS_N_INSNS (1),	/* Add.  */
      COSTS_N_INSNS (1),	/* Extend_add.  */
      COSTS_N_INSNS (7)		/* Idiv.  */
    },
    /* MULT DImode */
    {
      COSTS_N_INSNS (2),	/* Simple.  */
      0,			/* Flag_setting (N/A).  */
      COSTS_N_INSNS (2),	/* Extend.  */
      COSTS_N_INSNS (2),	/* Add.  */
      COSTS_N_INSNS (2),	/* Extend_add.  */
      COSTS_N_INSNS (15)	/* Idiv.  */
    }
  },
  /* LD/ST */
  {
    COSTS_N_INSNS (1),		/* Load.  */
    COSTS_N_INSNS (1),		/* Load_sign_extend.  */
    COSTS_N_INSNS (1),		/* Ldrd.  */
    COSTS_N_INSNS (1),		/* Ldm_1st.  */
    1,				/* Ldm_regs_per_insn_1st.  */
    2,				/* Ldm_regs_per_insn_subsequent.  */
    COSTS_N_INSNS (1),		/* Loadf.  */
    COSTS_N_INSNS (1),		/* Loadd.  */
    COSTS_N_INSNS (1),		/* Load_unaligned.  */
    0,				/* Store.  */
    0,				/* Strd.  */
    0,				/* Stm_1st.  */
    1,				/* Stm_regs_per_insn_1st.  */
    2,				/* Stm_regs_per_insn_subsequent.  */
    0,				/* Storef.  */
    0,				/* Stored.  */
    COSTS_N_INSNS (1)		/* Store_unaligned.  */
  },
  {
    /* FP SFmode */
    {
      COSTS_N_INSNS (15),	/* Div.  */
      COSTS_N_INSNS (3),	/* Mult.  */
      COSTS_N_INSNS (7),	/* Mult_addsub. */
      COSTS_N_INSNS (7),	/* Fma.  */
      COSTS_N_INSNS (3),	/* Addsub.  */
      COSTS_N_INSNS (1),	/* Fpconst. */
      COSTS_N_INSNS (2),	/* Neg.  */
      COSTS_N_INSNS (1),	/* Compare.  */
      COSTS_N_INSNS (3),	/* Widen.  */
      COSTS_N_INSNS (3),	/* Narrow.  */
      COSTS_N_INSNS (3),	/* Toint.  */
      COSTS_N_INSNS (3),	/* Fromint.  */
      COSTS_N_INSNS (3)		/* Roundint.  */
    },
    /* FP DFmode */
    {
      COSTS_N_INSNS (30),	/* Div.  */
      COSTS_N_INSNS (3),	/* Mult.  */
      COSTS_N_INSNS (7),	/* Mult_addsub.  */
      COSTS_N_INSNS (7),	/* Fma.  */
      COSTS_N_INSNS (3),	/* Addsub.  */
      COSTS_N_INSNS (1),	/* Fpconst.  */
      COSTS_N_INSNS (2),	/* Neg.  */
      COSTS_N_INSNS (1),	/* Compare.  */
      COSTS_N_INSNS (3),	/* Widen.  */
      COSTS_N_INSNS (3),	/* Narrow.  */
      COSTS_N_INSNS (3),	/* Toint.  */
      COSTS_N_INSNS (3),	/* Fromint.  */
      COSTS_N_INSNS (3)		/* Roundint.  */
    }
  },
  /* Vector */
  {
    COSTS_N_INSNS (1)	/* Alu.  */
  }
};

const struct cpu_cost_table cortexa57_extra_costs =
{
  /* ALU */
  {
    0,                 /* arith.  */
    0,                 /* logical.  */
    0,                 /* shift.  */
    COSTS_N_INSNS (1), /* shift_reg.  */
    COSTS_N_INSNS (1), /* arith_shift.  */
    COSTS_N_INSNS (1), /* arith_shift_reg.  */
    COSTS_N_INSNS (1), /* log_shift.  */
    COSTS_N_INSNS (1), /* log_shift_reg.  */
    0,                 /* extend.  */
    COSTS_N_INSNS (1), /* extend_arith.  */
    COSTS_N_INSNS (1), /* bfi.  */
    0,                 /* bfx.  */
    0,                 /* clz.  */
    0,                 /* non_exec.  */
    true               /* non_exec_costs_exec.  */
  },
  {
    /* MULT SImode */
    {
      COSTS_N_INSNS (2),       /* simple.  */
      COSTS_N_INSNS (3),       /* flag_setting.  */
      COSTS_N_INSNS (2),       /* extend.  */
      COSTS_N_INSNS (2),       /* add.  */
      COSTS_N_INSNS (2),       /* extend_add.  */
      COSTS_N_INSNS (18)       /* idiv.  */
    },
    /* MULT DImode */
    {
      COSTS_N_INSNS (4),       /* simple.  */
      0,                       /* flag_setting (N/A).  */
      COSTS_N_INSNS (2),       /* extend.  */
      COSTS_N_INSNS (4),       /* add.  */
      COSTS_N_INSNS (2),       /* extend_add.  */
      COSTS_N_INSNS (34)       /* idiv.  */
    }
  },
  /* LD/ST */
  {
    COSTS_N_INSNS (3),         /* load.  */
    COSTS_N_INSNS (3),         /* load_sign_extend.  */
    COSTS_N_INSNS (3),         /* ldrd.  */
    COSTS_N_INSNS (2),         /* ldm_1st.  */
    1,                         /* ldm_regs_per_insn_1st.  */
    2,                         /* ldm_regs_per_insn_subsequent.  */
    COSTS_N_INSNS (4),         /* loadf.  */
    COSTS_N_INSNS (4),         /* loadd.  */
    COSTS_N_INSNS (5),         /* load_unaligned.  */
    0,                         /* store.  */
    0,                         /* strd.  */
    0,                         /* stm_1st.  */
    1,                         /* stm_regs_per_insn_1st.  */
    2,                         /* stm_regs_per_insn_subsequent.  */
    0,                         /* storef.  */
    0,                         /* stored.  */
    COSTS_N_INSNS (1)          /* store_unaligned.  */
  },
  {
    /* FP SFmode */
    {
      COSTS_N_INSNS (17),      /* div.  */
      COSTS_N_INSNS (5),       /* mult.  */
      COSTS_N_INSNS (9),       /* mult_addsub. */
      COSTS_N_INSNS (9),       /* fma.  */
      COSTS_N_INSNS (4),       /* addsub.  */
      COSTS_N_INSNS (2),       /* fpconst. */
      COSTS_N_INSNS (2),       /* neg.  */
      COSTS_N_INSNS (2),       /* compare.  */
      COSTS_N_INSNS (4),       /* widen.  */
      COSTS_N_INSNS (4),       /* narrow.  */
      COSTS_N_INSNS (4),       /* toint.  */
      COSTS_N_INSNS (4),       /* fromint.  */
      COSTS_N_INSNS (4)        /* roundint.  */
    },
    /* FP DFmode */
    {
      COSTS_N_INSNS (31),      /* div.  */
      COSTS_N_INSNS (5),       /* mult.  */
      COSTS_N_INSNS (9),       /* mult_addsub.  */
      COSTS_N_INSNS (9),       /* fma.  */
      COSTS_N_INSNS (4),       /* addsub.  */
      COSTS_N_INSNS (2),       /* fpconst.  */
      COSTS_N_INSNS (2),       /* neg.  */
      COSTS_N_INSNS (2),       /* compare.  */
      COSTS_N_INSNS (4),       /* widen.  */
      COSTS_N_INSNS (4),       /* narrow.  */
      COSTS_N_INSNS (4),       /* toint.  */
      COSTS_N_INSNS (4),       /* fromint.  */
      COSTS_N_INSNS (4)        /* roundint.  */
    }
  },
  /* Vector */
  {
    COSTS_N_INSNS (1)  /* alu.  */
  }
};

#endif /* GCC_AARCH_COST_TABLES_H */
