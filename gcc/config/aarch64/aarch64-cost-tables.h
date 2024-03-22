/* RTX cost tables for AArch64.

   Copyright (C) 2014-2024 Free Software Foundation, Inc.

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

/* QDF24xx does not implement AArch32.  */
const struct cpu_cost_table qdf24xx_extra_costs =
{
  /* ALU */
  {
    0,                 /* arith.  */
    0,                 /* logical.  */
    0,                 /* shift.  */
    0,                 /* shift_reg.  */
    COSTS_N_INSNS (1), /* arith_shift.  */
    COSTS_N_INSNS (1), /* arith_shift_reg.  */
    0,                 /* log_shift.  */
    0,                 /* log_shift_reg.  */
    0,                 /* extend.  */
    0,                 /* extend_arith.  */
    0,                 /* bfi.  */
    0,                 /* bfx.  */
    0,                 /* clz.  */
    0,	               /* rev.  */
    0,                 /* non_exec.  */
    true               /* non_exec_costs_exec.  */
  },
  {
    /* MULT SImode */
    {
      COSTS_N_INSNS (2),       /* simple.  */
      COSTS_N_INSNS (2),       /* flag_setting.  */
      COSTS_N_INSNS (2),       /* extend.  */
      COSTS_N_INSNS (2),       /* add.  */
      COSTS_N_INSNS (2),       /* extend_add.  */
      COSTS_N_INSNS (4)       /* idiv.  */
    },
    /* MULT DImode */
    {
      COSTS_N_INSNS (3),       /* simple.  */
      0,                       /* flag_setting (N/A).  */
      COSTS_N_INSNS (3),       /* extend.  */
      COSTS_N_INSNS (3),       /* add.  */
      COSTS_N_INSNS (3),       /* extend_add.  */
      COSTS_N_INSNS (9)       /* idiv.  */
    }
  },
  /* LD/ST */
  {
    COSTS_N_INSNS (2),         /* load.  */
    COSTS_N_INSNS (2),         /* load_sign_extend.  */
    COSTS_N_INSNS (2),         /* ldrd.  */
    COSTS_N_INSNS (2),         /* ldm_1st.  */
    1,                         /* ldm_regs_per_insn_1st.  */
    2,                         /* ldm_regs_per_insn_subsequent.  */
    COSTS_N_INSNS (2),         /* loadf.  */
    COSTS_N_INSNS (2),         /* loadd.  */
    COSTS_N_INSNS (3),         /* load_unaligned.  */
    0,                         /* store.  */
    0,                         /* strd.  */
    0,                         /* stm_1st.  */
    1,                         /* stm_regs_per_insn_1st.  */
    2,                         /* stm_regs_per_insn_subsequent.  */
    0,                         /* storef.  */
    0,                         /* stored.  */
    COSTS_N_INSNS (1),         /* store_unaligned.  */
    COSTS_N_INSNS (1),         /* loadv.  */
    COSTS_N_INSNS (1)          /* storev.  */
  },
  {
    /* FP SFmode */
    {
      COSTS_N_INSNS (6),      /* div.  */
      COSTS_N_INSNS (5),       /* mult.  */
      COSTS_N_INSNS (5),       /* mult_addsub. */
      COSTS_N_INSNS (5),       /* fma.  */
      COSTS_N_INSNS (3),       /* addsub.  */
      COSTS_N_INSNS (1),       /* fpconst. */
      COSTS_N_INSNS (1),       /* neg.  */
      COSTS_N_INSNS (2),       /* compare.  */
      COSTS_N_INSNS (4),       /* widen.  */
      COSTS_N_INSNS (4),       /* narrow.  */
      COSTS_N_INSNS (4),       /* toint.  */
      COSTS_N_INSNS (4),       /* fromint.  */
      COSTS_N_INSNS (2)        /* roundint.  */
    },
    /* FP DFmode */
    {
      COSTS_N_INSNS (11),      /* div.  */
      COSTS_N_INSNS (6),       /* mult.  */
      COSTS_N_INSNS (6),       /* mult_addsub.  */
      COSTS_N_INSNS (6),       /* fma.  */
      COSTS_N_INSNS (3),       /* addsub.  */
      COSTS_N_INSNS (1),       /* fpconst.  */
      COSTS_N_INSNS (1),       /* neg.  */
      COSTS_N_INSNS (2),       /* compare.  */
      COSTS_N_INSNS (4),       /* widen.  */
      COSTS_N_INSNS (4),       /* narrow.  */
      COSTS_N_INSNS (4),       /* toint.  */
      COSTS_N_INSNS (4),       /* fromint.  */
      COSTS_N_INSNS (2)        /* roundint.  */
    }
  },
  /* Vector */
  {
    COSTS_N_INSNS (1),  /* alu.  */
    COSTS_N_INSNS (4),  /* mult.  */
    COSTS_N_INSNS (1),  /* movi.  */
    COSTS_N_INSNS (2),  /* dup.  */
    COSTS_N_INSNS (2)   /* extract.  */
  }
};

/* ThunderX does not implement AArch32.  */
const struct cpu_cost_table thunderx_extra_costs =
{
  /* ALU */
  {
    0,			/* Arith.  */
    0,			/* Logical.  */
    0,			/* Shift.  */
    0,			/* Shift_reg.  */
    COSTS_N_INSNS (1)+1,	/* Arith_shift.  */
    COSTS_N_INSNS (1)+1,	/* Arith_shift_reg.  */
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
    COSTS_N_INSNS (1),	/* Alu.  */
    COSTS_N_INSNS (4),	/* mult.  */
    COSTS_N_INSNS (1),	/* movi.  */
    COSTS_N_INSNS (2),	/* dup.  */
    COSTS_N_INSNS (2)	/* extract.  */
  }
};

const struct cpu_cost_table thunderx2t99_extra_costs =
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
    COSTS_N_INSNS (1),	/* Alu.  */
    COSTS_N_INSNS (4),	/* Mult.  */
    COSTS_N_INSNS (1),	/* movi.  */
    COSTS_N_INSNS (2),	/* dup.  */
    COSTS_N_INSNS (2)	/* extract.  */
  }
};

const struct cpu_cost_table thunderx3t110_extra_costs = 
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
    COSTS_N_INSNS (1),	/* Alu.  */
    COSTS_N_INSNS (4),	/* Mult.  */
    COSTS_N_INSNS (1),	/* movi.  */
    COSTS_N_INSNS (2),	/* dup.  */
    COSTS_N_INSNS (2)	/* extract.  */
  }
};

const struct cpu_cost_table tsv110_extra_costs =
{
  /* ALU */
  {
    0,                 /* arith.  */
    0,                 /* logical.  */
    0,                 /* shift.  */
    0,                 /* shift_reg.  */
    COSTS_N_INSNS (1), /* arith_shift.  */
    COSTS_N_INSNS (1), /* arith_shift_reg.  */
    COSTS_N_INSNS (1), /* log_shift.  */
    COSTS_N_INSNS (1), /* log_shift_reg.  */
    0,                 /* extend.  */
    COSTS_N_INSNS (1), /* extend_arith.  */
    0,                 /* bfi.  */
    0,                 /* bfx.  */
    0,                 /* clz.  */
    0,                 /* rev.  */
    0,                 /* non_exec.  */
    true               /* non_exec_costs_exec.  */
  },

  {
    /* MULT SImode */
    {
      COSTS_N_INSNS (2),       /* simple.  */
      COSTS_N_INSNS (2),       /* flag_setting.  */
      COSTS_N_INSNS (2),       /* extend.  */
      COSTS_N_INSNS (2),       /* add.  */
      COSTS_N_INSNS (2),       /* extend_add.  */
      COSTS_N_INSNS (11)       /* idiv.  */
    },
    /* MULT DImode */
    {
      COSTS_N_INSNS (3),       /* simple.  */
      0,                       /* flag_setting (N/A).  */
      COSTS_N_INSNS (3),       /* extend.  */
      COSTS_N_INSNS (3),       /* add.  */
      COSTS_N_INSNS (3),       /* extend_add.  */
      COSTS_N_INSNS (19)       /* idiv.  */
    }
  },
  /* LD/ST */
  {
    COSTS_N_INSNS (3),         /* load.  */
    COSTS_N_INSNS (4),         /* load_sign_extend.  */
    COSTS_N_INSNS (3),         /* ldrd.  */
    COSTS_N_INSNS (3),         /* ldm_1st.  */
    1,                         /* ldm_regs_per_insn_1st.  */
    2,                         /* ldm_regs_per_insn_subsequent.  */
    COSTS_N_INSNS (4),         /* loadf.  */
    COSTS_N_INSNS (4),         /* loadd.  */
    COSTS_N_INSNS (4),         /* load_unaligned.  */
    0,                         /* store.  */
    0,                         /* strd.  */
    0,                         /* stm_1st.  */
    1,                         /* stm_regs_per_insn_1st.  */
    2,                         /* stm_regs_per_insn_subsequent.  */
    0,                         /* storef.  */
    0,                         /* stored.  */
    COSTS_N_INSNS (1),         /* store_unaligned.  */
    COSTS_N_INSNS (4),         /* loadv.  */
    COSTS_N_INSNS (4)          /* storev.  */
  },
  {
    /* FP SFmode */
    {
      COSTS_N_INSNS (10),      /* div.  */
      COSTS_N_INSNS (4),       /* mult.  */
      COSTS_N_INSNS (4),       /* mult_addsub.  */
      COSTS_N_INSNS (4),       /* fma.  */
      COSTS_N_INSNS (4),       /* addsub.  */
      COSTS_N_INSNS (1),       /* fpconst.  */
      COSTS_N_INSNS (1),       /* neg.  */
      COSTS_N_INSNS (1),       /* compare.  */
      COSTS_N_INSNS (2),       /* widen.  */
      COSTS_N_INSNS (2),       /* narrow.  */
      COSTS_N_INSNS (2),       /* toint.  */
      COSTS_N_INSNS (1),       /* fromint.  */
      COSTS_N_INSNS (2)        /* roundint.  */
    },
    /* FP DFmode */
    {
      COSTS_N_INSNS (17),      /* div.  */
      COSTS_N_INSNS (4),       /* mult.  */
      COSTS_N_INSNS (6),       /* mult_addsub.  */
      COSTS_N_INSNS (6),       /* fma.  */
      COSTS_N_INSNS (3),       /* addsub.  */
      COSTS_N_INSNS (1),       /* fpconst.  */
      COSTS_N_INSNS (1),       /* neg.  */
      COSTS_N_INSNS (1),       /* compare.  */
      COSTS_N_INSNS (2),       /* widen.  */
      COSTS_N_INSNS (2),       /* narrow.  */
      COSTS_N_INSNS (2),       /* toint.  */
      COSTS_N_INSNS (1),       /* fromint.  */
      COSTS_N_INSNS (2)        /* roundint.  */
    }
  },
  /* Vector */
  {
    COSTS_N_INSNS (1),  /* alu.  */
    COSTS_N_INSNS (4),  /* mult.  */
    COSTS_N_INSNS (1),  /* movi.  */
    COSTS_N_INSNS (2),  /* dup.  */
    COSTS_N_INSNS (2)   /* extract.  */
  }
};

const struct cpu_cost_table a64fx_extra_costs =
{
  /* ALU */
  {
    0,                 /* arith.  */
    0,                 /* logical.  */
    0,                 /* shift.  */
    0,                 /* shift_reg.  */
    COSTS_N_INSNS (1), /* arith_shift.  */
    COSTS_N_INSNS (1), /* arith_shift_reg.  */
    COSTS_N_INSNS (1), /* log_shift.  */
    COSTS_N_INSNS (1), /* log_shift_reg.  */
    0,                 /* extend.  */
    COSTS_N_INSNS (1), /* extend_arith.  */
    0,                 /* bfi.  */
    0,                 /* bfx.  */
    0,                 /* clz.  */
    0,                 /* rev.  */
    0,                 /* non_exec.  */
    true               /* non_exec_costs_exec.  */
  },
  {
    /* MULT SImode */
    {
      COSTS_N_INSNS (4),       /* simple.  */
      COSTS_N_INSNS (4),       /* flag_setting.  */
      COSTS_N_INSNS (4),       /* extend.  */
      COSTS_N_INSNS (5),       /* add.  */
      COSTS_N_INSNS (5),       /* extend_add.  */
      COSTS_N_INSNS (18)       /* idiv.  */
    },
    /* MULT DImode */
    {
      COSTS_N_INSNS (4),       /* simple.  */
      0,                       /* flag_setting (N/A).  */
      COSTS_N_INSNS (4),       /* extend.  */
      COSTS_N_INSNS (5),       /* add.  */
      COSTS_N_INSNS (5),       /* extend_add.  */
      COSTS_N_INSNS (26)       /* idiv.  */
    }
  },
  /* LD/ST */
  {
    COSTS_N_INSNS (4),         /* load.  */
    COSTS_N_INSNS (4),         /* load_sign_extend.  */
    COSTS_N_INSNS (5),         /* ldrd.  */
    COSTS_N_INSNS (4),         /* ldm_1st.  */
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
    0,                         /* store_unaligned.  */
    COSTS_N_INSNS (1),         /* loadv.  */
    COSTS_N_INSNS (1)          /* storev.  */
  },
  {
    /* FP SFmode */
    {
      COSTS_N_INSNS (6),      /* div.  */
      COSTS_N_INSNS (1),       /* mult.  */
      COSTS_N_INSNS (1),       /* mult_addsub.  */
      COSTS_N_INSNS (2),       /* fma.  */
      COSTS_N_INSNS (1),       /* addsub.  */
      COSTS_N_INSNS (1),       /* fpconst.  */
      COSTS_N_INSNS (1),       /* neg.  */
      COSTS_N_INSNS (1),       /* compare.  */
      COSTS_N_INSNS (2),       /* widen.  */
      COSTS_N_INSNS (2),       /* narrow.  */
      COSTS_N_INSNS (2),       /* toint.  */
      COSTS_N_INSNS (2),       /* fromint.  */
      COSTS_N_INSNS (2)        /* roundint.  */
    },
    /* FP DFmode */
    {
      COSTS_N_INSNS (11),      /* div.  */
      COSTS_N_INSNS (1),       /* mult.  */
      COSTS_N_INSNS (1),       /* mult_addsub.  */
      COSTS_N_INSNS (2),       /* fma.  */
      COSTS_N_INSNS (1),       /* addsub.  */
      COSTS_N_INSNS (1),       /* fpconst.  */
      COSTS_N_INSNS (1),       /* neg.  */
      COSTS_N_INSNS (1),       /* compare.  */
      COSTS_N_INSNS (2),       /* widen.  */
      COSTS_N_INSNS (2),       /* narrow.  */
      COSTS_N_INSNS (2),       /* toint.  */
      COSTS_N_INSNS (2),       /* fromint.  */
      COSTS_N_INSNS (2)        /* roundint.  */
    }
  },
  /* Vector */
  {
    COSTS_N_INSNS (1),  /* alu.  */
    COSTS_N_INSNS (4),  /* mult.  */
    COSTS_N_INSNS (1),  /* movi.  */
    COSTS_N_INSNS (2),  /* dup.  */
    COSTS_N_INSNS (2)   /* extract.  */
  }
};

const struct cpu_cost_table ampere1_extra_costs =
{
  /* ALU */
  {
    0,                 /* arith.  */
    0,                 /* logical.  */
    0,                 /* shift.  */
    COSTS_N_INSNS (1), /* shift_reg.  */
    0,                 /* arith_shift.  */
    COSTS_N_INSNS (1), /* arith_shift_reg.  */
    0,                 /* log_shift.  */
    COSTS_N_INSNS (1), /* log_shift_reg.  */
    0,                 /* extend.  */
    COSTS_N_INSNS (1), /* extend_arith.  */
    0,                 /* bfi.  */
    0,                 /* bfx.  */
    0,                 /* clz.  */
    0,                 /* rev.  */
    0,                 /* non_exec.  */
    true               /* non_exec_costs_exec.  */
  },
  {
    /* MULT SImode */
    {
      COSTS_N_INSNS (3),       /* simple.  */
      COSTS_N_INSNS (3),       /* flag_setting.  */
      COSTS_N_INSNS (3),       /* extend.  */
      COSTS_N_INSNS (4),       /* add.  */
      COSTS_N_INSNS (4),       /* extend_add.  */
      COSTS_N_INSNS (18)       /* idiv.  */
    },
    /* MULT DImode */
    {
      COSTS_N_INSNS (3),       /* simple.  */
      0,                       /* flag_setting (N/A).  */
      COSTS_N_INSNS (3),       /* extend.  */
      COSTS_N_INSNS (4),       /* add.  */
      COSTS_N_INSNS (4),       /* extend_add.  */
      COSTS_N_INSNS (34)       /* idiv.  */
    }
  },
  /* LD/ST */
  {
    COSTS_N_INSNS (4),         /* load.  */
    COSTS_N_INSNS (4),         /* load_sign_extend.  */
    0,                         /* ldrd (n/a).  */
    0,                         /* ldm_1st.  */
    0,                         /* ldm_regs_per_insn_1st.  */
    0,                         /* ldm_regs_per_insn_subsequent.  */
    COSTS_N_INSNS (5),         /* loadf.  */
    COSTS_N_INSNS (5),         /* loadd.  */
    COSTS_N_INSNS (5),         /* load_unaligned.  */
    0,                         /* store.  */
    0,                         /* strd.  */
    0,                         /* stm_1st.  */
    0,                         /* stm_regs_per_insn_1st.  */
    0,                         /* stm_regs_per_insn_subsequent.  */
    COSTS_N_INSNS (2),         /* storef.  */
    COSTS_N_INSNS (2),         /* stored.  */
    COSTS_N_INSNS (2),         /* store_unaligned.  */
    COSTS_N_INSNS (3),         /* loadv.  */
    COSTS_N_INSNS (3)          /* storev.  */
  },
  {
    /* FP SFmode */
    {
      COSTS_N_INSNS (25),      /* div.  */
      COSTS_N_INSNS (4),       /* mult.  */
      COSTS_N_INSNS (4),       /* mult_addsub.  */
      COSTS_N_INSNS (4),       /* fma.  */
      COSTS_N_INSNS (4),       /* addsub.  */
      COSTS_N_INSNS (2),       /* fpconst.  */
      COSTS_N_INSNS (4),       /* neg.  */
      COSTS_N_INSNS (4),       /* compare.  */
      COSTS_N_INSNS (4),       /* widen.  */
      COSTS_N_INSNS (4),       /* narrow.  */
      COSTS_N_INSNS (4),       /* toint.  */
      COSTS_N_INSNS (4),       /* fromint.  */
      COSTS_N_INSNS (4)        /* roundint.  */
    },
    /* FP DFmode */
    {
      COSTS_N_INSNS (34),      /* div.  */
      COSTS_N_INSNS (5),       /* mult.  */
      COSTS_N_INSNS (5),       /* mult_addsub.  */
      COSTS_N_INSNS (5),       /* fma.  */
      COSTS_N_INSNS (5),       /* addsub.  */
      COSTS_N_INSNS (2),       /* fpconst.  */
      COSTS_N_INSNS (5),       /* neg.  */
      COSTS_N_INSNS (5),       /* compare.  */
      COSTS_N_INSNS (5),       /* widen.  */
      COSTS_N_INSNS (5),       /* narrow.  */
      COSTS_N_INSNS (6),       /* toint.  */
      COSTS_N_INSNS (6),       /* fromint.  */
      COSTS_N_INSNS (5)        /* roundint.  */
    }
  },
  /* Vector */
  {
    COSTS_N_INSNS (3),  /* alu.  */
    COSTS_N_INSNS (3),  /* mult.  */
    COSTS_N_INSNS (2),  /* movi.  */
    COSTS_N_INSNS (2),  /* dup.  */
    COSTS_N_INSNS (2)   /* extract.  */
  }
};

const struct cpu_cost_table ampere1a_extra_costs =
{
  /* ALU */
  {
    0,                 /* arith.  */
    0,                 /* logical.  */
    0,                 /* shift.  */
    COSTS_N_INSNS (1), /* shift_reg.  */
    0,                 /* arith_shift.  */
    COSTS_N_INSNS (1), /* arith_shift_reg.  */
    0,                 /* log_shift.  */
    COSTS_N_INSNS (1), /* log_shift_reg.  */
    0,                 /* extend.  */
    COSTS_N_INSNS (1), /* extend_arith.  */
    0,                 /* bfi.  */
    0,                 /* bfx.  */
    0,                 /* clz.  */
    0,                 /* rev.  */
    0,                 /* non_exec.  */
    true               /* non_exec_costs_exec.  */
  },
  {
    /* MULT SImode */
    {
      COSTS_N_INSNS (3),       /* simple.  */
      COSTS_N_INSNS (3),       /* flag_setting.  */
      COSTS_N_INSNS (3),       /* extend.  */
      COSTS_N_INSNS (4),       /* add.  */
      COSTS_N_INSNS (4),       /* extend_add.  */
      COSTS_N_INSNS (19)       /* idiv.  */
    },
    /* MULT DImode */
    {
      COSTS_N_INSNS (3),       /* simple.  */
      0,                       /* flag_setting (N/A).  */
      COSTS_N_INSNS (3),       /* extend.  */
      COSTS_N_INSNS (4),       /* add.  */
      COSTS_N_INSNS (4),       /* extend_add.  */
      COSTS_N_INSNS (35)       /* idiv.  */
    }
  },
  /* LD/ST */
  {
    COSTS_N_INSNS (4),         /* load.  */
    COSTS_N_INSNS (4),         /* load_sign_extend.  */
    0,                         /* ldrd (n/a).  */
    0,                         /* ldm_1st.  */
    0,                         /* ldm_regs_per_insn_1st.  */
    0,                         /* ldm_regs_per_insn_subsequent.  */
    COSTS_N_INSNS (5),         /* loadf.  */
    COSTS_N_INSNS (5),         /* loadd.  */
    COSTS_N_INSNS (5),         /* load_unaligned.  */
    0,                         /* store.  */
    0,                         /* strd.  */
    0,                         /* stm_1st.  */
    0,                         /* stm_regs_per_insn_1st.  */
    0,                         /* stm_regs_per_insn_subsequent.  */
    COSTS_N_INSNS (2),         /* storef.  */
    COSTS_N_INSNS (2),         /* stored.  */
    COSTS_N_INSNS (2),         /* store_unaligned.  */
    COSTS_N_INSNS (3),         /* loadv.  */
    COSTS_N_INSNS (3)          /* storev.  */
  },
  {
    /* FP SFmode */
    {
      COSTS_N_INSNS (25),      /* div.  */
      COSTS_N_INSNS (4),       /* mult.  */
      COSTS_N_INSNS (4),       /* mult_addsub.  */
      COSTS_N_INSNS (4),       /* fma.  */
      COSTS_N_INSNS (4),       /* addsub.  */
      COSTS_N_INSNS (2),       /* fpconst.  */
      COSTS_N_INSNS (4),       /* neg.  */
      COSTS_N_INSNS (4),       /* compare.  */
      COSTS_N_INSNS (4),       /* widen.  */
      COSTS_N_INSNS (4),       /* narrow.  */
      COSTS_N_INSNS (4),       /* toint.  */
      COSTS_N_INSNS (4),       /* fromint.  */
      COSTS_N_INSNS (4)        /* roundint.  */
    },
    /* FP DFmode */
    {
      COSTS_N_INSNS (34),      /* div.  */
      COSTS_N_INSNS (5),       /* mult.  */
      COSTS_N_INSNS (5),       /* mult_addsub.  */
      COSTS_N_INSNS (5),       /* fma.  */
      COSTS_N_INSNS (5),       /* addsub.  */
      COSTS_N_INSNS (2),       /* fpconst.  */
      COSTS_N_INSNS (5),       /* neg.  */
      COSTS_N_INSNS (5),       /* compare.  */
      COSTS_N_INSNS (5),       /* widen.  */
      COSTS_N_INSNS (5),       /* narrow.  */
      COSTS_N_INSNS (6),       /* toint.  */
      COSTS_N_INSNS (6),       /* fromint.  */
      COSTS_N_INSNS (5)        /* roundint.  */
    }
  },
  /* Vector */
  {
    COSTS_N_INSNS (3),  /* alu.  */
    COSTS_N_INSNS (3),  /* mult.  */
    COSTS_N_INSNS (2),  /* movi.  */
    COSTS_N_INSNS (2),  /* dup.  */
    COSTS_N_INSNS (2)   /* extract.  */
  }
};

const struct cpu_cost_table ampere1b_extra_costs =
{
  /* ALU */
  {
    0,                 /* arith.  */
    0,                 /* logical.  */
    0,                 /* shift.  */
    COSTS_N_INSNS (1), /* shift_reg.  */
    0,                 /* arith_shift.  */
    COSTS_N_INSNS (1), /* arith_shift_reg.  */
    0,                 /* log_shift.  */
    COSTS_N_INSNS (1), /* log_shift_reg.  */
    0,                 /* extend.  */
    COSTS_N_INSNS (1), /* extend_arith.  */
    0,                 /* bfi.  */
    0,                 /* bfx.  */
    0,                 /* clz.  */
    0,                 /* rev.  */
    0,                 /* non_exec.  */
    true               /* non_exec_costs_exec.  */
  },
  {
    /* MULT SImode */
    {
      COSTS_N_INSNS (2),       /* simple.  */
      COSTS_N_INSNS (2),       /* flag_setting.  */
      COSTS_N_INSNS (2),       /* extend.  */
      COSTS_N_INSNS (3),       /* add.  */
      COSTS_N_INSNS (3),       /* extend_add.  */
      COSTS_N_INSNS (12)       /* idiv.  */
    },
    /* MULT DImode */
    {
      COSTS_N_INSNS (2),       /* simple.  */
      0,                       /* flag_setting (N/A).  */
      COSTS_N_INSNS (2),       /* extend.  */
      COSTS_N_INSNS (3),       /* add.  */
      COSTS_N_INSNS (3),       /* extend_add.  */
      COSTS_N_INSNS (18)       /* idiv.  */
    }
  },
  /* LD/ST */
  {
    COSTS_N_INSNS (2),         /* load.  */
    COSTS_N_INSNS (2),         /* load_sign_extend.  */
    0,                         /* ldrd (n/a).  */
    0,                         /* ldm_1st.  */
    0,                         /* ldm_regs_per_insn_1st.  */
    0,                         /* ldm_regs_per_insn_subsequent.  */
    COSTS_N_INSNS (3),         /* loadf.  */
    COSTS_N_INSNS (3),         /* loadd.  */
    COSTS_N_INSNS (3),         /* load_unaligned.  */
    0,                         /* store.  */
    0,                         /* strd.  */
    0,                         /* stm_1st.  */
    0,                         /* stm_regs_per_insn_1st.  */
    0,                         /* stm_regs_per_insn_subsequent.  */
    COSTS_N_INSNS (1),         /* storef.  */
    COSTS_N_INSNS (1),         /* stored.  */
    COSTS_N_INSNS (1),         /* store_unaligned.  */
    COSTS_N_INSNS (3),         /* loadv.  */
    COSTS_N_INSNS (3)          /* storev.  */
  },
  {
    /* FP SFmode */
    {
      COSTS_N_INSNS (18),      /* div.  */
      COSTS_N_INSNS (3),       /* mult.  */
      COSTS_N_INSNS (3),       /* mult_addsub.  */
      COSTS_N_INSNS (3),       /* fma.  */
      COSTS_N_INSNS (2),       /* addsub.  */
      COSTS_N_INSNS (1),       /* fpconst.  */
      COSTS_N_INSNS (2),       /* neg.  */
      COSTS_N_INSNS (2),       /* compare.  */
      COSTS_N_INSNS (2),       /* widen.  */
      COSTS_N_INSNS (2),       /* narrow.  */
      COSTS_N_INSNS (6),       /* toint.  */
      COSTS_N_INSNS (4),       /* fromint.  */
      COSTS_N_INSNS (2)        /* roundint.  */
    },
    /* FP DFmode */
    {
      COSTS_N_INSNS (18),      /* div.  */
      COSTS_N_INSNS (3),       /* mult.  */
      COSTS_N_INSNS (3),       /* mult_addsub.  */
      COSTS_N_INSNS (3),       /* fma.  */
      COSTS_N_INSNS (2),       /* addsub.  */
      COSTS_N_INSNS (1),       /* fpconst.  */
      COSTS_N_INSNS (2),       /* neg.  */
      COSTS_N_INSNS (2),       /* compare.  */
      COSTS_N_INSNS (2),       /* widen.  */
      COSTS_N_INSNS (2),       /* narrow.  */
      COSTS_N_INSNS (6),       /* toint.  */
      COSTS_N_INSNS (4),       /* fromint.  */
      COSTS_N_INSNS (2)        /* roundint.  */
    }
  },
  /* Vector */
  {
    COSTS_N_INSNS (1),  /* alu.  */
    COSTS_N_INSNS (2),  /* mult.  */
    COSTS_N_INSNS (1),  /* movi.  */
    COSTS_N_INSNS (1),  /* dup.  */
    COSTS_N_INSNS (1)   /* extract.  */
  }
};

#endif
