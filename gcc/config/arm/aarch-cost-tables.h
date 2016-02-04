/* RTX cost tables shared between arm and aarch64.

   Copyright (C) 2013-2016 Free Software Foundation, Inc.
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
    0,			/* arith.  */
    0,			/* logical.  */
    0,			/* shift.  */
    COSTS_N_INSNS (1),	/* shift_reg.  */
    0,			/* arith_shift.  */
    COSTS_N_INSNS (1),	/* arith_shift_reg.  */
    0,			/* log_shift.  */
    COSTS_N_INSNS (1),	/* log_shift_reg.  */
    0,			/* extend.  */
    COSTS_N_INSNS (1),	/* extend_arith.  */
    0,			/* bfi.  */
    0,			/* bfx.  */
    0,			/* clz.  */
    0,			/* rev.  */
    COSTS_N_INSNS (1),	/* non_exec.  */
    false		/* non_exec_costs_exec.  */
  },
  {
    /* MULT SImode */
    {
      COSTS_N_INSNS (2),	/* simple.  */
      COSTS_N_INSNS (1),	/* flag_setting.  */
      COSTS_N_INSNS (2),	/* extend.  */
      COSTS_N_INSNS (3),	/* add.  */
      COSTS_N_INSNS (3),	/* extend_add.  */
      COSTS_N_INSNS (8)		/* idiv.  */
    },
    /* MULT DImode */
    {
      0,			/* simple (N/A).  */
      0,			/* flag_setting (N/A).  */
      COSTS_N_INSNS (2),	/* extend.  */
      0,			/* add (N/A).  */
      COSTS_N_INSNS (3),	/* extend_add.  */
      0				/* idiv (N/A).  */
    }
  },
  /* LD/ST */
  {
    COSTS_N_INSNS (2),	/* load.  */
    COSTS_N_INSNS (2),	/* load_sign_extend.  */
    COSTS_N_INSNS (3),	/* ldrd.  */
    COSTS_N_INSNS (2),	/* ldm_1st.  */
    1,			/* ldm_regs_per_insn_1st.  */
    1,			/* ldm_regs_per_insn_subsequent.  */
    COSTS_N_INSNS (2),	/* loadf.  */
    COSTS_N_INSNS (3),	/* loadd.  */
    COSTS_N_INSNS (1),  /* load_unaligned.  */
    COSTS_N_INSNS (2),	/* store.  */
    COSTS_N_INSNS (3),	/* strd.  */
    COSTS_N_INSNS (2),	/* stm_1st.  */
    1,			/* stm_regs_per_insn_1st.  */
    1,			/* stm_regs_per_insn_subsequent.  */
    COSTS_N_INSNS (2),	/* storef.  */
    COSTS_N_INSNS (3),	/* stored.  */
    COSTS_N_INSNS (1),	/* store_unaligned.  */
    COSTS_N_INSNS (1),	/* loadv.  */
    COSTS_N_INSNS (1)	/* storev.  */
  },
  {
    /* FP SFmode */
    {
      COSTS_N_INSNS (7),	/* div.  */
      COSTS_N_INSNS (2),	/* mult.  */
      COSTS_N_INSNS (3),	/* mult_addsub.  */
      COSTS_N_INSNS (3),	/* fma.  */
      COSTS_N_INSNS (1),	/* addsub.  */
      0,			/* fpconst.  */
      0,			/* neg.  */
      0,			/* compare.  */
      0,			/* widen.  */
      0,			/* narrow.  */
      0,			/* toint.  */
      0,			/* fromint.  */
      0				/* roundint.  */
    },
    /* FP DFmode */
    {
      COSTS_N_INSNS (15),	/* div.  */
      COSTS_N_INSNS (5),	/* mult.  */
      COSTS_N_INSNS (7),	/* mult_addsub.  */
      COSTS_N_INSNS (7),	/* fma.  */
      COSTS_N_INSNS (3),	/* addsub.  */
      0,			/* fpconst.  */
      0,			/* neg.  */
      0,			/* compare.  */
      0,			/* widen.  */
      0,			/* narrow.  */
      0,			/* toint.  */
      0,			/* fromint.  */
      0				/* roundint.  */
    }
  },
  /* Vector */
  {
    COSTS_N_INSNS (1)	/* alu.  */
  }
};

const struct cpu_cost_table cortexa53_extra_costs =
{
  /* ALU */
  {
    0,			/* arith.  */
    0,			/* logical.  */
    COSTS_N_INSNS (1),	/* shift.  */
    0,			/* shift_reg.  */
    COSTS_N_INSNS (1),	/* arith_shift.  */
    COSTS_N_INSNS (1),	/* arith_shift_reg.  */
    COSTS_N_INSNS (1),	/* log_shift.  */
    COSTS_N_INSNS (1),	/* log_shift_reg.  */
    COSTS_N_INSNS (1),	/* extend.  */
    COSTS_N_INSNS (1),	/* extend_arith.  */
    COSTS_N_INSNS (1),	/* bfi.  */
    COSTS_N_INSNS (1),	/* bfx.  */
    0,			/* clz.  */
    0,			/* rev.  */
    0,			/* non_exec.  */
    true		/* non_exec_costs_exec.  */
  },
  {
    /* MULT SImode */
    {
      COSTS_N_INSNS (1),	/* simple.  */
      COSTS_N_INSNS (2),	/* flag_setting.  */
      COSTS_N_INSNS (1),	/* extend.  */
      COSTS_N_INSNS (1),	/* add.  */
      COSTS_N_INSNS (1),	/* extend_add.  */
      COSTS_N_INSNS (7)		/* idiv.  */
    },
    /* MULT DImode */
    {
      COSTS_N_INSNS (2),	/* simple.  */
      0,			/* flag_setting (N/A).  */
      COSTS_N_INSNS (2),	/* extend.  */
      COSTS_N_INSNS (2),	/* add.  */
      COSTS_N_INSNS (2),	/* extend_add.  */
      COSTS_N_INSNS (15)	/* idiv.  */
    }
  },
  /* LD/ST */
  {
    COSTS_N_INSNS (1),		/* load.  */
    COSTS_N_INSNS (1),		/* load_sign_extend.  */
    COSTS_N_INSNS (1),		/* ldrd.  */
    COSTS_N_INSNS (1),		/* ldm_1st.  */
    1,				/* ldm_regs_per_insn_1st.  */
    2,				/* ldm_regs_per_insn_subsequent.  */
    COSTS_N_INSNS (1),		/* loadf.  */
    COSTS_N_INSNS (1),		/* loadd.  */
    COSTS_N_INSNS (1),		/* load_unaligned.  */
    0,				/* store.  */
    0,				/* strd.  */
    0,				/* stm_1st.  */
    1,				/* stm_regs_per_insn_1st.  */
    2,				/* stm_regs_per_insn_subsequent.  */
    0,				/* storef.  */
    0,				/* stored.  */
    COSTS_N_INSNS (1),		/* store_unaligned.  */
    COSTS_N_INSNS (1),		/* loadv.  */
    COSTS_N_INSNS (1)		/* storev.  */
  },
  {
    /* FP SFmode */
    {
      COSTS_N_INSNS (15),	/* div.  */
      COSTS_N_INSNS (3),	/* mult.  */
      COSTS_N_INSNS (7),	/* mult_addsub. */
      COSTS_N_INSNS (7),	/* fma.  */
      COSTS_N_INSNS (3),	/* addsub.  */
      COSTS_N_INSNS (1),	/* fpconst. */
      COSTS_N_INSNS (2),	/* neg.  */
      COSTS_N_INSNS (1),	/* compare.  */
      COSTS_N_INSNS (3),	/* widen.  */
      COSTS_N_INSNS (3),	/* narrow.  */
      COSTS_N_INSNS (3),	/* toint.  */
      COSTS_N_INSNS (3),	/* fromint.  */
      COSTS_N_INSNS (3)		/* roundint.  */
    },
    /* FP DFmode */
    {
      COSTS_N_INSNS (30),	/* div.  */
      COSTS_N_INSNS (3),	/* mult.  */
      COSTS_N_INSNS (7),	/* mult_addsub.  */
      COSTS_N_INSNS (7),	/* fma.  */
      COSTS_N_INSNS (3),	/* addsub.  */
      COSTS_N_INSNS (1),	/* fpconst.  */
      COSTS_N_INSNS (2),	/* neg.  */
      COSTS_N_INSNS (1),	/* compare.  */
      COSTS_N_INSNS (3),	/* widen.  */
      COSTS_N_INSNS (3),	/* narrow.  */
      COSTS_N_INSNS (3),	/* toint.  */
      COSTS_N_INSNS (3),	/* fromint.  */
      COSTS_N_INSNS (3)		/* roundint.  */
    }
  },
  /* Vector */
  {
    COSTS_N_INSNS (1)	/* alu.  */
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
    0,			/* rev.  */
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
    COSTS_N_INSNS (1),         /* store_unaligned.  */
    COSTS_N_INSNS (1),         /* loadv.  */
    COSTS_N_INSNS (1)          /* storev.  */
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

const struct cpu_cost_table exynosm1_extra_costs =
{
  /* ALU */
  {
    0,                 /* arith.  */
    0,                 /* logical.  */
    0,                 /* shift.  */
    COSTS_N_INSNS (0), /* shift_reg.  */
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
      COSTS_N_INSNS (3),       /* flag_setting.  */
      COSTS_N_INSNS (4),       /* extend.  */
      COSTS_N_INSNS (2),       /* add.  */
      COSTS_N_INSNS (4),       /* extend_add.  */
      COSTS_N_INSNS (19)       /* idiv.  */
    },
    /* MULT DImode */
    {
      COSTS_N_INSNS (3),       /* simple.  */
      0,                       /* flag_setting (N/A).  */
      COSTS_N_INSNS (4),       /* extend.  */
      COSTS_N_INSNS (3),       /* add.  */
      COSTS_N_INSNS (4),       /* extend_add.  */
      COSTS_N_INSNS (35)       /* idiv.  */
    }
  },
  /* LD/ST */
  {
    COSTS_N_INSNS (3),         /* load.  */
    COSTS_N_INSNS (4),         /* load_sign_extend.  */
    COSTS_N_INSNS (3),         /* ldrd.  */
    COSTS_N_INSNS (2),         /* ldm_1st.  */
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
    0,                         /* store_unaligned.  */
    COSTS_N_INSNS (1),         /* loadv.  */
    COSTS_N_INSNS (1)          /* storev.  */
  },
  {
    /* FP SFmode */
    {
      COSTS_N_INSNS (21),      /* div.  */
      COSTS_N_INSNS (3),       /* mult.  */
      COSTS_N_INSNS (4),       /* mult_addsub.  */
      COSTS_N_INSNS (4),       /* fma.  */
      COSTS_N_INSNS (2),       /* addsub.  */
      COSTS_N_INSNS (0),       /* fpconst.  */
      COSTS_N_INSNS (0),       /* neg.  */
      COSTS_N_INSNS (3),       /* compare.  */
      COSTS_N_INSNS (2),       /* widen.  */
      COSTS_N_INSNS (2),       /* narrow.  */
      COSTS_N_INSNS (12),      /* toint.  */
      COSTS_N_INSNS (7),       /* fromint.  */
      COSTS_N_INSNS (2)        /* roundint.  */
    },
    /* FP DFmode */
    {
      COSTS_N_INSNS (34),      /* div.  */
      COSTS_N_INSNS (3),       /* mult.  */
      COSTS_N_INSNS (4),       /* mult_addsub.  */
      COSTS_N_INSNS (4),       /* fma.  */
      COSTS_N_INSNS (2),       /* addsub.  */
      COSTS_N_INSNS (0),       /* fpconst.  */
      COSTS_N_INSNS (0),       /* neg.  */
      COSTS_N_INSNS (3),       /* compare.  */
      COSTS_N_INSNS (2),       /* widen.  */
      COSTS_N_INSNS (2),       /* narrow.  */
      COSTS_N_INSNS (12),      /* toint.  */
      COSTS_N_INSNS (7),       /* fromint.  */
      COSTS_N_INSNS (2)        /* roundint.  */
    }
  },
  /* Vector */
  {
    COSTS_N_INSNS (0)  /* alu.  */
  }
};

const struct cpu_cost_table xgene1_extra_costs =
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
    COSTS_N_INSNS (1), /* extend.  */
    0,                 /* extend_arithm.  */
    COSTS_N_INSNS (1), /* bfi.  */
    COSTS_N_INSNS (1), /* bfx.  */
    0,                 /* clz.  */
    COSTS_N_INSNS (1), /* rev.  */
    0,                 /* non_exec.  */
    true               /* non_exec_costs_exec.  */
  },
  {
    /* MULT SImode */
    {
      COSTS_N_INSNS (4),       /* simple.  */
      COSTS_N_INSNS (4),       /* flag_setting.  */
      COSTS_N_INSNS (4),       /* extend.  */
      COSTS_N_INSNS (4),       /* add.  */
      COSTS_N_INSNS (4),       /* extend_add.  */
      COSTS_N_INSNS (20)       /* idiv.  */
    },
    /* MULT DImode */
    {
      COSTS_N_INSNS (5),       /* simple.  */
      0,                       /* flag_setting (N/A).  */
      COSTS_N_INSNS (5),       /* extend.  */
      COSTS_N_INSNS (5),       /* add.  */
      COSTS_N_INSNS (5),       /* extend_add.  */
      COSTS_N_INSNS (21)       /* idiv.  */
    }
  },
  /* LD/ST */
  {
    COSTS_N_INSNS (5),         /* load.  */
    COSTS_N_INSNS (6),         /* load_sign_extend.  */
    COSTS_N_INSNS (5),         /* ldrd.  */
    COSTS_N_INSNS (5),         /* ldm_1st.  */
    1,                         /* ldm_regs_per_insn_1st.  */
    1,                         /* ldm_regs_per_insn_subsequent.  */
    COSTS_N_INSNS (10),        /* loadf.  */
    COSTS_N_INSNS (10),        /* loadd.  */
    COSTS_N_INSNS (5),         /* load_unaligned.  */
    0,                         /* store.  */
    0,                         /* strd.  */
    0,                         /* stm_1st.  */
    1,                         /* stm_regs_per_insn_1st.  */
    1,                         /* stm_regs_per_insn_subsequent.  */
    0,                         /* storef.  */
    0,                         /* stored.  */
    0,                         /* store_unaligned.  */
    COSTS_N_INSNS (1),         /* loadv.  */
    COSTS_N_INSNS (1)          /* storev.  */
  },
  {
    /* FP SFmode */
    {
      COSTS_N_INSNS (23),      /* div.  */
      COSTS_N_INSNS (5),       /* mult.  */
      COSTS_N_INSNS (5),       /* mult_addsub. */
      COSTS_N_INSNS (5),       /* fma.  */
      COSTS_N_INSNS (5),       /* addsub.  */
      COSTS_N_INSNS (2),       /* fpconst. */
      COSTS_N_INSNS (3),       /* neg.  */
      COSTS_N_INSNS (2),       /* compare.  */
      COSTS_N_INSNS (6),       /* widen.  */
      COSTS_N_INSNS (6),       /* narrow.  */
      COSTS_N_INSNS (4),       /* toint.  */
      COSTS_N_INSNS (4),       /* fromint.  */
      COSTS_N_INSNS (4)        /* roundint.  */
    },
    /* FP DFmode */
    {
      COSTS_N_INSNS (29),      /* div.  */
      COSTS_N_INSNS (5),       /* mult.  */
      COSTS_N_INSNS (5),       /* mult_addsub.  */
      COSTS_N_INSNS (5),       /* fma.  */
      COSTS_N_INSNS (5),       /* addsub.  */
      COSTS_N_INSNS (3),       /* fpconst.  */
      COSTS_N_INSNS (3),       /* neg.  */
      COSTS_N_INSNS (2),       /* compare.  */
      COSTS_N_INSNS (6),       /* widen.  */
      COSTS_N_INSNS (6),       /* narrow.  */
      COSTS_N_INSNS (4),       /* toint.  */
      COSTS_N_INSNS (4),       /* fromint.  */
      COSTS_N_INSNS (4)        /* roundint.  */
    }
  },
  /* Vector */
  {
    COSTS_N_INSNS (2)  /* alu.  */
  }
};

#endif /* GCC_AARCH_COST_TABLES_H */
