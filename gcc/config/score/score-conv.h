/* score-conv.h for Sunplus S+CORE processor
   Copyright (C) 2005, 2007, 2009, 2010 Free Software Foundation, Inc.

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

#ifndef GCC_SCORE_CONV_H
#define GCC_SCORE_CONV_H

#define GP_REG_FIRST                    0U
#define GP_REG_LAST                     31U
#define GP_REG_NUM                      (GP_REG_LAST - GP_REG_FIRST + 1U)
#define GP_DBX_FIRST                    0U

#define CE_REG_FIRST                    48U
#define CE_REG_LAST                     49U
#define CE_REG_NUM                      (CE_REG_LAST - CE_REG_FIRST + 1U)

#define ARG_REG_FIRST                   4U
#define ARG_REG_LAST                    7U
#define ARG_REG_NUM                     (ARG_REG_LAST - ARG_REG_FIRST + 1U)

#define REG_CONTAIN(REGNO, FIRST, NUM) \
  ((unsigned int)((int) (REGNO) - (FIRST)) < (NUM))

#define GP_REG_P(REGNO)        REG_CONTAIN (REGNO, GP_REG_FIRST, GP_REG_NUM)

#define G8_REG_P(REGNO)        REG_CONTAIN (REGNO, GP_REG_FIRST, 8)

#define G16_REG_P(REGNO)       REG_CONTAIN (REGNO, GP_REG_FIRST, 16)

#define CE_REG_P(REGNO)        REG_CONTAIN (REGNO, CE_REG_FIRST, CE_REG_NUM)

#define GR_REG_CLASS_P(C)        ((C) == G16_REGS || (C) == G32_REGS)
#define SP_REG_CLASS_P(C) \
  ((C) == CN_REG || (C) == LC_REG || (C) == SC_REG || (C) == SP_REGS)
#define CP_REG_CLASS_P(C) \
  ((C) == CP1_REGS || (C) == CP2_REGS || (C) == CP3_REGS || (C) == CPA_REGS)
#define CE_REG_CLASS_P(C) \
  ((C) == HI_REG || (C) == LO_REG || (C) == CE_REGS)

#define UIMM_IN_RANGE(V, W) \
  ((V) >= 0 \
   && ((unsigned HOST_WIDE_INT) (V) \
       <= (((unsigned HOST_WIDE_INT) 2 << ((W) - 1)) - 1)))

#define SIMM_IN_RANGE(V, W)                            \
  ((V) >= ((HOST_WIDE_INT) -1 << ((W) - 1))      \
   && (V) <= (((HOST_WIDE_INT) 1 << ((W) - 1)) - 1))

#define IMM_IN_RANGE(V, W, S)  \
  ((S) ? SIMM_IN_RANGE (V, W) : UIMM_IN_RANGE (V, W))

#define IMM_IS_POW_OF_2(V, E1, E2)                 \
  ((V) >= ((unsigned HOST_WIDE_INT) 1 << (E1))     \
   && (V) <= ((unsigned HOST_WIDE_INT) 1 << (E2))  \
   && ((V) & ((V) - 1)) == 0)

enum score_symbol_type
{
  SYMBOL_GENERAL,
  SYMBOL_SMALL_DATA  /* The symbol refers to something in a small data section  */
};

#endif
