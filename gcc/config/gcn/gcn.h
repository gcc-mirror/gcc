/* Copyright (C) 2016-2020 Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3 of the License, or (at your option)
   any later version.

   This file is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config/gcn/gcn-opts.h"

#define TARGET_CPU_CPP_BUILTINS()	\
  do					\
    {					\
      builtin_define ("__AMDGCN__");	\
      if (TARGET_GCN3)			\
	builtin_define ("__GCN3__");	\
      else if (TARGET_GCN5)		\
	builtin_define ("__GCN5__");	\
    }					\
  while(0)

/* Support for a compile-time default architecture and tuning.
   The rules are:
   --with-arch is ignored if -march is specified.
   --with-tune is ignored if -mtune is specified.  */
#define OPTION_DEFAULT_SPECS		    \
  {"arch", "%{!march=*:-march=%(VALUE)}" }, \
  {"tune", "%{!mtune=*:-mtune=%(VALUE)}" }

/* Default target_flags if no switches specified.  */
#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT 0
#endif


/* Storage Layout */
#define BITS_BIG_ENDIAN  0
#define BYTES_BIG_ENDIAN 0
#define WORDS_BIG_ENDIAN 0

#define BITS_PER_WORD 32
#define UNITS_PER_WORD (BITS_PER_WORD/BITS_PER_UNIT)
#define LIBGCC2_UNITS_PER_WORD 4

#define POINTER_SIZE	     64
#define PARM_BOUNDARY	     64
#define STACK_BOUNDARY	     64
#define FUNCTION_BOUNDARY    32
#define BIGGEST_ALIGNMENT    64
#define EMPTY_FIELD_BOUNDARY 32
#define MAX_FIXED_MODE_SIZE  64
#define MAX_REGS_PER_ADDRESS 2
#define STACK_SIZE_MODE      DImode
#define Pmode		     DImode
#define CASE_VECTOR_MODE     DImode
#define FUNCTION_MODE	     QImode

#define DATA_ALIGNMENT(TYPE,ALIGN) ((ALIGN) > 128 ? (ALIGN) : 128)
#define LOCAL_ALIGNMENT(TYPE,ALIGN) ((ALIGN) > 64 ? (ALIGN) : 64)
#define STACK_SLOT_ALIGNMENT(TYPE,MODE,ALIGN) ((ALIGN) > 64 ? (ALIGN) : 64)
#define STRICT_ALIGNMENT 1

/* Type Layout: match what x86_64 does.  */
#define INT_TYPE_SIZE		  32
#define LONG_TYPE_SIZE		  64
#define LONG_LONG_TYPE_SIZE	  64
#define FLOAT_TYPE_SIZE		  32
#define DOUBLE_TYPE_SIZE	  64
#define LONG_DOUBLE_TYPE_SIZE	  64
#define DEFAULT_SIGNED_CHAR	  1
#define PCC_BITFIELD_TYPE_MATTERS 1

/* Frame Layout */
#define FRAME_GROWS_DOWNWARD	     0
#define ARGS_GROW_DOWNWARD	     1
#define STACK_POINTER_OFFSET	     0
#define FIRST_PARM_OFFSET(FNDECL)    0
#define DYNAMIC_CHAIN_ADDRESS(FP)    plus_constant (Pmode, (FP), -16)
#define INCOMING_RETURN_ADDR_RTX     gen_rtx_REG (Pmode, LINK_REGNUM)
#define STACK_DYNAMIC_OFFSET(FNDECL) (-crtl->outgoing_args_size)
#define ACCUMULATE_OUTGOING_ARGS     1
#define RETURN_ADDR_RTX(COUNT,FRAMEADDR) \
  ((COUNT) == 0 ? get_hard_reg_initial_val (Pmode, LINK_REGNUM) : NULL_RTX)

/* Register Basics */
#define FIRST_SGPR_REG	    0
#define SGPR_REGNO(N)	    ((N)+FIRST_SGPR_REG)
#define LAST_SGPR_REG	    101

#define FLAT_SCRATCH_REG    102
#define FLAT_SCRATCH_LO_REG 102
#define FLAT_SCRATCH_HI_REG 103
#define XNACK_MASK_REG	    104
#define XNACK_MASK_LO_REG   104
#define XNACK_MASK_HI_REG   105
#define VCC_LO_REG	    106
#define VCC_HI_REG	    107
#define VCCZ_REG	    108
#define TBA_REG		    109
#define TBA_LO_REG	    109
#define TBA_HI_REG	    110
#define TMA_REG		    111
#define TMA_LO_REG	    111
#define TMA_HI_REG	    112
#define TTMP0_REG	    113
#define TTMP11_REG	    124
#define M0_REG		    125
#define EXEC_REG	    126
#define EXEC_LO_REG	    126
#define EXEC_HI_REG	    127
#define EXECZ_REG	    128
#define SCC_REG		    129
/* 132-159 are reserved to simplify masks.  */
#define FIRST_VGPR_REG	    160
#define VGPR_REGNO(N)	    ((N)+FIRST_VGPR_REG)
#define LAST_VGPR_REG	    415

/* Frame Registers, and other registers */

#define HARD_FRAME_POINTER_REGNUM 14
#define STACK_POINTER_REGNUM	  16
#define LINK_REGNUM		  18
#define EXEC_SAVE_REG		  20
#define CC_SAVE_REG		  22
#define RETURN_VALUE_REG	  24	/* Must be divisible by 4.  */
#define STATIC_CHAIN_REGNUM	  30
#define WORK_ITEM_ID_Z_REG	  162
#define SOFT_ARG_REG		  416
#define FRAME_POINTER_REGNUM	  418
#define FIRST_PSEUDO_REGISTER	  420

#define FIRST_PARM_REG 24
#define NUM_PARM_REGS  6

/* There is no arg pointer.  Just choose random fixed register that does
   not intefere with anything.  */
#define ARG_POINTER_REGNUM SOFT_ARG_REG

#define HARD_FRAME_POINTER_IS_ARG_POINTER   0
#define HARD_FRAME_POINTER_IS_FRAME_POINTER 0

#define SGPR_OR_VGPR_REGNO_P(N) ((N)>=FIRST_VGPR_REG && (N) <= LAST_SGPR_REG)
#define SGPR_REGNO_P(N)		((N) <= LAST_SGPR_REG)
#define VGPR_REGNO_P(N)		((N)>=FIRST_VGPR_REG && (N) <= LAST_VGPR_REG)
#define SSRC_REGNO_P(N)		((N) <= SCC_REG && (N) != VCCZ_REG)
#define SDST_REGNO_P(N)		((N) <= EXEC_HI_REG && (N) != VCCZ_REG)
#define CC_REG_P(X)		(REG_P (X) && CC_REGNO_P (REGNO (X)))
#define CC_REGNO_P(X)		((X) == SCC_REG || (X) == VCC_REG)
#define FUNCTION_ARG_REGNO_P(N) \
  ((N) >= FIRST_PARM_REG && (N) < (FIRST_PARM_REG + NUM_PARM_REGS))


#define FIXED_REGISTERS {			    \
    /* Scalars.  */				    \
    1, 1, 0, 0, 1, 1, 1, 1, 1, 1,		    \
/*		fp    sp    lr.  */		    \
    1, 1, 0, 0, 0, 0, 1, 1, 0, 0,		    \
/*  exec_save, cc_save */			    \
    1, 1, 1, 1, 0, 0, 0, 0, 0, 0,		    \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,		    \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,		    \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,		    \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,		    \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,		    \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,		    \
    0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1,		    \
    /* Special regs and padding.  */		    \
/*  flat  xnack vcc	 tba   tma   ttmp */	    \
    1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
/*			 m0 exec     scc */	    \
    1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1, \
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1,		    \
    /* VGRPs */					    \
    0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    /* Other registers.  */			    \
    1, 1, 1, 1					    \
}

#define CALL_USED_REGISTERS {			    \
    /* Scalars.  */				    \
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 		    \
    1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 		    \
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 		    \
    1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 		    \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 		    \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 		    \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 		    \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 		    \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 		    \
    0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1,		    \
    /* Special regs and padding.  */		    \
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1,		    \
    /* VGRPs */					    \
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    /* Other registers.  */			    \
    1, 1, 1, 1					    \
}


#define HARD_REGNO_RENAME_OK(FROM, TO) \
  gcn_hard_regno_rename_ok (FROM, TO)

#define HARD_REGNO_CALLER_SAVE_MODE(HARDREG, NREGS, MODE) \
  gcn_hard_regno_caller_save_mode ((HARDREG), (NREGS), (MODE))

/* Register Classes */

enum reg_class
{
  NO_REGS,

  /* SCC */
  SCC_CONDITIONAL_REG,

  /* VCCZ */
  VCCZ_CONDITIONAL_REG,

  /* VCC */
  VCC_CONDITIONAL_REG,

  /* EXECZ */
  EXECZ_CONDITIONAL_REG,

  /* SCC VCCZ EXECZ */
  ALL_CONDITIONAL_REGS,

  /* EXEC */
  EXEC_MASK_REG,

  /* SGPR0-101 */
  SGPR_REGS,

  /* SGPR0-101 EXEC_LO/EXEC_HI */
  SGPR_EXEC_REGS,

  /* SGPR0-101, FLAT_SCRATCH_LO/HI, VCC LO/HI, TBA LO/HI, TMA LO/HI, TTMP0-11,
     M0, VCCZ, SCC
     (EXEC_LO/HI, EXECZ excluded to prevent compiler misuse.)  */
  SGPR_VOP_SRC_REGS,

  /* SGPR0-101, FLAT_SCRATCH_LO/HI, XNACK_MASK_LO/HI, VCC LO/HI, TBA LO/HI
     TMA LO/HI, TTMP0-11 */
  SGPR_MEM_SRC_REGS,

  /* SGPR0-101, FLAT_SCRATCH_LO/HI, XNACK_MASK_LO/HI, VCC LO/HI, TBA LO/HI
     TMA LO/HI, TTMP0-11, M0, EXEC LO/HI */
  SGPR_DST_REGS,

  /* SGPR0-101, FLAT_SCRATCH_LO/HI, XNACK_MASK_LO/HI, VCC LO/HI, TBA LO/HI
     TMA LO/HI, TTMP0-11 */
  SGPR_SRC_REGS,
  GENERAL_REGS,
  VGPR_REGS,
  ALL_GPR_REGS,
  SRCDST_REGS,
  AFP_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES

#define REG_CLASS_NAMES     \
{  "NO_REGS",		    \
   "SCC_CONDITIONAL_REG",   \
   "VCCZ_CONDITIONAL_REG",  \
   "VCC_CONDITIONAL_REG",   \
   "EXECZ_CONDITIONAL_REG", \
   "ALL_CONDITIONAL_REGS",  \
   "EXEC_MASK_REG",	    \
   "SGPR_REGS",		    \
   "SGPR_EXEC_REGS",	    \
   "SGPR_VOP3A_SRC_REGS",   \
   "SGPR_MEM_SRC_REGS",     \
   "SGPR_DST_REGS",	    \
   "SGPR_SRC_REGS",	    \
   "GENERAL_REGS",	    \
   "VGPR_REGS",		    \
   "ALL_GPR_REGS",	    \
   "SRCDST_REGS",	    \
   "AFP_REGS",		    \
   "ALL_REGS"		    \
}

#define NAMED_REG_MASK(N)  (1<<((N)-3*32))
#define NAMED_REG_MASK2(N) (1<<((N)-4*32))

#define REG_CLASS_CONTENTS {						   \
    /* NO_REGS.  */							   \
    {0, 0, 0, 0,							   \
     0, 0, 0, 0,							   \
     0, 0, 0, 0, 0, 0},							   \
    /* SCC_CONDITIONAL_REG.  */						   \
    {0, 0, 0, 0,							   \
     NAMED_REG_MASK2 (SCC_REG), 0, 0, 0,				   \
     0, 0, 0, 0, 0},							   \
    /* VCCZ_CONDITIONAL_REG.  */					   \
    {0, 0, 0, NAMED_REG_MASK (VCCZ_REG),				   \
     0, 0, 0, 0,							   \
     0, 0, 0, 0, 0, 0},							   \
    /* VCC_CONDITIONAL_REG.  */						   \
    {0, 0, 0, NAMED_REG_MASK (VCC_LO_REG)|NAMED_REG_MASK (VCC_HI_REG),	   \
     0, 0, 0, 0,							   \
     0, 0, 0, 0, 0, 0},							   \
    /* EXECZ_CONDITIONAL_REG.  */					   \
    {0, 0, 0, 0,							   \
     NAMED_REG_MASK2 (EXECZ_REG), 0, 0, 0,				   \
     0, 0, 0, 0, 0},							   \
    /* ALL_CONDITIONAL_REGS.  */					   \
    {0, 0, 0, NAMED_REG_MASK (VCCZ_REG),				   \
     NAMED_REG_MASK2 (EXECZ_REG) | NAMED_REG_MASK2 (SCC_REG), 0, 0, 0,	   \
     0, 0, 0, 0, 0, 0},							   \
    /* EXEC_MASK_REG.  */						   \
    {0, 0, 0, NAMED_REG_MASK (EXEC_LO_REG) | NAMED_REG_MASK (EXEC_HI_REG), \
     0, 0, 0, 0,							   \
     0, 0, 0, 0, 0, 0},							   \
    /* SGPR_REGS.  */							   \
    {0xffffffff, 0xffffffff, 0xffffffff, 0xf1,				   \
     0, 0, 0, 0,							   \
     0, 0, 0, 0, 0, 0},							   \
    /* SGPR_EXEC_REGS.	*/						   \
    {0xffffffff, 0xffffffff, 0xffffffff,				   \
      0xf1 | NAMED_REG_MASK (EXEC_LO_REG) | NAMED_REG_MASK (EXEC_HI_REG),  \
     0, 0, 0, 0,							   \
     0, 0, 0, 0, 0, 0},							   \
    /* SGPR_VOP_SRC_REGS.  */						   \
    {0xffffffff, 0xffffffff, 0xffffffff,				   \
      0xffffffff							   \
       -NAMED_REG_MASK (EXEC_LO_REG)					   \
       -NAMED_REG_MASK (EXEC_HI_REG),					   \
     NAMED_REG_MASK2 (SCC_REG), 0, 0, 0,				   \
     0, 0, 0, 0, 0, 0},							   \
    /* SGPR_MEM_SRC_REGS.  */						   \
    {0xffffffff, 0xffffffff, 0xffffffff,				   \
     0xffffffff-NAMED_REG_MASK (VCCZ_REG)-NAMED_REG_MASK (M0_REG)	   \
     -NAMED_REG_MASK (EXEC_LO_REG)-NAMED_REG_MASK (EXEC_HI_REG),	   \
     0, 0, 0, 0,							   \
     0, 0, 0, 0, 0, 0},							   \
    /* SGPR_DST_REGS.  */						   \
    {0xffffffff, 0xffffffff, 0xffffffff,				   \
     0xffffffff-NAMED_REG_MASK (VCCZ_REG),				   \
     0, 0, 0, 0,							   \
     0, 0, 0, 0, 0, 0},							   \
    /* SGPR_SRC_REGS.  */						   \
    {0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,			   \
     NAMED_REG_MASK2 (EXECZ_REG) | NAMED_REG_MASK2 (SCC_REG), 0, 0, 0,	   \
     0, 0, 0, 0, 0, 0},							   \
    /* GENERAL_REGS.  */						   \
    {0xffffffff, 0xffffffff, 0xffffffff, 0xf1,				   \
     0, 0, 0, 0,							   \
     0, 0, 0, 0, 0, 0},							   \
    /* VGPR_REGS.  */							   \
    {0, 0, 0, 0,							   \
     0,		 0xffffffff, 0xffffffff, 0xffffffff,			   \
     0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0},	   \
    /* ALL_GPR_REGS.  */						   \
    {0xffffffff, 0xffffffff, 0xffffffff, 0xf1,				   \
     0,		 0xffffffff, 0xffffffff, 0xffffffff,			   \
     0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0},	   \
    /* SRCDST_REGS.  */							   \
    {0xffffffff, 0xffffffff, 0xffffffff,				   \
     0xffffffff-NAMED_REG_MASK (VCCZ_REG),				   \
     0,		 0xffffffff, 0xffffffff, 0xffffffff,			   \
     0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0},	   \
    /* AFP_REGS.  */							   \
    {0, 0, 0, 0,							   \
     0, 0, 0, 0,							   \
     0, 0, 0, 0, 0, 0xf},						   \
    /* ALL_REGS.  */							   \
    {0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,			   \
     0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff,			   \
     0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0 }}

#define REGNO_REG_CLASS(REGNO) gcn_regno_reg_class (REGNO)
#define MODE_CODE_BASE_REG_CLASS(MODE, AS, OUTER, INDEX) \
	 gcn_mode_code_base_reg_class (MODE, AS, OUTER, INDEX)
#define REGNO_MODE_CODE_OK_FOR_BASE_P(NUM, MODE, AS, OUTER, INDEX) \
	 gcn_regno_mode_code_ok_for_base_p (NUM, MODE, AS, OUTER, INDEX)
#define INDEX_REG_CLASS VGPR_REGS
#define REGNO_OK_FOR_INDEX_P(regno) regno_ok_for_index_p (regno)


/* Address spaces.  */
enum gcn_address_spaces
{
  ADDR_SPACE_DEFAULT = 0,
  ADDR_SPACE_FLAT,
  ADDR_SPACE_SCALAR_FLAT,
  ADDR_SPACE_FLAT_SCRATCH,
  ADDR_SPACE_LDS,
  ADDR_SPACE_GDS,
  ADDR_SPACE_SCRATCH,
  ADDR_SPACE_GLOBAL
};
#define REGISTER_TARGET_PRAGMAS() do {                               \
  c_register_addr_space ("__flat", ADDR_SPACE_FLAT);                 \
  c_register_addr_space ("__flat_scratch", ADDR_SPACE_FLAT_SCRATCH); \
  c_register_addr_space ("__scalar_flat", ADDR_SPACE_SCALAR_FLAT);   \
  c_register_addr_space ("__lds", ADDR_SPACE_LDS);                   \
  c_register_addr_space ("__gds", ADDR_SPACE_GDS);                   \
  c_register_addr_space ("__global", ADDR_SPACE_GLOBAL);             \
} while (0);

#define STACK_ADDR_SPACE \
  (TARGET_GCN5_PLUS ? ADDR_SPACE_GLOBAL : ADDR_SPACE_FLAT)
#define DEFAULT_ADDR_SPACE \
  ((cfun && cfun->machine && !cfun->machine->use_flat_addressing) \
   ? ADDR_SPACE_GLOBAL : ADDR_SPACE_FLAT)
#define AS_SCALAR_FLAT_P(AS)   ((AS) == ADDR_SPACE_SCALAR_FLAT)
#define AS_FLAT_SCRATCH_P(AS)  ((AS) == ADDR_SPACE_FLAT_SCRATCH)
#define AS_FLAT_P(AS)	       ((AS) == ADDR_SPACE_FLAT \
				|| ((AS) == ADDR_SPACE_DEFAULT \
				    && DEFAULT_ADDR_SPACE == ADDR_SPACE_FLAT))
#define AS_LDS_P(AS)	       ((AS) == ADDR_SPACE_LDS)
#define AS_GDS_P(AS)	       ((AS) == ADDR_SPACE_GDS)
#define AS_SCRATCH_P(AS)       ((AS) == ADDR_SPACE_SCRATCH)
#define AS_GLOBAL_P(AS)        ((AS) == ADDR_SPACE_GLOBAL \
				|| ((AS) == ADDR_SPACE_DEFAULT \
				    && DEFAULT_ADDR_SPACE == ADDR_SPACE_GLOBAL))
#define AS_ANY_FLAT_P(AS)      (AS_FLAT_SCRATCH_P (AS) || AS_FLAT_P (AS))
#define AS_ANY_DS_P(AS)	       (AS_LDS_P (AS) || AS_GDS_P (AS))


/* Instruction Output */
#define REGISTER_NAMES							    \
   {"s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10",	    \
    "s11", "s12", "s13", "s14", "s15", "s16", "s17", "s18", "s19", "s20",   \
    "s21", "s22", "s23", "s24", "s25", "s26", "s27", "s28", "s29", "s30",   \
    "s31", "s32", "s33", "s34", "s35", "s36", "s37", "s38", "s39", "s40",   \
    "s41", "s42", "s43", "s44", "s45", "s46", "s47", "s48", "s49", "s50",   \
    "s51", "s52", "s53", "s54", "s55", "s56", "s57", "s58", "s59", "s60",   \
    "s61", "s62", "s63", "s64", "s65", "s66", "s67", "s68", "s69", "s70",   \
    "s71", "s72", "s73", "s74", "s75", "s76", "s77", "s78", "s79", "s80",   \
    "s81", "s82", "s83", "s84", "s85", "s86", "s87", "s88", "s89", "s90",   \
    "s91", "s92", "s93", "s94", "s95", "s96", "s97", "s98", "s99",	    \
    "s100", "s101",							    \
    "flat_scratch_lo", "flat_scratch_hi", "xnack_mask_lo", "xnack_mask_hi", \
    "vcc_lo", "vcc_hi", "vccz", "tba_lo", "tba_hi", "tma_lo", "tma_hi",     \
    "ttmp0", "ttmp1", "ttmp2", "ttmp3", "ttmp4", "ttmp5", "ttmp6", "ttmp7", \
    "ttmp8", "ttmp9", "ttmp10", "ttmp11", "m0", "exec_lo", "exec_hi",	    \
    "execz", "scc",							    \
    "res130", "res131", "res132", "res133", "res134", "res135", "res136",   \
    "res137", "res138", "res139", "res140", "res141", "res142", "res143",   \
    "res144", "res145", "res146", "res147", "res148", "res149", "res150",   \
    "res151", "res152", "res153", "res154", "res155", "res156", "res157",   \
    "res158", "res159",							    \
    "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10",	    \
    "v11", "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20",   \
    "v21", "v22", "v23", "v24", "v25", "v26", "v27", "v28", "v29", "v30",   \
    "v31", "v32", "v33", "v34", "v35", "v36", "v37", "v38", "v39", "v40",   \
    "v41", "v42", "v43", "v44", "v45", "v46", "v47", "v48", "v49", "v50",   \
    "v51", "v52", "v53", "v54", "v55", "v56", "v57", "v58", "v59", "v60",   \
    "v61", "v62", "v63", "v64", "v65", "v66", "v67", "v68", "v69", "v70",   \
    "v71", "v72", "v73", "v74", "v75", "v76", "v77", "v78", "v79", "v80",   \
    "v81", "v82", "v83", "v84", "v85", "v86", "v87", "v88", "v89", "v90",   \
    "v91", "v92", "v93", "v94", "v95", "v96", "v97", "v98", "v99", "v100",  \
    "v101", "v102", "v103", "v104", "v105", "v106", "v107", "v108", "v109", \
    "v110", "v111", "v112", "v113", "v114", "v115", "v116", "v117", "v118", \
    "v119", "v120", "v121", "v122", "v123", "v124", "v125", "v126", "v127", \
    "v128", "v129", "v130", "v131", "v132", "v133", "v134", "v135", "v136", \
    "v137", "v138", "v139", "v140", "v141", "v142", "v143", "v144", "v145", \
    "v146", "v147", "v148", "v149", "v150", "v151", "v152", "v153", "v154", \
    "v155", "v156", "v157", "v158", "v159", "v160", "v161", "v162", "v163", \
    "v164", "v165", "v166", "v167", "v168", "v169", "v170", "v171", "v172", \
    "v173", "v174", "v175", "v176", "v177", "v178", "v179", "v180", "v181", \
    "v182", "v183", "v184", "v185", "v186", "v187", "v188", "v189", "v190", \
    "v191", "v192", "v193", "v194", "v195", "v196", "v197", "v198", "v199", \
    "v200", "v201", "v202", "v203", "v204", "v205", "v206", "v207", "v208", \
    "v209", "v210", "v211", "v212", "v213", "v214", "v215", "v216", "v217", \
    "v218", "v219", "v220", "v221", "v222", "v223", "v224", "v225", "v226", \
    "v227", "v228", "v229", "v230", "v231", "v232", "v233", "v234", "v235", \
    "v236", "v237", "v238", "v239", "v240", "v241", "v242", "v243", "v244", \
    "v245", "v246", "v247", "v248", "v249", "v250", "v251", "v252", "v253", \
    "v254", "v255",							    \
    "?ap0", "?ap1", "?fp0", "?fp1" }

#define PRINT_OPERAND(FILE, X, CODE)  print_operand(FILE, X, CODE)
#define PRINT_OPERAND_ADDRESS(FILE, ADDR)  print_operand_address (FILE, ADDR)
#define PRINT_OPERAND_PUNCT_VALID_P(CODE) (CODE == '^')


/* Register Arguments */

#ifndef USED_FOR_TARGET

#define GCN_KERNEL_ARG_TYPES 19
struct GTY(()) gcn_kernel_args
{
  long requested;
  int reg[GCN_KERNEL_ARG_TYPES];
  int order[GCN_KERNEL_ARG_TYPES];
  int nargs, nsgprs;
};

typedef struct gcn_args
{
  /* True if this isn't a kernel (HSA runtime entrypoint).  */
  bool normal_function;
  tree fntype;
  struct gcn_kernel_args args;
  int num;
  int offset;
  int alignment;
} CUMULATIVE_ARGS;
#endif

#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME,FNDECL,N_NAMED_ARGS) \
  gcn_init_cumulative_args (&(CUM), (FNTYPE), (LIBNAME), (FNDECL),   \
			    (N_NAMED_ARGS) != -1)


#ifndef USED_FOR_TARGET

#include "hash-table.h"
#include "hash-map.h"
#include "vec.h"

struct GTY(()) machine_function
{
  struct gcn_kernel_args args;
  int kernarg_segment_alignment;
  int kernarg_segment_byte_size;
  /* Frame layout info for normal functions.  */
  bool normal_function;
  bool need_frame_pointer;
  bool lr_needs_saving;
  HOST_WIDE_INT outgoing_args_size;
  HOST_WIDE_INT pretend_size;
  HOST_WIDE_INT local_vars;
  HOST_WIDE_INT callee_saves;

  unsigned lds_allocated;
  hash_map<tree, int> *lds_allocs;

  vec<tree, va_gc> *reduc_decls;

  bool use_flat_addressing;
};
#endif


/* Codes for all the GCN builtins.  */

enum gcn_builtin_codes
{
#define DEF_BUILTIN(fcode, icode, name, type, params, expander) \
  GCN_BUILTIN_ ## fcode,
#define DEF_BUILTIN_BINOP_INT_FP(fcode, ic, name)	\
  GCN_BUILTIN_ ## fcode ## _V64SI,			\
  GCN_BUILTIN_ ## fcode ## _V64SI_unspec,
#include "gcn-builtins.def"
#undef DEF_BUILTIN
#undef DEF_BUILTIN_BINOP_INT_FP
  GCN_BUILTIN_MAX
};


/* Misc */

/* We can load/store 128-bit quantities, but having this larger than
   MAX_FIXED_MODE_SIZE (which we want to be 64 bits) causes problems.  */
#define MOVE_MAX 8

#define AVOID_CCMODE_COPIES 1
#define SLOW_BYTE_ACCESS 0
#define WORD_REGISTER_OPERATIONS 1

/* Flag values are either BImode or DImode, but either way the compiler
   should assume that all the bits are live.  */
#define STORE_FLAG_VALUE -1

/* Definitions for register eliminations.

   This is an array of structures.  Each structure initializes one pair
   of eliminable registers.  The "from" register number is given first,
   followed by "to".  Eliminations of the same "from" register are listed
   in order of preference.  */

#define ELIMINABLE_REGS					\
{{ ARG_POINTER_REGNUM, STACK_POINTER_REGNUM },		\
 { ARG_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM },	\
 { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM },	\
 { FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM }}

/* Define the offset between two registers, one to be eliminated, and the
   other its replacement, at the start of a routine.  */

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)	\
  ((OFFSET) = gcn_initial_elimination_offset ((FROM), (TO)))


/* Define this macro if it is advisable to hold scalars in registers
   in a wider mode than that declared by the program.  In such cases,
   the value is constrained to be within the bounds of the declared
   type, but kept valid in the wider mode.  The signedness of the
   extension may differ from that of the type.  */

#define PROMOTE_MODE(MODE,UNSIGNEDP,TYPE)			\
  if (GET_MODE_CLASS (MODE) == MODE_INT				\
      && (TYPE == NULL || TREE_CODE (TYPE) != VECTOR_TYPE)	\
      && GET_MODE_SIZE (MODE) < UNITS_PER_WORD)			\
    {								\
      (MODE) = SImode;						\
    }

/* This needs to match gcn_function_value.  */
#define LIBCALL_VALUE(MODE) gen_rtx_REG (MODE, SGPR_REGNO (RETURN_VALUE_REG))

/* The s_ff0 and s_flbit instructions return -1 if no input bits are set.  */
#define CLZ_DEFINED_VALUE_AT_ZERO(MODE, VALUE) ((VALUE) = -1, 2)
#define CTZ_DEFINED_VALUE_AT_ZERO(MODE, VALUE) ((VALUE) = -1, 2)


/* Costs.  */

/* Branches are to be dicouraged when theres an alternative.
   FIXME: This number is plucked from the air.  */
#define BRANCH_COST(SPEED_P, PREDICABLE_P) 10


/* Profiling */
#define FUNCTION_PROFILER(FILE, LABELNO)
#define NO_PROFILE_COUNTERS 1
#define PROFILE_BEFORE_PROLOGUE 0

/* Trampolines */
#define TRAMPOLINE_SIZE 36
#define TRAMPOLINE_ALIGNMENT 64
