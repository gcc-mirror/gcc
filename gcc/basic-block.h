/* Define control and data flow tables, and regsets.
   Copyright (C) 1987, 1997 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


/* Number of bits in each actual element of a regset.  We get slightly
   better code for reg%bits and reg/bits if bits is unsigned, assuming
   it is a power of 2.  */

#define REGSET_ELT_BITS ((unsigned) HOST_BITS_PER_WIDE_INT)

/* Type to use for a regset element.  Note that lots of code assumes
   that the initial part of a regset that contains information on the
   hard registers is the same format as a HARD_REG_SET.  */

#define REGSET_ELT_TYPE unsigned HOST_WIDE_INT

/* Define the type for a pointer to a set with a bit for each
   (hard or pseudo) register.  */

typedef REGSET_ELT_TYPE *regset;

/* Size of a regset for the current function,
   in (1) bytes and (2) elements.  */

extern int regset_bytes;
extern int regset_size;

/* clear a register set */
#define CLEAR_REG_SET(TO)						\
do { register REGSET_ELT_TYPE *scan_tp_ = (TO);				\
     register int i_;							\
     for (i_ = 0; i_ < regset_size; i_++)				\
       *scan_tp_++ = 0; } while (0)

/* copy a register to another register */
#define COPY_REG_SET(TO, FROM)						\
do { register REGSET_ELT_TYPE *scan_tp_ = (TO), *scan_fp_ = (FROM);	\
     register int i_;							\
     for (i_ = 0; i_ < regset_size; i_++)				\
       *scan_tp_++ = *scan_fp_++; } while (0)

/* complent a register set, storing it in a second register set.  */
#define COMPL_REG_SET(TO, FROM)						\
do { register REGSET_ELT_TYPE *scan_tp_ = (TO), *scan_fp_ = (FROM);	\
     register int i_;							\
     for (i_ = 0; i_ < regset_size; i_++)				\
       *scan_tp_++ = ~ *scan_fp_++; } while (0)

/* and a register set with a second register set.  */
#define AND_REG_SET(TO, FROM)						\
do { register REGSET_ELT_TYPE *scan_tp_ = (TO), *scan_fp_ = (FROM);	\
     register int i_;							\
     for (i_ = 0; i_ < regset_size; i_++)				\
       *scan_tp_++ &= *scan_fp_++; } while (0)

/* and the complement of a register set to a register set.  */
#define AND_COMPL_REG_SET(TO, FROM)					\
do { register REGSET_ELT_TYPE *scan_tp_ = (TO), *scan_fp_ = (FROM);	\
     register int i_;							\
     for (i_ = 0; i_ < regset_size; i_++)				\
       *scan_tp_++ &= ~ *scan_fp_++; } while (0)

/* inclusive or a register set with a second register set.  */
#define IOR_REG_SET(TO, FROM)						\
do { register REGSET_ELT_TYPE *scan_tp_ = (TO), *scan_fp_ = (FROM);	\
     register int i_;							\
     for (i_ = 0; i_ < regset_size; i_++)				\
       *scan_tp_++ |= *scan_fp_++; } while (0)

/* complement two register sets and or in the result into a third.  */
#define IOR_AND_COMPL_REG_SET(TO, FROM1, FROM2)				\
do { register REGSET_ELT_TYPE *scan_tp_ = (TO);				\
     register REGSET_ELT_TYPE *scan_fp1_ = (FROM1);			\
     register REGSET_ELT_TYPE *scan_fp2_ = (FROM2);			\
     register int i_;							\
     for (i_ = 0; i_ < regset_size; i_++)				\
       *scan_tp_++ |= *scan_fp1_++ & ~ *scan_fp2_++; } while (0)

/* Clear a single register in a register set.  */
#define CLEAR_REGNO_REG_SET(TO, REG)					\
do {									\
  register REGSET_ELT_TYPE *tp_ = (TO);					\
  tp_[ (REG) / REGSET_ELT_BITS ]					\
    &= ~ ((REGSET_ELT_TYPE) 1 << ((REG) % REGSET_ELT_BITS)); } while (0);

/* Set a single register in a register set.  */
#define SET_REGNO_REG_SET(TO, REG)					\
do {									\
  register REGSET_ELT_TYPE *tp_ = (TO);					\
  tp_[ (REG) / REGSET_ELT_BITS ]					\
    |= ((REGSET_ELT_TYPE) 1 << ((REG) % REGSET_ELT_BITS)); } while (0);

/* Return true if a register is set in a register set.  */
#define REGNO_REG_SET_P(TO, REG)					\
 (((TO)[ (REG) / REGSET_ELT_BITS ]					\
   & (((REGSET_ELT_TYPE)1) << (REG) % REGSET_ELT_BITS)) != 0)

/* Copy the hard registers in a register set to the hard register set.  */
#define REG_SET_TO_HARD_REG_SET(TO, FROM)				\
do {									\
  int i_;								\
  CLEAR_HARD_REG_SET (TO);						\
  for (i_ = 0; i_ < FIRST_PSEUDO_REGISTER; i_++)			\
    if (REGNO_REG_SET_P (FROM, i_))					\
      SET_HARD_REG_BIT (TO, i_);					\
} while (0)

/* Loop over all registers in REGSET, starting with MIN, setting REGNUM to the
   register number and executing CODE for all registers that are set. */
#define EXECUTE_IF_SET_IN_REG_SET(REGSET, MIN, REGNUM, CODE)		\
do {									\
  register int i_ = (MIN) / REGSET_ELT_BITS;				\
  register int shift_ = (MIN) % REGSET_ELT_BITS;			\
  register REGSET_ELT_TYPE *scan_rs_ = (REGSET) + i_;			\
  for ( ; i_ < regset_size; i_++)					\
    {									\
      REGSET_ELT_TYPE word_ = *scan_rs_++;				\
      if (word_)							\
	{								\
	  REGSET_ELT_TYPE j_;						\
	  REGNUM = (i_ * REGSET_ELT_BITS) + shift_;			\
	  for (j_ = ((REGSET_ELT_TYPE)1) << shift_;			\
	       j_ != 0;							\
	       (j_ <<= 1), REGNUM++)					\
	    {								\
	      if (word_ & j_)						\
		{							\
		  CODE;							\
		  word_ &= ~ j_;					\
		  if (!word_)						\
		    break;						\
		}							\
	    }								\
	}								\
      shift_ = 0;							\
    }									\
} while (0)

/* Like EXECUTE_IF_SET_IN_REG_SET, but also clear the register set.  */
#define EXECUTE_IF_SET_AND_RESET_IN_REG_SET(REGSET, MIN, REGNUM, CODE)	\
do {									\
  register int i_ = (MIN) / REGSET_ELT_BITS;				\
  register int shift_ = (MIN) % REGSET_ELT_BITS;			\
  register REGSET_ELT_TYPE *scan_rs_ = (REGSET) + i_;			\
  for ( ; i_ < regset_size; i_++)					\
    {									\
      REGSET_ELT_TYPE word_ = *scan_rs_++;				\
      if (word_)							\
	{								\
	  REGSET_ELT_TYPE j_;						\
	  REGNUM = (i_ * REGSET_ELT_BITS) + shift_;			\
	  scan_rs_[-1] = 0;						\
	  for (j_ = ((REGSET_ELT_TYPE)1) << shift_;			\
	       j_ != 0;							\
	       (j_ <<= 1), REGNUM++)					\
	    {								\
	      if (word_ & j_)						\
		{							\
		  CODE;							\
		  word_ &= ~ j_;					\
		  if (!word_)						\
		    break;						\
		}							\
	    }								\
	}								\
      shift_ = 0;							\
    }									\
} while (0)

/* Loop over all registers in REGSET1 and REGSET2, starting with MIN, setting
   REGNUM to the register number and executing CODE for all registers that are
   set in both regsets. */
#define EXECUTE_IF_AND_IN_REG_SET(REGSET1, REGSET2, MIN, REGNUM, CODE)	\
do {									\
  register int i_ = (MIN) / REGSET_ELT_BITS;				\
  register int shift_ = (MIN) % REGSET_ELT_BITS;			\
  register REGSET_ELT_TYPE *scan_rs1_ = (REGSET1) + i_;			\
  register REGSET_ELT_TYPE *scan_rs2_ = (REGSET2) + i_;			\
  for ( ; i_ < regset_size; i_++)					\
    {									\
      REGSET_ELT_TYPE word_ = *scan_rs1_++ & *scan_rs2_++;		\
      if (word_)							\
	{								\
	  REGSET_ELT_TYPE j_;						\
	  REGNUM = (i_ * REGSET_ELT_BITS) + shift_;			\
	  for (j_ = ((REGSET_ELT_TYPE)1) << shift_;			\
	       j_ != 0;							\
	       (j_ <<= 1), REGNUM++)					\
	    {								\
	      if (word_ & j_)						\
		{							\
		  CODE;							\
		  word_ &= ~ j_;					\
		  if (!word_)						\
		    break;						\
		}							\
	    }								\
	}								\
      shift_ = 0;							\
    }									\
} while (0)

/* Loop over all registers in REGSET1 and REGSET2, starting with MIN, setting
   REGNUM to the register number and executing CODE for all registers that are
   set in the first regset and not set in the second. */
#define EXECUTE_IF_AND_COMPL_IN_REG_SET(REGSET1, REGSET2, MIN, REGNUM, CODE) \
do {									\
  register REGSET_ELT_TYPE *scan_rs1_ = (REGSET1);			\
  register REGSET_ELT_TYPE *scan_rs2_ = (REGSET2);			\
  register int i_;							\
  register int shift_ = (MIN) % REGSET_ELT_BITS;			\
  for (i_ = (MIN) / REGSET_ELT_BITS; i_ < regset_size; i_++)		\
    {									\
      REGSET_ELT_TYPE word_ = *scan_rs1_++ & ~ *scan_rs2_++;		\
      if (word_)							\
	{								\
	  REGSET_ELT_TYPE j_;						\
	  REGNUM = (i_ * REGSET_ELT_BITS) + shift_;			\
	  for (j_ = ((REGSET_ELT_TYPE)1) << shift_;			\
	       j_ != 0;							\
	       (j_ <<= 1), REGNUM++)					\
	    {								\
	      if (word_ & j_)						\
		{							\
		  CODE;							\
		  word_ &= ~ j_;					\
		  if (!word_)						\
		    break;						\
		}							\
	    }								\
	}								\
      shift_ = 0;							\
    }									\
} while (0)

/* Allocate a register set with oballoc.  */
#define OBSTACK_ALLOC_REG_SET(OBSTACK)					\
  ((regset) obstack_alloc (OBSTACK, regset_bytes))

/* Allocate a register set with alloca.  */
#define ALLOCA_REG_SET() ((regset) alloca (regset_bytes))

/* Number of basic blocks in the current function.  */

extern int n_basic_blocks;

/* Index by basic block number, get first insn in the block.  */

extern rtx *basic_block_head;

/* Index by basic block number, get last insn in the block.  */

extern rtx *basic_block_end;

/* Index by basic block number, get address of regset
   describing the registers live at the start of that block.  */

extern regset *basic_block_live_at_start;

/* Indexed by n, gives number of basic block that  (REG n) is used in.
   If the value is REG_BLOCK_GLOBAL (-2),
   it means (REG n) is used in more than one basic block.
   REG_BLOCK_UNKNOWN (-1) means it hasn't been seen yet so we don't know.
   This information remains valid for the rest of the compilation
   of the current function; it is used to control register allocation.  */

#define REG_BLOCK_UNKNOWN -1
#define REG_BLOCK_GLOBAL -2

#define REG_BASIC_BLOCK(N) (reg_n_info[(N)].basic_block)
