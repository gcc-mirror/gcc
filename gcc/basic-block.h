/* Define control and data flow tables, and regsets.
   Copyright (C) 1987, 1997, 1998 Free Software Foundation, Inc.

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


#include "bitmap.h"

typedef bitmap regset;		/* Head of register set linked list.  */

/* Clear a register set by freeing up the linked list.  */
#define CLEAR_REG_SET(HEAD) bitmap_clear (HEAD)

/* Copy a register set to another register set.  */
#define COPY_REG_SET(TO, FROM) bitmap_copy (TO, FROM)

/* `and' a register set with a second register set.  */
#define AND_REG_SET(TO, FROM) bitmap_operation (TO, TO, FROM, BITMAP_AND)

/* `and' the complement of a register set with a register set.  */
#define AND_COMPL_REG_SET(TO, FROM) \
  bitmap_operation (TO, TO, FROM, BITMAP_AND_COMPL)

/* Inclusive or a register set with a second register set.  */
#define IOR_REG_SET(TO, FROM) bitmap_operation (TO, TO, FROM, BITMAP_IOR)

/* Or into TO the register set FROM1 `and'ed with the complement of FROM2.  */
#define IOR_AND_COMPL_REG_SET(TO, FROM1, FROM2) \
  bitmap_ior_and_compl (TO, FROM1, FROM2)

/* Clear a single register in a register set.  */
#define CLEAR_REGNO_REG_SET(HEAD, REG) bitmap_clear_bit (HEAD, REG)

/* Set a single register in a register set.  */
#define SET_REGNO_REG_SET(HEAD, REG) bitmap_set_bit (HEAD, REG)

/* Return true if a register is set in a register set.  */
#define REGNO_REG_SET_P(TO, REG) bitmap_bit_p (TO, REG)

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
  EXECUTE_IF_SET_IN_BITMAP (REGSET, MIN, REGNUM, CODE)

/* Loop over all registers in REGSET1 and REGSET2, starting with MIN, setting
   REGNUM to the register number and executing CODE for all registers that are
   set in the first regset and not set in the second. */
#define EXECUTE_IF_AND_COMPL_IN_REG_SET(REGSET1, REGSET2, MIN, REGNUM, CODE) \
  EXECUTE_IF_AND_COMPL_IN_BITMAP (REGSET1, REGSET2, MIN, REGNUM, CODE)

/* Loop over all registers in REGSET1 and REGSET2, starting with MIN, setting
   REGNUM to the register number and executing CODE for all registers that are
   set in both regsets. */
#define EXECUTE_IF_AND_IN_REG_SET(REGSET1, REGSET2, MIN, REGNUM, CODE) \
  EXECUTE_IF_AND_IN_BITMAP (REGSET1, REGSET2, MIN, REGNUM, CODE)

/* Allocate a register set with oballoc.  */
#define OBSTACK_ALLOC_REG_SET(OBSTACK) BITMAP_OBSTACK_ALLOC (OBSTACK)

/* Allocate a register set with alloca.  */
#define ALLOCA_REG_SET() BITMAP_ALLOCA ()

/* Do any cleanup needed on a regset when it is no longer used.  */
#define FREE_REG_SET(REGSET) BITMAP_FREE(REGSET)

/* Do any one-time initializations needed for regsets.  */
#define INIT_ONCE_REG_SET() BITMAP_INIT_ONCE ()

/* Grow any tables needed when the number of registers is calculated
   or extended.  For the linked list allocation, nothing needs to
   be done, other than zero the statistics on the first allocation.  */
#define MAX_REGNO_REG_SET(NUM_REGS, NEW_P, RENUMBER_P)

/* Number of basic blocks in the current function.  */

extern int n_basic_blocks;

/* Index by basic block number, get first insn in the block.  */

extern rtx *basic_block_head;

/* Index by basic block number, get last insn in the block.  */

extern rtx *basic_block_end;

/* Index by basic block number, determine whether the block can be reached
   through a computed jump.  */

extern char *basic_block_computed_jump_target;

/* Index by basic block number, get address of regset
   describing the registers live at the start of that block.  */

extern regset *basic_block_live_at_start;

/* What registers are live at the setjmp call.  */

extern regset regs_live_at_setjmp;

/* Indexed by n, gives number of basic block that  (REG n) is used in.
   If the value is REG_BLOCK_GLOBAL (-2),
   it means (REG n) is used in more than one basic block.
   REG_BLOCK_UNKNOWN (-1) means it hasn't been seen yet so we don't know.
   This information remains valid for the rest of the compilation
   of the current function; it is used to control register allocation.  */

#define REG_BLOCK_UNKNOWN -1
#define REG_BLOCK_GLOBAL -2

#define REG_BASIC_BLOCK(N) (VARRAY_REG (reg_n_info, N)->basic_block)

/* List of integers.
   These are used for storing things like predecessors, etc.

   This scheme isn't very space efficient, especially on 64 bit machines.
   The interface is designed so that the implementation can be replaced with
   something more efficient if desirable.  */

typedef struct int_list {
  struct int_list *next;
  int val;
} int_list;

typedef int_list *int_list_ptr;

/* Integer list elements are allocated in blocks to reduce the frequency
   of calls to malloc and to reduce the associated space overhead.  */

typedef struct int_list_block {
  struct int_list_block *next;
  int nodes_left;
#define INT_LIST_NODES_IN_BLK 500
  struct int_list nodes[INT_LIST_NODES_IN_BLK];
} int_list_block;

/* Given a pointer to the list, return pointer to first element.  */
#define INT_LIST_FIRST(il) (il)

/* Given a pointer to a list element, return pointer to next element.  */
#define INT_LIST_NEXT(p) ((p)->next)

/* Return non-zero if P points to the end of the list.  */
#define INT_LIST_END(p) ((p) == NULL)

/* Return element pointed to by P.  */
#define INT_LIST_VAL(p) ((p)->val)

#define INT_LIST_SET_VAL(p, new_val) ((p)->val = (new_val))

extern void free_int_list               PROTO ((int_list_block **));

/* Stuff for recording basic block info.  */

#define BLOCK_HEAD(B)      basic_block_head[(B)]
#define BLOCK_END(B)       basic_block_end[(B)]

/* Special block numbers [markers] for entry and exit.  */
#define ENTRY_BLOCK (-1)
#define EXIT_BLOCK (-2)

/* from flow.c */
extern void free_regset_vector PROTO ((regset *, int nelts));
extern int *uid_block_number;
#define BLOCK_NUM(INSN)    uid_block_number[INSN_UID (INSN)]

extern void compute_preds_succs PROTO ((int_list_ptr *, int_list_ptr *,
				        int *, int *));
extern void dump_bb_data       PROTO ((FILE *, int_list_ptr *, int_list_ptr *,
				       int));
extern void free_bb_mem        PROTO ((void));
extern void free_basic_block_vars	PROTO ((int));


/* Simple bitmaps.
   It's not clear yet whether using bitmap.[ch] will be a win.
   It should be straightforward to convert so for now we keep things simple
   while more important issues are dealt with.  */

#define SBITMAP_ELT_BITS HOST_BITS_PER_WIDE_INT
#define SBITMAP_ELT_TYPE unsigned HOST_WIDE_INT

typedef struct simple_bitmap_def {
  /* Number of bits.  */
  int n_bits;
  /* Size in elements.  */
  int size;
  /* Size in bytes.  */
  int bytes;
  /* The elements.  */
  SBITMAP_ELT_TYPE elms[1];
} *sbitmap;

typedef SBITMAP_ELT_TYPE *sbitmap_ptr;

/* Return the set size needed for N elements.  */
#define SBITMAP_SET_SIZE(n) (((n) + SBITMAP_ELT_BITS - 1) / SBITMAP_ELT_BITS)

/* set bit number bitno in the bitmap */
#define SET_BIT(bitmap, bitno) \
do { \
  (bitmap)->elms [(bitno) / SBITMAP_ELT_BITS] |= (SBITMAP_ELT_TYPE) 1 << (bitno) % SBITMAP_ELT_BITS; \
} while (0)

/* test if bit number bitno in the bitmap is set */
#define TEST_BIT(bitmap, bitno) \
((bitmap)->elms [(bitno) / SBITMAP_ELT_BITS] & ((SBITMAP_ELT_TYPE) 1 << (bitno) % SBITMAP_ELT_BITS))

/* reset bit number bitno in the bitmap  */
#define RESET_BIT(bitmap, bitno) \
do { \
  (bitmap)->elms [(bitno) / SBITMAP_ELT_BITS] &= ~((SBITMAP_ELT_TYPE) 1 << (bitno) % SBITMAP_ELT_BITS); \
} while (0)

/* Loop over all elements of SBITSET, starting with MIN.  */
#define EXECUTE_IF_SET_IN_SBITMAP(SBITMAP, MIN, N, CODE)		\
do {									\
  unsigned int bit_num_ = (MIN) % (unsigned) SBITMAP_ELT_BITS;		\
  unsigned int word_num_ = (MIN) / (unsigned) SBITMAP_ELT_BITS;		\
  unsigned int size_ = (SBITMAP)->size;					\
  SBITMAP_ELT_TYPE *ptr_ = (SBITMAP)->elms;				\
									\
  while (word_num_ < size_)						\
    {									\
      SBITMAP_ELT_TYPE word_ = ptr_[word_num_];				\
      if (word_ != 0)							\
	{								\
	  for (; bit_num_ < SBITMAP_ELT_BITS; ++bit_num_)		\
	    {								\
	      SBITMAP_ELT_TYPE mask_ = (SBITMAP_ELT_TYPE)1 << bit_num_;	\
	      if ((word_ & mask_) != 0)					\
		{							\
		  word_ &= ~mask_;					\
		  (N) = word_num_ * SBITMAP_ELT_BITS + bit_num_;	\
		  CODE;							\
		  if (word_ == 0)					\
		    break;						\
		}							\
	    }								\
	}								\
      bit_num_ = 0;							\
      word_num_++;							\
   }									\
} while (0)

#define sbitmap_free(map)		free(map)
#define sbitmap_vector_free(vec)	free(vec)

extern void dump_sbitmap PROTO ((FILE *, sbitmap));
extern void dump_sbitmap_vector PROTO ((FILE *, char *, char *,
					sbitmap *, int));
extern sbitmap sbitmap_alloc PROTO ((int));
extern sbitmap *sbitmap_vector_alloc PROTO ((int, int));
extern void sbitmap_copy PROTO ((sbitmap, sbitmap));
extern void sbitmap_zero PROTO ((sbitmap));
extern void sbitmap_ones PROTO ((sbitmap));
extern void sbitmap_vector_zero PROTO ((sbitmap *, int));
extern void sbitmap_vector_ones PROTO ((sbitmap *, int));
extern int sbitmap_union_of_diff PROTO ((sbitmap, sbitmap, sbitmap, sbitmap));
extern void sbitmap_difference PROTO ((sbitmap, sbitmap, sbitmap));
extern void sbitmap_not PROTO ((sbitmap, sbitmap));
extern int sbitmap_a_or_b_and_c PROTO ((sbitmap, sbitmap, sbitmap, sbitmap));
extern int sbitmap_a_and_b_or_c PROTO ((sbitmap, sbitmap, sbitmap, sbitmap));
extern int sbitmap_a_and_b PROTO ((sbitmap, sbitmap, sbitmap));
extern int sbitmap_a_or_b PROTO ((sbitmap, sbitmap, sbitmap));
extern void sbitmap_intersect_of_predsucc PROTO ((sbitmap, sbitmap *,
						  int, int_list_ptr *));
extern void sbitmap_intersect_of_predecessors PROTO ((sbitmap, sbitmap *, int,
						      int_list_ptr *));
extern void sbitmap_intersect_of_successors PROTO ((sbitmap, sbitmap *, int,
						    int_list_ptr *));
extern void sbitmap_union_of_predecessors PROTO ((sbitmap, sbitmap *, int,
						  int_list_ptr *));
extern void sbitmap_union_of_successors PROTO ((sbitmap, sbitmap *, int,
						int_list_ptr *));
extern void compute_dominators PROTO ((sbitmap *, sbitmap *,
				       int_list_ptr *, int_list_ptr *));
