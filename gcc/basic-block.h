/* Define control and data flow tables, and regsets.
   Copyright (C) 1987, 1997, 1998, 1999, 2000 Free Software Foundation, Inc.

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

#ifndef _BASIC_BLOCK_H
#define _BASIC_BLOCK_H 1

#include "bitmap.h"
#include "sbitmap.h"
#include "varray.h"

/* Head of register set linked list.  */
typedef bitmap_head regset_head;
/* A pointer to a regset_head.  */
typedef bitmap regset;

/* Initialize a new regset.  */
#define INIT_REG_SET(HEAD) bitmap_initialize (HEAD)

/* Clear a register set by freeing up the linked list.  */
#define CLEAR_REG_SET(HEAD) bitmap_clear (HEAD)

/* Copy a register set to another register set.  */
#define COPY_REG_SET(TO, FROM) bitmap_copy (TO, FROM)

/* Compare two register sets.  */
#define REG_SET_EQUAL_P(A, B) bitmap_equal_p (A, B)

/* `and' a register set with a second register set.  */
#define AND_REG_SET(TO, FROM) bitmap_operation (TO, TO, FROM, BITMAP_AND)

/* `and' the complement of a register set with a register set.  */
#define AND_COMPL_REG_SET(TO, FROM) \
  bitmap_operation (TO, TO, FROM, BITMAP_AND_COMPL)

/* Inclusive or a register set with a second register set.  */
#define IOR_REG_SET(TO, FROM) bitmap_operation (TO, TO, FROM, BITMAP_IOR)

/* Exclusive or a register set with a second register set.  */
#define XOR_REG_SET(TO, FROM) bitmap_operation (TO, TO, FROM, BITMAP_XOR)

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

/* Control flow edge information.  */
typedef struct edge_def {
  /* Links through the predecessor and successor lists.  */
  struct edge_def *pred_next, *succ_next;

  /* The two blocks at the ends of the edge.  */
  struct basic_block_def *src, *dest;

  /* Instructions queued on the edge.  */
  rtx insns;

  /* Auxiliary info specific to a pass.  */
  void *aux;

  int flags;			/* see EDGE_* below  */
  int probability;		/* biased by REG_BR_PROB_BASE */
} *edge;

#define EDGE_FALLTHRU		1
#define EDGE_CRITICAL		2
#define EDGE_ABNORMAL		4
#define EDGE_ABNORMAL_CALL	8
#define EDGE_EH			16
#define EDGE_FAKE		32


/* Basic block information indexed by block number.  */
typedef struct basic_block_def {
  /* The first and last insns of the block.  */
  rtx head, end;

  /* The edges into and out of the block.  */
  edge pred, succ;

  /* Liveness info.  */
  regset local_set;
  regset global_live_at_start;
  regset global_live_at_end;

  /* Auxiliary info specific to a pass.  */
  void *aux;

  /* The index of this block.  */
  int index;
  /* The loop depth of this block plus one.  */
  int loop_depth;

  /* The active eh region before head and after end.  */
  int eh_beg, eh_end;
} *basic_block;

/* Number of basic blocks in the current function.  */

extern int n_basic_blocks;

/* Number of edges in the current function.  */

extern int n_edges;

/* Index by basic block number, get basic block struct info.  */

extern varray_type basic_block_info;

#define BASIC_BLOCK(N)  (VARRAY_BB (basic_block_info, (N)))

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

/* Stuff for recording basic block info.  */

#define BLOCK_HEAD(B)      (BASIC_BLOCK (B)->head)
#define BLOCK_END(B)       (BASIC_BLOCK (B)->end)

/* Special block numbers [markers] for entry and exit.  */
#define ENTRY_BLOCK (-1)
#define EXIT_BLOCK (-2)

/* Similarly, block pointers for the edge list.  */
extern struct basic_block_def entry_exit_blocks[2];
#define ENTRY_BLOCK_PTR	(&entry_exit_blocks[0])
#define EXIT_BLOCK_PTR	(&entry_exit_blocks[1])

extern varray_type basic_block_for_insn;
#define BLOCK_FOR_INSN(INSN)  VARRAY_BB (basic_block_for_insn, INSN_UID (INSN))
#define BLOCK_NUM(INSN)	      (BLOCK_FOR_INSN (INSN)->index + 0)

extern void compute_bb_for_insn		PARAMS ((int));
extern void set_block_for_insn		PARAMS ((rtx, basic_block));
extern void set_block_num		PARAMS ((rtx, int));

extern void free_basic_block_vars	PARAMS ((int));

extern basic_block split_edge		PARAMS ((edge));
extern void insert_insn_on_edge		PARAMS ((rtx, edge));
extern void commit_edge_insertions	PARAMS ((void));
extern void remove_fake_edges		PARAMS ((void));
extern void add_noreturn_fake_exit_edges	PARAMS ((void));
extern void flow_delete_insn_chain	PARAMS ((rtx, rtx));


/* Structure to hold information for each natural loop.  */
struct loop
{
  int num;

  /* Basic block of loop header.  */
  basic_block header;

  /* Basic block of loop latch.  */
  basic_block latch;

  /* Basic block of loop pre-header or NULL if it does not exist.  */
  basic_block pre_header;

  /* The first block in the loop.  This is not necessarily the same as
     the loop header.  */
  basic_block first;

  /* The last block in the loop.  This is not necessarily the same as
     the loop latch.  */
  basic_block last;

  /* Bitmap of blocks contained within the loop.  */
  sbitmap nodes;

  /* Number of blocks contained within the loop.  */
  int num_nodes;

  /* Array of edges that exit the loop.  */
  edge *exits;

  /* Number of edges that exit the loop.  */
  int num_exits;

  /* The loop nesting depth.  */
  int depth;

  /* The height of the loop (enclosed loop levels) within the loop
     hierarchy tree.  */
  int level;

  /* The outer (parent) loop or NULL if outermost loop.  */
  struct loop *outer;

  /* The first inner (child) loop or NULL if innermost loop.  */
  struct loop *inner;

  /* Link to the next (sibling) loop.  */
  struct loop *next;

  /* Non-zero if the loop shares a header with another loop.  */
  int shared;

  /* Non-zero if the loop is invalid (e.g., contains setjmp.).  */
  int invalid;

  /* Auxiliary info specific to a pass.  */
  void *aux;

  /* The following are currently used by loop.c but they are likely to
     disappear as loop.c is converted to use the CFG.  */

  /* Non-zero if the loop has a NOTE_INSN_LOOP_VTOP.  */
  rtx vtop;

  /* Non-zero if the loop has a NOTE_INSN_LOOP_CONT.
     A continue statement will generate a branch to NEXT_INSN (cont).  */
  rtx cont;

  /* The dominator of cont.  */
  rtx cont_dominator;

  /* The NOTE_INSN_LOOP_BEG.  */
  rtx start;

  /* The NOTE_INSN_LOOP_END.  */
  rtx end;

  /* For a rotated loop that is entered near the bottom,
     this is the label at the top.  Otherwise it is zero.  */
  rtx top;

  /* Place in the loop where control enters.  */
  rtx scan_start;

  /* List of all LABEL_REFs which refer to code labels outside the
     loop.  Used by routines that need to know all loop exits, such as
     final_biv_value and final_giv_value.
     
     This does not include loop exits due to return instructions.
     This is because all bivs and givs are pseudos, and hence must be
     dead after a return, so the presense of a return does not affect
     any of the optimizations that use this info.  It is simpler to
     just not include return instructions on this list.  */
  rtx exit_labels;

  /* The number of LABEL_REFs on exit_labels for this loop and all
     loops nested inside it.  */
  int exit_count;
};

#define FLOW_LOOP_FIRST_BLOCK(loop) sbitmap_first_set_bit ((loop).nodes)
#define FLOW_LOOP_LAST_BLOCK(loop) sbitmap_last_set_bit ((loop).nodes)

/* Structure to hold CFG information about natural loops within a function.  */
struct loops
{
  /* Number of natural loops in the function.  */
  int num;

  /* Maxium nested loop level in the function.  */
  int levels;

  /* Array of natural loop descriptors (scanning this array in reverse order
     will find the inner loops before their enclosing outer loops).  */
  struct loop *array;

  /* Pointer to root of loop heirachy tree.  */
  struct loop *tree;

  /* Information derived from the CFG.  */
  struct cfg
  {
    /* The bitmap vector of dominators or NULL if not computed.  */
    sbitmap *dom;

    /* The ordering of the basic blocks in a depth first search.  */
    int *dfs_order;
  } cfg;

  /* Headers shared by multiple loops that should be merged.  */
  sbitmap shared_headers;
};

extern int flow_loops_find PARAMS ((struct loops *));
extern void flow_loops_free PARAMS ((struct loops *));
extern void flow_loops_dump PARAMS ((const struct loops *, FILE *, int));


/* This structure maintains an edge list vector.  */
struct edge_list 
{
  int num_blocks;
  int num_edges;
  edge *index_to_edge;
};

/* This is the value which indicates no edge is present.  */
#define EDGE_INDEX_NO_EDGE	-1

/* EDGE_INDEX returns an integer index for an edge, or EDGE_INDEX_NO_EDGE
   if there is no edge between the 2 basic blocks.  */
#define EDGE_INDEX(el, pred, succ) (find_edge_index ((el), (pred), (succ)))

/* INDEX_EDGE_PRED_BB and INDEX_EDGE_SUCC_BB return a pointer to the basic
   block which is either the pred or succ end of the indexed edge.  */
#define INDEX_EDGE_PRED_BB(el, index)	((el)->index_to_edge[(index)]->src)
#define INDEX_EDGE_SUCC_BB(el, index)	((el)->index_to_edge[(index)]->dest)

/* INDEX_EDGE returns a pointer to the edge.  */
#define INDEX_EDGE(el, index)           ((el)->index_to_edge[(index)])

/* Number of edges in the compressed edge list.  */
#define NUM_EDGES(el)			((el)->num_edges)

struct edge_list * create_edge_list	PARAMS ((void));
void free_edge_list			PARAMS ((struct edge_list *));
void print_edge_list			PARAMS ((FILE *, struct edge_list *));
void verify_edge_list			PARAMS ((FILE *, struct edge_list *));
int find_edge_index			PARAMS ((struct edge_list *, 
						 basic_block, basic_block));

extern void compute_flow_dominators	PARAMS ((sbitmap *, sbitmap *));
extern void compute_immediate_dominators	PARAMS ((int *, sbitmap *));


enum update_life_extent
{
  UPDATE_LIFE_LOCAL = 0,
  UPDATE_LIFE_GLOBAL = 1,
  UPDATE_LIFE_GLOBAL_RM_NOTES = 2
};

/* Flags for life_analysis and update_life_info.  */

#define PROP_DEATH_NOTES	1	/* Create DEAD and UNUSED notes.  */
#define PROP_LOG_LINKS		2	/* Create LOG_LINKS.  */
#define PROP_REG_INFO		4	/* Update regs_ever_live et al.  */
#define PROP_KILL_DEAD_CODE	8	/* Remove dead code.  */
#define PROP_SCAN_DEAD_CODE	16	/* Scan for dead code.  */
#define PROP_AUTOINC		32	/* Create autoinc mem references.  */
#define PROP_FINAL		63	/* All of the above.  */

extern void update_life_info	PARAMS ((sbitmap, enum update_life_extent,
					 int));
extern int count_or_remove_death_notes	PARAMS ((sbitmap, int));

/* In lcm.c */
extern struct edge_list *pre_edge_lcm 	PARAMS ((FILE *, int, sbitmap *,
						 sbitmap *, sbitmap *, 
						 sbitmap *, sbitmap **,
						 sbitmap **));
extern struct edge_list *pre_edge_rev_lcm PARAMS ((FILE *, int, sbitmap *,
						   sbitmap *, sbitmap *, 
						   sbitmap *, sbitmap **, 
						   sbitmap **));
extern void compute_available		PARAMS ((sbitmap *, sbitmap *,
						 sbitmap *, sbitmap *));

/* In emit-rtl.c.  */
extern rtx emit_block_insn_after	PARAMS ((rtx, rtx, basic_block));
extern rtx emit_block_insn_before	PARAMS ((rtx, rtx, basic_block));

/* In predict.c */
extern void estimate_probability        PARAMS ((struct loops *));

#endif /* _BASIC_BLOCK_H */
