/* Global common subexpression elimination/Partial redundancy elimination
   and global constant/copy propagation for GNU compiler.
   Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

/* TODO
   - reordering of memory allocation and freeing to be more space efficient
   - do rough calc of how many regs are needed in each block, and a rough
     calc of how many regs are available in each class and use that to
     throttle back the code in cases where RTX_COST is minimal.
   - a store to the same address as a load does not kill the load if the
     source of the store is also the destination of the load.  Handling this
     allows more load motion, particularly out of loops.
   - ability to realloc sbitmap vectors would allow one initial computation
     of reg_set_in_block with only subsequent additions, rather than
     recomputing it for each pass

*/

/* References searched while implementing this.

   Compilers Principles, Techniques and Tools
   Aho, Sethi, Ullman
   Addison-Wesley, 1988

   Global Optimization by Suppression of Partial Redundancies
   E. Morel, C. Renvoise
   communications of the acm, Vol. 22, Num. 2, Feb. 1979

   A Portable Machine-Independent Global Optimizer - Design and Measurements
   Frederick Chow
   Stanford Ph.D. thesis, Dec. 1983

   A Fast Algorithm for Code Movement Optimization
   D.M. Dhamdhere
   SIGPLAN Notices, Vol. 23, Num. 10, Oct. 1988

   A Solution to a Problem with Morel and Renvoise's
   Global Optimization by Suppression of Partial Redundancies
   K-H Drechsler, M.P. Stadel
   ACM TOPLAS, Vol. 10, Num. 4, Oct. 1988

   Practical Adaptation of the Global Optimization
   Algorithm of Morel and Renvoise
   D.M. Dhamdhere
   ACM TOPLAS, Vol. 13, Num. 2. Apr. 1991

   Efficiently Computing Static Single Assignment Form and the Control
   Dependence Graph
   R. Cytron, J. Ferrante, B.K. Rosen, M.N. Wegman, and F.K. Zadeck
   ACM TOPLAS, Vol. 13, Num. 4, Oct. 1991

   Lazy Code Motion
   J. Knoop, O. Ruthing, B. Steffen
   ACM SIGPLAN Notices Vol. 27, Num. 7, Jul. 1992, '92 Conference on PLDI

   What's In a Region?  Or Computing Control Dependence Regions in Near-Linear
   Time for Reducible Flow Control
   Thomas Ball
   ACM Letters on Programming Languages and Systems,
   Vol. 2, Num. 1-4, Mar-Dec 1993

   An Efficient Representation for Sparse Sets
   Preston Briggs, Linda Torczon
   ACM Letters on Programming Languages and Systems,
   Vol. 2, Num. 1-4, Mar-Dec 1993

   A Variation of Knoop, Ruthing, and Steffen's Lazy Code Motion
   K-H Drechsler, M.P. Stadel
   ACM SIGPLAN Notices, Vol. 28, Num. 5, May 1993

   Partial Dead Code Elimination
   J. Knoop, O. Ruthing, B. Steffen
   ACM SIGPLAN Notices, Vol. 29, Num. 6, Jun. 1994

   Effective Partial Redundancy Elimination
   P. Briggs, K.D. Cooper
   ACM SIGPLAN Notices, Vol. 29, Num. 6, Jun. 1994

   The Program Structure Tree: Computing Control Regions in Linear Time
   R. Johnson, D. Pearson, K. Pingali
   ACM SIGPLAN Notices, Vol. 29, Num. 6, Jun. 1994

   Optimal Code Motion: Theory and Practice
   J. Knoop, O. Ruthing, B. Steffen
   ACM TOPLAS, Vol. 16, Num. 4, Jul. 1994

   The power of assignment motion
   J. Knoop, O. Ruthing, B. Steffen
   ACM SIGPLAN Notices Vol. 30, Num. 6, Jun. 1995, '95 Conference on PLDI

   Global code motion / global value numbering
   C. Click
   ACM SIGPLAN Notices Vol. 30, Num. 6, Jun. 1995, '95 Conference on PLDI

   Value Driven Redundancy Elimination
   L.T. Simpson
   Rice University Ph.D. thesis, Apr. 1996

   Value Numbering
   L.T. Simpson
   Massively Scalar Compiler Project, Rice University, Sep. 1996

   High Performance Compilers for Parallel Computing
   Michael Wolfe
   Addison-Wesley, 1996

   Advanced Compiler Design and Implementation
   Steven Muchnick
   Morgan Kaufmann, 1997

   Building an Optimizing Compiler
   Robert Morgan
   Digital Press, 1998

   People wishing to speed up the code here should read:
     Elimination Algorithms for Data Flow Analysis
     B.G. Ryder, M.C. Paull
     ACM Computing Surveys, Vol. 18, Num. 3, Sep. 1986

     How to Analyze Large Programs Efficiently and Informatively
     D.M. Dhamdhere, B.K. Rosen, F.K. Zadeck
     ACM SIGPLAN Notices Vol. 27, Num. 7, Jul. 1992, '92 Conference on PLDI

   People wishing to do something different can find various possibilities
   in the above papers and elsewhere.
*/

#include "config.h"
#include "system.h"
#include "toplev.h"

#include "rtl.h"
#include "tm_p.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "flags.h"
#include "real.h"
#include "insn-config.h"
#include "recog.h"
#include "basic-block.h"
#include "output.h"
#include "function.h"
#include "expr.h"
#include "except.h"
#include "ggc.h"
#include "params.h"
#include "cselib.h"

#include "obstack.h"

/* Propagate flow information through back edges and thus enable PRE's
   moving loop invariant calculations out of loops.

   Originally this tended to create worse overall code, but several
   improvements during the development of PRE seem to have made following
   back edges generally a win.

   Note much of the loop invariant code motion done here would normally
   be done by loop.c, which has more heuristics for when to move invariants
   out of loops.  At some point we might need to move some of those
   heuristics into gcse.c.  */

/* We support GCSE via Partial Redundancy Elimination.  PRE optimizations
   are a superset of those done by GCSE.

   We perform the following steps:

   1) Compute basic block information.

   2) Compute table of places where registers are set.

   3) Perform copy/constant propagation.

   4) Perform global cse.

   5) Perform another pass of copy/constant propagation.

   Two passes of copy/constant propagation are done because the first one
   enables more GCSE and the second one helps to clean up the copies that
   GCSE creates.  This is needed more for PRE than for Classic because Classic
   GCSE will try to use an existing register containing the common
   subexpression rather than create a new one.  This is harder to do for PRE
   because of the code motion (which Classic GCSE doesn't do).

   Expressions we are interested in GCSE-ing are of the form
   (set (pseudo-reg) (expression)).
   Function want_to_gcse_p says what these are.

   PRE handles moving invariant expressions out of loops (by treating them as
   partially redundant).

   Eventually it would be nice to replace cse.c/gcse.c with SSA (static single
   assignment) based GVN (global value numbering).  L. T. Simpson's paper
   (Rice University) on value numbering is a useful reference for this.

   **********************

   We used to support multiple passes but there are diminishing returns in
   doing so.  The first pass usually makes 90% of the changes that are doable.
   A second pass can make a few more changes made possible by the first pass.
   Experiments show any further passes don't make enough changes to justify
   the expense.

   A study of spec92 using an unlimited number of passes:
   [1 pass] = 1208 substitutions, [2] = 577, [3] = 202, [4] = 192, [5] = 83,
   [6] = 34, [7] = 17, [8] = 9, [9] = 4, [10] = 4, [11] = 2,
   [12] = 2, [13] = 1, [15] = 1, [16] = 2, [41] = 1

   It was found doing copy propagation between each pass enables further
   substitutions.

   PRE is quite expensive in complicated functions because the DFA can take
   awhile to converge.  Hence we only perform one pass.  The parameter max-gcse-passes can
   be modified if one wants to experiment.

   **********************

   The steps for PRE are:

   1) Build the hash table of expressions we wish to GCSE (expr_hash_table).

   2) Perform the data flow analysis for PRE.

   3) Delete the redundant instructions

   4) Insert the required copies [if any] that make the partially
      redundant instructions fully redundant.

   5) For other reaching expressions, insert an instruction to copy the value
      to a newly created pseudo that will reach the redundant instruction.

   The deletion is done first so that when we do insertions we
   know which pseudo reg to use.

   Various papers have argued that PRE DFA is expensive (O(n^2)) and others
   argue it is not.  The number of iterations for the algorithm to converge
   is typically 2-4 so I don't view it as that expensive (relatively speaking).

   PRE GCSE depends heavily on the second CSE pass to clean up the copies
   we create.  To make an expression reach the place where it's redundant,
   the result of the expression is copied to a new register, and the redundant
   expression is deleted by replacing it with this new register.  Classic GCSE
   doesn't have this problem as much as it computes the reaching defs of
   each register in each block and thus can try to use an existing register.

   **********************

   A fair bit of simplicity is created by creating small functions for simple
   tasks, even when the function is only called in one place.  This may
   measurably slow things down [or may not] by creating more function call
   overhead than is necessary.  The source is laid out so that it's trivial
   to make the affected functions inline so that one can measure what speed
   up, if any, can be achieved, and maybe later when things settle things can
   be rearranged.

   Help stamp out big monolithic functions!  */

/* GCSE global vars.  */

/* -dG dump file.  */
static FILE *gcse_file;

/* Note whether or not we should run jump optimization after gcse.  We
   want to do this for two cases.

    * If we changed any jumps via cprop.

    * If we added any labels via edge splitting.  */

static int run_jump_opt_after_gcse;

/* Bitmaps are normally not included in debugging dumps.
   However it's useful to be able to print them from GDB.
   We could create special functions for this, but it's simpler to
   just allow passing stderr to the dump_foo fns.  Since stderr can
   be a macro, we store a copy here.  */
static FILE *debug_stderr;

/* An obstack for our working variables.  */
static struct obstack gcse_obstack;

/* Nonzero for each mode that supports (set (reg) (reg)).
   This is trivially true for integer and floating point values.
   It may or may not be true for condition codes.  */
static char can_copy_p[(int) NUM_MACHINE_MODES];

/* Nonzero if can_copy_p has been initialized.  */
static int can_copy_init_p;

struct reg_use {rtx reg_rtx; };

/* Hash table of expressions.  */

struct expr
{
  /* The expression (SET_SRC for expressions, PATTERN for assignments).  */
  rtx expr;
  /* Index in the available expression bitmaps.  */
  int bitmap_index;
  /* Next entry with the same hash.  */
  struct expr *next_same_hash;
  /* List of anticipatable occurrences in basic blocks in the function.
     An "anticipatable occurrence" is one that is the first occurrence in the
     basic block, the operands are not modified in the basic block prior
     to the occurrence and the output is not used between the start of
     the block and the occurrence.  */
  struct occr *antic_occr;
  /* List of available occurrence in basic blocks in the function.
     An "available occurrence" is one that is the last occurrence in the
     basic block and the operands are not modified by following statements in
     the basic block [including this insn].  */
  struct occr *avail_occr;
  /* Non-null if the computation is PRE redundant.
     The value is the newly created pseudo-reg to record a copy of the
     expression in all the places that reach the redundant copy.  */
  rtx reaching_reg;
};

/* Occurrence of an expression.
   There is one per basic block.  If a pattern appears more than once the
   last appearance is used [or first for anticipatable expressions].  */

struct occr
{
  /* Next occurrence of this expression.  */
  struct occr *next;
  /* The insn that computes the expression.  */
  rtx insn;
  /* Nonzero if this [anticipatable] occurrence has been deleted.  */
  char deleted_p;
  /* Nonzero if this [available] occurrence has been copied to
     reaching_reg.  */
  /* ??? This is mutually exclusive with deleted_p, so they could share
     the same byte.  */
  char copied_p;
};

/* Expression and copy propagation hash tables.
   Each hash table is an array of buckets.
   ??? It is known that if it were an array of entries, structure elements
   `next_same_hash' and `bitmap_index' wouldn't be necessary.  However, it is
   not clear whether in the final analysis a sufficient amount of memory would
   be saved as the size of the available expression bitmaps would be larger
   [one could build a mapping table without holes afterwards though].
   Someday I'll perform the computation and figure it out.  */

struct hash_table
{
  /* The table itself.
     This is an array of `expr_hash_table_size' elements.  */
  struct expr **table;

  /* Size of the hash table, in elements.  */
  unsigned int size;

  /* Number of hash table elements.  */
  unsigned int n_elems;

  /* Whether the table is expression of copy propagation one.  */
  int set_p;
};

/* Expression hash table.  */
static struct hash_table expr_hash_table;

/* Copy propagation hash table.  */
static struct hash_table set_hash_table;

/* Mapping of uids to cuids.
   Only real insns get cuids.  */
static int *uid_cuid;

/* Highest UID in UID_CUID.  */
static int max_uid;

/* Get the cuid of an insn.  */
#ifdef ENABLE_CHECKING
#define INSN_CUID(INSN) (INSN_UID (INSN) > max_uid ? (abort (), 0) : uid_cuid[INSN_UID (INSN)])
#else
#define INSN_CUID(INSN) (uid_cuid[INSN_UID (INSN)])
#endif

/* Number of cuids.  */
static int max_cuid;

/* Mapping of cuids to insns.  */
static rtx *cuid_insn;

/* Get insn from cuid.  */
#define CUID_INSN(CUID) (cuid_insn[CUID])

/* Maximum register number in function prior to doing gcse + 1.
   Registers created during this pass have regno >= max_gcse_regno.
   This is named with "gcse" to not collide with global of same name.  */
static unsigned int max_gcse_regno;

/* Table of registers that are modified.

   For each register, each element is a list of places where the pseudo-reg
   is set.

   For simplicity, GCSE is done on sets of pseudo-regs only.  PRE GCSE only
   requires knowledge of which blocks kill which regs [and thus could use
   a bitmap instead of the lists `reg_set_table' uses].

   `reg_set_table' and could be turned into an array of bitmaps (num-bbs x
   num-regs) [however perhaps it may be useful to keep the data as is].  One
   advantage of recording things this way is that `reg_set_table' is fairly
   sparse with respect to pseudo regs but for hard regs could be fairly dense
   [relatively speaking].  And recording sets of pseudo-regs in lists speeds
   up functions like compute_transp since in the case of pseudo-regs we only
   need to iterate over the number of times a pseudo-reg is set, not over the
   number of basic blocks [clearly there is a bit of a slow down in the cases
   where a pseudo is set more than once in a block, however it is believed
   that the net effect is to speed things up].  This isn't done for hard-regs
   because recording call-clobbered hard-regs in `reg_set_table' at each
   function call can consume a fair bit of memory, and iterating over
   hard-regs stored this way in compute_transp will be more expensive.  */

typedef struct reg_set
{
  /* The next setting of this register.  */
  struct reg_set *next;
  /* The insn where it was set.  */
  rtx insn;
} reg_set;

static reg_set **reg_set_table;

/* Size of `reg_set_table'.
   The table starts out at max_gcse_regno + slop, and is enlarged as
   necessary.  */
static int reg_set_table_size;

/* Amount to grow `reg_set_table' by when it's full.  */
#define REG_SET_TABLE_SLOP 100

/* This is a list of expressions which are MEMs and will be used by load
   or store motion.
   Load motion tracks MEMs which aren't killed by
   anything except itself. (ie, loads and stores to a single location).
   We can then allow movement of these MEM refs with a little special
   allowance. (all stores copy the same value to the reaching reg used
   for the loads).  This means all values used to store into memory must have
   no side effects so we can re-issue the setter value.
   Store Motion uses this structure as an expression table to track stores
   which look interesting, and might be moveable towards the exit block.  */

struct ls_expr
{
  struct expr * expr;		/* Gcse expression reference for LM.  */
  rtx pattern;			/* Pattern of this mem.  */
  rtx loads;			/* INSN list of loads seen.  */
  rtx stores;			/* INSN list of stores seen.  */
  struct ls_expr * next;	/* Next in the list.  */
  int invalid;			/* Invalid for some reason.  */
  int index;			/* If it maps to a bitmap index.  */
  int hash_index;		/* Index when in a hash table.  */
  rtx reaching_reg;		/* Register to use when re-writing.  */
};

/* Head of the list of load/store memory refs.  */
static struct ls_expr * pre_ldst_mems = NULL;

/* Bitmap containing one bit for each register in the program.
   Used when performing GCSE to track which registers have been set since
   the start of the basic block.  */
static regset reg_set_bitmap;

/* For each block, a bitmap of registers set in the block.
   This is used by expr_killed_p and compute_transp.
   It is computed during hash table computation and not by compute_sets
   as it includes registers added since the last pass (or between cprop and
   gcse) and it's currently not easy to realloc sbitmap vectors.  */
static sbitmap *reg_set_in_block;

/* Array, indexed by basic block number for a list of insns which modify
   memory within that block.  */
static rtx * modify_mem_list;
bitmap modify_mem_list_set;

/* This array parallels modify_mem_list, but is kept canonicalized.  */
static rtx * canon_modify_mem_list;
bitmap canon_modify_mem_list_set;
/* Various variables for statistics gathering.  */

/* Memory used in a pass.
   This isn't intended to be absolutely precise.  Its intent is only
   to keep an eye on memory usage.  */
static int bytes_used;

/* GCSE substitutions made.  */
static int gcse_subst_count;
/* Number of copy instructions created.  */
static int gcse_create_count;
/* Number of constants propagated.  */
static int const_prop_count;
/* Number of copys propagated.  */
static int copy_prop_count;

/* These variables are used by classic GCSE.
   Normally they'd be defined a bit later, but `rd_gen' needs to
   be declared sooner.  */

/* Each block has a bitmap of each type.
   The length of each blocks bitmap is:

       max_cuid  - for reaching definitions
       n_exprs - for available expressions

   Thus we view the bitmaps as 2 dimensional arrays.  i.e.
   rd_kill[block_num][cuid_num]
   ae_kill[block_num][expr_num]			 */

/* For reaching defs */
static sbitmap *rd_kill, *rd_gen, *reaching_defs, *rd_out;

/* for available exprs */
static sbitmap *ae_kill, *ae_gen, *ae_in, *ae_out;

/* Objects of this type are passed around by the null-pointer check
   removal routines.  */
struct null_pointer_info
{
  /* The basic block being processed.  */
  basic_block current_block;
  /* The first register to be handled in this pass.  */
  unsigned int min_reg;
  /* One greater than the last register to be handled in this pass.  */
  unsigned int max_reg;
  sbitmap *nonnull_local;
  sbitmap *nonnull_killed;
};

static void compute_can_copy	PARAMS ((void));
static char *gmalloc		PARAMS ((unsigned int));
static char *grealloc		PARAMS ((char *, unsigned int));
static char *gcse_alloc		PARAMS ((unsigned long));
static void alloc_gcse_mem	PARAMS ((rtx));
static void free_gcse_mem	PARAMS ((void));
static void alloc_reg_set_mem	PARAMS ((int));
static void free_reg_set_mem	PARAMS ((void));
static int get_bitmap_width     PARAMS ((int, int, int));
static void record_one_set	PARAMS ((int, rtx));
static void record_set_info	PARAMS ((rtx, rtx, void *));
static void compute_sets	PARAMS ((rtx));
static void hash_scan_insn	PARAMS ((rtx, struct hash_table *, int));
static void hash_scan_set	PARAMS ((rtx, rtx, struct hash_table *));
static void hash_scan_clobber	PARAMS ((rtx, rtx, struct hash_table *));
static void hash_scan_call	PARAMS ((rtx, rtx, struct hash_table *));
static int want_to_gcse_p	PARAMS ((rtx));
static int oprs_unchanged_p	PARAMS ((rtx, rtx, int));
static int oprs_anticipatable_p PARAMS ((rtx, rtx));
static int oprs_available_p	PARAMS ((rtx, rtx));
static void insert_expr_in_table PARAMS ((rtx, enum machine_mode, rtx,
					  int, int, struct hash_table *));
static void insert_set_in_table PARAMS ((rtx, rtx, struct hash_table *));
static unsigned int hash_expr	PARAMS ((rtx, enum machine_mode, int *, int));
static unsigned int hash_expr_1 PARAMS ((rtx, enum machine_mode, int *));
static unsigned int hash_string_1 PARAMS ((const char *));
static unsigned int hash_set	PARAMS ((int, int));
static int expr_equiv_p	        PARAMS ((rtx, rtx));
static void record_last_reg_set_info PARAMS ((rtx, int));
static void record_last_mem_set_info PARAMS ((rtx));
static void record_last_set_info PARAMS ((rtx, rtx, void *));
static void compute_hash_table	PARAMS ((struct hash_table *));
static void alloc_hash_table PARAMS ((int, struct hash_table *, int));
static void free_hash_table PARAMS ((struct hash_table *));
static void compute_hash_table_work PARAMS ((struct hash_table *));
static void dump_hash_table	PARAMS ((FILE *, const char *,
					struct hash_table *));
static struct expr *lookup_expr	PARAMS ((rtx, struct hash_table *));
static struct expr *lookup_set	PARAMS ((unsigned int, rtx, struct hash_table *));
static struct expr *next_set	PARAMS ((unsigned int, struct expr *));
static void reset_opr_set_tables PARAMS ((void));
static int oprs_not_set_p	PARAMS ((rtx, rtx));
static void mark_call		PARAMS ((rtx));
static void mark_set		PARAMS ((rtx, rtx));
static void mark_clobber	PARAMS ((rtx, rtx));
static void mark_oprs_set	PARAMS ((rtx));
static void alloc_cprop_mem	PARAMS ((int, int));
static void free_cprop_mem	PARAMS ((void));
static void compute_transp	PARAMS ((rtx, int, sbitmap *, int));
static void compute_transpout	PARAMS ((void));
static void compute_local_properties PARAMS ((sbitmap *, sbitmap *, sbitmap *,
					      struct hash_table *));
static void compute_cprop_data	PARAMS ((void));
static void find_used_regs	PARAMS ((rtx *, void *));
static int try_replace_reg	PARAMS ((rtx, rtx, rtx));
static struct expr *find_avail_set PARAMS ((int, rtx));
static int cprop_jump		PARAMS ((basic_block, rtx, rtx, rtx, rtx));
static void mems_conflict_for_gcse_p PARAMS ((rtx, rtx, void *));
static int load_killed_in_block_p    PARAMS ((basic_block, int, rtx, int));
static void canon_list_insert        PARAMS ((rtx, rtx, void *));
static int cprop_insn		PARAMS ((rtx, int));
static int cprop		PARAMS ((int));
static int one_cprop_pass	PARAMS ((int, int));
static bool constprop_register	PARAMS ((rtx, rtx, rtx, int));
static struct expr *find_bypass_set PARAMS ((int, int));
static bool reg_killed_on_edge	    PARAMS ((rtx, edge));
static int bypass_block		    PARAMS ((basic_block, rtx, rtx));
static int bypass_conditional_jumps PARAMS ((void));
static void alloc_pre_mem	PARAMS ((int, int));
static void free_pre_mem	PARAMS ((void));
static void compute_pre_data	PARAMS ((void));
static int pre_expr_reaches_here_p PARAMS ((basic_block, struct expr *,
					    basic_block));
static void insert_insn_end_bb	PARAMS ((struct expr *, basic_block, int));
static void pre_insert_copy_insn PARAMS ((struct expr *, rtx));
static void pre_insert_copies	PARAMS ((void));
static int pre_delete		PARAMS ((void));
static int pre_gcse		PARAMS ((void));
static int one_pre_gcse_pass	PARAMS ((int));
static void add_label_notes	PARAMS ((rtx, rtx));
static void alloc_code_hoist_mem PARAMS ((int, int));
static void free_code_hoist_mem	PARAMS ((void));
static void compute_code_hoist_vbeinout	PARAMS ((void));
static void compute_code_hoist_data PARAMS ((void));
static int hoist_expr_reaches_here_p PARAMS ((basic_block, int, basic_block,
					      char *));
static void hoist_code		PARAMS ((void));
static int one_code_hoisting_pass PARAMS ((void));
static void alloc_rd_mem	PARAMS ((int, int));
static void free_rd_mem		PARAMS ((void));
static void handle_rd_kill_set	PARAMS ((rtx, int, basic_block));
static void compute_kill_rd	PARAMS ((void));
static void compute_rd		PARAMS ((void));
static void alloc_avail_expr_mem PARAMS ((int, int));
static void free_avail_expr_mem PARAMS ((void));
static void compute_ae_gen	PARAMS ((struct hash_table *));
static int expr_killed_p	PARAMS ((rtx, basic_block));
static void compute_ae_kill	PARAMS ((sbitmap *, sbitmap *, struct hash_table *));
static int expr_reaches_here_p	PARAMS ((struct occr *, struct expr *,
					 basic_block, int));
static rtx computing_insn	PARAMS ((struct expr *, rtx));
static int def_reaches_here_p	PARAMS ((rtx, rtx));
static int can_disregard_other_sets PARAMS ((struct reg_set **, rtx, int));
static int handle_avail_expr	PARAMS ((rtx, struct expr *));
static int classic_gcse		PARAMS ((void));
static int one_classic_gcse_pass PARAMS ((int));
static void invalidate_nonnull_info PARAMS ((rtx, rtx, void *));
static int delete_null_pointer_checks_1 PARAMS ((unsigned int *,
						  sbitmap *, sbitmap *,
						  struct null_pointer_info *));
static rtx process_insert_insn	PARAMS ((struct expr *));
static int pre_edge_insert	PARAMS ((struct edge_list *, struct expr **));
static int expr_reaches_here_p_work PARAMS ((struct occr *, struct expr *,
					     basic_block, int, char *));
static int pre_expr_reaches_here_p_work	PARAMS ((basic_block, struct expr *,
						 basic_block, char *));
static struct ls_expr * ldst_entry 	PARAMS ((rtx));
static void free_ldst_entry 		PARAMS ((struct ls_expr *));
static void free_ldst_mems		PARAMS ((void));
static void print_ldst_list 		PARAMS ((FILE *));
static struct ls_expr * find_rtx_in_ldst PARAMS ((rtx));
static int enumerate_ldsts		PARAMS ((void));
static inline struct ls_expr * first_ls_expr PARAMS ((void));
static inline struct ls_expr * next_ls_expr  PARAMS ((struct ls_expr *));
static int simple_mem			PARAMS ((rtx));
static void invalidate_any_buried_refs	PARAMS ((rtx));
static void compute_ld_motion_mems	PARAMS ((void));
static void trim_ld_motion_mems		PARAMS ((void));
static void update_ld_motion_stores	PARAMS ((struct expr *));
static void reg_set_info		PARAMS ((rtx, rtx, void *));
static int store_ops_ok			PARAMS ((rtx, basic_block));
static void find_moveable_store		PARAMS ((rtx));
static int compute_store_table		PARAMS ((void));
static int load_kills_store		PARAMS ((rtx, rtx));
static int find_loads			PARAMS ((rtx, rtx));
static int store_killed_in_insn		PARAMS ((rtx, rtx));
static int store_killed_after		PARAMS ((rtx, rtx, basic_block));
static int store_killed_before		PARAMS ((rtx, rtx, basic_block));
static void build_store_vectors		PARAMS ((void));
static void insert_insn_start_bb	PARAMS ((rtx, basic_block));
static int insert_store			PARAMS ((struct ls_expr *, edge));
static void replace_store_insn		PARAMS ((rtx, rtx, basic_block));
static void delete_store		PARAMS ((struct ls_expr *,
						 basic_block));
static void free_store_memory		PARAMS ((void));
static void store_motion		PARAMS ((void));
static void free_insn_expr_list_list	PARAMS ((rtx *));
static void clear_modify_mem_tables	PARAMS ((void));
static void free_modify_mem_tables	PARAMS ((void));
static rtx gcse_emit_move_after		PARAMS ((rtx, rtx, rtx));
static void local_cprop_find_used_regs	PARAMS ((rtx *, void *));
static bool do_local_cprop		PARAMS ((rtx, rtx, int, rtx*));
static bool adjust_libcall_notes	PARAMS ((rtx, rtx, rtx, rtx*));
static void local_cprop_pass		PARAMS ((int));

/* Entry point for global common subexpression elimination.
   F is the first instruction in the function.  */

int
gcse_main (f, file)
     rtx f;
     FILE *file;
{
  int changed, pass;
  /* Bytes used at start of pass.  */
  int initial_bytes_used;
  /* Maximum number of bytes used by a pass.  */
  int max_pass_bytes;
  /* Point to release obstack data from for each pass.  */
  char *gcse_obstack_bottom;

  /* Insertion of instructions on edges can create new basic blocks; we
     need the original basic block count so that we can properly deallocate
     arrays sized on the number of basic blocks originally in the cfg.  */
  int orig_bb_count;
  /* We do not construct an accurate cfg in functions which call
     setjmp, so just punt to be safe.  */
  if (current_function_calls_setjmp)
    return 0;

  /* Assume that we do not need to run jump optimizations after gcse.  */
  run_jump_opt_after_gcse = 0;

  /* For calling dump_foo fns from gdb.  */
  debug_stderr = stderr;
  gcse_file = file;

  /* Identify the basic block information for this function, including
     successors and predecessors.  */
  max_gcse_regno = max_reg_num ();

  if (file)
    dump_flow_info (file);

  orig_bb_count = n_basic_blocks;
  /* Return if there's nothing to do.  */
  if (n_basic_blocks <= 1)
    return 0;

  /* Trying to perform global optimizations on flow graphs which have
     a high connectivity will take a long time and is unlikely to be
     particularly useful.

     In normal circumstances a cfg should have about twice as many edges
     as blocks.  But we do not want to punish small functions which have
     a couple switch statements.  So we require a relatively large number
     of basic blocks and the ratio of edges to blocks to be high.  */
  if (n_basic_blocks > 1000 && n_edges / n_basic_blocks >= 20)
    {
      if (warn_disabled_optimization)
	warning ("GCSE disabled: %d > 1000 basic blocks and %d >= 20 edges/basic block",
		 n_basic_blocks, n_edges / n_basic_blocks);
      return 0;
    }

  /* If allocating memory for the cprop bitmap would take up too much
     storage it's better just to disable the optimization.  */
  if ((n_basic_blocks
       * SBITMAP_SET_SIZE (max_gcse_regno)
       * sizeof (SBITMAP_ELT_TYPE)) > MAX_GCSE_MEMORY)
    {
      if (warn_disabled_optimization)
	warning ("GCSE disabled: %d basic blocks and %d registers",
		 n_basic_blocks, max_gcse_regno);

      return 0;
    }

  /* See what modes support reg/reg copy operations.  */
  if (! can_copy_init_p)
    {
      compute_can_copy ();
      can_copy_init_p = 1;
    }

  gcc_obstack_init (&gcse_obstack);
  bytes_used = 0;

  /* We need alias.  */
  init_alias_analysis ();
  /* Record where pseudo-registers are set.  This data is kept accurate
     during each pass.  ??? We could also record hard-reg information here
     [since it's unchanging], however it is currently done during hash table
     computation.

     It may be tempting to compute MEM set information here too, but MEM sets
     will be subject to code motion one day and thus we need to compute
     information about memory sets when we build the hash tables.  */

  alloc_reg_set_mem (max_gcse_regno);
  compute_sets (f);

  pass = 0;
  initial_bytes_used = bytes_used;
  max_pass_bytes = 0;
  gcse_obstack_bottom = gcse_alloc (1);
  changed = 1;
  while (changed && pass < MAX_GCSE_PASSES)
    {
      changed = 0;
      if (file)
	fprintf (file, "GCSE pass %d\n\n", pass + 1);

      /* Initialize bytes_used to the space for the pred/succ lists,
	 and the reg_set_table data.  */
      bytes_used = initial_bytes_used;

      /* Each pass may create new registers, so recalculate each time.  */
      max_gcse_regno = max_reg_num ();

      alloc_gcse_mem (f);

      /* Don't allow constant propagation to modify jumps
	 during this pass.  */
      changed = one_cprop_pass (pass + 1, 0);

      if (optimize_size)
	changed |= one_classic_gcse_pass (pass + 1);
      else
	{
	  changed |= one_pre_gcse_pass (pass + 1);
	  /* We may have just created new basic blocks.  Release and
	     recompute various things which are sized on the number of
	     basic blocks.  */
	  if (changed)
	    {
	      free_modify_mem_tables ();
	      modify_mem_list
		= (rtx *) gmalloc (last_basic_block * sizeof (rtx));
	      canon_modify_mem_list
		= (rtx *) gmalloc (last_basic_block * sizeof (rtx));
	      memset ((char *) modify_mem_list, 0, last_basic_block * sizeof (rtx));
	      memset ((char *) canon_modify_mem_list, 0, last_basic_block * sizeof (rtx));
	      orig_bb_count = n_basic_blocks;
	    }
	  free_reg_set_mem ();
	  alloc_reg_set_mem (max_reg_num ());
	  compute_sets (f);
	  run_jump_opt_after_gcse = 1;
	}

      if (max_pass_bytes < bytes_used)
	max_pass_bytes = bytes_used;

      /* Free up memory, then reallocate for code hoisting.  We can
	 not re-use the existing allocated memory because the tables
	 will not have info for the insns or registers created by
	 partial redundancy elimination.  */
      free_gcse_mem ();

      /* It does not make sense to run code hoisting unless we optimizing
	 for code size -- it rarely makes programs faster, and can make
	 them bigger if we did partial redundancy elimination (when optimizing
	 for space, we use a classic gcse algorithm instead of partial
	 redundancy algorithms).  */
      if (optimize_size)
	{
	  max_gcse_regno = max_reg_num ();
	  alloc_gcse_mem (f);
	  changed |= one_code_hoisting_pass ();
	  free_gcse_mem ();

	  if (max_pass_bytes < bytes_used)
	    max_pass_bytes = bytes_used;
	}

      if (file)
	{
	  fprintf (file, "\n");
	  fflush (file);
	}

      obstack_free (&gcse_obstack, gcse_obstack_bottom);
      pass++;
    }

  /* Do one last pass of copy propagation, including cprop into
     conditional jumps.  */

  max_gcse_regno = max_reg_num ();
  alloc_gcse_mem (f);
  /* This time, go ahead and allow cprop to alter jumps.  */
  one_cprop_pass (pass + 1, 1);
  free_gcse_mem ();

  if (file)
    {
      fprintf (file, "GCSE of %s: %d basic blocks, ",
	       current_function_name, n_basic_blocks);
      fprintf (file, "%d pass%s, %d bytes\n\n",
	       pass, pass > 1 ? "es" : "", max_pass_bytes);
    }

  obstack_free (&gcse_obstack, NULL);
  free_reg_set_mem ();
  /* We are finished with alias.  */
  end_alias_analysis ();
  allocate_reg_info (max_reg_num (), FALSE, FALSE);

  /* Store motion disabled until it is fixed.  */
  if (0 && !optimize_size && flag_gcse_sm)
    store_motion ();
  /* Record where pseudo-registers are set.  */
  return run_jump_opt_after_gcse;
}

/* Misc. utilities.  */

/* Compute which modes support reg/reg copy operations.  */

static void
compute_can_copy ()
{
  int i;
#ifndef AVOID_CCMODE_COPIES
  rtx reg, insn;
#endif
  memset (can_copy_p, 0, NUM_MACHINE_MODES);

  start_sequence ();
  for (i = 0; i < NUM_MACHINE_MODES; i++)
    if (GET_MODE_CLASS (i) == MODE_CC)
      {
#ifdef AVOID_CCMODE_COPIES
	can_copy_p[i] = 0;
#else
	reg = gen_rtx_REG ((enum machine_mode) i, LAST_VIRTUAL_REGISTER + 1);
	insn = emit_insn (gen_rtx_SET (VOIDmode, reg, reg));
	if (recog (PATTERN (insn), insn, NULL) >= 0)
	  can_copy_p[i] = 1;
#endif
      }
    else
      can_copy_p[i] = 1;

  end_sequence ();
}

/* Cover function to xmalloc to record bytes allocated.  */

static char *
gmalloc (size)
     unsigned int size;
{
  bytes_used += size;
  return xmalloc (size);
}

/* Cover function to xrealloc.
   We don't record the additional size since we don't know it.
   It won't affect memory usage stats much anyway.  */

static char *
grealloc (ptr, size)
     char *ptr;
     unsigned int size;
{
  return xrealloc (ptr, size);
}

/* Cover function to obstack_alloc.  */

static char *
gcse_alloc (size)
     unsigned long size;
{
  bytes_used += size;
  return (char *) obstack_alloc (&gcse_obstack, size);
}

/* Allocate memory for the cuid mapping array,
   and reg/memory set tracking tables.

   This is called at the start of each pass.  */

static void
alloc_gcse_mem (f)
     rtx f;
{
  int i, n;
  rtx insn;

  /* Find the largest UID and create a mapping from UIDs to CUIDs.
     CUIDs are like UIDs except they increase monotonically, have no gaps,
     and only apply to real insns.  */

  max_uid = get_max_uid ();
  n = (max_uid + 1) * sizeof (int);
  uid_cuid = (int *) gmalloc (n);
  memset ((char *) uid_cuid, 0, n);
  for (insn = f, i = 0; insn; insn = NEXT_INSN (insn))
    {
      if (INSN_P (insn))
	uid_cuid[INSN_UID (insn)] = i++;
      else
	uid_cuid[INSN_UID (insn)] = i;
    }

  /* Create a table mapping cuids to insns.  */

  max_cuid = i;
  n = (max_cuid + 1) * sizeof (rtx);
  cuid_insn = (rtx *) gmalloc (n);
  memset ((char *) cuid_insn, 0, n);
  for (insn = f, i = 0; insn; insn = NEXT_INSN (insn))
    if (INSN_P (insn))
      CUID_INSN (i++) = insn;

  /* Allocate vars to track sets of regs.  */
  reg_set_bitmap = BITMAP_XMALLOC ();

  /* Allocate vars to track sets of regs, memory per block.  */
  reg_set_in_block = (sbitmap *) sbitmap_vector_alloc (last_basic_block,
						       max_gcse_regno);
  /* Allocate array to keep a list of insns which modify memory in each
     basic block.  */
  modify_mem_list = (rtx *) gmalloc (last_basic_block * sizeof (rtx));
  canon_modify_mem_list = (rtx *) gmalloc (last_basic_block * sizeof (rtx));
  memset ((char *) modify_mem_list, 0, last_basic_block * sizeof (rtx));
  memset ((char *) canon_modify_mem_list, 0, last_basic_block * sizeof (rtx));
  modify_mem_list_set = BITMAP_XMALLOC ();
  canon_modify_mem_list_set = BITMAP_XMALLOC ();
}

/* Free memory allocated by alloc_gcse_mem.  */

static void
free_gcse_mem ()
{
  free (uid_cuid);
  free (cuid_insn);

  BITMAP_XFREE (reg_set_bitmap);

  sbitmap_vector_free (reg_set_in_block);
  free_modify_mem_tables ();
  BITMAP_XFREE (modify_mem_list_set);
  BITMAP_XFREE (canon_modify_mem_list_set);
}

/* Many of the global optimization algorithms work by solving dataflow
   equations for various expressions.  Initially, some local value is
   computed for each expression in each block.  Then, the values across the
   various blocks are combined (by following flow graph edges) to arrive at
   global values.  Conceptually, each set of equations is independent.  We
   may therefore solve all the equations in parallel, solve them one at a
   time, or pick any intermediate approach.

   When you're going to need N two-dimensional bitmaps, each X (say, the
   number of blocks) by Y (say, the number of expressions), call this
   function.  It's not important what X and Y represent; only that Y
   correspond to the things that can be done in parallel.  This function will
   return an appropriate chunking factor C; you should solve C sets of
   equations in parallel.  By going through this function, we can easily
   trade space against time; by solving fewer equations in parallel we use
   less space.  */

static int
get_bitmap_width (n, x, y)
     int n;
     int x;
     int y;
{
  /* It's not really worth figuring out *exactly* how much memory will
     be used by a particular choice.  The important thing is to get
     something approximately right.  */
  size_t max_bitmap_memory = 10 * 1024 * 1024;

  /* The number of bytes we'd use for a single column of minimum
     width.  */
  size_t column_size = n * x * sizeof (SBITMAP_ELT_TYPE);

  /* Often, it's reasonable just to solve all the equations in
     parallel.  */
  if (column_size * SBITMAP_SET_SIZE (y) <= max_bitmap_memory)
    return y;

  /* Otherwise, pick the largest width we can, without going over the
     limit.  */
  return SBITMAP_ELT_BITS * ((max_bitmap_memory + column_size - 1)
			     / column_size);
}

/* Compute the local properties of each recorded expression.

   Local properties are those that are defined by the block, irrespective of
   other blocks.

   An expression is transparent in a block if its operands are not modified
   in the block.

   An expression is computed (locally available) in a block if it is computed
   at least once and expression would contain the same value if the
   computation was moved to the end of the block.

   An expression is locally anticipatable in a block if it is computed at
   least once and expression would contain the same value if the computation
   was moved to the beginning of the block.

   We call this routine for cprop, pre and code hoisting.  They all compute
   basically the same information and thus can easily share this code.

   TRANSP, COMP, and ANTLOC are destination sbitmaps for recording local
   properties.  If NULL, then it is not necessary to compute or record that
   particular property.

   TABLE controls which hash table to look at.  If it is  set hash table,
   additionally, TRANSP is computed as ~TRANSP, since this is really cprop's
   ABSALTERED.  */

static void
compute_local_properties (transp, comp, antloc, table)
     sbitmap *transp;
     sbitmap *comp;
     sbitmap *antloc;
     struct hash_table *table;
{
  unsigned int i;

  /* Initialize any bitmaps that were passed in.  */
  if (transp)
    {
      if (table->set_p)
	sbitmap_vector_zero (transp, last_basic_block);
      else
	sbitmap_vector_ones (transp, last_basic_block);
    }

  if (comp)
    sbitmap_vector_zero (comp, last_basic_block);
  if (antloc)
    sbitmap_vector_zero (antloc, last_basic_block);

  for (i = 0; i < table->size; i++)
    {
      struct expr *expr;

      for (expr = table->table[i]; expr != NULL; expr = expr->next_same_hash)
	{
	  int indx = expr->bitmap_index;
	  struct occr *occr;

	  /* The expression is transparent in this block if it is not killed.
	     We start by assuming all are transparent [none are killed], and
	     then reset the bits for those that are.  */
	  if (transp)
	    compute_transp (expr->expr, indx, transp, table->set_p);

	  /* The occurrences recorded in antic_occr are exactly those that
	     we want to set to nonzero in ANTLOC.  */
	  if (antloc)
	    for (occr = expr->antic_occr; occr != NULL; occr = occr->next)
	      {
		SET_BIT (antloc[BLOCK_NUM (occr->insn)], indx);

		/* While we're scanning the table, this is a good place to
		   initialize this.  */
		occr->deleted_p = 0;
	      }

	  /* The occurrences recorded in avail_occr are exactly those that
	     we want to set to nonzero in COMP.  */
	  if (comp)
	    for (occr = expr->avail_occr; occr != NULL; occr = occr->next)
	      {
		SET_BIT (comp[BLOCK_NUM (occr->insn)], indx);

		/* While we're scanning the table, this is a good place to
		   initialize this.  */
		occr->copied_p = 0;
	      }

	  /* While we're scanning the table, this is a good place to
	     initialize this.  */
	  expr->reaching_reg = 0;
	}
    }
}

/* Register set information.

   `reg_set_table' records where each register is set or otherwise
   modified.  */

static struct obstack reg_set_obstack;

static void
alloc_reg_set_mem (n_regs)
     int n_regs;
{
  unsigned int n;

  reg_set_table_size = n_regs + REG_SET_TABLE_SLOP;
  n = reg_set_table_size * sizeof (struct reg_set *);
  reg_set_table = (struct reg_set **) gmalloc (n);
  memset ((char *) reg_set_table, 0, n);

  gcc_obstack_init (&reg_set_obstack);
}

static void
free_reg_set_mem ()
{
  free (reg_set_table);
  obstack_free (&reg_set_obstack, NULL);
}

/* Record REGNO in the reg_set table.  */

static void
record_one_set (regno, insn)
     int regno;
     rtx insn;
{
  /* Allocate a new reg_set element and link it onto the list.  */
  struct reg_set *new_reg_info;

  /* If the table isn't big enough, enlarge it.  */
  if (regno >= reg_set_table_size)
    {
      int new_size = regno + REG_SET_TABLE_SLOP;

      reg_set_table
	= (struct reg_set **) grealloc ((char *) reg_set_table,
					new_size * sizeof (struct reg_set *));
      memset ((char *) (reg_set_table + reg_set_table_size), 0,
	      (new_size - reg_set_table_size) * sizeof (struct reg_set *));
      reg_set_table_size = new_size;
    }

  new_reg_info = (struct reg_set *) obstack_alloc (&reg_set_obstack,
						   sizeof (struct reg_set));
  bytes_used += sizeof (struct reg_set);
  new_reg_info->insn = insn;
  new_reg_info->next = reg_set_table[regno];
  reg_set_table[regno] = new_reg_info;
}

/* Called from compute_sets via note_stores to handle one SET or CLOBBER in
   an insn.  The DATA is really the instruction in which the SET is
   occurring.  */

static void
record_set_info (dest, setter, data)
     rtx dest, setter ATTRIBUTE_UNUSED;
     void *data;
{
  rtx record_set_insn = (rtx) data;

  if (GET_CODE (dest) == REG && REGNO (dest) >= FIRST_PSEUDO_REGISTER)
    record_one_set (REGNO (dest), record_set_insn);
}

/* Scan the function and record each set of each pseudo-register.

   This is called once, at the start of the gcse pass.  See the comments for
   `reg_set_table' for further documenation.  */

static void
compute_sets (f)
     rtx f;
{
  rtx insn;

  for (insn = f; insn != 0; insn = NEXT_INSN (insn))
    if (INSN_P (insn))
      note_stores (PATTERN (insn), record_set_info, insn);
}

/* Hash table support.  */

struct reg_avail_info
{
  basic_block last_bb;
  int first_set;
  int last_set;
};

static struct reg_avail_info *reg_avail_info;
static basic_block current_bb;


/* See whether X, the source of a set, is something we want to consider for
   GCSE.  */

static GTY(()) rtx test_insn;
static int
want_to_gcse_p (x)
     rtx x;
{
  int num_clobbers = 0;
  int icode;

  switch (GET_CODE (x))
    {
    case REG:
    case SUBREG:
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST_VECTOR:
    case CALL:
      return 0;

    default:
      break;
    }

  /* If this is a valid operand, we are OK.  If it's VOIDmode, we aren't.  */
  if (general_operand (x, GET_MODE (x)))
    return 1;
  else if (GET_MODE (x) == VOIDmode)
    return 0;

  /* Otherwise, check if we can make a valid insn from it.  First initialize
     our test insn if we haven't already.  */
  if (test_insn == 0)
    {
      test_insn
	= make_insn_raw (gen_rtx_SET (VOIDmode,
				      gen_rtx_REG (word_mode,
						   FIRST_PSEUDO_REGISTER * 2),
				      const0_rtx));
      NEXT_INSN (test_insn) = PREV_INSN (test_insn) = 0;
    }

  /* Now make an insn like the one we would make when GCSE'ing and see if
     valid.  */
  PUT_MODE (SET_DEST (PATTERN (test_insn)), GET_MODE (x));
  SET_SRC (PATTERN (test_insn)) = x;
  return ((icode = recog (PATTERN (test_insn), test_insn, &num_clobbers)) >= 0
	  && (num_clobbers == 0 || ! added_clobbers_hard_reg_p (icode)));
}

/* Return nonzero if the operands of expression X are unchanged from the
   start of INSN's basic block up to but not including INSN (if AVAIL_P == 0),
   or from INSN to the end of INSN's basic block (if AVAIL_P != 0).  */

static int
oprs_unchanged_p (x, insn, avail_p)
     rtx x, insn;
     int avail_p;
{
  int i, j;
  enum rtx_code code;
  const char *fmt;

  if (x == 0)
    return 1;

  code = GET_CODE (x);
  switch (code)
    {
    case REG:
      {
	struct reg_avail_info *info = &reg_avail_info[REGNO (x)];

	if (info->last_bb != current_bb)
	  return 1;
	if (avail_p)
	  return info->last_set < INSN_CUID (insn);
	else
	  return info->first_set >= INSN_CUID (insn);
      }

    case MEM:
      if (load_killed_in_block_p (current_bb, INSN_CUID (insn),
				  x, avail_p))
	return 0;
      else
	return oprs_unchanged_p (XEXP (x, 0), insn, avail_p);

    case PRE_DEC:
    case PRE_INC:
    case POST_DEC:
    case POST_INC:
    case PRE_MODIFY:
    case POST_MODIFY:
      return 0;

    case PC:
    case CC0: /*FIXME*/
    case CONST:
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST_VECTOR:
    case SYMBOL_REF:
    case LABEL_REF:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
      return 1;

    default:
      break;
    }

  for (i = GET_RTX_LENGTH (code) - 1, fmt = GET_RTX_FORMAT (code); i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  /* If we are about to do the last recursive call needed at this
	     level, change it into iteration.  This function is called enough
	     to be worth it.  */
	  if (i == 0)
	    return oprs_unchanged_p (XEXP (x, i), insn, avail_p);

	  else if (! oprs_unchanged_p (XEXP (x, i), insn, avail_p))
	    return 0;
	}
      else if (fmt[i] == 'E')
	for (j = 0; j < XVECLEN (x, i); j++)
	  if (! oprs_unchanged_p (XVECEXP (x, i, j), insn, avail_p))
	    return 0;
    }

  return 1;
}

/* Used for communication between mems_conflict_for_gcse_p and
   load_killed_in_block_p.  Nonzero if mems_conflict_for_gcse_p finds a
   conflict between two memory references.  */
static int gcse_mems_conflict_p;

/* Used for communication between mems_conflict_for_gcse_p and
   load_killed_in_block_p.  A memory reference for a load instruction,
   mems_conflict_for_gcse_p will see if a memory store conflicts with
   this memory load.  */
static rtx gcse_mem_operand;

/* DEST is the output of an instruction.  If it is a memory reference, and
   possibly conflicts with the load found in gcse_mem_operand, then set
   gcse_mems_conflict_p to a nonzero value.  */

static void
mems_conflict_for_gcse_p (dest, setter, data)
     rtx dest, setter ATTRIBUTE_UNUSED;
     void *data ATTRIBUTE_UNUSED;
{
  while (GET_CODE (dest) == SUBREG
	 || GET_CODE (dest) == ZERO_EXTRACT
	 || GET_CODE (dest) == SIGN_EXTRACT
	 || GET_CODE (dest) == STRICT_LOW_PART)
    dest = XEXP (dest, 0);

  /* If DEST is not a MEM, then it will not conflict with the load.  Note
     that function calls are assumed to clobber memory, but are handled
     elsewhere.  */
  if (GET_CODE (dest) != MEM)
    return;

  /* If we are setting a MEM in our list of specially recognized MEMs,
     don't mark as killed this time.  */

  if (dest == gcse_mem_operand && pre_ldst_mems != NULL)
    {
      if (!find_rtx_in_ldst (dest))
	gcse_mems_conflict_p = 1;
      return;
    }

  if (true_dependence (dest, GET_MODE (dest), gcse_mem_operand,
		       rtx_addr_varies_p))
    gcse_mems_conflict_p = 1;
}

/* Return nonzero if the expression in X (a memory reference) is killed
   in block BB before or after the insn with the CUID in UID_LIMIT.
   AVAIL_P is nonzero for kills after UID_LIMIT, and zero for kills
   before UID_LIMIT.

   To check the entire block, set UID_LIMIT to max_uid + 1 and
   AVAIL_P to 0.  */

static int
load_killed_in_block_p (bb, uid_limit, x, avail_p)
     basic_block bb;
     int uid_limit;
     rtx x;
     int avail_p;
{
  rtx list_entry = modify_mem_list[bb->index];
  while (list_entry)
    {
      rtx setter;
      /* Ignore entries in the list that do not apply.  */
      if ((avail_p
	   && INSN_CUID (XEXP (list_entry, 0)) < uid_limit)
	  || (! avail_p
	      && INSN_CUID (XEXP (list_entry, 0)) > uid_limit))
	{
	  list_entry = XEXP (list_entry, 1);
	  continue;
	}

      setter = XEXP (list_entry, 0);

      /* If SETTER is a call everything is clobbered.  Note that calls
	 to pure functions are never put on the list, so we need not
	 worry about them.  */
      if (GET_CODE (setter) == CALL_INSN)
	return 1;

      /* SETTER must be an INSN of some kind that sets memory.  Call
	 note_stores to examine each hunk of memory that is modified.

	 The note_stores interface is pretty limited, so we have to
	 communicate via global variables.  Yuk.  */
      gcse_mem_operand = x;
      gcse_mems_conflict_p = 0;
      note_stores (PATTERN (setter), mems_conflict_for_gcse_p, NULL);
      if (gcse_mems_conflict_p)
	return 1;
      list_entry = XEXP (list_entry, 1);
    }
  return 0;
}

/* Return nonzero if the operands of expression X are unchanged from
   the start of INSN's basic block up to but not including INSN.  */

static int
oprs_anticipatable_p (x, insn)
     rtx x, insn;
{
  return oprs_unchanged_p (x, insn, 0);
}

/* Return nonzero if the operands of expression X are unchanged from
   INSN to the end of INSN's basic block.  */

static int
oprs_available_p (x, insn)
     rtx x, insn;
{
  return oprs_unchanged_p (x, insn, 1);
}

/* Hash expression X.

   MODE is only used if X is a CONST_INT.  DO_NOT_RECORD_P is a boolean
   indicating if a volatile operand is found or if the expression contains
   something we don't want to insert in the table.

   ??? One might want to merge this with canon_hash.  Later.  */

static unsigned int
hash_expr (x, mode, do_not_record_p, hash_table_size)
     rtx x;
     enum machine_mode mode;
     int *do_not_record_p;
     int hash_table_size;
{
  unsigned int hash;

  *do_not_record_p = 0;

  hash = hash_expr_1 (x, mode, do_not_record_p);
  return hash % hash_table_size;
}

/* Hash a string.  Just add its bytes up.  */

static inline unsigned
hash_string_1 (ps)
     const char *ps;
{
  unsigned hash = 0;
  const unsigned char *p = (const unsigned char *) ps;

  if (p)
    while (*p)
      hash += *p++;

  return hash;
}

/* Subroutine of hash_expr to do the actual work.  */

static unsigned int
hash_expr_1 (x, mode, do_not_record_p)
     rtx x;
     enum machine_mode mode;
     int *do_not_record_p;
{
  int i, j;
  unsigned hash = 0;
  enum rtx_code code;
  const char *fmt;

  /* Used to turn recursion into iteration.  We can't rely on GCC's
     tail-recursion eliminatio since we need to keep accumulating values
     in HASH.  */

  if (x == 0)
    return hash;

 repeat:
  code = GET_CODE (x);
  switch (code)
    {
    case REG:
      hash += ((unsigned int) REG << 7) + REGNO (x);
      return hash;

    case CONST_INT:
      hash += (((unsigned int) CONST_INT << 7) + (unsigned int) mode
	       + (unsigned int) INTVAL (x));
      return hash;

    case CONST_DOUBLE:
      /* This is like the general case, except that it only counts
	 the integers representing the constant.  */
      hash += (unsigned int) code + (unsigned int) GET_MODE (x);
      if (GET_MODE (x) != VOIDmode)
	for (i = 2; i < GET_RTX_LENGTH (CONST_DOUBLE); i++)
	  hash += (unsigned int) XWINT (x, i);
      else
	hash += ((unsigned int) CONST_DOUBLE_LOW (x)
		 + (unsigned int) CONST_DOUBLE_HIGH (x));
      return hash;

    case CONST_VECTOR:
      {
	int units;
	rtx elt;

	units = CONST_VECTOR_NUNITS (x);

	for (i = 0; i < units; ++i)
	  {
	    elt = CONST_VECTOR_ELT (x, i);
	    hash += hash_expr_1 (elt, GET_MODE (elt), do_not_record_p);
	  }

	return hash;
      }

      /* Assume there is only one rtx object for any given label.  */
    case LABEL_REF:
      /* We don't hash on the address of the CODE_LABEL to avoid bootstrap
	 differences and differences between each stage's debugging dumps.  */
      hash += (((unsigned int) LABEL_REF << 7)
	       + CODE_LABEL_NUMBER (XEXP (x, 0)));
      return hash;

    case SYMBOL_REF:
      {
	/* Don't hash on the symbol's address to avoid bootstrap differences.
	   Different hash values may cause expressions to be recorded in
	   different orders and thus different registers to be used in the
	   final assembler.  This also avoids differences in the dump files
	   between various stages.  */
	unsigned int h = 0;
	const unsigned char *p = (const unsigned char *) XSTR (x, 0);

	while (*p)
	  h += (h << 7) + *p++; /* ??? revisit */

	hash += ((unsigned int) SYMBOL_REF << 7) + h;
	return hash;
      }

    case MEM:
      if (MEM_VOLATILE_P (x))
	{
	  *do_not_record_p = 1;
	  return 0;
	}

      hash += (unsigned int) MEM;
      /* We used alias set for hashing, but this is not good, since the alias
	 set may differ in -fprofile-arcs and -fbranch-probabilities compilation
	 causing the profiles to fail to match.  */
      x = XEXP (x, 0);
      goto repeat;

    case PRE_DEC:
    case PRE_INC:
    case POST_DEC:
    case POST_INC:
    case PC:
    case CC0:
    case CALL:
    case UNSPEC_VOLATILE:
      *do_not_record_p = 1;
      return 0;

    case ASM_OPERANDS:
      if (MEM_VOLATILE_P (x))
	{
	  *do_not_record_p = 1;
	  return 0;
	}
      else
	{
	  /* We don't want to take the filename and line into account.  */
	  hash += (unsigned) code + (unsigned) GET_MODE (x)
	    + hash_string_1 (ASM_OPERANDS_TEMPLATE (x))
	    + hash_string_1 (ASM_OPERANDS_OUTPUT_CONSTRAINT (x))
	    + (unsigned) ASM_OPERANDS_OUTPUT_IDX (x);

	  if (ASM_OPERANDS_INPUT_LENGTH (x))
	    {
	      for (i = 1; i < ASM_OPERANDS_INPUT_LENGTH (x); i++)
		{
		  hash += (hash_expr_1 (ASM_OPERANDS_INPUT (x, i),
					GET_MODE (ASM_OPERANDS_INPUT (x, i)),
					do_not_record_p)
			   + hash_string_1 (ASM_OPERANDS_INPUT_CONSTRAINT
					    (x, i)));
		}

	      hash += hash_string_1 (ASM_OPERANDS_INPUT_CONSTRAINT (x, 0));
	      x = ASM_OPERANDS_INPUT (x, 0);
	      mode = GET_MODE (x);
	      goto repeat;
	    }
	  return hash;
	}

    default:
      break;
    }

  hash += (unsigned) code + (unsigned) GET_MODE (x);
  for (i = GET_RTX_LENGTH (code) - 1, fmt = GET_RTX_FORMAT (code); i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  /* If we are about to do the last recursive call
	     needed at this level, change it into iteration.
	     This function is called enough to be worth it.  */
	  if (i == 0)
	    {
	      x = XEXP (x, i);
	      goto repeat;
	    }

	  hash += hash_expr_1 (XEXP (x, i), 0, do_not_record_p);
	  if (*do_not_record_p)
	    return 0;
	}

      else if (fmt[i] == 'E')
	for (j = 0; j < XVECLEN (x, i); j++)
	  {
	    hash += hash_expr_1 (XVECEXP (x, i, j), 0, do_not_record_p);
	    if (*do_not_record_p)
	      return 0;
	  }

      else if (fmt[i] == 's')
	hash += hash_string_1 (XSTR (x, i));
      else if (fmt[i] == 'i')
	hash += (unsigned int) XINT (x, i);
      else
	abort ();
    }

  return hash;
}

/* Hash a set of register REGNO.

   Sets are hashed on the register that is set.  This simplifies the PRE copy
   propagation code.

   ??? May need to make things more elaborate.  Later, as necessary.  */

static unsigned int
hash_set (regno, hash_table_size)
     int regno;
     int hash_table_size;
{
  unsigned int hash;

  hash = regno;
  return hash % hash_table_size;
}

/* Return nonzero if exp1 is equivalent to exp2.
   ??? Borrowed from cse.c.  Might want to remerge with cse.c.  Later.  */

static int
expr_equiv_p (x, y)
     rtx x, y;
{
  int i, j;
  enum rtx_code code;
  const char *fmt;

  if (x == y)
    return 1;

  if (x == 0 || y == 0)
    return x == y;

  code = GET_CODE (x);
  if (code != GET_CODE (y))
    return 0;

  /* (MULT:SI x y) and (MULT:HI x y) are NOT equivalent.  */
  if (GET_MODE (x) != GET_MODE (y))
    return 0;

  switch (code)
    {
    case PC:
    case CC0:
      return x == y;

    case CONST_INT:
      return INTVAL (x) == INTVAL (y);

    case LABEL_REF:
      return XEXP (x, 0) == XEXP (y, 0);

    case SYMBOL_REF:
      return XSTR (x, 0) == XSTR (y, 0);

    case REG:
      return REGNO (x) == REGNO (y);

    case MEM:
      /* Can't merge two expressions in different alias sets, since we can
	 decide that the expression is transparent in a block when it isn't,
	 due to it being set with the different alias set.  */
      if (MEM_ALIAS_SET (x) != MEM_ALIAS_SET (y))
	return 0;
      break;

    /*  For commutative operations, check both orders.  */
    case PLUS:
    case MULT:
    case AND:
    case IOR:
    case XOR:
    case NE:
    case EQ:
      return ((expr_equiv_p (XEXP (x, 0), XEXP (y, 0))
	       && expr_equiv_p (XEXP (x, 1), XEXP (y, 1)))
	      || (expr_equiv_p (XEXP (x, 0), XEXP (y, 1))
		  && expr_equiv_p (XEXP (x, 1), XEXP (y, 0))));

    case ASM_OPERANDS:
      /* We don't use the generic code below because we want to
	 disregard filename and line numbers.  */

      /* A volatile asm isn't equivalent to any other.  */
      if (MEM_VOLATILE_P (x) || MEM_VOLATILE_P (y))
	return 0;

      if (GET_MODE (x) != GET_MODE (y)
	  || strcmp (ASM_OPERANDS_TEMPLATE (x), ASM_OPERANDS_TEMPLATE (y))
	  || strcmp (ASM_OPERANDS_OUTPUT_CONSTRAINT (x),
		     ASM_OPERANDS_OUTPUT_CONSTRAINT (y))
	  || ASM_OPERANDS_OUTPUT_IDX (x) != ASM_OPERANDS_OUTPUT_IDX (y)
	  || ASM_OPERANDS_INPUT_LENGTH (x) != ASM_OPERANDS_INPUT_LENGTH (y))
	return 0;

      if (ASM_OPERANDS_INPUT_LENGTH (x))
	{
	  for (i = ASM_OPERANDS_INPUT_LENGTH (x) - 1; i >= 0; i--)
	    if (! expr_equiv_p (ASM_OPERANDS_INPUT (x, i),
				ASM_OPERANDS_INPUT (y, i))
		|| strcmp (ASM_OPERANDS_INPUT_CONSTRAINT (x, i),
			   ASM_OPERANDS_INPUT_CONSTRAINT (y, i)))
	      return 0;
	}

      return 1;

    default:
      break;
    }

  /* Compare the elements.  If any pair of corresponding elements
     fail to match, return 0 for the whole thing.  */

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      switch (fmt[i])
	{
	case 'e':
	  if (! expr_equiv_p (XEXP (x, i), XEXP (y, i)))
	    return 0;
	  break;

	case 'E':
	  if (XVECLEN (x, i) != XVECLEN (y, i))
	    return 0;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    if (! expr_equiv_p (XVECEXP (x, i, j), XVECEXP (y, i, j)))
	      return 0;
	  break;

	case 's':
	  if (strcmp (XSTR (x, i), XSTR (y, i)))
	    return 0;
	  break;

	case 'i':
	  if (XINT (x, i) != XINT (y, i))
	    return 0;
	  break;

	case 'w':
	  if (XWINT (x, i) != XWINT (y, i))
	    return 0;
	break;

	case '0':
	  break;

	default:
	  abort ();
	}
    }

  return 1;
}

/* Insert expression X in INSN in the hash TABLE.
   If it is already present, record it as the last occurrence in INSN's
   basic block.

   MODE is the mode of the value X is being stored into.
   It is only used if X is a CONST_INT.

   ANTIC_P is nonzero if X is an anticipatable expression.
   AVAIL_P is nonzero if X is an available expression.  */

static void
insert_expr_in_table (x, mode, insn, antic_p, avail_p, table)
     rtx x;
     enum machine_mode mode;
     rtx insn;
     int antic_p, avail_p;
     struct hash_table *table;
{
  int found, do_not_record_p;
  unsigned int hash;
  struct expr *cur_expr, *last_expr = NULL;
  struct occr *antic_occr, *avail_occr;
  struct occr *last_occr = NULL;

  hash = hash_expr (x, mode, &do_not_record_p, table->size);

  /* Do not insert expression in table if it contains volatile operands,
     or if hash_expr determines the expression is something we don't want
     to or can't handle.  */
  if (do_not_record_p)
    return;

  cur_expr = table->table[hash];
  found = 0;

  while (cur_expr && 0 == (found = expr_equiv_p (cur_expr->expr, x)))
    {
      /* If the expression isn't found, save a pointer to the end of
	 the list.  */
      last_expr = cur_expr;
      cur_expr = cur_expr->next_same_hash;
    }

  if (! found)
    {
      cur_expr = (struct expr *) gcse_alloc (sizeof (struct expr));
      bytes_used += sizeof (struct expr);
      if (table->table[hash] == NULL)
	/* This is the first pattern that hashed to this index.  */
	table->table[hash] = cur_expr;
      else
	/* Add EXPR to end of this hash chain.  */
	last_expr->next_same_hash = cur_expr;

      /* Set the fields of the expr element.  */
      cur_expr->expr = x;
      cur_expr->bitmap_index = table->n_elems++;
      cur_expr->next_same_hash = NULL;
      cur_expr->antic_occr = NULL;
      cur_expr->avail_occr = NULL;
    }

  /* Now record the occurrence(s).  */
  if (antic_p)
    {
      antic_occr = cur_expr->antic_occr;

      /* Search for another occurrence in the same basic block.  */
      while (antic_occr && BLOCK_NUM (antic_occr->insn) != BLOCK_NUM (insn))
	{
	  /* If an occurrence isn't found, save a pointer to the end of
	     the list.  */
	  last_occr = antic_occr;
	  antic_occr = antic_occr->next;
	}

      if (antic_occr)
	/* Found another instance of the expression in the same basic block.
	   Prefer the currently recorded one.  We want the first one in the
	   block and the block is scanned from start to end.  */
	; /* nothing to do */
      else
	{
	  /* First occurrence of this expression in this basic block.  */
	  antic_occr = (struct occr *) gcse_alloc (sizeof (struct occr));
	  bytes_used += sizeof (struct occr);
	  /* First occurrence of this expression in any block?  */
	  if (cur_expr->antic_occr == NULL)
	    cur_expr->antic_occr = antic_occr;
	  else
	    last_occr->next = antic_occr;

	  antic_occr->insn = insn;
	  antic_occr->next = NULL;
	}
    }

  if (avail_p)
    {
      avail_occr = cur_expr->avail_occr;

      /* Search for another occurrence in the same basic block.  */
      while (avail_occr && BLOCK_NUM (avail_occr->insn) != BLOCK_NUM (insn))
	{
	  /* If an occurrence isn't found, save a pointer to the end of
	     the list.  */
	  last_occr = avail_occr;
	  avail_occr = avail_occr->next;
	}

      if (avail_occr)
	/* Found another instance of the expression in the same basic block.
	   Prefer this occurrence to the currently recorded one.  We want
	   the last one in the block and the block is scanned from start
	   to end.  */
	avail_occr->insn = insn;
      else
	{
	  /* First occurrence of this expression in this basic block.  */
	  avail_occr = (struct occr *) gcse_alloc (sizeof (struct occr));
	  bytes_used += sizeof (struct occr);

	  /* First occurrence of this expression in any block?  */
	  if (cur_expr->avail_occr == NULL)
	    cur_expr->avail_occr = avail_occr;
	  else
	    last_occr->next = avail_occr;

	  avail_occr->insn = insn;
	  avail_occr->next = NULL;
	}
    }
}

/* Insert pattern X in INSN in the hash table.
   X is a SET of a reg to either another reg or a constant.
   If it is already present, record it as the last occurrence in INSN's
   basic block.  */

static void
insert_set_in_table (x, insn, table)
     rtx x;
     rtx insn;
     struct hash_table *table;
{
  int found;
  unsigned int hash;
  struct expr *cur_expr, *last_expr = NULL;
  struct occr *cur_occr, *last_occr = NULL;

  if (GET_CODE (x) != SET
      || GET_CODE (SET_DEST (x)) != REG)
    abort ();

  hash = hash_set (REGNO (SET_DEST (x)), table->size);

  cur_expr = table->table[hash];
  found = 0;

  while (cur_expr && 0 == (found = expr_equiv_p (cur_expr->expr, x)))
    {
      /* If the expression isn't found, save a pointer to the end of
	 the list.  */
      last_expr = cur_expr;
      cur_expr = cur_expr->next_same_hash;
    }

  if (! found)
    {
      cur_expr = (struct expr *) gcse_alloc (sizeof (struct expr));
      bytes_used += sizeof (struct expr);
      if (table->table[hash] == NULL)
	/* This is the first pattern that hashed to this index.  */
	table->table[hash] = cur_expr;
      else
	/* Add EXPR to end of this hash chain.  */
	last_expr->next_same_hash = cur_expr;

      /* Set the fields of the expr element.
	 We must copy X because it can be modified when copy propagation is
	 performed on its operands.  */
      cur_expr->expr = copy_rtx (x);
      cur_expr->bitmap_index = table->n_elems++;
      cur_expr->next_same_hash = NULL;
      cur_expr->antic_occr = NULL;
      cur_expr->avail_occr = NULL;
    }

  /* Now record the occurrence.  */
  cur_occr = cur_expr->avail_occr;

  /* Search for another occurrence in the same basic block.  */
  while (cur_occr && BLOCK_NUM (cur_occr->insn) != BLOCK_NUM (insn))
    {
      /* If an occurrence isn't found, save a pointer to the end of
	 the list.  */
      last_occr = cur_occr;
      cur_occr = cur_occr->next;
    }

  if (cur_occr)
    /* Found another instance of the expression in the same basic block.
       Prefer this occurrence to the currently recorded one.  We want the
       last one in the block and the block is scanned from start to end.  */
    cur_occr->insn = insn;
  else
    {
      /* First occurrence of this expression in this basic block.  */
      cur_occr = (struct occr *) gcse_alloc (sizeof (struct occr));
      bytes_used += sizeof (struct occr);

      /* First occurrence of this expression in any block?  */
      if (cur_expr->avail_occr == NULL)
	cur_expr->avail_occr = cur_occr;
      else
	last_occr->next = cur_occr;

      cur_occr->insn = insn;
      cur_occr->next = NULL;
    }
}

/* Scan pattern PAT of INSN and add an entry to the hash TABLE (set or
   expression one).  */

static void
hash_scan_set (pat, insn, table)
     rtx pat, insn;
     struct hash_table *table;
{
  rtx src = SET_SRC (pat);
  rtx dest = SET_DEST (pat);
  rtx note;

  if (GET_CODE (src) == CALL)
    hash_scan_call (src, insn, table);

  else if (GET_CODE (dest) == REG)
    {
      unsigned int regno = REGNO (dest);
      rtx tmp;

      /* If this is a single set and we are doing constant propagation,
	 see if a REG_NOTE shows this equivalent to a constant.  */
      if (table->set_p && (note = find_reg_equal_equiv_note (insn)) != 0
	  && CONSTANT_P (XEXP (note, 0)))
	src = XEXP (note, 0), pat = gen_rtx_SET (VOIDmode, dest, src);

      /* Only record sets of pseudo-regs in the hash table.  */
      if (! table->set_p
	  && regno >= FIRST_PSEUDO_REGISTER
	  /* Don't GCSE something if we can't do a reg/reg copy.  */
	  && can_copy_p [GET_MODE (dest)]
	  /* GCSE commonly inserts instruction after the insn.  We can't
	     do that easily for EH_REGION notes so disable GCSE on these
	     for now.  */
	  && !find_reg_note (insn, REG_EH_REGION, NULL_RTX)
	  /* Is SET_SRC something we want to gcse?  */
	  && want_to_gcse_p (src)
	  /* Don't CSE a nop.  */
	  && ! set_noop_p (pat)
	  /* Don't GCSE if it has attached REG_EQUIV note.
	     At this point this only function parameters should have
	     REG_EQUIV notes and if the argument slot is used somewhere
	     explicitly, it means address of parameter has been taken,
	     so we should not extend the lifetime of the pseudo.  */
	  && ((note = find_reg_note (insn, REG_EQUIV, NULL_RTX)) == 0
	      || GET_CODE (XEXP (note, 0)) != MEM))
	{
	  /* An expression is not anticipatable if its operands are
	     modified before this insn or if this is not the only SET in
	     this insn.  */
	  int antic_p = oprs_anticipatable_p (src, insn) && single_set (insn);
	  /* An expression is not available if its operands are
	     subsequently modified, including this insn.  It's also not
	     available if this is a branch, because we can't insert
	     a set after the branch.  */
	  int avail_p = (oprs_available_p (src, insn)
			 && ! JUMP_P (insn));

	  insert_expr_in_table (src, GET_MODE (dest), insn, antic_p, avail_p, table);
	}

      /* Record sets for constant/copy propagation.  */
      else if (table->set_p
	       && regno >= FIRST_PSEUDO_REGISTER
	       && ((GET_CODE (src) == REG
		    && REGNO (src) >= FIRST_PSEUDO_REGISTER
		    && can_copy_p [GET_MODE (dest)]
		    && REGNO (src) != regno)
		   || CONSTANT_P (src))
	       /* A copy is not available if its src or dest is subsequently
		  modified.  Here we want to search from INSN+1 on, but
		  oprs_available_p searches from INSN on.  */
	       && (insn == BLOCK_END (BLOCK_NUM (insn))
		   || ((tmp = next_nonnote_insn (insn)) != NULL_RTX
		       && oprs_available_p (pat, tmp))))
	insert_set_in_table (pat, insn, table);
    }
}

static void
hash_scan_clobber (x, insn, table)
     rtx x ATTRIBUTE_UNUSED, insn ATTRIBUTE_UNUSED;
     struct hash_table *table ATTRIBUTE_UNUSED;
{
  /* Currently nothing to do.  */
}

static void
hash_scan_call (x, insn, table)
     rtx x ATTRIBUTE_UNUSED, insn ATTRIBUTE_UNUSED;
     struct hash_table *table ATTRIBUTE_UNUSED;
{
  /* Currently nothing to do.  */
}

/* Process INSN and add hash table entries as appropriate.

   Only available expressions that set a single pseudo-reg are recorded.

   Single sets in a PARALLEL could be handled, but it's an extra complication
   that isn't dealt with right now.  The trick is handling the CLOBBERs that
   are also in the PARALLEL.  Later.

   If SET_P is nonzero, this is for the assignment hash table,
   otherwise it is for the expression hash table.
   If IN_LIBCALL_BLOCK nonzero, we are in a libcall block, and should
   not record any expressions.  */

static void
hash_scan_insn (insn, table, in_libcall_block)
     rtx insn;
     struct hash_table *table;
     int in_libcall_block;
{
  rtx pat = PATTERN (insn);
  int i;

  if (in_libcall_block)
    return;

  /* Pick out the sets of INSN and for other forms of instructions record
     what's been modified.  */

  if (GET_CODE (pat) == SET)
    hash_scan_set (pat, insn, table);
  else if (GET_CODE (pat) == PARALLEL)
    for (i = 0; i < XVECLEN (pat, 0); i++)
      {
	rtx x = XVECEXP (pat, 0, i);

	if (GET_CODE (x) == SET)
	  hash_scan_set (x, insn, table);
	else if (GET_CODE (x) == CLOBBER)
	  hash_scan_clobber (x, insn, table);
	else if (GET_CODE (x) == CALL)
	  hash_scan_call (x, insn, table);
      }

  else if (GET_CODE (pat) == CLOBBER)
    hash_scan_clobber (pat, insn, table);
  else if (GET_CODE (pat) == CALL)
    hash_scan_call (pat, insn, table);
}

static void
dump_hash_table (file, name, table)
     FILE *file;
     const char *name;
     struct hash_table *table;
{
  int i;
  /* Flattened out table, so it's printed in proper order.  */
  struct expr **flat_table;
  unsigned int *hash_val;
  struct expr *expr;

  flat_table
    = (struct expr **) xcalloc (table->n_elems, sizeof (struct expr *));
  hash_val = (unsigned int *) xmalloc (table->n_elems * sizeof (unsigned int));

  for (i = 0; i < (int) table->size; i++)
    for (expr = table->table[i]; expr != NULL; expr = expr->next_same_hash)
      {
	flat_table[expr->bitmap_index] = expr;
	hash_val[expr->bitmap_index] = i;
      }

  fprintf (file, "%s hash table (%d buckets, %d entries)\n",
	   name, table->size, table->n_elems);

  for (i = 0; i < (int) table->n_elems; i++)
    if (flat_table[i] != 0)
      {
	expr = flat_table[i];
	fprintf (file, "Index %d (hash value %d)\n  ",
		 expr->bitmap_index, hash_val[i]);
	print_rtl (file, expr->expr);
	fprintf (file, "\n");
      }

  fprintf (file, "\n");

  free (flat_table);
  free (hash_val);
}

/* Record register first/last/block set information for REGNO in INSN.

   first_set records the first place in the block where the register
   is set and is used to compute "anticipatability".

   last_set records the last place in the block where the register
   is set and is used to compute "availability".

   last_bb records the block for which first_set and last_set are
   valid, as a quick test to invalidate them.

   reg_set_in_block records whether the register is set in the block
   and is used to compute "transparency".  */

static void
record_last_reg_set_info (insn, regno)
     rtx insn;
     int regno;
{
  struct reg_avail_info *info = &reg_avail_info[regno];
  int cuid = INSN_CUID (insn);

  info->last_set = cuid;
  if (info->last_bb != current_bb)
    {
      info->last_bb = current_bb;
      info->first_set = cuid;
      SET_BIT (reg_set_in_block[current_bb->index], regno);
    }
}


/* Record all of the canonicalized MEMs of record_last_mem_set_info's insn.
   Note we store a pair of elements in the list, so they have to be
   taken off pairwise.  */

static void
canon_list_insert (dest, unused1, v_insn)
     rtx    dest ATTRIBUTE_UNUSED;
     rtx    unused1 ATTRIBUTE_UNUSED;
     void * v_insn;
{
  rtx dest_addr, insn;
  int bb;

  while (GET_CODE (dest) == SUBREG
      || GET_CODE (dest) == ZERO_EXTRACT
      || GET_CODE (dest) == SIGN_EXTRACT
      || GET_CODE (dest) == STRICT_LOW_PART)
    dest = XEXP (dest, 0);

  /* If DEST is not a MEM, then it will not conflict with a load.  Note
     that function calls are assumed to clobber memory, but are handled
     elsewhere.  */

  if (GET_CODE (dest) != MEM)
    return;

  dest_addr = get_addr (XEXP (dest, 0));
  dest_addr = canon_rtx (dest_addr);
  insn = (rtx) v_insn;
  bb = BLOCK_NUM (insn);

  canon_modify_mem_list[bb] =
    alloc_EXPR_LIST (VOIDmode, dest_addr, canon_modify_mem_list[bb]);
  canon_modify_mem_list[bb] =
    alloc_EXPR_LIST (VOIDmode, dest, canon_modify_mem_list[bb]);
  bitmap_set_bit (canon_modify_mem_list_set, bb);
}

/* Record memory modification information for INSN.  We do not actually care
   about the memory location(s) that are set, or even how they are set (consider
   a CALL_INSN).  We merely need to record which insns modify memory.  */

static void
record_last_mem_set_info (insn)
     rtx insn;
{
  int bb = BLOCK_NUM (insn);

  /* load_killed_in_block_p will handle the case of calls clobbering
     everything.  */
  modify_mem_list[bb] = alloc_INSN_LIST (insn, modify_mem_list[bb]);
  bitmap_set_bit (modify_mem_list_set, bb);

  if (GET_CODE (insn) == CALL_INSN)
    {
      /* Note that traversals of this loop (other than for free-ing)
	 will break after encountering a CALL_INSN.  So, there's no
	 need to insert a pair of items, as canon_list_insert does.  */
      canon_modify_mem_list[bb] =
	alloc_INSN_LIST (insn, canon_modify_mem_list[bb]);
      bitmap_set_bit (canon_modify_mem_list_set, bb);
    }
  else
    note_stores (PATTERN (insn), canon_list_insert, (void*) insn);
}

/* Called from compute_hash_table via note_stores to handle one
   SET or CLOBBER in an insn.  DATA is really the instruction in which
   the SET is taking place.  */

static void
record_last_set_info (dest, setter, data)
     rtx dest, setter ATTRIBUTE_UNUSED;
     void *data;
{
  rtx last_set_insn = (rtx) data;

  if (GET_CODE (dest) == SUBREG)
    dest = SUBREG_REG (dest);

  if (GET_CODE (dest) == REG)
    record_last_reg_set_info (last_set_insn, REGNO (dest));
  else if (GET_CODE (dest) == MEM
	   /* Ignore pushes, they clobber nothing.  */
	   && ! push_operand (dest, GET_MODE (dest)))
    record_last_mem_set_info (last_set_insn);
}

/* Top level function to create an expression or assignment hash table.

   Expression entries are placed in the hash table if
   - they are of the form (set (pseudo-reg) src),
   - src is something we want to perform GCSE on,
   - none of the operands are subsequently modified in the block

   Assignment entries are placed in the hash table if
   - they are of the form (set (pseudo-reg) src),
   - src is something we want to perform const/copy propagation on,
   - none of the operands or target are subsequently modified in the block

   Currently src must be a pseudo-reg or a const_int.

   F is the first insn.
   TABLE is the table computed.  */

static void
compute_hash_table_work (table)
     struct hash_table *table;
{
  unsigned int i;

  /* While we compute the hash table we also compute a bit array of which
     registers are set in which blocks.
     ??? This isn't needed during const/copy propagation, but it's cheap to
     compute.  Later.  */
  sbitmap_vector_zero (reg_set_in_block, last_basic_block);

  /* re-Cache any INSN_LIST nodes we have allocated.  */
  clear_modify_mem_tables ();
  /* Some working arrays used to track first and last set in each block.  */
  reg_avail_info = (struct reg_avail_info*)
    gmalloc (max_gcse_regno * sizeof (struct reg_avail_info));

  for (i = 0; i < max_gcse_regno; ++i)
    reg_avail_info[i].last_bb = NULL;

  FOR_EACH_BB (current_bb)
    {
      rtx insn;
      unsigned int regno;
      int in_libcall_block;

      /* First pass over the instructions records information used to
	 determine when registers and memory are first and last set.
	 ??? hard-reg reg_set_in_block computation
	 could be moved to compute_sets since they currently don't change.  */

      for (insn = current_bb->head;
	   insn && insn != NEXT_INSN (current_bb->end);
	   insn = NEXT_INSN (insn))
	{
	  if (! INSN_P (insn))
	    continue;

	  if (GET_CODE (insn) == CALL_INSN)
	    {
	      bool clobbers_all = false;
#ifdef NON_SAVING_SETJMP
	      if (NON_SAVING_SETJMP
		  && find_reg_note (insn, REG_SETJMP, NULL_RTX))
		clobbers_all = true;
#endif

	      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
		if (clobbers_all
		    || TEST_HARD_REG_BIT (regs_invalidated_by_call, regno))
		  record_last_reg_set_info (insn, regno);

	      mark_call (insn);
	    }

	  note_stores (PATTERN (insn), record_last_set_info, insn);
	}

      /* The next pass builds the hash table.  */

      for (insn = current_bb->head, in_libcall_block = 0;
	   insn && insn != NEXT_INSN (current_bb->end);
	   insn = NEXT_INSN (insn))
	if (INSN_P (insn))
	  {
	    if (find_reg_note (insn, REG_LIBCALL, NULL_RTX))
	      in_libcall_block = 1;
	    else if (table->set_p && find_reg_note (insn, REG_RETVAL, NULL_RTX))
	      in_libcall_block = 0;
	    hash_scan_insn (insn, table, in_libcall_block);
	    if (!table->set_p && find_reg_note (insn, REG_RETVAL, NULL_RTX))
	      in_libcall_block = 0;
	  }
    }

  free (reg_avail_info);
  reg_avail_info = NULL;
}

/* Allocate space for the set/expr hash TABLE.
   N_INSNS is the number of instructions in the function.
   It is used to determine the number of buckets to use.
   SET_P determines whether set or expression table will
   be created.  */

static void
alloc_hash_table (n_insns, table, set_p)
     int n_insns;
     struct hash_table *table;
     int set_p;
{
  int n;

  table->size = n_insns / 4;
  if (table->size < 11)
    table->size = 11;

  /* Attempt to maintain efficient use of hash table.
     Making it an odd number is simplest for now.
     ??? Later take some measurements.  */
  table->size |= 1;
  n = table->size * sizeof (struct expr *);
  table->table = (struct expr **) gmalloc (n);
  table->set_p = set_p;
}

/* Free things allocated by alloc_hash_table.  */

static void
free_hash_table (table)
     struct hash_table *table;
{
  free (table->table);
}

/* Compute the hash TABLE for doing copy/const propagation or
   expression hash table.  */

static void
compute_hash_table (table)
    struct hash_table *table;
{
  /* Initialize count of number of entries in hash table.  */
  table->n_elems = 0;
  memset ((char *) table->table, 0,
	  table->size * sizeof (struct expr *));

  compute_hash_table_work (table);
}

/* Expression tracking support.  */

/* Lookup pattern PAT in the expression TABLE.
   The result is a pointer to the table entry, or NULL if not found.  */

static struct expr *
lookup_expr (pat, table)
     rtx pat;
     struct hash_table *table;
{
  int do_not_record_p;
  unsigned int hash = hash_expr (pat, GET_MODE (pat), &do_not_record_p,
				 table->size);
  struct expr *expr;

  if (do_not_record_p)
    return NULL;

  expr = table->table[hash];

  while (expr && ! expr_equiv_p (expr->expr, pat))
    expr = expr->next_same_hash;

  return expr;
}

/* Lookup REGNO in the set TABLE.  If PAT is non-NULL look for the entry that
   matches it, otherwise return the first entry for REGNO.  The result is a
   pointer to the table entry, or NULL if not found.  */

static struct expr *
lookup_set (regno, pat, table)
     unsigned int regno;
     rtx pat;
     struct hash_table *table;
{
  unsigned int hash = hash_set (regno, table->size);
  struct expr *expr;

  expr = table->table[hash];

  if (pat)
    {
      while (expr && ! expr_equiv_p (expr->expr, pat))
	expr = expr->next_same_hash;
    }
  else
    {
      while (expr && REGNO (SET_DEST (expr->expr)) != regno)
	expr = expr->next_same_hash;
    }

  return expr;
}

/* Return the next entry for REGNO in list EXPR.  */

static struct expr *
next_set (regno, expr)
     unsigned int regno;
     struct expr *expr;
{
  do
    expr = expr->next_same_hash;
  while (expr && REGNO (SET_DEST (expr->expr)) != regno);

  return expr;
}

/* Like free_INSN_LIST_list or free_EXPR_LIST_list, except that the node
   types may be mixed.  */

static void
free_insn_expr_list_list (listp)
     rtx *listp;
{
  rtx list, next;

  for (list = *listp; list ; list = next)
    {
      next = XEXP (list, 1);
      if (GET_CODE (list) == EXPR_LIST)
	free_EXPR_LIST_node (list);
      else
	free_INSN_LIST_node (list);
    }

  *listp = NULL;
}

/* Clear canon_modify_mem_list and modify_mem_list tables.  */
static void
clear_modify_mem_tables ()
{
  int i;

  EXECUTE_IF_SET_IN_BITMAP
    (modify_mem_list_set, 0, i, free_INSN_LIST_list (modify_mem_list + i));
  bitmap_clear (modify_mem_list_set);

  EXECUTE_IF_SET_IN_BITMAP
    (canon_modify_mem_list_set, 0, i,
     free_insn_expr_list_list (canon_modify_mem_list + i));
  bitmap_clear (canon_modify_mem_list_set);
}

/* Release memory used by modify_mem_list_set and canon_modify_mem_list_set.  */

static void
free_modify_mem_tables ()
{
  clear_modify_mem_tables ();
  free (modify_mem_list);
  free (canon_modify_mem_list);
  modify_mem_list = 0;
  canon_modify_mem_list = 0;
}

/* Reset tables used to keep track of what's still available [since the
   start of the block].  */

static void
reset_opr_set_tables ()
{
  /* Maintain a bitmap of which regs have been set since beginning of
     the block.  */
  CLEAR_REG_SET (reg_set_bitmap);

  /* Also keep a record of the last instruction to modify memory.
     For now this is very trivial, we only record whether any memory
     location has been modified.  */
  clear_modify_mem_tables ();
}

/* Return nonzero if the operands of X are not set before INSN in
   INSN's basic block.  */

static int
oprs_not_set_p (x, insn)
     rtx x, insn;
{
  int i, j;
  enum rtx_code code;
  const char *fmt;

  if (x == 0)
    return 1;

  code = GET_CODE (x);
  switch (code)
    {
    case PC:
    case CC0:
    case CONST:
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST_VECTOR:
    case SYMBOL_REF:
    case LABEL_REF:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
      return 1;

    case MEM:
      if (load_killed_in_block_p (BLOCK_FOR_INSN (insn),
				  INSN_CUID (insn), x, 0))
	return 0;
      else
	return oprs_not_set_p (XEXP (x, 0), insn);

    case REG:
      return ! REGNO_REG_SET_P (reg_set_bitmap, REGNO (x));

    default:
      break;
    }

  for (i = GET_RTX_LENGTH (code) - 1, fmt = GET_RTX_FORMAT (code); i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  /* If we are about to do the last recursive call
	     needed at this level, change it into iteration.
	     This function is called enough to be worth it.  */
	  if (i == 0)
	    return oprs_not_set_p (XEXP (x, i), insn);

	  if (! oprs_not_set_p (XEXP (x, i), insn))
	    return 0;
	}
      else if (fmt[i] == 'E')
	for (j = 0; j < XVECLEN (x, i); j++)
	  if (! oprs_not_set_p (XVECEXP (x, i, j), insn))
	    return 0;
    }

  return 1;
}

/* Mark things set by a CALL.  */

static void
mark_call (insn)
     rtx insn;
{
  if (! CONST_OR_PURE_CALL_P (insn))
    record_last_mem_set_info (insn);
}

/* Mark things set by a SET.  */

static void
mark_set (pat, insn)
     rtx pat, insn;
{
  rtx dest = SET_DEST (pat);

  while (GET_CODE (dest) == SUBREG
	 || GET_CODE (dest) == ZERO_EXTRACT
	 || GET_CODE (dest) == SIGN_EXTRACT
	 || GET_CODE (dest) == STRICT_LOW_PART)
    dest = XEXP (dest, 0);

  if (GET_CODE (dest) == REG)
    SET_REGNO_REG_SET (reg_set_bitmap, REGNO (dest));
  else if (GET_CODE (dest) == MEM)
    record_last_mem_set_info (insn);

  if (GET_CODE (SET_SRC (pat)) == CALL)
    mark_call (insn);
}

/* Record things set by a CLOBBER.  */

static void
mark_clobber (pat, insn)
     rtx pat, insn;
{
  rtx clob = XEXP (pat, 0);

  while (GET_CODE (clob) == SUBREG || GET_CODE (clob) == STRICT_LOW_PART)
    clob = XEXP (clob, 0);

  if (GET_CODE (clob) == REG)
    SET_REGNO_REG_SET (reg_set_bitmap, REGNO (clob));
  else
    record_last_mem_set_info (insn);
}

/* Record things set by INSN.
   This data is used by oprs_not_set_p.  */

static void
mark_oprs_set (insn)
     rtx insn;
{
  rtx pat = PATTERN (insn);
  int i;

  if (GET_CODE (pat) == SET)
    mark_set (pat, insn);
  else if (GET_CODE (pat) == PARALLEL)
    for (i = 0; i < XVECLEN (pat, 0); i++)
      {
	rtx x = XVECEXP (pat, 0, i);

	if (GET_CODE (x) == SET)
	  mark_set (x, insn);
	else if (GET_CODE (x) == CLOBBER)
	  mark_clobber (x, insn);
	else if (GET_CODE (x) == CALL)
	  mark_call (insn);
      }

  else if (GET_CODE (pat) == CLOBBER)
    mark_clobber (pat, insn);
  else if (GET_CODE (pat) == CALL)
    mark_call (insn);
}


/* Classic GCSE reaching definition support.  */

/* Allocate reaching def variables.  */

static void
alloc_rd_mem (n_blocks, n_insns)
     int n_blocks, n_insns;
{
  rd_kill = (sbitmap *) sbitmap_vector_alloc (n_blocks, n_insns);
  sbitmap_vector_zero (rd_kill, n_blocks);

  rd_gen = (sbitmap *) sbitmap_vector_alloc (n_blocks, n_insns);
  sbitmap_vector_zero (rd_gen, n_blocks);

  reaching_defs = (sbitmap *) sbitmap_vector_alloc (n_blocks, n_insns);
  sbitmap_vector_zero (reaching_defs, n_blocks);

  rd_out = (sbitmap *) sbitmap_vector_alloc (n_blocks, n_insns);
  sbitmap_vector_zero (rd_out, n_blocks);
}

/* Free reaching def variables.  */

static void
free_rd_mem ()
{
  sbitmap_vector_free (rd_kill);
  sbitmap_vector_free (rd_gen);
  sbitmap_vector_free (reaching_defs);
  sbitmap_vector_free (rd_out);
}

/* Add INSN to the kills of BB.  REGNO, set in BB, is killed by INSN.  */

static void
handle_rd_kill_set (insn, regno, bb)
     rtx insn;
     int regno;
     basic_block bb;
{
  struct reg_set *this_reg;

  for (this_reg = reg_set_table[regno]; this_reg; this_reg = this_reg ->next)
    if (BLOCK_NUM (this_reg->insn) != BLOCK_NUM (insn))
      SET_BIT (rd_kill[bb->index], INSN_CUID (this_reg->insn));
}

/* Compute the set of kill's for reaching definitions.  */

static void
compute_kill_rd ()
{
  int cuid;
  unsigned int regno;
  int i;
  basic_block bb;

  /* For each block
       For each set bit in `gen' of the block (i.e each insn which
	   generates a definition in the block)
	 Call the reg set by the insn corresponding to that bit regx
	 Look at the linked list starting at reg_set_table[regx]
	 For each setting of regx in the linked list, which is not in
	     this block
	   Set the bit in `kill' corresponding to that insn.  */
  FOR_EACH_BB (bb)
    for (cuid = 0; cuid < max_cuid; cuid++)
      if (TEST_BIT (rd_gen[bb->index], cuid))
	{
	  rtx insn = CUID_INSN (cuid);
	  rtx pat = PATTERN (insn);

	  if (GET_CODE (insn) == CALL_INSN)
	    {
	      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
		if (TEST_HARD_REG_BIT (regs_invalidated_by_call, regno))
		  handle_rd_kill_set (insn, regno, bb);
	    }

	  if (GET_CODE (pat) == PARALLEL)
	    {
	      for (i = XVECLEN (pat, 0) - 1; i >= 0; i--)
		{
		  enum rtx_code code = GET_CODE (XVECEXP (pat, 0, i));

		  if ((code == SET || code == CLOBBER)
		      && GET_CODE (XEXP (XVECEXP (pat, 0, i), 0)) == REG)
		    handle_rd_kill_set (insn,
					REGNO (XEXP (XVECEXP (pat, 0, i), 0)),
					bb);
		}
	    }
	  else if (GET_CODE (pat) == SET && GET_CODE (SET_DEST (pat)) == REG)
	    /* Each setting of this register outside of this block
	       must be marked in the set of kills in this block.  */
	    handle_rd_kill_set (insn, REGNO (SET_DEST (pat)), bb);
	}
}

/* Compute the reaching definitions as in
   Compilers Principles, Techniques, and Tools. Aho, Sethi, Ullman,
   Chapter 10.  It is the same algorithm as used for computing available
   expressions but applied to the gens and kills of reaching definitions.  */

static void
compute_rd ()
{
  int changed, passes;
  basic_block bb;

  FOR_EACH_BB (bb)
    sbitmap_copy (rd_out[bb->index] /*dst*/, rd_gen[bb->index] /*src*/);

  passes = 0;
  changed = 1;
  while (changed)
    {
      changed = 0;
      FOR_EACH_BB (bb)
	{
	  sbitmap_union_of_preds (reaching_defs[bb->index], rd_out, bb->index);
	  changed |= sbitmap_union_of_diff_cg (rd_out[bb->index], rd_gen[bb->index],
					       reaching_defs[bb->index], rd_kill[bb->index]);
	}
      passes++;
    }

  if (gcse_file)
    fprintf (gcse_file, "reaching def computation: %d passes\n", passes);
}

/* Classic GCSE available expression support.  */

/* Allocate memory for available expression computation.  */

static void
alloc_avail_expr_mem (n_blocks, n_exprs)
     int n_blocks, n_exprs;
{
  ae_kill = (sbitmap *) sbitmap_vector_alloc (n_blocks, n_exprs);
  sbitmap_vector_zero (ae_kill, n_blocks);

  ae_gen = (sbitmap *) sbitmap_vector_alloc (n_blocks, n_exprs);
  sbitmap_vector_zero (ae_gen, n_blocks);

  ae_in = (sbitmap *) sbitmap_vector_alloc (n_blocks, n_exprs);
  sbitmap_vector_zero (ae_in, n_blocks);

  ae_out = (sbitmap *) sbitmap_vector_alloc (n_blocks, n_exprs);
  sbitmap_vector_zero (ae_out, n_blocks);
}

static void
free_avail_expr_mem ()
{
  sbitmap_vector_free (ae_kill);
  sbitmap_vector_free (ae_gen);
  sbitmap_vector_free (ae_in);
  sbitmap_vector_free (ae_out);
}

/* Compute the set of available expressions generated in each basic block.  */

static void
compute_ae_gen (expr_hash_table)
     struct hash_table *expr_hash_table;
{
  unsigned int i;
  struct expr *expr;
  struct occr *occr;

  /* For each recorded occurrence of each expression, set ae_gen[bb][expr].
     This is all we have to do because an expression is not recorded if it
     is not available, and the only expressions we want to work with are the
     ones that are recorded.  */
  for (i = 0; i < expr_hash_table->size; i++)
    for (expr = expr_hash_table->table[i]; expr != 0; expr = expr->next_same_hash)
      for (occr = expr->avail_occr; occr != 0; occr = occr->next)
	SET_BIT (ae_gen[BLOCK_NUM (occr->insn)], expr->bitmap_index);
}

/* Return nonzero if expression X is killed in BB.  */

static int
expr_killed_p (x, bb)
     rtx x;
     basic_block bb;
{
  int i, j;
  enum rtx_code code;
  const char *fmt;

  if (x == 0)
    return 1;

  code = GET_CODE (x);
  switch (code)
    {
    case REG:
      return TEST_BIT (reg_set_in_block[bb->index], REGNO (x));

    case MEM:
      if (load_killed_in_block_p (bb, get_max_uid () + 1, x, 0))
	return 1;
      else
	return expr_killed_p (XEXP (x, 0), bb);

    case PC:
    case CC0: /*FIXME*/
    case CONST:
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST_VECTOR:
    case SYMBOL_REF:
    case LABEL_REF:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
      return 0;

    default:
      break;
    }

  for (i = GET_RTX_LENGTH (code) - 1, fmt = GET_RTX_FORMAT (code); i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  /* If we are about to do the last recursive call
	     needed at this level, change it into iteration.
	     This function is called enough to be worth it.  */
	  if (i == 0)
	    return expr_killed_p (XEXP (x, i), bb);
	  else if (expr_killed_p (XEXP (x, i), bb))
	    return 1;
	}
      else if (fmt[i] == 'E')
	for (j = 0; j < XVECLEN (x, i); j++)
	  if (expr_killed_p (XVECEXP (x, i, j), bb))
	    return 1;
    }

  return 0;
}

/* Compute the set of available expressions killed in each basic block.  */

static void
compute_ae_kill (ae_gen, ae_kill, expr_hash_table)
     sbitmap *ae_gen, *ae_kill;
     struct hash_table *expr_hash_table;
{
  basic_block bb;
  unsigned int i;
  struct expr *expr;

  FOR_EACH_BB (bb)
    for (i = 0; i < expr_hash_table->size; i++)
      for (expr = expr_hash_table->table[i]; expr; expr = expr->next_same_hash)
	{
	  /* Skip EXPR if generated in this block.  */
	  if (TEST_BIT (ae_gen[bb->index], expr->bitmap_index))
	    continue;

	  if (expr_killed_p (expr->expr, bb))
	    SET_BIT (ae_kill[bb->index], expr->bitmap_index);
	}
}

/* Actually perform the Classic GCSE optimizations.  */

/* Return nonzero if occurrence OCCR of expression EXPR reaches block BB.

   CHECK_SELF_LOOP is nonzero if we should consider a block reaching itself
   as a positive reach.  We want to do this when there are two computations
   of the expression in the block.

   VISITED is a pointer to a working buffer for tracking which BB's have
   been visited.  It is NULL for the top-level call.

   We treat reaching expressions that go through blocks containing the same
   reaching expression as "not reaching".  E.g. if EXPR is generated in blocks
   2 and 3, INSN is in block 4, and 2->3->4, we treat the expression in block
   2 as not reaching.  The intent is to improve the probability of finding
   only one reaching expression and to reduce register lifetimes by picking
   the closest such expression.  */

static int
expr_reaches_here_p_work (occr, expr, bb, check_self_loop, visited)
     struct occr *occr;
     struct expr *expr;
     basic_block bb;
     int check_self_loop;
     char *visited;
{
  edge pred;

  for (pred = bb->pred; pred != NULL; pred = pred->pred_next)
    {
      basic_block pred_bb = pred->src;

      if (visited[pred_bb->index])
	/* This predecessor has already been visited. Nothing to do.  */
	  ;
      else if (pred_bb == bb)
	{
	  /* BB loops on itself.  */
	  if (check_self_loop
	      && TEST_BIT (ae_gen[pred_bb->index], expr->bitmap_index)
	      && BLOCK_NUM (occr->insn) == pred_bb->index)
	    return 1;

	  visited[pred_bb->index] = 1;
	}

      /* Ignore this predecessor if it kills the expression.  */
      else if (TEST_BIT (ae_kill[pred_bb->index], expr->bitmap_index))
	visited[pred_bb->index] = 1;

      /* Does this predecessor generate this expression?  */
      else if (TEST_BIT (ae_gen[pred_bb->index], expr->bitmap_index))
	{
	  /* Is this the occurrence we're looking for?
	     Note that there's only one generating occurrence per block
	     so we just need to check the block number.  */
	  if (BLOCK_NUM (occr->insn) == pred_bb->index)
	    return 1;

	  visited[pred_bb->index] = 1;
	}

      /* Neither gen nor kill.  */
      else
	{
	  visited[pred_bb->index] = 1;
	  if (expr_reaches_here_p_work (occr, expr, pred_bb, check_self_loop,
	      visited))

	    return 1;
	}
    }

  /* All paths have been checked.  */
  return 0;
}

/* This wrapper for expr_reaches_here_p_work() is to ensure that any
   memory allocated for that function is returned.  */

static int
expr_reaches_here_p (occr, expr, bb, check_self_loop)
     struct occr *occr;
     struct expr *expr;
     basic_block bb;
     int check_self_loop;
{
  int rval;
  char *visited = (char *) xcalloc (last_basic_block, 1);

  rval = expr_reaches_here_p_work (occr, expr, bb, check_self_loop, visited);

  free (visited);
  return rval;
}

/* Return the instruction that computes EXPR that reaches INSN's basic block.
   If there is more than one such instruction, return NULL.

   Called only by handle_avail_expr.  */

static rtx
computing_insn (expr, insn)
     struct expr *expr;
     rtx insn;
{
  basic_block bb = BLOCK_FOR_INSN (insn);

  if (expr->avail_occr->next == NULL)
    {
      if (BLOCK_FOR_INSN (expr->avail_occr->insn) == bb)
	/* The available expression is actually itself
	   (i.e. a loop in the flow graph) so do nothing.  */
	return NULL;

      /* (FIXME) Case that we found a pattern that was created by
	 a substitution that took place.  */
      return expr->avail_occr->insn;
    }
  else
    {
      /* Pattern is computed more than once.
	 Search backwards from this insn to see how many of these
	 computations actually reach this insn.  */
      struct occr *occr;
      rtx insn_computes_expr = NULL;
      int can_reach = 0;

      for (occr = expr->avail_occr; occr != NULL; occr = occr->next)
	{
	  if (BLOCK_FOR_INSN (occr->insn) == bb)
	    {
	      /* The expression is generated in this block.
		 The only time we care about this is when the expression
		 is generated later in the block [and thus there's a loop].
		 We let the normal cse pass handle the other cases.  */
	      if (INSN_CUID (insn) < INSN_CUID (occr->insn)
		  && expr_reaches_here_p (occr, expr, bb, 1))
		{
		  can_reach++;
		  if (can_reach > 1)
		    return NULL;

		  insn_computes_expr = occr->insn;
		}
	    }
	  else if (expr_reaches_here_p (occr, expr, bb, 0))
	    {
	      can_reach++;
	      if (can_reach > 1)
		return NULL;

	      insn_computes_expr = occr->insn;
	    }
	}

      if (insn_computes_expr == NULL)
	abort ();

      return insn_computes_expr;
    }
}

/* Return nonzero if the definition in DEF_INSN can reach INSN.
   Only called by can_disregard_other_sets.  */

static int
def_reaches_here_p (insn, def_insn)
     rtx insn, def_insn;
{
  rtx reg;

  if (TEST_BIT (reaching_defs[BLOCK_NUM (insn)], INSN_CUID (def_insn)))
    return 1;

  if (BLOCK_NUM (insn) == BLOCK_NUM (def_insn))
    {
      if (INSN_CUID (def_insn) < INSN_CUID (insn))
	{
	  if (GET_CODE (PATTERN (def_insn)) == PARALLEL)
	    return 1;
	  else if (GET_CODE (PATTERN (def_insn)) == CLOBBER)
	    reg = XEXP (PATTERN (def_insn), 0);
	  else if (GET_CODE (PATTERN (def_insn)) == SET)
	    reg = SET_DEST (PATTERN (def_insn));
	  else
	    abort ();

	  return ! reg_set_between_p (reg, NEXT_INSN (def_insn), insn);
	}
      else
	return 0;
    }

  return 0;
}

/* Return nonzero if *ADDR_THIS_REG can only have one value at INSN.  The
   value returned is the number of definitions that reach INSN.  Returning a
   value of zero means that [maybe] more than one definition reaches INSN and
   the caller can't perform whatever optimization it is trying.  i.e. it is
   always safe to return zero.  */

static int
can_disregard_other_sets (addr_this_reg, insn, for_combine)
     struct reg_set **addr_this_reg;
     rtx insn;
     int for_combine;
{
  int number_of_reaching_defs = 0;
  struct reg_set *this_reg;

  for (this_reg = *addr_this_reg; this_reg != 0; this_reg = this_reg->next)
    if (def_reaches_here_p (insn, this_reg->insn))
      {
	number_of_reaching_defs++;
	/* Ignore parallels for now.  */
	if (GET_CODE (PATTERN (this_reg->insn)) == PARALLEL)
	  return 0;

	if (!for_combine
	    && (GET_CODE (PATTERN (this_reg->insn)) == CLOBBER
		|| ! rtx_equal_p (SET_SRC (PATTERN (this_reg->insn)),
				  SET_SRC (PATTERN (insn)))))
	  /* A setting of the reg to a different value reaches INSN.  */
	  return 0;

	if (number_of_reaching_defs > 1)
	  {
	    /* If in this setting the value the register is being set to is
	       equal to the previous value the register was set to and this
	       setting reaches the insn we are trying to do the substitution
	       on then we are ok.  */
	    if (GET_CODE (PATTERN (this_reg->insn)) == CLOBBER)
	      return 0;
	    else if (! rtx_equal_p (SET_SRC (PATTERN (this_reg->insn)),
				    SET_SRC (PATTERN (insn))))
	      return 0;
	  }

	*addr_this_reg = this_reg;
      }

  return number_of_reaching_defs;
}

/* Expression computed by insn is available and the substitution is legal,
   so try to perform the substitution.

   The result is nonzero if any changes were made.  */

static int
handle_avail_expr (insn, expr)
     rtx insn;
     struct expr *expr;
{
  rtx pat, insn_computes_expr, expr_set;
  rtx to;
  struct reg_set *this_reg;
  int found_setting, use_src;
  int changed = 0;

  /* We only handle the case where one computation of the expression
     reaches this instruction.  */
  insn_computes_expr = computing_insn (expr, insn);
  if (insn_computes_expr == NULL)
    return 0;
  expr_set = single_set (insn_computes_expr);
  if (!expr_set)
    abort ();

  found_setting = 0;
  use_src = 0;

  /* At this point we know only one computation of EXPR outside of this
     block reaches this insn.  Now try to find a register that the
     expression is computed into.  */
  if (GET_CODE (SET_SRC (expr_set)) == REG)
    {
      /* This is the case when the available expression that reaches
	 here has already been handled as an available expression.  */
      unsigned int regnum_for_replacing
	= REGNO (SET_SRC (expr_set));

      /* If the register was created by GCSE we can't use `reg_set_table',
	 however we know it's set only once.  */
      if (regnum_for_replacing >= max_gcse_regno
	  /* If the register the expression is computed into is set only once,
	     or only one set reaches this insn, we can use it.  */
	  || (((this_reg = reg_set_table[regnum_for_replacing]),
	       this_reg->next == NULL)
	      || can_disregard_other_sets (&this_reg, insn, 0)))
	{
	  use_src = 1;
	  found_setting = 1;
	}
    }

  if (!found_setting)
    {
      unsigned int regnum_for_replacing
	= REGNO (SET_DEST (expr_set));

      /* This shouldn't happen.  */
      if (regnum_for_replacing >= max_gcse_regno)
	abort ();

      this_reg = reg_set_table[regnum_for_replacing];

      /* If the register the expression is computed into is set only once,
	 or only one set reaches this insn, use it.  */
      if (this_reg->next == NULL
	  || can_disregard_other_sets (&this_reg, insn, 0))
	found_setting = 1;
    }

  if (found_setting)
    {
      pat = PATTERN (insn);
      if (use_src)
	to = SET_SRC (expr_set);
      else
	to = SET_DEST (expr_set);
      changed = validate_change (insn, &SET_SRC (pat), to, 0);

      /* We should be able to ignore the return code from validate_change but
	 to play it safe we check.  */
      if (changed)
	{
	  gcse_subst_count++;
	  if (gcse_file != NULL)
	    {
	      fprintf (gcse_file, "GCSE: Replacing the source in insn %d with",
		       INSN_UID (insn));
	      fprintf (gcse_file, " reg %d %s insn %d\n",
		       REGNO (to), use_src ? "from" : "set in",
		       INSN_UID (insn_computes_expr));
	    }
	}
    }

  /* The register that the expr is computed into is set more than once.  */
  else if (1 /*expensive_op(this_pattrn->op) && do_expensive_gcse)*/)
    {
      /* Insert an insn after insnx that copies the reg set in insnx
	 into a new pseudo register call this new register REGN.
	 From insnb until end of basic block or until REGB is set
	 replace all uses of REGB with REGN.  */
      rtx new_insn;

      to = gen_reg_rtx (GET_MODE (SET_DEST (expr_set)));

      /* Generate the new insn.  */
      /* ??? If the change fails, we return 0, even though we created
	 an insn.  I think this is ok.  */
      new_insn
	= emit_insn_after (gen_rtx_SET (VOIDmode, to,
					SET_DEST (expr_set)),
			   insn_computes_expr);

      /* Keep register set table up to date.  */
      record_one_set (REGNO (to), new_insn);

      gcse_create_count++;
      if (gcse_file != NULL)
	{
	  fprintf (gcse_file, "GCSE: Creating insn %d to copy value of reg %d",
		   INSN_UID (NEXT_INSN (insn_computes_expr)),
		   REGNO (SET_SRC (PATTERN (NEXT_INSN (insn_computes_expr)))));
	  fprintf (gcse_file, ", computed in insn %d,\n",
		   INSN_UID (insn_computes_expr));
	  fprintf (gcse_file, "      into newly allocated reg %d\n",
		   REGNO (to));
	}

      pat = PATTERN (insn);

      /* Do register replacement for INSN.  */
      changed = validate_change (insn, &SET_SRC (pat),
				 SET_DEST (PATTERN
					   (NEXT_INSN (insn_computes_expr))),
				 0);

      /* We should be able to ignore the return code from validate_change but
	 to play it safe we check.  */
      if (changed)
	{
	  gcse_subst_count++;
	  if (gcse_file != NULL)
	    {
	      fprintf (gcse_file,
		       "GCSE: Replacing the source in insn %d with reg %d ",
		       INSN_UID (insn),
		       REGNO (SET_DEST (PATTERN (NEXT_INSN
						 (insn_computes_expr)))));
	      fprintf (gcse_file, "set in insn %d\n",
		       INSN_UID (insn_computes_expr));
	    }
	}
    }

  return changed;
}

/* Perform classic GCSE.  This is called by one_classic_gcse_pass after all
   the dataflow analysis has been done.

   The result is nonzero if a change was made.  */

static int
classic_gcse ()
{
  int changed;
  rtx insn;
  basic_block bb;

  /* Note we start at block 1.  */

  if (ENTRY_BLOCK_PTR->next_bb == EXIT_BLOCK_PTR)
    return 0;

  changed = 0;
  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR->next_bb->next_bb, EXIT_BLOCK_PTR, next_bb)
    {
      /* Reset tables used to keep track of what's still valid [since the
	 start of the block].  */
      reset_opr_set_tables ();

      for (insn = bb->head;
	   insn != NULL && insn != NEXT_INSN (bb->end);
	   insn = NEXT_INSN (insn))
	{
	  /* Is insn of form (set (pseudo-reg) ...)?  */
	  if (GET_CODE (insn) == INSN
	      && GET_CODE (PATTERN (insn)) == SET
	      && GET_CODE (SET_DEST (PATTERN (insn))) == REG
	      && REGNO (SET_DEST (PATTERN (insn))) >= FIRST_PSEUDO_REGISTER)
	    {
	      rtx pat = PATTERN (insn);
	      rtx src = SET_SRC (pat);
	      struct expr *expr;

	      if (want_to_gcse_p (src)
		  /* Is the expression recorded?  */
		  && ((expr = lookup_expr (src, &expr_hash_table)) != NULL)
		  /* Is the expression available [at the start of the
		     block]?  */
		  && TEST_BIT (ae_in[bb->index], expr->bitmap_index)
		  /* Are the operands unchanged since the start of the
		     block?  */
		  && oprs_not_set_p (src, insn))
		changed |= handle_avail_expr (insn, expr);
	    }

	  /* Keep track of everything modified by this insn.  */
	  /* ??? Need to be careful w.r.t. mods done to INSN.  */
	  if (INSN_P (insn))
	    mark_oprs_set (insn);
	}
    }

  return changed;
}

/* Top level routine to perform one classic GCSE pass.

   Return nonzero if a change was made.  */

static int
one_classic_gcse_pass (pass)
     int pass;
{
  int changed = 0;

  gcse_subst_count = 0;
  gcse_create_count = 0;

  alloc_hash_table (max_cuid, &expr_hash_table, 0);
  alloc_rd_mem (last_basic_block, max_cuid);
  compute_hash_table (&expr_hash_table);
  if (gcse_file)
    dump_hash_table (gcse_file, "Expression", &expr_hash_table);

  if (expr_hash_table.n_elems > 0)
    {
      compute_kill_rd ();
      compute_rd ();
      alloc_avail_expr_mem (last_basic_block, expr_hash_table.n_elems);
      compute_ae_gen (&expr_hash_table);
      compute_ae_kill (ae_gen, ae_kill, &expr_hash_table);
      compute_available (ae_gen, ae_kill, ae_out, ae_in);
      changed = classic_gcse ();
      free_avail_expr_mem ();
    }

  free_rd_mem ();
  free_hash_table (&expr_hash_table);

  if (gcse_file)
    {
      fprintf (gcse_file, "\n");
      fprintf (gcse_file, "GCSE of %s, pass %d: %d bytes needed, %d substs,",
	       current_function_name, pass, bytes_used, gcse_subst_count);
      fprintf (gcse_file, "%d insns created\n", gcse_create_count);
    }

  return changed;
}

/* Compute copy/constant propagation working variables.  */

/* Local properties of assignments.  */
static sbitmap *cprop_pavloc;
static sbitmap *cprop_absaltered;

/* Global properties of assignments (computed from the local properties).  */
static sbitmap *cprop_avin;
static sbitmap *cprop_avout;

/* Allocate vars used for copy/const propagation.  N_BLOCKS is the number of
   basic blocks.  N_SETS is the number of sets.  */

static void
alloc_cprop_mem (n_blocks, n_sets)
     int n_blocks, n_sets;
{
  cprop_pavloc = sbitmap_vector_alloc (n_blocks, n_sets);
  cprop_absaltered = sbitmap_vector_alloc (n_blocks, n_sets);

  cprop_avin = sbitmap_vector_alloc (n_blocks, n_sets);
  cprop_avout = sbitmap_vector_alloc (n_blocks, n_sets);
}

/* Free vars used by copy/const propagation.  */

static void
free_cprop_mem ()
{
  sbitmap_vector_free (cprop_pavloc);
  sbitmap_vector_free (cprop_absaltered);
  sbitmap_vector_free (cprop_avin);
  sbitmap_vector_free (cprop_avout);
}

/* For each block, compute whether X is transparent.  X is either an
   expression or an assignment [though we don't care which, for this context
   an assignment is treated as an expression].  For each block where an
   element of X is modified, set (SET_P == 1) or reset (SET_P == 0) the INDX
   bit in BMAP.  */

static void
compute_transp (x, indx, bmap, set_p)
     rtx x;
     int indx;
     sbitmap *bmap;
     int set_p;
{
  int i, j;
  basic_block bb;
  enum rtx_code code;
  reg_set *r;
  const char *fmt;

  /* repeat is used to turn tail-recursion into iteration since GCC
     can't do it when there's no return value.  */
 repeat:

  if (x == 0)
    return;

  code = GET_CODE (x);
  switch (code)
    {
    case REG:
      if (set_p)
	{
	  if (REGNO (x) < FIRST_PSEUDO_REGISTER)
	    {
	      FOR_EACH_BB (bb)
		if (TEST_BIT (reg_set_in_block[bb->index], REGNO (x)))
		  SET_BIT (bmap[bb->index], indx);
	    }
	  else
	    {
	      for (r = reg_set_table[REGNO (x)]; r != NULL; r = r->next)
		SET_BIT (bmap[BLOCK_NUM (r->insn)], indx);
	    }
	}
      else
	{
	  if (REGNO (x) < FIRST_PSEUDO_REGISTER)
	    {
	      FOR_EACH_BB (bb)
		if (TEST_BIT (reg_set_in_block[bb->index], REGNO (x)))
		  RESET_BIT (bmap[bb->index], indx);
	    }
	  else
	    {
	      for (r = reg_set_table[REGNO (x)]; r != NULL; r = r->next)
		RESET_BIT (bmap[BLOCK_NUM (r->insn)], indx);
	    }
	}

      return;

    case MEM:
      FOR_EACH_BB (bb)
	{
	  rtx list_entry = canon_modify_mem_list[bb->index];

	  while (list_entry)
	    {
	      rtx dest, dest_addr;

	      if (GET_CODE (XEXP (list_entry, 0)) == CALL_INSN)
		{
		  if (set_p)
		    SET_BIT (bmap[bb->index], indx);
		  else
		    RESET_BIT (bmap[bb->index], indx);
		  break;
		}
	      /* LIST_ENTRY must be an INSN of some kind that sets memory.
		 Examine each hunk of memory that is modified.  */

	      dest = XEXP (list_entry, 0);
	      list_entry = XEXP (list_entry, 1);
	      dest_addr = XEXP (list_entry, 0);

	      if (canon_true_dependence (dest, GET_MODE (dest), dest_addr,
					 x, rtx_addr_varies_p))
		{
		  if (set_p)
		    SET_BIT (bmap[bb->index], indx);
		  else
		    RESET_BIT (bmap[bb->index], indx);
		  break;
		}
	      list_entry = XEXP (list_entry, 1);
	    }
	}

      x = XEXP (x, 0);
      goto repeat;

    case PC:
    case CC0: /*FIXME*/
    case CONST:
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST_VECTOR:
    case SYMBOL_REF:
    case LABEL_REF:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
      return;

    default:
      break;
    }

  for (i = GET_RTX_LENGTH (code) - 1, fmt = GET_RTX_FORMAT (code); i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  /* If we are about to do the last recursive call
	     needed at this level, change it into iteration.
	     This function is called enough to be worth it.  */
	  if (i == 0)
	    {
	      x = XEXP (x, i);
	      goto repeat;
	    }

	  compute_transp (XEXP (x, i), indx, bmap, set_p);
	}
      else if (fmt[i] == 'E')
	for (j = 0; j < XVECLEN (x, i); j++)
	  compute_transp (XVECEXP (x, i, j), indx, bmap, set_p);
    }
}

/* Top level routine to do the dataflow analysis needed by copy/const
   propagation.  */

static void
compute_cprop_data ()
{
  compute_local_properties (cprop_absaltered, cprop_pavloc, NULL, &set_hash_table);
  compute_available (cprop_pavloc, cprop_absaltered,
		     cprop_avout, cprop_avin);
}

/* Copy/constant propagation.  */

/* Maximum number of register uses in an insn that we handle.  */
#define MAX_USES 8

/* Table of uses found in an insn.
   Allocated statically to avoid alloc/free complexity and overhead.  */
static struct reg_use reg_use_table[MAX_USES];

/* Index into `reg_use_table' while building it.  */
static int reg_use_count;

/* Set up a list of register numbers used in INSN.  The found uses are stored
   in `reg_use_table'.  `reg_use_count' is initialized to zero before entry,
   and contains the number of uses in the table upon exit.

   ??? If a register appears multiple times we will record it multiple times.
   This doesn't hurt anything but it will slow things down.  */

static void
find_used_regs (xptr, data)
     rtx *xptr;
     void *data ATTRIBUTE_UNUSED;
{
  int i, j;
  enum rtx_code code;
  const char *fmt;
  rtx x = *xptr;

  /* repeat is used to turn tail-recursion into iteration since GCC
     can't do it when there's no return value.  */
 repeat:
  if (x == 0)
    return;

  code = GET_CODE (x);
  if (REG_P (x))
    {
      if (reg_use_count == MAX_USES)
	return;

      reg_use_table[reg_use_count].reg_rtx = x;
      reg_use_count++;
    }

  /* Recursively scan the operands of this expression.  */

  for (i = GET_RTX_LENGTH (code) - 1, fmt = GET_RTX_FORMAT (code); i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  /* If we are about to do the last recursive call
	     needed at this level, change it into iteration.
	     This function is called enough to be worth it.  */
	  if (i == 0)
	    {
	      x = XEXP (x, 0);
	      goto repeat;
	    }

	  find_used_regs (&XEXP (x, i), data);
	}
      else if (fmt[i] == 'E')
	for (j = 0; j < XVECLEN (x, i); j++)
	  find_used_regs (&XVECEXP (x, i, j), data);
    }
}

/* Try to replace all non-SET_DEST occurrences of FROM in INSN with TO.
   Returns nonzero is successful.  */

static int
try_replace_reg (from, to, insn)
     rtx from, to, insn;
{
  rtx note = find_reg_equal_equiv_note (insn);
  rtx src = 0;
  int success = 0;
  rtx set = single_set (insn);

  validate_replace_src_group (from, to, insn);
  if (num_changes_pending () && apply_change_group ())
    success = 1;

  /* Try to simplify SET_SRC if we have substituted a constant.  */
  if (success && set && CONSTANT_P (to))
    {
      src = simplify_rtx (SET_SRC (set));

      if (src)
	validate_change (insn, &SET_SRC (set), src, 0);
    }

  if (!success && set && reg_mentioned_p (from, SET_SRC (set)))
    {
      /* If above failed and this is a single set, try to simplify the source of
	 the set given our substitution.  We could perhaps try this for multiple
	 SETs, but it probably won't buy us anything.  */
      src = simplify_replace_rtx (SET_SRC (set), from, to);

      if (!rtx_equal_p (src, SET_SRC (set))
	  && validate_change (insn, &SET_SRC (set), src, 0))
	success = 1;

      /* If we've failed to do replacement, have a single SET, and don't already
	 have a note, add a REG_EQUAL note to not lose information.  */
      if (!success && note == 0 && set != 0)
	note = set_unique_reg_note (insn, REG_EQUAL, copy_rtx (src));
    }

  /* If there is already a NOTE, update the expression in it with our
     replacement.  */
  else if (note != 0)
    XEXP (note, 0) = simplify_replace_rtx (XEXP (note, 0), from, to);

  /* REG_EQUAL may get simplified into register.
     We don't allow that. Remove that note. This code ought
     not to hapen, because previous code ought to syntetize
     reg-reg move, but be on the safe side.  */
  if (note && REG_P (XEXP (note, 0)))
    remove_note (insn, note);

  return success;
}

/* Find a set of REGNOs that are available on entry to INSN's block.  Returns
   NULL no such set is found.  */

static struct expr *
find_avail_set (regno, insn)
     int regno;
     rtx insn;
{
  /* SET1 contains the last set found that can be returned to the caller for
     use in a substitution.  */
  struct expr *set1 = 0;

  /* Loops are not possible here.  To get a loop we would need two sets
     available at the start of the block containing INSN.  ie we would
     need two sets like this available at the start of the block:

       (set (reg X) (reg Y))
       (set (reg Y) (reg X))

     This can not happen since the set of (reg Y) would have killed the
     set of (reg X) making it unavailable at the start of this block.  */
  while (1)
    {
      rtx src;
      struct expr *set = lookup_set (regno, NULL_RTX, &set_hash_table);

      /* Find a set that is available at the start of the block
	 which contains INSN.  */
      while (set)
	{
	  if (TEST_BIT (cprop_avin[BLOCK_NUM (insn)], set->bitmap_index))
	    break;
	  set = next_set (regno, set);
	}

      /* If no available set was found we've reached the end of the
	 (possibly empty) copy chain.  */
      if (set == 0)
	break;

      if (GET_CODE (set->expr) != SET)
	abort ();

      src = SET_SRC (set->expr);

      /* We know the set is available.
	 Now check that SRC is ANTLOC (i.e. none of the source operands
	 have changed since the start of the block).

         If the source operand changed, we may still use it for the next
         iteration of this loop, but we may not use it for substitutions.  */

      if (CONSTANT_P (src) || oprs_not_set_p (src, insn))
	set1 = set;

      /* If the source of the set is anything except a register, then
	 we have reached the end of the copy chain.  */
      if (GET_CODE (src) != REG)
	break;

      /* Follow the copy chain, ie start another iteration of the loop
	 and see if we have an available copy into SRC.  */
      regno = REGNO (src);
    }

  /* SET1 holds the last set that was available and anticipatable at
     INSN.  */
  return set1;
}

/* Subroutine of cprop_insn that tries to propagate constants into
   JUMP_INSNS.  JUMP must be a conditional jump.  If SETCC is non-NULL
   it is the instruction that immediately preceeds JUMP, and must be a
   single SET of a register.  FROM is what we will try to replace,
   SRC is the constant we will try to substitute for it.  Returns nonzero
   if a change was made.  */

static int
cprop_jump (bb, setcc, jump, from, src)
     basic_block bb;
     rtx setcc;
     rtx jump;
     rtx from;
     rtx src;
{
  rtx new, new_set;
  rtx set = pc_set (jump);

  /* First substitute in the INSN condition as the SET_SRC of the JUMP,
     then substitute that given values in this expanded JUMP.  */
  if (setcc != NULL
      && !modified_between_p (from, setcc, jump)
      && !modified_between_p (src, setcc, jump))
    {
      rtx setcc_set = single_set (setcc);
      new_set = simplify_replace_rtx (SET_SRC (set),
				      SET_DEST (setcc_set),
				      SET_SRC (setcc_set));
    }
  else
    new_set = set;

  new = simplify_replace_rtx (new_set, from, src);

  /* If no simplification can be made, then try the next
     register.  */
  if (rtx_equal_p (new, new_set) || rtx_equal_p (new, SET_SRC (set)))
    return 0;

  /* If this is now a no-op delete it, otherwise this must be a valid insn.  */
  if (new == pc_rtx)
    delete_insn (jump);
  else
    {
      /* Ensure the value computed inside the jump insn to be equivalent
         to one computed by setcc.  */
      if (setcc 
	  && modified_in_p (new, setcc))
	return 0;
      if (! validate_change (jump, &SET_SRC (set), new, 0))
	return 0;

      /* If this has turned into an unconditional jump,
	 then put a barrier after it so that the unreachable
	 code will be deleted.  */
      if (GET_CODE (SET_SRC (set)) == LABEL_REF)
	emit_barrier_after (jump);
     }

#ifdef HAVE_cc0
  /* Delete the cc0 setter.  */
  if (setcc != NULL && CC0_P (SET_DEST (single_set (setcc))))
    delete_insn (setcc);
#endif

  run_jump_opt_after_gcse = 1;

  const_prop_count++;
  if (gcse_file != NULL)
    {
      fprintf (gcse_file,
	       "CONST-PROP: Replacing reg %d in jump_insn %d with constant ",
	       REGNO (from), INSN_UID (jump));
      print_rtl (gcse_file, src);
      fprintf (gcse_file, "\n");
    }
  purge_dead_edges (bb);

  return 1;
}

static bool
constprop_register (insn, from, to, alter_jumps)
     rtx insn;
     rtx from;
     rtx to;
     int alter_jumps;
{
  rtx sset;

  /* Check for reg or cc0 setting instructions followed by
     conditional branch instructions first.  */
  if (alter_jumps
      && (sset = single_set (insn)) != NULL
      && NEXT_INSN (insn)
      && any_condjump_p (NEXT_INSN (insn)) && onlyjump_p (NEXT_INSN (insn)))
    {
      rtx dest = SET_DEST (sset);
      if ((REG_P (dest) || CC0_P (dest))
	  && cprop_jump (BLOCK_FOR_INSN (insn), insn, NEXT_INSN (insn), from, to))
	return 1;
    }

  /* Handle normal insns next.  */
  if (GET_CODE (insn) == INSN
      && try_replace_reg (from, to, insn))
    return 1;

  /* Try to propagate a CONST_INT into a conditional jump.
     We're pretty specific about what we will handle in this
     code, we can extend this as necessary over time.

     Right now the insn in question must look like
     (set (pc) (if_then_else ...))  */
  else if (alter_jumps && any_condjump_p (insn) && onlyjump_p (insn))
    return cprop_jump (BLOCK_FOR_INSN (insn), NULL, insn, from, to);
  return 0;
}

/* Perform constant and copy propagation on INSN.
   The result is nonzero if a change was made.  */

static int
cprop_insn (insn, alter_jumps)
     rtx insn;
     int alter_jumps;
{
  struct reg_use *reg_used;
  int changed = 0;
  rtx note;

  if (!INSN_P (insn))
    return 0;

  reg_use_count = 0;
  note_uses (&PATTERN (insn), find_used_regs, NULL);

  note = find_reg_equal_equiv_note (insn);

  /* We may win even when propagating constants into notes.  */
  if (note)
    find_used_regs (&XEXP (note, 0), NULL);

  for (reg_used = &reg_use_table[0]; reg_use_count > 0;
       reg_used++, reg_use_count--)
    {
      unsigned int regno = REGNO (reg_used->reg_rtx);
      rtx pat, src;
      struct expr *set;

      /* Ignore registers created by GCSE.
	 We do this because ...  */
      if (regno >= max_gcse_regno)
	continue;

      /* If the register has already been set in this block, there's
	 nothing we can do.  */
      if (! oprs_not_set_p (reg_used->reg_rtx, insn))
	continue;

      /* Find an assignment that sets reg_used and is available
	 at the start of the block.  */
      set = find_avail_set (regno, insn);
      if (! set)
	continue;

      pat = set->expr;
      /* ??? We might be able to handle PARALLELs.  Later.  */
      if (GET_CODE (pat) != SET)
	abort ();

      src = SET_SRC (pat);

      /* Constant propagation.  */
      if (CONSTANT_P (src))
	{
          if (constprop_register (insn, reg_used->reg_rtx, src, alter_jumps))
	    {
	      changed = 1;
	      const_prop_count++;
	      if (gcse_file != NULL)
		{
		  fprintf (gcse_file, "GLOBAL CONST-PROP: Replacing reg %d in ", regno);
		  fprintf (gcse_file, "insn %d with constant ", INSN_UID (insn));
		  print_rtl (gcse_file, src);
		  fprintf (gcse_file, "\n");
		}
	    }
	}
      else if (GET_CODE (src) == REG
	       && REGNO (src) >= FIRST_PSEUDO_REGISTER
	       && REGNO (src) != regno)
	{
	  if (try_replace_reg (reg_used->reg_rtx, src, insn))
	    {
	      changed = 1;
	      copy_prop_count++;
	      if (gcse_file != NULL)
		{
		  fprintf (gcse_file, "GLOBAL COPY-PROP: Replacing reg %d in insn %d",
			   regno, INSN_UID (insn));
		  fprintf (gcse_file, " with reg %d\n", REGNO (src));
		}

	      /* The original insn setting reg_used may or may not now be
		 deletable.  We leave the deletion to flow.  */
	      /* FIXME: If it turns out that the insn isn't deletable,
		 then we may have unnecessarily extended register lifetimes
		 and made things worse.  */
	    }
	}
    }

  return changed;
}

/* Like find_used_regs, but avoid recording uses that appear in
   input-output contexts such as zero_extract or pre_dec.  This
   restricts the cases we consider to those for which local cprop
   can legitimately make replacements.  */

static void
local_cprop_find_used_regs (xptr, data)
     rtx *xptr;
     void *data;
{
  rtx x = *xptr;

  if (x == 0)
    return;

  switch (GET_CODE (x))
    {
    case ZERO_EXTRACT:
    case SIGN_EXTRACT:
    case STRICT_LOW_PART:
      return;

    case PRE_DEC:
    case PRE_INC:
    case POST_DEC:
    case POST_INC:
    case PRE_MODIFY:
    case POST_MODIFY:
      /* Can only legitimately appear this early in the context of
	 stack pushes for function arguments, but handle all of the
	 codes nonetheless.  */
      return;

    case SUBREG:
      /* Setting a subreg of a register larger than word_mode leaves
	 the non-written words unchanged.  */
      if (GET_MODE_BITSIZE (GET_MODE (SUBREG_REG (x))) > BITS_PER_WORD)
	return;
      break;

    default:
      break;
    }

  find_used_regs (xptr, data);
}
  
/* LIBCALL_SP is a zero-terminated array of insns at the end of a libcall;
   their REG_EQUAL notes need updating.  */

static bool
do_local_cprop (x, insn, alter_jumps, libcall_sp)
     rtx x;
     rtx insn;
     int alter_jumps;
     rtx *libcall_sp;
{
  rtx newreg = NULL, newcnst = NULL;

  /* Rule out USE instructions and ASM statements as we don't want to
     change the hard registers mentioned.  */
  if (GET_CODE (x) == REG
      && (REGNO (x) >= FIRST_PSEUDO_REGISTER
          || (GET_CODE (PATTERN (insn)) != USE
	      && asm_noperands (PATTERN (insn)) < 0)))
    {
      cselib_val *val = cselib_lookup (x, GET_MODE (x), 0);
      struct elt_loc_list *l;

      if (!val)
	return false;
      for (l = val->locs; l; l = l->next)
	{
	  rtx this_rtx = l->loc;
	  rtx note;

	  if (l->in_libcall)
	    continue;

	  if (CONSTANT_P (this_rtx))
	    newcnst = this_rtx;
	  if (REG_P (this_rtx) && REGNO (this_rtx) >= FIRST_PSEUDO_REGISTER
	      /* Don't copy propagate if it has attached REG_EQUIV note.
		 At this point this only function parameters should have
		 REG_EQUIV notes and if the argument slot is used somewhere
		 explicitly, it means address of parameter has been taken,
		 so we should not extend the lifetime of the pseudo.  */
	      && (!(note = find_reg_note (l->setting_insn, REG_EQUIV, NULL_RTX))
		  || GET_CODE (XEXP (note, 0)) != MEM))
	    newreg = this_rtx;
	}
      if (newcnst && constprop_register (insn, x, newcnst, alter_jumps))
	{
	  /* If we find a case where we can't fix the retval REG_EQUAL notes
	     match the new register, we either have to abandom this replacement
	     or fix delete_trivially_dead_insns to preserve the setting insn,
	     or make it delete the REG_EUAQL note, and fix up all passes that
	     require the REG_EQUAL note there.  */
	  if (!adjust_libcall_notes (x, newcnst, insn, libcall_sp))
	    abort ();
	  if (gcse_file != NULL)
	    {
	      fprintf (gcse_file, "LOCAL CONST-PROP: Replacing reg %d in ",
		       REGNO (x));
	      fprintf (gcse_file, "insn %d with constant ",
		       INSN_UID (insn));
	      print_rtl (gcse_file, newcnst);
	      fprintf (gcse_file, "\n");
	    }
	  const_prop_count++;
	  return true;
	}
      else if (newreg && newreg != x && try_replace_reg (x, newreg, insn))
	{
	  adjust_libcall_notes (x, newreg, insn, libcall_sp);
	  if (gcse_file != NULL)
	    {
	      fprintf (gcse_file,
		       "LOCAL COPY-PROP: Replacing reg %d in insn %d",
		       REGNO (x), INSN_UID (insn));
	      fprintf (gcse_file, " with reg %d\n", REGNO (newreg));
	    }
	  copy_prop_count++;
	  return true;
	}
    }
  return false;
}

/* LIBCALL_SP is a zero-terminated array of insns at the end of a libcall;
   their REG_EQUAL notes need updating to reflect that OLDREG has been
   replaced with NEWVAL in INSN.  Return true if all substitutions could
   be made.  */
static bool
adjust_libcall_notes (oldreg, newval, insn, libcall_sp)
     rtx oldreg, newval, insn, *libcall_sp;
{
  rtx end;

  while ((end = *libcall_sp++))
    {
      rtx note = find_reg_equal_equiv_note (end);

      if (! note)
	continue;

      if (REG_P (newval))
	{
	  if (reg_set_between_p (newval, PREV_INSN (insn), end))
	    {
	      do
		{
		  note = find_reg_equal_equiv_note (end);
		  if (! note)
		    continue;
		  if (reg_mentioned_p (newval, XEXP (note, 0)))
		    return false;
		}
	      while ((end = *libcall_sp++));
	      return true;
	    }
	}
      XEXP (note, 0) = replace_rtx (XEXP (note, 0), oldreg, newval);
      insn = end;
    }
  return true;
}

#define MAX_NESTED_LIBCALLS 9

static void
local_cprop_pass (alter_jumps)
     int alter_jumps;
{
  rtx insn;
  struct reg_use *reg_used;
  rtx libcall_stack[MAX_NESTED_LIBCALLS + 1], *libcall_sp;
  bool changed = false;

  cselib_init ();
  libcall_sp = &libcall_stack[MAX_NESTED_LIBCALLS];
  *libcall_sp = 0;
  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      if (INSN_P (insn))
	{
	  rtx note = find_reg_note (insn, REG_LIBCALL, NULL_RTX);

	  if (note)
	    {
	      if (libcall_sp == libcall_stack)
		abort ();
	      *--libcall_sp = XEXP (note, 0);
	    }
	  note = find_reg_note (insn, REG_RETVAL, NULL_RTX);
	  if (note)
	    libcall_sp++;
	  note = find_reg_equal_equiv_note (insn);
	  do
	    {
	      reg_use_count = 0;
	      note_uses (&PATTERN (insn), local_cprop_find_used_regs, NULL);
	      if (note)
		local_cprop_find_used_regs (&XEXP (note, 0), NULL);

	      for (reg_used = &reg_use_table[0]; reg_use_count > 0;
		   reg_used++, reg_use_count--)
		if (do_local_cprop (reg_used->reg_rtx, insn, alter_jumps,
		    libcall_sp))
		  {
		    changed = true;
		    break;
		  }
	    }
	  while (reg_use_count);
	}
      cselib_process_insn (insn);
    }
  cselib_finish ();
  /* Global analysis may get into infinite loops for unreachable blocks.  */
  if (changed && alter_jumps)
    {
      delete_unreachable_blocks ();
      free_reg_set_mem ();
      alloc_reg_set_mem (max_reg_num ());
      compute_sets (get_insns ());
    }
}

/* Forward propagate copies.  This includes copies and constants.  Return
   nonzero if a change was made.  */

static int
cprop (alter_jumps)
     int alter_jumps;
{
  int changed;
  basic_block bb;
  rtx insn;

  /* Note we start at block 1.  */
  if (ENTRY_BLOCK_PTR->next_bb == EXIT_BLOCK_PTR)
    {
      if (gcse_file != NULL)
	fprintf (gcse_file, "\n");
      return 0;
    }

  changed = 0;
  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR->next_bb->next_bb, EXIT_BLOCK_PTR, next_bb)
    {
      /* Reset tables used to keep track of what's still valid [since the
	 start of the block].  */
      reset_opr_set_tables ();

      for (insn = bb->head;
	   insn != NULL && insn != NEXT_INSN (bb->end);
	   insn = NEXT_INSN (insn))
	if (INSN_P (insn))
	  {
	    changed |= cprop_insn (insn, alter_jumps);

	    /* Keep track of everything modified by this insn.  */
	    /* ??? Need to be careful w.r.t. mods done to INSN.  Don't
	       call mark_oprs_set if we turned the insn into a NOTE.  */
	    if (GET_CODE (insn) != NOTE)
	      mark_oprs_set (insn);
	  }
    }

  if (gcse_file != NULL)
    fprintf (gcse_file, "\n");

  return changed;
}

/* Perform one copy/constant propagation pass.
   F is the first insn in the function.
   PASS is the pass count.  */

static int
one_cprop_pass (pass, alter_jumps)
     int pass;
     int alter_jumps;
{
  int changed = 0;

  const_prop_count = 0;
  copy_prop_count = 0;

  local_cprop_pass (alter_jumps);

  alloc_hash_table (max_cuid, &set_hash_table, 1);
  compute_hash_table (&set_hash_table);
  if (gcse_file)
    dump_hash_table (gcse_file, "SET", &set_hash_table);
  if (set_hash_table.n_elems > 0)
    {
      alloc_cprop_mem (last_basic_block, set_hash_table.n_elems);
      compute_cprop_data ();
      changed = cprop (alter_jumps);
      if (alter_jumps)
	changed |= bypass_conditional_jumps ();
      free_cprop_mem ();
    }

  free_hash_table (&set_hash_table);

  if (gcse_file)
    {
      fprintf (gcse_file, "CPROP of %s, pass %d: %d bytes needed, ",
	       current_function_name, pass, bytes_used);
      fprintf (gcse_file, "%d const props, %d copy props\n\n",
	       const_prop_count, copy_prop_count);
    }
  /* Global analysis may get into infinite loops for unreachable blocks.  */
  if (changed && alter_jumps)
    delete_unreachable_blocks ();

  return changed;
}

/* Bypass conditional jumps.  */

/* Find a set of REGNO to a constant that is available at the end of basic
   block BB.  Returns NULL if no such set is found.  Based heavily upon
   find_avail_set.  */

static struct expr *
find_bypass_set (regno, bb)
     int regno;
     int bb;
{
  struct expr *result = 0;

  for (;;)
    {
      rtx src;
      struct expr *set = lookup_set (regno, NULL_RTX, &set_hash_table);

      while (set)
	{
	  if (TEST_BIT (cprop_avout[bb], set->bitmap_index))
	    break;
	  set = next_set (regno, set);
	}

      if (set == 0)
	break;

      if (GET_CODE (set->expr) != SET)
	abort ();

      src = SET_SRC (set->expr);
      if (CONSTANT_P (src))
	result = set;

      if (GET_CODE (src) != REG)
	break;

      regno = REGNO (src);
    }
  return result;
}


/* Subroutine of bypass_block that checks whether a pseudo is killed by
   any of the instructions inserted on an edge.  Jump bypassing places
   condition code setters on CFG edges using insert_insn_on_edge.  This
   function is required to check that our data flow analysis is still
   valid prior to commit_edge_insertions.  */

static bool
reg_killed_on_edge (reg, e)
     rtx reg;
     edge e;
{
  rtx insn;

  for (insn = e->insns; insn; insn = NEXT_INSN (insn))
    if (INSN_P (insn) && reg_set_p (reg, insn))
      return true;

  return false;
}

/* Subroutine of bypass_conditional_jumps that attempts to bypass the given
   basic block BB which has more than one predecessor.  If not NULL, SETCC
   is the first instruction of BB, which is immediately followed by JUMP_INSN
   JUMP.  Otherwise, SETCC is NULL, and JUMP is the first insn of BB.
   Returns nonzero if a change was made.

   During the jump bypassing pass, we may place copies of SETCC instuctions
   on CFG edges.  The following routine must be careful to pay attention to
   these inserted insns when performing its transformations.  */

static int
bypass_block (bb, setcc, jump)
     basic_block bb;
     rtx setcc, jump;
{
  rtx insn, note;
  edge e, enext, edest;
  int i, change;

  insn = (setcc != NULL) ? setcc : jump;

  /* Determine set of register uses in INSN.  */
  reg_use_count = 0;
  note_uses (&PATTERN (insn), find_used_regs, NULL);
  note = find_reg_equal_equiv_note (insn);
  if (note)
    find_used_regs (&XEXP (note, 0), NULL);

  change = 0;
  for (e = bb->pred; e; e = enext)
    {
      enext = e->pred_next;
      for (i = 0; i < reg_use_count; i++)
	{
	  struct reg_use *reg_used = &reg_use_table[i];
	  unsigned int regno = REGNO (reg_used->reg_rtx);
	  basic_block dest, old_dest;
	  struct expr *set;
	  rtx src, new;

	  if (regno >= max_gcse_regno)
	    continue;

	  set = find_bypass_set (regno, e->src->index);

	  if (! set)
	    continue;

	  /* Check the data flow is valid after edge insertions.  */
	  if (e->insns && reg_killed_on_edge (reg_used->reg_rtx, e))
	    continue;

	  src = SET_SRC (pc_set (jump));

	  if (setcc != NULL)
	      src = simplify_replace_rtx (src,
					  SET_DEST (PATTERN (setcc)),
					  SET_SRC (PATTERN (setcc)));

	  new = simplify_replace_rtx (src, reg_used->reg_rtx,
				      SET_SRC (set->expr));

	  /* Jump bypassing may have already placed instructions on 
	     edges of the CFG.  We can't bypass an outgoing edge that
	     has instructions associated with it, as these insns won't
	     get executed if the incoming edge is redirected.  */

	  if (new == pc_rtx)
	    {
	      edest = FALLTHRU_EDGE (bb);
	      dest = edest->insns ? NULL : edest->dest;
	    }
	  else if (GET_CODE (new) == LABEL_REF)
	    {
	      dest = BLOCK_FOR_INSN (XEXP (new, 0));
	      /* Don't bypass edges containing instructions.  */
	      for (edest = bb->succ; edest; edest = edest->succ_next)
		if (edest->dest == dest && edest->insns)
		  {
		    dest = NULL;
		    break;
		  }
	    }
	  else
	    dest = NULL;

	  /* Once basic block indices are stable, we should be able
	     to use redirect_edge_and_branch_force instead.  */
	  old_dest = e->dest;
	  if (dest != NULL && dest != old_dest
	      && redirect_edge_and_branch (e, dest))
	    {
	      /* Copy the register setter to the redirected edge.
		 Don't copy CC0 setters, as CC0 is dead after jump.  */
	      if (setcc)
		{
		  rtx pat = PATTERN (setcc);
		  if (!CC0_P (SET_DEST (pat)))
		    insert_insn_on_edge (copy_insn (pat), e);
		}

	      if (gcse_file != NULL)
		{
		  fprintf (gcse_file, "JUMP-BYPASS: Proved reg %d in jump_insn %d equals constant ",
			   regno, INSN_UID (jump));
		  print_rtl (gcse_file, SET_SRC (set->expr));
		  fprintf (gcse_file, "\nBypass edge from %d->%d to %d\n",
			   e->src->index, old_dest->index, dest->index);
		}
	      change = 1;
	      break;
	    }
	}
    }
  return change;
}

/* Find basic blocks with more than one predecessor that only contain a
   single conditional jump.  If the result of the comparison is known at
   compile-time from any incoming edge, redirect that edge to the
   appropriate target.  Returns nonzero if a change was made.  */

static int
bypass_conditional_jumps ()
{
  basic_block bb;
  int changed;
  rtx setcc;
  rtx insn;
  rtx dest;

  /* Note we start at block 1.  */
  if (ENTRY_BLOCK_PTR->next_bb == EXIT_BLOCK_PTR)
    return 0;

  changed = 0;
  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR->next_bb->next_bb,
		  EXIT_BLOCK_PTR, next_bb)
    {
      /* Check for more than one predecessor.  */
      if (bb->pred && bb->pred->pred_next)
	{
	  setcc = NULL_RTX;
	  for (insn = bb->head;
	       insn != NULL && insn != NEXT_INSN (bb->end);
	       insn = NEXT_INSN (insn))
	    if (GET_CODE (insn) == INSN)
	      {
		if (setcc)
		  break;
		if (GET_CODE (PATTERN (insn)) != SET)
		  break;

		dest = SET_DEST (PATTERN (insn));
		if (REG_P (dest) || CC0_P (dest))
		  setcc = insn;
		else
		  break;
	      }
	    else if (GET_CODE (insn) == JUMP_INSN)
	      {
		if (any_condjump_p (insn) && onlyjump_p (insn))
		  changed |= bypass_block (bb, setcc, insn);
		break;
	      }
	    else if (INSN_P (insn))
	      break;
	}
    }

  /* If we bypassed any register setting insns, we inserted a
     copy on the redirected edge.  These need to be commited.  */
  if (changed)
    commit_edge_insertions();

  return changed;
}

/* Compute PRE+LCM working variables.  */

/* Local properties of expressions.  */
/* Nonzero for expressions that are transparent in the block.  */
static sbitmap *transp;

/* Nonzero for expressions that are transparent at the end of the block.
   This is only zero for expressions killed by abnormal critical edge
   created by a calls.  */
static sbitmap *transpout;

/* Nonzero for expressions that are computed (available) in the block.  */
static sbitmap *comp;

/* Nonzero for expressions that are locally anticipatable in the block.  */
static sbitmap *antloc;

/* Nonzero for expressions where this block is an optimal computation
   point.  */
static sbitmap *pre_optimal;

/* Nonzero for expressions which are redundant in a particular block.  */
static sbitmap *pre_redundant;

/* Nonzero for expressions which should be inserted on a specific edge.  */
static sbitmap *pre_insert_map;

/* Nonzero for expressions which should be deleted in a specific block.  */
static sbitmap *pre_delete_map;

/* Contains the edge_list returned by pre_edge_lcm.  */
static struct edge_list *edge_list;

/* Redundant insns.  */
static sbitmap pre_redundant_insns;

/* Allocate vars used for PRE analysis.  */

static void
alloc_pre_mem (n_blocks, n_exprs)
     int n_blocks, n_exprs;
{
  transp = sbitmap_vector_alloc (n_blocks, n_exprs);
  comp = sbitmap_vector_alloc (n_blocks, n_exprs);
  antloc = sbitmap_vector_alloc (n_blocks, n_exprs);

  pre_optimal = NULL;
  pre_redundant = NULL;
  pre_insert_map = NULL;
  pre_delete_map = NULL;
  ae_in = NULL;
  ae_out = NULL;
  ae_kill = sbitmap_vector_alloc (n_blocks, n_exprs);

  /* pre_insert and pre_delete are allocated later.  */
}

/* Free vars used for PRE analysis.  */

static void
free_pre_mem ()
{
  sbitmap_vector_free (transp);
  sbitmap_vector_free (comp);

  /* ANTLOC and AE_KILL are freed just after pre_lcm finishes.  */

  if (pre_optimal)
    sbitmap_vector_free (pre_optimal);
  if (pre_redundant)
    sbitmap_vector_free (pre_redundant);
  if (pre_insert_map)
    sbitmap_vector_free (pre_insert_map);
  if (pre_delete_map)
    sbitmap_vector_free (pre_delete_map);
  if (ae_in)
    sbitmap_vector_free (ae_in);
  if (ae_out)
    sbitmap_vector_free (ae_out);

  transp = comp = NULL;
  pre_optimal = pre_redundant = pre_insert_map = pre_delete_map = NULL;
  ae_in = ae_out = NULL;
}

/* Top level routine to do the dataflow analysis needed by PRE.  */

static void
compute_pre_data ()
{
  sbitmap trapping_expr;
  basic_block bb;
  unsigned int ui;

  compute_local_properties (transp, comp, antloc, &expr_hash_table);
  sbitmap_vector_zero (ae_kill, last_basic_block);

  /* Collect expressions which might trap.  */
  trapping_expr = sbitmap_alloc (expr_hash_table.n_elems);
  sbitmap_zero (trapping_expr);
  for (ui = 0; ui < expr_hash_table.size; ui++)
    {
      struct expr *e;
      for (e = expr_hash_table.table[ui]; e != NULL; e = e->next_same_hash)
	if (may_trap_p (e->expr))
	  SET_BIT (trapping_expr, e->bitmap_index);
    }

  /* Compute ae_kill for each basic block using:

     ~(TRANSP | COMP)

     This is significantly faster than compute_ae_kill.  */

  FOR_EACH_BB (bb)
    {
      edge e;

      /* If the current block is the destination of an abnormal edge, we
	 kill all trapping expressions because we won't be able to properly
	 place the instruction on the edge.  So make them neither
	 anticipatable nor transparent.  This is fairly conservative.  */
      for (e = bb->pred; e ; e = e->pred_next)
	if (e->flags & EDGE_ABNORMAL)
	  {
	    sbitmap_difference (antloc[bb->index], antloc[bb->index], trapping_expr);
	    sbitmap_difference (transp[bb->index], transp[bb->index], trapping_expr);
	    break;
	  }

      sbitmap_a_or_b (ae_kill[bb->index], transp[bb->index], comp[bb->index]);
      sbitmap_not (ae_kill[bb->index], ae_kill[bb->index]);
    }

  edge_list = pre_edge_lcm (gcse_file, expr_hash_table.n_elems, transp, comp, antloc,
			    ae_kill, &pre_insert_map, &pre_delete_map);
  sbitmap_vector_free (antloc);
  antloc = NULL;
  sbitmap_vector_free (ae_kill);
  ae_kill = NULL;
  sbitmap_free (trapping_expr);
}

/* PRE utilities */

/* Return nonzero if an occurrence of expression EXPR in OCCR_BB would reach
   block BB.

   VISITED is a pointer to a working buffer for tracking which BB's have
   been visited.  It is NULL for the top-level call.

   We treat reaching expressions that go through blocks containing the same
   reaching expression as "not reaching".  E.g. if EXPR is generated in blocks
   2 and 3, INSN is in block 4, and 2->3->4, we treat the expression in block
   2 as not reaching.  The intent is to improve the probability of finding
   only one reaching expression and to reduce register lifetimes by picking
   the closest such expression.  */

static int
pre_expr_reaches_here_p_work (occr_bb, expr, bb, visited)
     basic_block occr_bb;
     struct expr *expr;
     basic_block bb;
     char *visited;
{
  edge pred;

  for (pred = bb->pred; pred != NULL; pred = pred->pred_next)
    {
      basic_block pred_bb = pred->src;

      if (pred->src == ENTRY_BLOCK_PTR
	  /* Has predecessor has already been visited?  */
	  || visited[pred_bb->index])
	;/* Nothing to do.  */

      /* Does this predecessor generate this expression?  */
      else if (TEST_BIT (comp[pred_bb->index], expr->bitmap_index))
	{
	  /* Is this the occurrence we're looking for?
	     Note that there's only one generating occurrence per block
	     so we just need to check the block number.  */
	  if (occr_bb == pred_bb)
	    return 1;

	  visited[pred_bb->index] = 1;
	}
      /* Ignore this predecessor if it kills the expression.  */
      else if (! TEST_BIT (transp[pred_bb->index], expr->bitmap_index))
	visited[pred_bb->index] = 1;

      /* Neither gen nor kill.  */
      else
	{
	  visited[pred_bb->index] = 1;
	  if (pre_expr_reaches_here_p_work (occr_bb, expr, pred_bb, visited))
	    return 1;
	}
    }

  /* All paths have been checked.  */
  return 0;
}

/* The wrapper for pre_expr_reaches_here_work that ensures that any
   memory allocated for that function is returned.  */

static int
pre_expr_reaches_here_p (occr_bb, expr, bb)
     basic_block occr_bb;
     struct expr *expr;
     basic_block bb;
{
  int rval;
  char *visited = (char *) xcalloc (last_basic_block, 1);

  rval = pre_expr_reaches_here_p_work (occr_bb, expr, bb, visited);

  free (visited);
  return rval;
}


/* Given an expr, generate RTL which we can insert at the end of a BB,
   or on an edge.  Set the block number of any insns generated to
   the value of BB.  */

static rtx
process_insert_insn (expr)
     struct expr *expr;
{
  rtx reg = expr->reaching_reg;
  rtx exp = copy_rtx (expr->expr);
  rtx pat;

  start_sequence ();

  /* If the expression is something that's an operand, like a constant,
     just copy it to a register.  */
  if (general_operand (exp, GET_MODE (reg)))
    emit_move_insn (reg, exp);

  /* Otherwise, make a new insn to compute this expression and make sure the
     insn will be recognized (this also adds any needed CLOBBERs).  Copy the
     expression to make sure we don't have any sharing issues.  */
  else if (insn_invalid_p (emit_insn (gen_rtx_SET (VOIDmode, reg, exp))))
    abort ();

  pat = get_insns ();
  end_sequence ();

  return pat;
}

/* Add EXPR to the end of basic block BB.

   This is used by both the PRE and code hoisting.

   For PRE, we want to verify that the expr is either transparent
   or locally anticipatable in the target block.  This check makes
   no sense for code hoisting.  */

static void
insert_insn_end_bb (expr, bb, pre)
     struct expr *expr;
     basic_block bb;
     int pre;
{
  rtx insn = bb->end;
  rtx new_insn;
  rtx reg = expr->reaching_reg;
  int regno = REGNO (reg);
  rtx pat, pat_end;

  pat = process_insert_insn (expr);
  if (pat == NULL_RTX || ! INSN_P (pat))
    abort ();

  pat_end = pat;
  while (NEXT_INSN (pat_end) != NULL_RTX)
    pat_end = NEXT_INSN (pat_end);

  /* If the last insn is a jump, insert EXPR in front [taking care to
     handle cc0, etc. properly].  Similary we need to care trapping
     instructions in presence of non-call exceptions.  */

  if (GET_CODE (insn) == JUMP_INSN
      || (GET_CODE (insn) == INSN
	  && (bb->succ->succ_next || (bb->succ->flags & EDGE_ABNORMAL))))
    {
#ifdef HAVE_cc0
      rtx note;
#endif
      /* It should always be the case that we can put these instructions
	 anywhere in the basic block with performing PRE optimizations.
	 Check this.  */
      if (GET_CODE (insn) == INSN && pre
	  && !TEST_BIT (antloc[bb->index], expr->bitmap_index)
	  && !TEST_BIT (transp[bb->index], expr->bitmap_index))
	abort ();

      /* If this is a jump table, then we can't insert stuff here.  Since
	 we know the previous real insn must be the tablejump, we insert
	 the new instruction just before the tablejump.  */
      if (GET_CODE (PATTERN (insn)) == ADDR_VEC
	  || GET_CODE (PATTERN (insn)) == ADDR_DIFF_VEC)
	insn = prev_real_insn (insn);

#ifdef HAVE_cc0
      /* FIXME: 'twould be nice to call prev_cc0_setter here but it aborts
	 if cc0 isn't set.  */
      note = find_reg_note (insn, REG_CC_SETTER, NULL_RTX);
      if (note)
	insn = XEXP (note, 0);
      else
	{
	  rtx maybe_cc0_setter = prev_nonnote_insn (insn);
	  if (maybe_cc0_setter
	      && INSN_P (maybe_cc0_setter)
	      && sets_cc0_p (PATTERN (maybe_cc0_setter)))
	    insn = maybe_cc0_setter;
	}
#endif
      /* FIXME: What if something in cc0/jump uses value set in new insn?  */
      new_insn = emit_insn_before (pat, insn);
    }

  /* Likewise if the last insn is a call, as will happen in the presence
     of exception handling.  */
  else if (GET_CODE (insn) == CALL_INSN
	   && (bb->succ->succ_next || (bb->succ->flags & EDGE_ABNORMAL)))
    {
      /* Keeping in mind SMALL_REGISTER_CLASSES and parameters in registers,
	 we search backward and place the instructions before the first
	 parameter is loaded.  Do this for everyone for consistency and a
	 presumtion that we'll get better code elsewhere as well.

	 It should always be the case that we can put these instructions
	 anywhere in the basic block with performing PRE optimizations.
	 Check this.  */

      if (pre
	  && !TEST_BIT (antloc[bb->index], expr->bitmap_index)
	  && !TEST_BIT (transp[bb->index], expr->bitmap_index))
	abort ();

      /* Since different machines initialize their parameter registers
	 in different orders, assume nothing.  Collect the set of all
	 parameter registers.  */
      insn = find_first_parameter_load (insn, bb->head);

      /* If we found all the parameter loads, then we want to insert
	 before the first parameter load.

	 If we did not find all the parameter loads, then we might have
	 stopped on the head of the block, which could be a CODE_LABEL.
	 If we inserted before the CODE_LABEL, then we would be putting
	 the insn in the wrong basic block.  In that case, put the insn
	 after the CODE_LABEL.  Also, respect NOTE_INSN_BASIC_BLOCK.  */
      while (GET_CODE (insn) == CODE_LABEL
	     || NOTE_INSN_BASIC_BLOCK_P (insn))
	insn = NEXT_INSN (insn);

      new_insn = emit_insn_before (pat, insn);
    }
  else
    new_insn = emit_insn_after (pat, insn);

  while (1)
    {
      if (INSN_P (pat))
	{
	  add_label_notes (PATTERN (pat), new_insn);
	  note_stores (PATTERN (pat), record_set_info, pat);
	}
      if (pat == pat_end)
	break;
      pat = NEXT_INSN (pat);
    }

  gcse_create_count++;

  if (gcse_file)
    {
      fprintf (gcse_file, "PRE/HOIST: end of bb %d, insn %d, ",
	       bb->index, INSN_UID (new_insn));
      fprintf (gcse_file, "copying expression %d to reg %d\n",
	       expr->bitmap_index, regno);
    }
}

/* Insert partially redundant expressions on edges in the CFG to make
   the expressions fully redundant.  */

static int
pre_edge_insert (edge_list, index_map)
     struct edge_list *edge_list;
     struct expr **index_map;
{
  int e, i, j, num_edges, set_size, did_insert = 0;
  sbitmap *inserted;

  /* Where PRE_INSERT_MAP is nonzero, we add the expression on that edge
     if it reaches any of the deleted expressions.  */

  set_size = pre_insert_map[0]->size;
  num_edges = NUM_EDGES (edge_list);
  inserted = sbitmap_vector_alloc (num_edges, expr_hash_table.n_elems);
  sbitmap_vector_zero (inserted, num_edges);

  for (e = 0; e < num_edges; e++)
    {
      int indx;
      basic_block bb = INDEX_EDGE_PRED_BB (edge_list, e);

      for (i = indx = 0; i < set_size; i++, indx += SBITMAP_ELT_BITS)
	{
	  SBITMAP_ELT_TYPE insert = pre_insert_map[e]->elms[i];

	  for (j = indx; insert && j < (int) expr_hash_table.n_elems; j++, insert >>= 1)
	    if ((insert & 1) != 0 && index_map[j]->reaching_reg != NULL_RTX)
	      {
		struct expr *expr = index_map[j];
		struct occr *occr;

		/* Now look at each deleted occurrence of this expression.  */
		for (occr = expr->antic_occr; occr != NULL; occr = occr->next)
		  {
		    if (! occr->deleted_p)
		      continue;

		    /* Insert this expression on this edge if if it would
		       reach the deleted occurrence in BB.  */
		    if (!TEST_BIT (inserted[e], j))
		      {
			rtx insn;
			edge eg = INDEX_EDGE (edge_list, e);

			/* We can't insert anything on an abnormal and
			   critical edge, so we insert the insn at the end of
			   the previous block. There are several alternatives
			   detailed in Morgans book P277 (sec 10.5) for
			   handling this situation.  This one is easiest for
			   now.  */

			if ((eg->flags & EDGE_ABNORMAL) == EDGE_ABNORMAL)
			  insert_insn_end_bb (index_map[j], bb, 0);
			else
			  {
			    insn = process_insert_insn (index_map[j]);
			    insert_insn_on_edge (insn, eg);
			  }

			if (gcse_file)
			  {
			    fprintf (gcse_file, "PRE/HOIST: edge (%d,%d), ",
				     bb->index,
				     INDEX_EDGE_SUCC_BB (edge_list, e)->index);
			    fprintf (gcse_file, "copy expression %d\n",
				     expr->bitmap_index);
			  }

			update_ld_motion_stores (expr);
			SET_BIT (inserted[e], j);
			did_insert = 1;
			gcse_create_count++;
		      }
		  }
	      }
	}
    }

  sbitmap_vector_free (inserted);
  return did_insert;
}

/* Copy the result of INSN to REG.  INDX is the expression number.  */

static void
pre_insert_copy_insn (expr, insn)
     struct expr *expr;
     rtx insn;
{
  rtx reg = expr->reaching_reg;
  int regno = REGNO (reg);
  int indx = expr->bitmap_index;
  rtx set = single_set (insn);
  rtx new_insn;

  if (!set)
    abort ();

  new_insn = emit_insn_after (gen_move_insn (reg, SET_DEST (set)), insn);

  /* Keep register set table up to date.  */
  record_one_set (regno, new_insn);

  gcse_create_count++;

  if (gcse_file)
    fprintf (gcse_file,
	     "PRE: bb %d, insn %d, copy expression %d in insn %d to reg %d\n",
	      BLOCK_NUM (insn), INSN_UID (new_insn), indx,
	      INSN_UID (insn), regno);
  update_ld_motion_stores (expr);
}

/* Copy available expressions that reach the redundant expression
   to `reaching_reg'.  */

static void
pre_insert_copies ()
{
  unsigned int i;
  struct expr *expr;
  struct occr *occr;
  struct occr *avail;

  /* For each available expression in the table, copy the result to
     `reaching_reg' if the expression reaches a deleted one.

     ??? The current algorithm is rather brute force.
     Need to do some profiling.  */

  for (i = 0; i < expr_hash_table.size; i++)
    for (expr = expr_hash_table.table[i]; expr != NULL; expr = expr->next_same_hash)
      {
	/* If the basic block isn't reachable, PPOUT will be TRUE.  However,
	   we don't want to insert a copy here because the expression may not
	   really be redundant.  So only insert an insn if the expression was
	   deleted.  This test also avoids further processing if the
	   expression wasn't deleted anywhere.  */
	if (expr->reaching_reg == NULL)
	  continue;

	for (occr = expr->antic_occr; occr != NULL; occr = occr->next)
	  {
	    if (! occr->deleted_p)
	      continue;

	    for (avail = expr->avail_occr; avail != NULL; avail = avail->next)
	      {
		rtx insn = avail->insn;

		/* No need to handle this one if handled already.  */
		if (avail->copied_p)
		  continue;

		/* Don't handle this one if it's a redundant one.  */
		if (TEST_BIT (pre_redundant_insns, INSN_CUID (insn)))
		  continue;

		/* Or if the expression doesn't reach the deleted one.  */
		if (! pre_expr_reaches_here_p (BLOCK_FOR_INSN (avail->insn),
					       expr,
					       BLOCK_FOR_INSN (occr->insn)))
		  continue;

		/* Copy the result of avail to reaching_reg.  */
		pre_insert_copy_insn (expr, insn);
		avail->copied_p = 1;
	      }
	  }
      }
}

/* Emit move from SRC to DEST noting the equivalence with expression computed
   in INSN.  */
static rtx
gcse_emit_move_after (src, dest, insn)
     rtx src, dest, insn;
{
  rtx new;
  rtx set = single_set (insn), set2;
  rtx note;
  rtx eqv;

  /* This should never fail since we're creating a reg->reg copy
     we've verified to be valid.  */

  new = emit_insn_after (gen_move_insn (dest, src), insn);

  /* Note the equivalence for local CSE pass.  */
  set2 = single_set (new);
  if (!set2 || !rtx_equal_p (SET_DEST (set2), dest))
    return new;
  if ((note = find_reg_equal_equiv_note (insn)))
    eqv = XEXP (note, 0);
  else
    eqv = SET_SRC (set);

  set_unique_reg_note (new, REG_EQUAL, copy_insn_1 (eqv));

  return new;
}

/* Delete redundant computations.
   Deletion is done by changing the insn to copy the `reaching_reg' of
   the expression into the result of the SET.  It is left to later passes
   (cprop, cse2, flow, combine, regmove) to propagate the copy or eliminate it.

   Returns nonzero if a change is made.  */

static int
pre_delete ()
{
  unsigned int i;
  int changed;
  struct expr *expr;
  struct occr *occr;

  changed = 0;
  for (i = 0; i < expr_hash_table.size; i++)
    for (expr = expr_hash_table.table[i]; expr != NULL; expr = expr->next_same_hash)
      {
	int indx = expr->bitmap_index;

	/* We only need to search antic_occr since we require
	   ANTLOC != 0.  */

	for (occr = expr->antic_occr; occr != NULL; occr = occr->next)
	  {
	    rtx insn = occr->insn;
	    rtx set;
	    basic_block bb = BLOCK_FOR_INSN (insn);

	    if (TEST_BIT (pre_delete_map[bb->index], indx))
	      {
		set = single_set (insn);
		if (! set)
		  abort ();

		/* Create a pseudo-reg to store the result of reaching
		   expressions into.  Get the mode for the new pseudo from
		   the mode of the original destination pseudo.  */
		if (expr->reaching_reg == NULL)
		  expr->reaching_reg
		    = gen_reg_rtx (GET_MODE (SET_DEST (set)));

		gcse_emit_move_after (expr->reaching_reg, SET_DEST (set), insn);
		delete_insn (insn);
		occr->deleted_p = 1;
		SET_BIT (pre_redundant_insns, INSN_CUID (insn));
		changed = 1;
		gcse_subst_count++;

		if (gcse_file)
		  {
		    fprintf (gcse_file,
			     "PRE: redundant insn %d (expression %d) in ",
			       INSN_UID (insn), indx);
		    fprintf (gcse_file, "bb %d, reaching reg is %d\n",
			     bb->index, REGNO (expr->reaching_reg));
		  }
	      }
	  }
      }

  return changed;
}

/* Perform GCSE optimizations using PRE.
   This is called by one_pre_gcse_pass after all the dataflow analysis
   has been done.

   This is based on the original Morel-Renvoise paper Fred Chow's thesis, and
   lazy code motion from Knoop, Ruthing and Steffen as described in Advanced
   Compiler Design and Implementation.

   ??? A new pseudo reg is created to hold the reaching expression.  The nice
   thing about the classical approach is that it would try to use an existing
   reg.  If the register can't be adequately optimized [i.e. we introduce
   reload problems], one could add a pass here to propagate the new register
   through the block.

   ??? We don't handle single sets in PARALLELs because we're [currently] not
   able to copy the rest of the parallel when we insert copies to create full
   redundancies from partial redundancies.  However, there's no reason why we
   can't handle PARALLELs in the cases where there are no partial
   redundancies.  */

static int
pre_gcse ()
{
  unsigned int i;
  int did_insert, changed;
  struct expr **index_map;
  struct expr *expr;

  /* Compute a mapping from expression number (`bitmap_index') to
     hash table entry.  */

  index_map = (struct expr **) xcalloc (expr_hash_table.n_elems, sizeof (struct expr *));
  for (i = 0; i < expr_hash_table.size; i++)
    for (expr = expr_hash_table.table[i]; expr != NULL; expr = expr->next_same_hash)
      index_map[expr->bitmap_index] = expr;

  /* Reset bitmap used to track which insns are redundant.  */
  pre_redundant_insns = sbitmap_alloc (max_cuid);
  sbitmap_zero (pre_redundant_insns);

  /* Delete the redundant insns first so that
     - we know what register to use for the new insns and for the other
       ones with reaching expressions
     - we know which insns are redundant when we go to create copies  */

  changed = pre_delete ();

  did_insert = pre_edge_insert (edge_list, index_map);

  /* In other places with reaching expressions, copy the expression to the
     specially allocated pseudo-reg that reaches the redundant expr.  */
  pre_insert_copies ();
  if (did_insert)
    {
      commit_edge_insertions ();
      changed = 1;
    }

  free (index_map);
  sbitmap_free (pre_redundant_insns);
  return changed;
}

/* Top level routine to perform one PRE GCSE pass.

   Return nonzero if a change was made.  */

static int
one_pre_gcse_pass (pass)
     int pass;
{
  int changed = 0;

  gcse_subst_count = 0;
  gcse_create_count = 0;

  alloc_hash_table (max_cuid, &expr_hash_table, 0);
  add_noreturn_fake_exit_edges ();
  if (flag_gcse_lm)
    compute_ld_motion_mems ();

  compute_hash_table (&expr_hash_table);
  trim_ld_motion_mems ();
  if (gcse_file)
    dump_hash_table (gcse_file, "Expression", &expr_hash_table);

  if (expr_hash_table.n_elems > 0)
    {
      alloc_pre_mem (last_basic_block, expr_hash_table.n_elems);
      compute_pre_data ();
      changed |= pre_gcse ();
      free_edge_list (edge_list);
      free_pre_mem ();
    }

  free_ldst_mems ();
  remove_fake_edges ();
  free_hash_table (&expr_hash_table);

  if (gcse_file)
    {
      fprintf (gcse_file, "\nPRE GCSE of %s, pass %d: %d bytes needed, ",
	       current_function_name, pass, bytes_used);
      fprintf (gcse_file, "%d substs, %d insns created\n",
	       gcse_subst_count, gcse_create_count);
    }

  return changed;
}

/* If X contains any LABEL_REF's, add REG_LABEL notes for them to INSN.
   If notes are added to an insn which references a CODE_LABEL, the
   LABEL_NUSES count is incremented.  We have to add REG_LABEL notes,
   because the following loop optimization pass requires them.  */

/* ??? This is very similar to the loop.c add_label_notes function.  We
   could probably share code here.  */

/* ??? If there was a jump optimization pass after gcse and before loop,
   then we would not need to do this here, because jump would add the
   necessary REG_LABEL notes.  */

static void
add_label_notes (x, insn)
     rtx x;
     rtx insn;
{
  enum rtx_code code = GET_CODE (x);
  int i, j;
  const char *fmt;

  if (code == LABEL_REF && !LABEL_REF_NONLOCAL_P (x))
    {
      /* This code used to ignore labels that referred to dispatch tables to
	 avoid flow generating (slighly) worse code.

	 We no longer ignore such label references (see LABEL_REF handling in
	 mark_jump_label for additional information).  */

      REG_NOTES (insn) = gen_rtx_INSN_LIST (REG_LABEL, XEXP (x, 0),
					    REG_NOTES (insn));
      if (LABEL_P (XEXP (x, 0)))
	LABEL_NUSES (XEXP (x, 0))++;
      return;
    }

  for (i = GET_RTX_LENGTH (code) - 1, fmt = GET_RTX_FORMAT (code); i >= 0; i--)
    {
      if (fmt[i] == 'e')
	add_label_notes (XEXP (x, i), insn);
      else if (fmt[i] == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  add_label_notes (XVECEXP (x, i, j), insn);
    }
}

/* Compute transparent outgoing information for each block.

   An expression is transparent to an edge unless it is killed by
   the edge itself.  This can only happen with abnormal control flow,
   when the edge is traversed through a call.  This happens with
   non-local labels and exceptions.

   This would not be necessary if we split the edge.  While this is
   normally impossible for abnormal critical edges, with some effort
   it should be possible with exception handling, since we still have
   control over which handler should be invoked.  But due to increased
   EH table sizes, this may not be worthwhile.  */

static void
compute_transpout ()
{
  basic_block bb;
  unsigned int i;
  struct expr *expr;

  sbitmap_vector_ones (transpout, last_basic_block);

  FOR_EACH_BB (bb)
    {
      /* Note that flow inserted a nop a the end of basic blocks that
	 end in call instructions for reasons other than abnormal
	 control flow.  */
      if (GET_CODE (bb->end) != CALL_INSN)
	continue;

      for (i = 0; i < expr_hash_table.size; i++)
	for (expr = expr_hash_table.table[i]; expr ; expr = expr->next_same_hash)
	  if (GET_CODE (expr->expr) == MEM)
	    {
	      if (GET_CODE (XEXP (expr->expr, 0)) == SYMBOL_REF
		  && CONSTANT_POOL_ADDRESS_P (XEXP (expr->expr, 0)))
		continue;

	      /* ??? Optimally, we would use interprocedural alias
		 analysis to determine if this mem is actually killed
		 by this call.  */
	      RESET_BIT (transpout[bb->index], expr->bitmap_index);
	    }
    }
}

/* Removal of useless null pointer checks */

/* Called via note_stores.  X is set by SETTER.  If X is a register we must
   invalidate nonnull_local and set nonnull_killed.  DATA is really a
   `null_pointer_info *'.

   We ignore hard registers.  */

static void
invalidate_nonnull_info (x, setter, data)
     rtx x;
     rtx setter ATTRIBUTE_UNUSED;
     void *data;
{
  unsigned int regno;
  struct null_pointer_info *npi = (struct null_pointer_info *) data;

  while (GET_CODE (x) == SUBREG)
    x = SUBREG_REG (x);

  /* Ignore anything that is not a register or is a hard register.  */
  if (GET_CODE (x) != REG
      || REGNO (x) < npi->min_reg
      || REGNO (x) >= npi->max_reg)
    return;

  regno = REGNO (x) - npi->min_reg;

  RESET_BIT (npi->nonnull_local[npi->current_block->index], regno);
  SET_BIT (npi->nonnull_killed[npi->current_block->index], regno);
}

/* Do null-pointer check elimination for the registers indicated in
   NPI.  NONNULL_AVIN and NONNULL_AVOUT are pre-allocated sbitmaps;
   they are not our responsibility to free.  */

static int
delete_null_pointer_checks_1 (block_reg, nonnull_avin,
			      nonnull_avout, npi)
     unsigned int *block_reg;
     sbitmap *nonnull_avin;
     sbitmap *nonnull_avout;
     struct null_pointer_info *npi;
{
  basic_block bb, current_block;
  sbitmap *nonnull_local = npi->nonnull_local;
  sbitmap *nonnull_killed = npi->nonnull_killed;
  int something_changed = 0;

  /* Compute local properties, nonnull and killed.  A register will have
     the nonnull property if at the end of the current block its value is
     known to be nonnull.  The killed property indicates that somewhere in
     the block any information we had about the register is killed.

     Note that a register can have both properties in a single block.  That
     indicates that it's killed, then later in the block a new value is
     computed.  */
  sbitmap_vector_zero (nonnull_local, last_basic_block);
  sbitmap_vector_zero (nonnull_killed, last_basic_block);

  FOR_EACH_BB (current_block)
    {
      rtx insn, stop_insn;

      /* Set the current block for invalidate_nonnull_info.  */
      npi->current_block = current_block;

      /* Scan each insn in the basic block looking for memory references and
	 register sets.  */
      stop_insn = NEXT_INSN (current_block->end);
      for (insn = current_block->head;
	   insn != stop_insn;
	   insn = NEXT_INSN (insn))
	{
	  rtx set;
	  rtx reg;

	  /* Ignore anything that is not a normal insn.  */
	  if (! INSN_P (insn))
	    continue;

	  /* Basically ignore anything that is not a simple SET.  We do have
	     to make sure to invalidate nonnull_local and set nonnull_killed
	     for such insns though.  */
	  set = single_set (insn);
	  if (!set)
	    {
	      note_stores (PATTERN (insn), invalidate_nonnull_info, npi);
	      continue;
	    }

	  /* See if we've got a usable memory load.  We handle it first
	     in case it uses its address register as a dest (which kills
	     the nonnull property).  */
	  if (GET_CODE (SET_SRC (set)) == MEM
	      && GET_CODE ((reg = XEXP (SET_SRC (set), 0))) == REG
	      && REGNO (reg) >= npi->min_reg
	      && REGNO (reg) < npi->max_reg)
	    SET_BIT (nonnull_local[current_block->index],
		     REGNO (reg) - npi->min_reg);

	  /* Now invalidate stuff clobbered by this insn.  */
	  note_stores (PATTERN (insn), invalidate_nonnull_info, npi);

	  /* And handle stores, we do these last since any sets in INSN can
	     not kill the nonnull property if it is derived from a MEM
	     appearing in a SET_DEST.  */
	  if (GET_CODE (SET_DEST (set)) == MEM
	      && GET_CODE ((reg = XEXP (SET_DEST (set), 0))) == REG
	      && REGNO (reg) >= npi->min_reg
	      && REGNO (reg) < npi->max_reg)
	    SET_BIT (nonnull_local[current_block->index],
		     REGNO (reg) - npi->min_reg);
	}
    }

  /* Now compute global properties based on the local properties.   This
     is a classic global availablity algorithm.  */
  compute_available (nonnull_local, nonnull_killed,
		     nonnull_avout, nonnull_avin);

  /* Now look at each bb and see if it ends with a compare of a value
     against zero.  */
  FOR_EACH_BB (bb)
    {
      rtx last_insn = bb->end;
      rtx condition, earliest;
      int compare_and_branch;

      /* Since MIN_REG is always at least FIRST_PSEUDO_REGISTER, and
	 since BLOCK_REG[BB] is zero if this block did not end with a
	 comparison against zero, this condition works.  */
      if (block_reg[bb->index] < npi->min_reg
	  || block_reg[bb->index] >= npi->max_reg)
	continue;

      /* LAST_INSN is a conditional jump.  Get its condition.  */
      condition = get_condition (last_insn, &earliest);

      /* If we can't determine the condition then skip.  */
      if (! condition)
	continue;

      /* Is the register known to have a nonzero value?  */
      if (!TEST_BIT (nonnull_avout[bb->index], block_reg[bb->index] - npi->min_reg))
	continue;

      /* Try to compute whether the compare/branch at the loop end is one or
	 two instructions.  */
      if (earliest == last_insn)
	compare_and_branch = 1;
      else if (earliest == prev_nonnote_insn (last_insn))
	compare_and_branch = 2;
      else
	continue;

      /* We know the register in this comparison is nonnull at exit from
	 this block.  We can optimize this comparison.  */
      if (GET_CODE (condition) == NE)
	{
	  rtx new_jump;

	  new_jump = emit_jump_insn_after (gen_jump (JUMP_LABEL (last_insn)),
					   last_insn);
	  JUMP_LABEL (new_jump) = JUMP_LABEL (last_insn);
	  LABEL_NUSES (JUMP_LABEL (new_jump))++;
	  emit_barrier_after (new_jump);
	}

      something_changed = 1;
      delete_insn (last_insn);
      if (compare_and_branch == 2)
	delete_insn (earliest);
      purge_dead_edges (bb);

      /* Don't check this block again.  (Note that BLOCK_END is
	 invalid here; we deleted the last instruction in the
	 block.)  */
      block_reg[bb->index] = 0;
    }

  return something_changed;
}

/* Find EQ/NE comparisons against zero which can be (indirectly) evaluated
   at compile time.

   This is conceptually similar to global constant/copy propagation and
   classic global CSE (it even uses the same dataflow equations as cprop).

   If a register is used as memory address with the form (mem (reg)), then we
   know that REG can not be zero at that point in the program.  Any instruction
   which sets REG "kills" this property.

   So, if every path leading to a conditional branch has an available memory
   reference of that form, then we know the register can not have the value
   zero at the conditional branch.

   So we merely need to compute the local properies and propagate that data
   around the cfg, then optimize where possible.

   We run this pass two times.  Once before CSE, then again after CSE.  This
   has proven to be the most profitable approach.  It is rare for new
   optimization opportunities of this nature to appear after the first CSE
   pass.

   This could probably be integrated with global cprop with a little work.  */

int
delete_null_pointer_checks (f)
     rtx f ATTRIBUTE_UNUSED;
{
  sbitmap *nonnull_avin, *nonnull_avout;
  unsigned int *block_reg;
  basic_block bb;
  int reg;
  int regs_per_pass;
  int max_reg;
  struct null_pointer_info npi;
  int something_changed = 0;

  /* If we have only a single block, then there's nothing to do.  */
  if (n_basic_blocks <= 1)
    return 0;

  /* Trying to perform global optimizations on flow graphs which have
     a high connectivity will take a long time and is unlikely to be
     particularly useful.

     In normal circumstances a cfg should have about twice as many edges
     as blocks.  But we do not want to punish small functions which have
     a couple switch statements.  So we require a relatively large number
     of basic blocks and the ratio of edges to blocks to be high.  */
  if (n_basic_blocks > 1000 && n_edges / n_basic_blocks >= 20)
    return 0;

  /* We need four bitmaps, each with a bit for each register in each
     basic block.  */
  max_reg = max_reg_num ();
  regs_per_pass = get_bitmap_width (4, last_basic_block, max_reg);

  /* Allocate bitmaps to hold local and global properties.  */
  npi.nonnull_local = sbitmap_vector_alloc (last_basic_block, regs_per_pass);
  npi.nonnull_killed = sbitmap_vector_alloc (last_basic_block, regs_per_pass);
  nonnull_avin = sbitmap_vector_alloc (last_basic_block, regs_per_pass);
  nonnull_avout = sbitmap_vector_alloc (last_basic_block, regs_per_pass);

  /* Go through the basic blocks, seeing whether or not each block
     ends with a conditional branch whose condition is a comparison
     against zero.  Record the register compared in BLOCK_REG.  */
  block_reg = (unsigned int *) xcalloc (last_basic_block, sizeof (int));
  FOR_EACH_BB (bb)
    {
      rtx last_insn = bb->end;
      rtx condition, earliest, reg;

      /* We only want conditional branches.  */
      if (GET_CODE (last_insn) != JUMP_INSN
	  || !any_condjump_p (last_insn)
	  || !onlyjump_p (last_insn))
	continue;

      /* LAST_INSN is a conditional jump.  Get its condition.  */
      condition = get_condition (last_insn, &earliest);

      /* If we were unable to get the condition, or it is not an equality
	 comparison against zero then there's nothing we can do.  */
      if (!condition
	  || (GET_CODE (condition) != NE && GET_CODE (condition) != EQ)
	  || GET_CODE (XEXP (condition, 1)) != CONST_INT
	  || (XEXP (condition, 1)
	      != CONST0_RTX (GET_MODE (XEXP (condition, 0)))))
	continue;

      /* We must be checking a register against zero.  */
      reg = XEXP (condition, 0);
      if (GET_CODE (reg) != REG)
	continue;

      block_reg[bb->index] = REGNO (reg);
    }

  /* Go through the algorithm for each block of registers.  */
  for (reg = FIRST_PSEUDO_REGISTER; reg < max_reg; reg += regs_per_pass)
    {
      npi.min_reg = reg;
      npi.max_reg = MIN (reg + regs_per_pass, max_reg);
      something_changed |= delete_null_pointer_checks_1 (block_reg,
							 nonnull_avin,
							 nonnull_avout,
							 &npi);
    }

  /* Free the table of registers compared at the end of every block.  */
  free (block_reg);

  /* Free bitmaps.  */
  sbitmap_vector_free (npi.nonnull_local);
  sbitmap_vector_free (npi.nonnull_killed);
  sbitmap_vector_free (nonnull_avin);
  sbitmap_vector_free (nonnull_avout);

  return something_changed;
}

/* Code Hoisting variables and subroutines.  */

/* Very busy expressions.  */
static sbitmap *hoist_vbein;
static sbitmap *hoist_vbeout;

/* Hoistable expressions.  */
static sbitmap *hoist_exprs;

/* Dominator bitmaps.  */
dominance_info dominators;

/* ??? We could compute post dominators and run this algorithm in
   reverse to perform tail merging, doing so would probably be
   more effective than the tail merging code in jump.c.

   It's unclear if tail merging could be run in parallel with
   code hoisting.  It would be nice.  */

/* Allocate vars used for code hoisting analysis.  */

static void
alloc_code_hoist_mem (n_blocks, n_exprs)
     int n_blocks, n_exprs;
{
  antloc = sbitmap_vector_alloc (n_blocks, n_exprs);
  transp = sbitmap_vector_alloc (n_blocks, n_exprs);
  comp = sbitmap_vector_alloc (n_blocks, n_exprs);

  hoist_vbein = sbitmap_vector_alloc (n_blocks, n_exprs);
  hoist_vbeout = sbitmap_vector_alloc (n_blocks, n_exprs);
  hoist_exprs = sbitmap_vector_alloc (n_blocks, n_exprs);
  transpout = sbitmap_vector_alloc (n_blocks, n_exprs);
}

/* Free vars used for code hoisting analysis.  */

static void
free_code_hoist_mem ()
{
  sbitmap_vector_free (antloc);
  sbitmap_vector_free (transp);
  sbitmap_vector_free (comp);

  sbitmap_vector_free (hoist_vbein);
  sbitmap_vector_free (hoist_vbeout);
  sbitmap_vector_free (hoist_exprs);
  sbitmap_vector_free (transpout);

  free_dominance_info (dominators);
}

/* Compute the very busy expressions at entry/exit from each block.

   An expression is very busy if all paths from a given point
   compute the expression.  */

static void
compute_code_hoist_vbeinout ()
{
  int changed, passes;
  basic_block bb;

  sbitmap_vector_zero (hoist_vbeout, last_basic_block);
  sbitmap_vector_zero (hoist_vbein, last_basic_block);

  passes = 0;
  changed = 1;

  while (changed)
    {
      changed = 0;

      /* We scan the blocks in the reverse order to speed up
	 the convergence.  */
      FOR_EACH_BB_REVERSE (bb)
	{
	  changed |= sbitmap_a_or_b_and_c_cg (hoist_vbein[bb->index], antloc[bb->index],
					      hoist_vbeout[bb->index], transp[bb->index]);
	  if (bb->next_bb != EXIT_BLOCK_PTR)
	    sbitmap_intersection_of_succs (hoist_vbeout[bb->index], hoist_vbein, bb->index);
	}

      passes++;
    }

  if (gcse_file)
    fprintf (gcse_file, "hoisting vbeinout computation: %d passes\n", passes);
}

/* Top level routine to do the dataflow analysis needed by code hoisting.  */

static void
compute_code_hoist_data ()
{
  compute_local_properties (transp, comp, antloc, &expr_hash_table);
  compute_transpout ();
  compute_code_hoist_vbeinout ();
  dominators = calculate_dominance_info (CDI_DOMINATORS);
  if (gcse_file)
    fprintf (gcse_file, "\n");
}

/* Determine if the expression identified by EXPR_INDEX would
   reach BB unimpared if it was placed at the end of EXPR_BB.

   It's unclear exactly what Muchnick meant by "unimpared".  It seems
   to me that the expression must either be computed or transparent in
   *every* block in the path(s) from EXPR_BB to BB.  Any other definition
   would allow the expression to be hoisted out of loops, even if
   the expression wasn't a loop invariant.

   Contrast this to reachability for PRE where an expression is
   considered reachable if *any* path reaches instead of *all*
   paths.  */

static int
hoist_expr_reaches_here_p (expr_bb, expr_index, bb, visited)
     basic_block expr_bb;
     int expr_index;
     basic_block bb;
     char *visited;
{
  edge pred;
  int visited_allocated_locally = 0;


  if (visited == NULL)
    {
      visited_allocated_locally = 1;
      visited = xcalloc (last_basic_block, 1);
    }

  for (pred = bb->pred; pred != NULL; pred = pred->pred_next)
    {
      basic_block pred_bb = pred->src;

      if (pred->src == ENTRY_BLOCK_PTR)
	break;
      else if (pred_bb == expr_bb)
	continue;
      else if (visited[pred_bb->index])
	continue;

      /* Does this predecessor generate this expression?  */
      else if (TEST_BIT (comp[pred_bb->index], expr_index))
	break;
      else if (! TEST_BIT (transp[pred_bb->index], expr_index))
	break;

      /* Not killed.  */
      else
	{
	  visited[pred_bb->index] = 1;
	  if (! hoist_expr_reaches_here_p (expr_bb, expr_index,
					   pred_bb, visited))
	    break;
	}
    }
  if (visited_allocated_locally)
    free (visited);

  return (pred == NULL);
}

/* Actually perform code hoisting.  */

static void
hoist_code ()
{
  basic_block bb, dominated;
  basic_block *domby;
  unsigned int domby_len;
  unsigned int i,j;
  struct expr **index_map;
  struct expr *expr;

  sbitmap_vector_zero (hoist_exprs, last_basic_block);

  /* Compute a mapping from expression number (`bitmap_index') to
     hash table entry.  */

  index_map = (struct expr **) xcalloc (expr_hash_table.n_elems, sizeof (struct expr *));
  for (i = 0; i < expr_hash_table.size; i++)
    for (expr = expr_hash_table.table[i]; expr != NULL; expr = expr->next_same_hash)
      index_map[expr->bitmap_index] = expr;

  /* Walk over each basic block looking for potentially hoistable
     expressions, nothing gets hoisted from the entry block.  */
  FOR_EACH_BB (bb)
    {
      int found = 0;
      int insn_inserted_p;

      domby_len = get_dominated_by (dominators, bb, &domby);
      /* Examine each expression that is very busy at the exit of this
	 block.  These are the potentially hoistable expressions.  */
      for (i = 0; i < hoist_vbeout[bb->index]->n_bits; i++)
	{
	  int hoistable = 0;

	  if (TEST_BIT (hoist_vbeout[bb->index], i)
	      && TEST_BIT (transpout[bb->index], i))
	    {
	      /* We've found a potentially hoistable expression, now
		 we look at every block BB dominates to see if it
		 computes the expression.  */
	      for (j = 0; j < domby_len; j++)
		{
		  dominated = domby[j];
		  /* Ignore self dominance.  */
		  if (bb == dominated)
		    continue;
		  /* We've found a dominated block, now see if it computes
		     the busy expression and whether or not moving that
		     expression to the "beginning" of that block is safe.  */
		  if (!TEST_BIT (antloc[dominated->index], i))
		    continue;

		  /* Note if the expression would reach the dominated block
		     unimpared if it was placed at the end of BB.

		     Keep track of how many times this expression is hoistable
		     from a dominated block into BB.  */
		  if (hoist_expr_reaches_here_p (bb, i, dominated, NULL))
		    hoistable++;
		}

	      /* If we found more than one hoistable occurrence of this
		 expression, then note it in the bitmap of expressions to
		 hoist.  It makes no sense to hoist things which are computed
		 in only one BB, and doing so tends to pessimize register
		 allocation.  One could increase this value to try harder
		 to avoid any possible code expansion due to register
		 allocation issues; however experiments have shown that
		 the vast majority of hoistable expressions are only movable
		 from two successors, so raising this threshhold is likely
		 to nullify any benefit we get from code hoisting.  */
	      if (hoistable > 1)
		{
		  SET_BIT (hoist_exprs[bb->index], i);
		  found = 1;
		}
	    }
	}
      /* If we found nothing to hoist, then quit now.  */
      if (! found)
        {
  	  free (domby);
	continue;
	}

      /* Loop over all the hoistable expressions.  */
      for (i = 0; i < hoist_exprs[bb->index]->n_bits; i++)
	{
	  /* We want to insert the expression into BB only once, so
	     note when we've inserted it.  */
	  insn_inserted_p = 0;

	  /* These tests should be the same as the tests above.  */
	  if (TEST_BIT (hoist_vbeout[bb->index], i))
	    {
	      /* We've found a potentially hoistable expression, now
		 we look at every block BB dominates to see if it
		 computes the expression.  */
	      for (j = 0; j < domby_len; j++)
		{
		  dominated = domby[j];
		  /* Ignore self dominance.  */
		  if (bb == dominated)
		    continue;

		  /* We've found a dominated block, now see if it computes
		     the busy expression and whether or not moving that
		     expression to the "beginning" of that block is safe.  */
		  if (!TEST_BIT (antloc[dominated->index], i))
		    continue;

		  /* The expression is computed in the dominated block and
		     it would be safe to compute it at the start of the
		     dominated block.  Now we have to determine if the
		     expression would reach the dominated block if it was
		     placed at the end of BB.  */
		  if (hoist_expr_reaches_here_p (bb, i, dominated, NULL))
		    {
		      struct expr *expr = index_map[i];
		      struct occr *occr = expr->antic_occr;
		      rtx insn;
		      rtx set;

		      /* Find the right occurrence of this expression.  */
		      while (BLOCK_FOR_INSN (occr->insn) != dominated && occr)
			occr = occr->next;

		      /* Should never happen.  */
		      if (!occr)
			abort ();

		      insn = occr->insn;

		      set = single_set (insn);
		      if (! set)
			abort ();

		      /* Create a pseudo-reg to store the result of reaching
			 expressions into.  Get the mode for the new pseudo
			 from the mode of the original destination pseudo.  */
		      if (expr->reaching_reg == NULL)
			expr->reaching_reg
			  = gen_reg_rtx (GET_MODE (SET_DEST (set)));

		      gcse_emit_move_after (expr->reaching_reg, SET_DEST (set), insn);
		      delete_insn (insn);
		      occr->deleted_p = 1;
		      if (!insn_inserted_p)
			{
			  insert_insn_end_bb (index_map[i], bb, 0);
			  insn_inserted_p = 1;
			}
		    }
		}
	    }
	}
      free (domby);
    }

  free (index_map);
}

/* Top level routine to perform one code hoisting (aka unification) pass

   Return nonzero if a change was made.  */

static int
one_code_hoisting_pass ()
{
  int changed = 0;

  alloc_hash_table (max_cuid, &expr_hash_table, 0);
  compute_hash_table (&expr_hash_table);
  if (gcse_file)
    dump_hash_table (gcse_file, "Code Hosting Expressions", &expr_hash_table);

  if (expr_hash_table.n_elems > 0)
    {
      alloc_code_hoist_mem (last_basic_block, expr_hash_table.n_elems);
      compute_code_hoist_data ();
      hoist_code ();
      free_code_hoist_mem ();
    }

  free_hash_table (&expr_hash_table);

  return changed;
}

/*  Here we provide the things required to do store motion towards
    the exit. In order for this to be effective, gcse also needed to
    be taught how to move a load when it is kill only by a store to itself.

	    int i;
	    float a[10];

	    void foo(float scale)
	    {
	      for (i=0; i<10; i++)
		a[i] *= scale;
	    }

    'i' is both loaded and stored to in the loop. Normally, gcse cannot move
    the load out since its live around the loop, and stored at the bottom
    of the loop.

      The 'Load Motion' referred to and implemented in this file is
    an enhancement to gcse which when using edge based lcm, recognizes
    this situation and allows gcse to move the load out of the loop.

      Once gcse has hoisted the load, store motion can then push this
    load towards the exit, and we end up with no loads or stores of 'i'
    in the loop.  */

/* This will search the ldst list for a matching expression. If it
   doesn't find one, we create one and initialize it.  */

static struct ls_expr *
ldst_entry (x)
     rtx x;
{
  struct ls_expr * ptr;

  for (ptr = first_ls_expr(); ptr != NULL; ptr = next_ls_expr (ptr))
    if (expr_equiv_p (ptr->pattern, x))
      break;

  if (!ptr)
    {
      ptr = (struct ls_expr *) xmalloc (sizeof (struct ls_expr));

      ptr->next         = pre_ldst_mems;
      ptr->expr         = NULL;
      ptr->pattern      = x;
      ptr->loads        = NULL_RTX;
      ptr->stores       = NULL_RTX;
      ptr->reaching_reg = NULL_RTX;
      ptr->invalid      = 0;
      ptr->index        = 0;
      ptr->hash_index   = 0;
      pre_ldst_mems     = ptr;
    }

  return ptr;
}

/* Free up an individual ldst entry.  */

static void
free_ldst_entry (ptr)
     struct ls_expr * ptr;
{
  free_INSN_LIST_list (& ptr->loads);
  free_INSN_LIST_list (& ptr->stores);

  free (ptr);
}

/* Free up all memory associated with the ldst list.  */

static void
free_ldst_mems ()
{
  while (pre_ldst_mems)
    {
      struct ls_expr * tmp = pre_ldst_mems;

      pre_ldst_mems = pre_ldst_mems->next;

      free_ldst_entry (tmp);
    }

  pre_ldst_mems = NULL;
}

/* Dump debugging info about the ldst list.  */

static void
print_ldst_list (file)
     FILE * file;
{
  struct ls_expr * ptr;

  fprintf (file, "LDST list: \n");

  for (ptr = first_ls_expr(); ptr != NULL; ptr = next_ls_expr (ptr))
    {
      fprintf (file, "  Pattern (%3d): ", ptr->index);

      print_rtl (file, ptr->pattern);

      fprintf (file, "\n	 Loads : ");

      if (ptr->loads)
	print_rtl (file, ptr->loads);
      else
	fprintf (file, "(nil)");

      fprintf (file, "\n	Stores : ");

      if (ptr->stores)
	print_rtl (file, ptr->stores);
      else
	fprintf (file, "(nil)");

      fprintf (file, "\n\n");
    }

  fprintf (file, "\n");
}

/* Returns 1 if X is in the list of ldst only expressions.  */

static struct ls_expr *
find_rtx_in_ldst (x)
     rtx x;
{
  struct ls_expr * ptr;

  for (ptr = pre_ldst_mems; ptr != NULL; ptr = ptr->next)
    if (expr_equiv_p (ptr->pattern, x) && ! ptr->invalid)
      return ptr;

  return NULL;
}

/* Assign each element of the list of mems a monotonically increasing value.  */

static int
enumerate_ldsts ()
{
  struct ls_expr * ptr;
  int n = 0;

  for (ptr = pre_ldst_mems; ptr != NULL; ptr = ptr->next)
    ptr->index = n++;

  return n;
}

/* Return first item in the list.  */

static inline struct ls_expr *
first_ls_expr ()
{
  return pre_ldst_mems;
}

/* Return the next item in ther list after the specified one.  */

static inline struct ls_expr *
next_ls_expr (ptr)
     struct ls_expr * ptr;
{
  return ptr->next;
}

/* Load Motion for loads which only kill themselves.  */

/* Return true if x is a simple MEM operation, with no registers or
   side effects. These are the types of loads we consider for the
   ld_motion list, otherwise we let the usual aliasing take care of it.  */

static int
simple_mem (x)
     rtx x;
{
  if (GET_CODE (x) != MEM)
    return 0;

  if (MEM_VOLATILE_P (x))
    return 0;

  if (GET_MODE (x) == BLKmode)
    return 0;

  if (!rtx_varies_p (XEXP (x, 0), 0))
    return 1;

  return 0;
}

/* Make sure there isn't a buried reference in this pattern anywhere.
   If there is, invalidate the entry for it since we're not capable
   of fixing it up just yet.. We have to be sure we know about ALL
   loads since the aliasing code will allow all entries in the
   ld_motion list to not-alias itself.  If we miss a load, we will get
   the wrong value since gcse might common it and we won't know to
   fix it up.  */

static void
invalidate_any_buried_refs (x)
     rtx x;
{
  const char * fmt;
  int i, j;
  struct ls_expr * ptr;

  /* Invalidate it in the list.  */
  if (GET_CODE (x) == MEM && simple_mem (x))
    {
      ptr = ldst_entry (x);
      ptr->invalid = 1;
    }

  /* Recursively process the insn.  */
  fmt = GET_RTX_FORMAT (GET_CODE (x));

  for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	invalidate_any_buried_refs (XEXP (x, i));
      else if (fmt[i] == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  invalidate_any_buried_refs (XVECEXP (x, i, j));
    }
}

/* Find all the 'simple' MEMs which are used in LOADs and STORES. Simple
   being defined as MEM loads and stores to symbols, with no
   side effects and no registers in the expression. If there are any
   uses/defs which don't match this criteria, it is invalidated and
   trimmed out later.  */

static void
compute_ld_motion_mems ()
{
  struct ls_expr * ptr;
  basic_block bb;
  rtx insn;

  pre_ldst_mems = NULL;

  FOR_EACH_BB (bb)
    {
      for (insn = bb->head;
	   insn && insn != NEXT_INSN (bb->end);
	   insn = NEXT_INSN (insn))
	{
	  if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
	    {
	      if (GET_CODE (PATTERN (insn)) == SET)
		{
		  rtx src = SET_SRC (PATTERN (insn));
		  rtx dest = SET_DEST (PATTERN (insn));

		  /* Check for a simple LOAD...  */
		  if (GET_CODE (src) == MEM && simple_mem (src))
		    {
		      ptr = ldst_entry (src);
		      if (GET_CODE (dest) == REG)
			ptr->loads = alloc_INSN_LIST (insn, ptr->loads);
		      else
			ptr->invalid = 1;
		    }
		  else
		    {
		      /* Make sure there isn't a buried load somewhere.  */
		      invalidate_any_buried_refs (src);
		    }

		  /* Check for stores. Don't worry about aliased ones, they
		     will block any movement we might do later. We only care
		     about this exact pattern since those are the only
		     circumstance that we will ignore the aliasing info.  */
		  if (GET_CODE (dest) == MEM && simple_mem (dest))
		    {
		      ptr = ldst_entry (dest);

		      if (GET_CODE (src) != MEM
			  && GET_CODE (src) != ASM_OPERANDS)
			ptr->stores = alloc_INSN_LIST (insn, ptr->stores);
		      else
			ptr->invalid = 1;
		    }
		}
	      else
		invalidate_any_buried_refs (PATTERN (insn));
	    }
	}
    }
}

/* Remove any references that have been either invalidated or are not in the
   expression list for pre gcse.  */

static void
trim_ld_motion_mems ()
{
  struct ls_expr * last = NULL;
  struct ls_expr * ptr = first_ls_expr ();

  while (ptr != NULL)
    {
      int del = ptr->invalid;
      struct expr * expr = NULL;

      /* Delete if entry has been made invalid.  */
      if (!del)
	{
	  unsigned int i;

	  del = 1;
	  /* Delete if we cannot find this mem in the expression list.  */
	  for (i = 0; i < expr_hash_table.size && del; i++)
	    {
	      for (expr = expr_hash_table.table[i];
		   expr != NULL;
		   expr = expr->next_same_hash)
		if (expr_equiv_p (expr->expr, ptr->pattern))
		  {
		    del = 0;
		    break;
		  }
	    }
	}

      if (del)
	{
	  if (last != NULL)
	    {
	      last->next = ptr->next;
	      free_ldst_entry (ptr);
	      ptr = last->next;
	    }
	  else
	    {
	      pre_ldst_mems = pre_ldst_mems->next;
	      free_ldst_entry (ptr);
	      ptr = pre_ldst_mems;
	    }
	}
      else
	{
	  /* Set the expression field if we are keeping it.  */
	  last = ptr;
	  ptr->expr = expr;
	  ptr = ptr->next;
	}
    }

  /* Show the world what we've found.  */
  if (gcse_file && pre_ldst_mems != NULL)
    print_ldst_list (gcse_file);
}

/* This routine will take an expression which we are replacing with
   a reaching register, and update any stores that are needed if
   that expression is in the ld_motion list.  Stores are updated by
   copying their SRC to the reaching register, and then storeing
   the reaching register into the store location. These keeps the
   correct value in the reaching register for the loads.  */

static void
update_ld_motion_stores (expr)
     struct expr * expr;
{
  struct ls_expr * mem_ptr;

  if ((mem_ptr = find_rtx_in_ldst (expr->expr)))
    {
      /* We can try to find just the REACHED stores, but is shouldn't
	 matter to set the reaching reg everywhere...  some might be
	 dead and should be eliminated later.  */

      /* We replace  SET mem = expr   with
	   SET reg = expr
	   SET mem = reg , where reg is the
	   reaching reg used in the load.  */
      rtx list = mem_ptr->stores;

      for ( ; list != NULL_RTX; list = XEXP (list, 1))
	{
	  rtx insn = XEXP (list, 0);
	  rtx pat = PATTERN (insn);
	  rtx src = SET_SRC (pat);
	  rtx reg = expr->reaching_reg;
	  rtx copy, new;

	  /* If we've already copied it, continue.  */
	  if (expr->reaching_reg == src)
	    continue;

	  if (gcse_file)
	    {
	      fprintf (gcse_file, "PRE:  store updated with reaching reg ");
	      print_rtl (gcse_file, expr->reaching_reg);
	      fprintf (gcse_file, ":\n	");
	      print_inline_rtx (gcse_file, insn, 8);
	      fprintf (gcse_file, "\n");
	    }

	  copy = gen_move_insn ( reg, SET_SRC (pat));
	  new = emit_insn_before (copy, insn);
	  record_one_set (REGNO (reg), new);
	  SET_SRC (pat) = reg;

	  /* un-recognize this pattern since it's probably different now.  */
	  INSN_CODE (insn) = -1;
	  gcse_create_count++;
	}
    }
}

/* Store motion code.  */

/* This is used to communicate the target bitvector we want to use in the
   reg_set_info routine when called via the note_stores mechanism.  */
static sbitmap * regvec;

/* Used in computing the reverse edge graph bit vectors.  */
static sbitmap * st_antloc;

/* Global holding the number of store expressions we are dealing with.  */
static int num_stores;

/* Checks to set if we need to mark a register set. Called from note_stores.  */

static void
reg_set_info (dest, setter, data)
     rtx dest, setter ATTRIBUTE_UNUSED;
     void * data ATTRIBUTE_UNUSED;
{
  if (GET_CODE (dest) == SUBREG)
    dest = SUBREG_REG (dest);

  if (GET_CODE (dest) == REG)
    SET_BIT (*regvec, REGNO (dest));
}

/* Return nonzero if the register operands of expression X are killed
   anywhere in basic block BB.  */

static int
store_ops_ok (x, bb)
     rtx x;
     basic_block bb;
{
  int i;
  enum rtx_code code;
  const char * fmt;

  /* Repeat is used to turn tail-recursion into iteration.  */
 repeat:

  if (x == 0)
    return 1;

  code = GET_CODE (x);
  switch (code)
    {
    case REG:
	/* If a reg has changed after us in this
	   block, the operand has been killed.  */
	return TEST_BIT (reg_set_in_block[bb->index], REGNO (x));

    case MEM:
      x = XEXP (x, 0);
      goto repeat;

    case PRE_DEC:
    case PRE_INC:
    case POST_DEC:
    case POST_INC:
      return 0;

    case PC:
    case CC0: /*FIXME*/
    case CONST:
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST_VECTOR:
    case SYMBOL_REF:
    case LABEL_REF:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
      return 1;

    default:
      break;
    }

  i = GET_RTX_LENGTH (code) - 1;
  fmt = GET_RTX_FORMAT (code);

  for (; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  rtx tem = XEXP (x, i);

	  /* If we are about to do the last recursive call
	     needed at this level, change it into iteration.
	     This function is called enough to be worth it.  */
	  if (i == 0)
	    {
	      x = tem;
	      goto repeat;
	    }

	  if (! store_ops_ok (tem, bb))
	    return 0;
	}
      else if (fmt[i] == 'E')
	{
	  int j;

	  for (j = 0; j < XVECLEN (x, i); j++)
	    {
	      if (! store_ops_ok (XVECEXP (x, i, j), bb))
		return 0;
	    }
	}
    }

  return 1;
}

/* Determine whether insn is MEM store pattern that we will consider moving.  */

static void
find_moveable_store (insn)
     rtx insn;
{
  struct ls_expr * ptr;
  rtx dest = PATTERN (insn);

  if (GET_CODE (dest) != SET
      || GET_CODE (SET_SRC (dest)) == ASM_OPERANDS)
    return;

  dest = SET_DEST (dest);

  if (GET_CODE (dest) != MEM || MEM_VOLATILE_P (dest)
      || GET_MODE (dest) == BLKmode)
    return;

  if (GET_CODE (XEXP (dest, 0)) != SYMBOL_REF)
      return;

  if (rtx_varies_p (XEXP (dest, 0), 0))
    return;

  ptr = ldst_entry (dest);
  ptr->stores = alloc_INSN_LIST (insn, ptr->stores);
}

/* Perform store motion. Much like gcse, except we move expressions the
   other way by looking at the flowgraph in reverse.  */

static int
compute_store_table ()
{
  int ret;
  basic_block bb;
  unsigned regno;
  rtx insn, pat;

  max_gcse_regno = max_reg_num ();

  reg_set_in_block = (sbitmap *) sbitmap_vector_alloc (last_basic_block,
						       max_gcse_regno);
  sbitmap_vector_zero (reg_set_in_block, last_basic_block);
  pre_ldst_mems = 0;

  /* Find all the stores we care about.  */
  FOR_EACH_BB (bb)
    {
      regvec = & (reg_set_in_block[bb->index]);
      for (insn = bb->end;
	   insn && insn != PREV_INSN (bb->end);
	   insn = PREV_INSN (insn))
	{
	  /* Ignore anything that is not a normal insn.  */
	  if (! INSN_P (insn))
	    continue;

	  if (GET_CODE (insn) == CALL_INSN)
	    {
	      bool clobbers_all = false;
#ifdef NON_SAVING_SETJMP
	      if (NON_SAVING_SETJMP
		  && find_reg_note (insn, REG_SETJMP, NULL_RTX))
		clobbers_all = true;
#endif

	      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
		if (clobbers_all
		    || TEST_HARD_REG_BIT (regs_invalidated_by_call, regno))
		  SET_BIT (reg_set_in_block[bb->index], regno);
	    }

	  pat = PATTERN (insn);
	  note_stores (pat, reg_set_info, NULL);

	  /* Now that we've marked regs, look for stores.  */
	  if (GET_CODE (pat) == SET)
	    find_moveable_store (insn);
	}
    }

  ret = enumerate_ldsts ();

  if (gcse_file)
    {
      fprintf (gcse_file, "Store Motion Expressions.\n");
      print_ldst_list (gcse_file);
    }

  return ret;
}

/* Check to see if the load X is aliased with STORE_PATTERN.  */

static int
load_kills_store (x, store_pattern)
     rtx x, store_pattern;
{
  if (true_dependence (x, GET_MODE (x), store_pattern, rtx_addr_varies_p))
    return 1;
  return 0;
}

/* Go through the entire insn X, looking for any loads which might alias
   STORE_PATTERN.  Return 1 if found.  */

static int
find_loads (x, store_pattern)
     rtx x, store_pattern;
{
  const char * fmt;
  int i, j;
  int ret = 0;

  if (!x)
    return 0;

  if (GET_CODE (x) == SET)
    x = SET_SRC (x);

  if (GET_CODE (x) == MEM)
    {
      if (load_kills_store (x, store_pattern))
	return 1;
    }

  /* Recursively process the insn.  */
  fmt = GET_RTX_FORMAT (GET_CODE (x));

  for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0 && !ret; i--)
    {
      if (fmt[i] == 'e')
	ret |= find_loads (XEXP (x, i), store_pattern);
      else if (fmt[i] == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  ret |= find_loads (XVECEXP (x, i, j), store_pattern);
    }
  return ret;
}

/* Check if INSN kills the store pattern X (is aliased with it).
   Return 1 if it it does.  */

static int
store_killed_in_insn (x, insn)
     rtx x, insn;
{
  if (GET_RTX_CLASS (GET_CODE (insn)) != 'i')
    return 0;

  if (GET_CODE (insn) == CALL_INSN)
    {
      /* A normal or pure call might read from pattern,
	 but a const call will not.  */
      return ! CONST_OR_PURE_CALL_P (insn) || pure_call_p (insn);
    }

  if (GET_CODE (PATTERN (insn)) == SET)
    {
      rtx pat = PATTERN (insn);
      /* Check for memory stores to aliased objects.  */
      if (GET_CODE (SET_DEST (pat)) == MEM && !expr_equiv_p (SET_DEST (pat), x))
	/* pretend its a load and check for aliasing.  */
	if (find_loads (SET_DEST (pat), x))
	  return 1;
      return find_loads (SET_SRC (pat), x);
    }
  else
    return find_loads (PATTERN (insn), x);
}

/* Returns 1 if the expression X is loaded or clobbered on or after INSN
   within basic block BB.  */

static int
store_killed_after (x, insn, bb)
     rtx x, insn;
     basic_block bb;
{
  rtx last = bb->end;

  if (insn == last)
    return 0;

  /* Check if the register operands of the store are OK in this block.
     Note that if registers are changed ANYWHERE in the block, we'll
     decide we can't move it, regardless of whether it changed above
     or below the store. This could be improved by checking the register
     operands while lookinng for aliasing in each insn.  */
  if (!store_ops_ok (XEXP (x, 0), bb))
    return 1;

  for ( ; insn && insn != NEXT_INSN (last); insn = NEXT_INSN (insn))
    if (store_killed_in_insn (x, insn))
      return 1;

  return 0;
}

/* Returns 1 if the expression X is loaded or clobbered on or before INSN
   within basic block BB.  */
static int
store_killed_before (x, insn, bb)
     rtx x, insn;
     basic_block bb;
{
  rtx first = bb->head;

  if (insn == first)
    return store_killed_in_insn (x, insn);

  /* Check if the register operands of the store are OK in this block.
     Note that if registers are changed ANYWHERE in the block, we'll
     decide we can't move it, regardless of whether it changed above
     or below the store. This could be improved by checking the register
     operands while lookinng for aliasing in each insn.  */
  if (!store_ops_ok (XEXP (x, 0), bb))
    return 1;

  for ( ; insn && insn != PREV_INSN (first); insn = PREV_INSN (insn))
    if (store_killed_in_insn (x, insn))
      return 1;

  return 0;
}

#define ANTIC_STORE_LIST(x)	((x)->loads)
#define AVAIL_STORE_LIST(x)	((x)->stores)

/* Given the table of available store insns at the end of blocks,
   determine which ones are not killed by aliasing, and generate
   the appropriate vectors for gen and killed.  */
static void
build_store_vectors ()
{
  basic_block bb, b;
  rtx insn, st;
  struct ls_expr * ptr;

  /* Build the gen_vector. This is any store in the table which is not killed
     by aliasing later in its block.  */
  ae_gen = (sbitmap *) sbitmap_vector_alloc (last_basic_block, num_stores);
  sbitmap_vector_zero (ae_gen, last_basic_block);

  st_antloc = (sbitmap *) sbitmap_vector_alloc (last_basic_block, num_stores);
  sbitmap_vector_zero (st_antloc, last_basic_block);

  for (ptr = first_ls_expr (); ptr != NULL; ptr = next_ls_expr (ptr))
    {
      /* Put all the stores into either the antic list, or the avail list,
	 or both.  */
      rtx store_list = ptr->stores;
      ptr->stores = NULL_RTX;

      for (st = store_list; st != NULL; st = XEXP (st, 1))
	{
	  insn = XEXP (st, 0);
	  bb = BLOCK_FOR_INSN (insn);

	  if (!store_killed_after (ptr->pattern, insn, bb))
	    {
	      /* If we've already seen an availale expression in this block,
		 we can delete the one we saw already (It occurs earlier in
		 the block), and replace it with this one). We'll copy the
		 old SRC expression to an unused register in case there
		 are any side effects.  */
	      if (TEST_BIT (ae_gen[bb->index], ptr->index))
		{
		  /* Find previous store.  */
		  rtx st;
		  for (st = AVAIL_STORE_LIST (ptr); st ; st = XEXP (st, 1))
		    if (BLOCK_FOR_INSN (XEXP (st, 0)) == bb)
		      break;
		  if (st)
		    {
		      rtx r = gen_reg_rtx (GET_MODE (ptr->pattern));
		      if (gcse_file)
			fprintf (gcse_file, "Removing redundant store:\n");
		      replace_store_insn (r, XEXP (st, 0), bb);
		      XEXP (st, 0) = insn;
		      continue;
		    }
		}
	      SET_BIT (ae_gen[bb->index], ptr->index);
	      AVAIL_STORE_LIST (ptr) = alloc_INSN_LIST (insn,
							AVAIL_STORE_LIST (ptr));
	    }

	  if (!store_killed_before (ptr->pattern, insn, bb))
	    {
	      SET_BIT (st_antloc[BLOCK_NUM (insn)], ptr->index);
	      ANTIC_STORE_LIST (ptr) = alloc_INSN_LIST (insn,
							ANTIC_STORE_LIST (ptr));
	    }
	}

      /* Free the original list of store insns.  */
      free_INSN_LIST_list (&store_list);
    }

  ae_kill = (sbitmap *) sbitmap_vector_alloc (last_basic_block, num_stores);
  sbitmap_vector_zero (ae_kill, last_basic_block);

  transp = (sbitmap *) sbitmap_vector_alloc (last_basic_block, num_stores);
  sbitmap_vector_zero (transp, last_basic_block);

  for (ptr = first_ls_expr (); ptr != NULL; ptr = next_ls_expr (ptr))
    FOR_EACH_BB (b)
      {
	if (store_killed_after (ptr->pattern, b->head, b))
	  {
	    /* The anticipatable expression is not killed if it's gen'd.  */
	    /*
	      We leave this check out for now. If we have a code sequence
	      in a block which looks like:
			ST MEMa = x
			L     y = MEMa
			ST MEMa = z
	      We should flag this as having an ANTIC expression, NOT
	      transparent, NOT killed, and AVAIL.
	      Unfortunately, since we haven't re-written all loads to
	      use the reaching reg, we'll end up doing an incorrect
	      Load in the middle here if we push the store down. It happens in
		    gcc.c-torture/execute/960311-1.c with -O3
	      If we always kill it in this case, we'll sometimes do
	      uneccessary work, but it shouldn't actually hurt anything.
	    if (!TEST_BIT (ae_gen[b], ptr->index)).  */
	    SET_BIT (ae_kill[b->index], ptr->index);
	  }
	else
	  SET_BIT (transp[b->index], ptr->index);
      }

  /* Any block with no exits calls some non-returning function, so
     we better mark the store killed here, or we might not store to
     it at all.  If we knew it was abort, we wouldn't have to store,
     but we don't know that for sure.  */
  if (gcse_file)
    {
      fprintf (gcse_file, "ST_avail and ST_antic (shown under loads..)\n");
      print_ldst_list (gcse_file);
      dump_sbitmap_vector (gcse_file, "st_antloc", "", st_antloc, last_basic_block);
      dump_sbitmap_vector (gcse_file, "st_kill", "", ae_kill, last_basic_block);
      dump_sbitmap_vector (gcse_file, "Transpt", "", transp, last_basic_block);
      dump_sbitmap_vector (gcse_file, "st_avloc", "", ae_gen, last_basic_block);
    }
}

/* Insert an instruction at the begining of a basic block, and update
   the BLOCK_HEAD if needed.  */

static void
insert_insn_start_bb (insn, bb)
     rtx insn;
     basic_block bb;
{
  /* Insert at start of successor block.  */
  rtx prev = PREV_INSN (bb->head);
  rtx before = bb->head;
  while (before != 0)
    {
      if (GET_CODE (before) != CODE_LABEL
	  && (GET_CODE (before) != NOTE
	      || NOTE_LINE_NUMBER (before) != NOTE_INSN_BASIC_BLOCK))
	break;
      prev = before;
      if (prev == bb->end)
	break;
      before = NEXT_INSN (before);
    }

  insn = emit_insn_after (insn, prev);

  if (gcse_file)
    {
      fprintf (gcse_file, "STORE_MOTION  insert store at start of BB %d:\n",
	       bb->index);
      print_inline_rtx (gcse_file, insn, 6);
      fprintf (gcse_file, "\n");
    }
}

/* This routine will insert a store on an edge. EXPR is the ldst entry for
   the memory reference, and E is the edge to insert it on.  Returns nonzero
   if an edge insertion was performed.  */

static int
insert_store (expr, e)
     struct ls_expr * expr;
     edge e;
{
  rtx reg, insn;
  basic_block bb;
  edge tmp;

  /* We did all the deleted before this insert, so if we didn't delete a
     store, then we haven't set the reaching reg yet either.  */
  if (expr->reaching_reg == NULL_RTX)
    return 0;

  reg = expr->reaching_reg;
  insn = gen_move_insn (expr->pattern, reg);

  /* If we are inserting this expression on ALL predecessor edges of a BB,
     insert it at the start of the BB, and reset the insert bits on the other
     edges so we don't try to insert it on the other edges.  */
  bb = e->dest;
  for (tmp = e->dest->pred; tmp ; tmp = tmp->pred_next)
    {
      int index = EDGE_INDEX (edge_list, tmp->src, tmp->dest);
      if (index == EDGE_INDEX_NO_EDGE)
	abort ();
      if (! TEST_BIT (pre_insert_map[index], expr->index))
	break;
    }

  /* If tmp is NULL, we found an insertion on every edge, blank the
     insertion vector for these edges, and insert at the start of the BB.  */
  if (!tmp && bb != EXIT_BLOCK_PTR)
    {
      for (tmp = e->dest->pred; tmp ; tmp = tmp->pred_next)
	{
	  int index = EDGE_INDEX (edge_list, tmp->src, tmp->dest);
	  RESET_BIT (pre_insert_map[index], expr->index);
	}
      insert_insn_start_bb (insn, bb);
      return 0;
    }

  /* We can't insert on this edge, so we'll insert at the head of the
     successors block.  See Morgan, sec 10.5.  */
  if ((e->flags & EDGE_ABNORMAL) == EDGE_ABNORMAL)
    {
      insert_insn_start_bb (insn, bb);
      return 0;
    }

  insert_insn_on_edge (insn, e);

  if (gcse_file)
    {
      fprintf (gcse_file, "STORE_MOTION  insert insn on edge (%d, %d):\n",
	       e->src->index, e->dest->index);
      print_inline_rtx (gcse_file, insn, 6);
      fprintf (gcse_file, "\n");
    }

  return 1;
}

/* This routine will replace a store with a SET to a specified register.  */

static void
replace_store_insn (reg, del, bb)
     rtx reg, del;
     basic_block bb;
{
  rtx insn;

  insn = gen_move_insn (reg, SET_SRC (PATTERN (del)));
  insn = emit_insn_after (insn, del);

  if (gcse_file)
    {
      fprintf (gcse_file,
	       "STORE_MOTION  delete insn in BB %d:\n      ", bb->index);
      print_inline_rtx (gcse_file, del, 6);
      fprintf (gcse_file, "\nSTORE MOTION  replaced with insn:\n      ");
      print_inline_rtx (gcse_file, insn, 6);
      fprintf (gcse_file, "\n");
    }

  delete_insn (del);
}


/* Delete a store, but copy the value that would have been stored into
   the reaching_reg for later storing.  */

static void
delete_store (expr, bb)
     struct ls_expr * expr;
     basic_block bb;
{
  rtx reg, i, del;

  if (expr->reaching_reg == NULL_RTX)
    expr->reaching_reg = gen_reg_rtx (GET_MODE (expr->pattern));


  /* If there is more than 1 store, the earlier ones will be dead,
     but it doesn't hurt to replace them here.  */
  reg = expr->reaching_reg;

  for (i = AVAIL_STORE_LIST (expr); i; i = XEXP (i, 1))
    {
      del = XEXP (i, 0);
      if (BLOCK_FOR_INSN (del) == bb)
	{
	  /* We know there is only one since we deleted redundant
	     ones during the available computation.  */
	  replace_store_insn (reg, del, bb);
	  break;
	}
    }
}

/* Free memory used by store motion.  */

static void
free_store_memory ()
{
  free_ldst_mems ();

  if (ae_gen)
    sbitmap_vector_free (ae_gen);
  if (ae_kill)
    sbitmap_vector_free (ae_kill);
  if (transp)
    sbitmap_vector_free (transp);
  if (st_antloc)
    sbitmap_vector_free (st_antloc);
  if (pre_insert_map)
    sbitmap_vector_free (pre_insert_map);
  if (pre_delete_map)
    sbitmap_vector_free (pre_delete_map);
  if (reg_set_in_block)
    sbitmap_vector_free (reg_set_in_block);

  ae_gen = ae_kill = transp = st_antloc = NULL;
  pre_insert_map = pre_delete_map = reg_set_in_block = NULL;
}

/* Perform store motion. Much like gcse, except we move expressions the
   other way by looking at the flowgraph in reverse.  */

static void
store_motion ()
{
  basic_block bb;
  int x;
  struct ls_expr * ptr;
  int update_flow = 0;

  if (gcse_file)
    {
      fprintf (gcse_file, "before store motion\n");
      print_rtl (gcse_file, get_insns ());
    }


  init_alias_analysis ();

  /* Find all the stores that are live to the end of their block.  */
  num_stores = compute_store_table ();
  if (num_stores == 0)
    {
      sbitmap_vector_free (reg_set_in_block);
      end_alias_analysis ();
      return;
    }

  /* Now compute whats actually available to move.  */
  add_noreturn_fake_exit_edges ();
  build_store_vectors ();

  edge_list = pre_edge_rev_lcm (gcse_file, num_stores, transp, ae_gen,
				st_antloc, ae_kill, &pre_insert_map,
				&pre_delete_map);

  /* Now we want to insert the new stores which are going to be needed.  */
  for (ptr = first_ls_expr (); ptr != NULL; ptr = next_ls_expr (ptr))
    {
      FOR_EACH_BB (bb)
	if (TEST_BIT (pre_delete_map[bb->index], ptr->index))
	  delete_store (ptr, bb);

      for (x = 0; x < NUM_EDGES (edge_list); x++)
	if (TEST_BIT (pre_insert_map[x], ptr->index))
	  update_flow |= insert_store (ptr, INDEX_EDGE (edge_list, x));
    }

  if (update_flow)
    commit_edge_insertions ();

  free_store_memory ();
  free_edge_list (edge_list);
  remove_fake_edges ();
  end_alias_analysis ();
}

#include "gt-gcse.h"
