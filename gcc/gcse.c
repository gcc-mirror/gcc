/* Global common subexpression elimination/Partial redundancy elimination
   and global constant/copy propagation for GNU compiler.
   Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation, Inc.

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

/* TODO
   - reordering of memory allocation and freeing to be more space efficient
   - do rough calc of how many regs are needed in each block, and a rough
     calc of how many regs are available in each class and use that to
     throttle back the code in cases where RTX_COST is minimal.
   - dead store elimination
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

#include "obstack.h"
#define obstack_chunk_alloc gmalloc
#define obstack_chunk_free free

/* Maximum number of passes to perform.  */
#define MAX_PASSES 1

/* Propagate flow information through back edges and thus enable PRE's
   moving loop invariant calculations out of loops.

   Originally this tended to create worse overall code, but several
   improvements during the development of PRE seem to have made following
   back edges generally a win.

   Note much of the loop invariant code motion done here would normally
   be done by loop.c, which has more heuristics for when to move invariants
   out of loops.  At some point we might need to move some of those
   heuristics into gcse.c.  */
#define FOLLOW_BACK_EDGES 1

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
   awhile to converge.  Hence we only perform one pass.  Macro MAX_PASSES can
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

/* Non-zero for each mode that supports (set (reg) (reg)).
   This is trivially true for integer and floating point values.
   It may or may not be true for condition codes.  */
static char can_copy_p[(int) NUM_MACHINE_MODES];

/* Non-zero if can_copy_p has been initialized.  */
static int can_copy_init_p;

struct reg_use {
  rtx reg_rtx;
};

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
  /* Non-zero if this [anticipatable] occurrence has been deleted.  */
  char deleted_p;
  /* Non-zero if this [available] occurrence has been copied to
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
   Someday I'll perform the computation and figure it out.
*/

/* Total size of the expression hash table, in elements.  */
static int expr_hash_table_size;
/* The table itself.
   This is an array of `expr_hash_table_size' elements.  */
static struct expr **expr_hash_table;

/* Total size of the copy propagation hash table, in elements.  */
static int set_hash_table_size;
/* The table itself.
   This is an array of `set_hash_table_size' elements.  */
static struct expr **set_hash_table;

/* Mapping of uids to cuids.
   Only real insns get cuids.  */
static int *uid_cuid;

/* Highest UID in UID_CUID.  */
static int max_uid;

/* Get the cuid of an insn.  */
#define INSN_CUID(INSN) (uid_cuid[INSN_UID (INSN)])

/* Number of cuids.  */
static int max_cuid;

/* Mapping of cuids to insns.  */
static rtx *cuid_insn;

/* Get insn from cuid.  */
#define CUID_INSN(CUID) (cuid_insn[CUID])

/* Maximum register number in function prior to doing gcse + 1.
   Registers created during this pass have regno >= max_gcse_regno.
   This is named with "gcse" to not collide with global of same name.  */
static int max_gcse_regno;

/* Maximum number of cse-able expressions found.  */
static int n_exprs;
/* Maximum number of assignments for copy propagation found.  */
static int n_sets;

/* Table of registers that are modified.
   For each register, each element is a list of places where the pseudo-reg
   is set.

   For simplicity, GCSE is done on sets of pseudo-regs only.  PRE GCSE only
   requires knowledge of which blocks kill which regs [and thus could use
   a bitmap instead of the lists `reg_set_table' uses].

   `reg_set_table' and could be turned into an array of bitmaps
   (num-bbs x num-regs)
   [however perhaps it may be useful to keep the data as is].
   One advantage of recording things this way is that `reg_set_table' is
   fairly sparse with respect to pseudo regs but for hard regs could be
   fairly dense [relatively speaking].
   And recording sets of pseudo-regs in lists speeds
   up functions like compute_transp since in the case of pseudo-regs we only
   need to iterate over the number of times a pseudo-reg is set, not over the
   number of basic blocks [clearly there is a bit of a slow down in the cases
   where a pseudo is set more than once in a block, however it is believed
   that the net effect is to speed things up].  This isn't done for hard-regs
   because recording call-clobbered hard-regs in `reg_set_table' at each
   function call can consume a fair bit of memory, and iterating over hard-regs
   stored this way in compute_transp will be more expensive.  */

typedef struct reg_set {
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

/* Bitmap containing one bit for each register in the program.
   Used when performing GCSE to track which registers have been set since
   the start of the basic block.  */
static sbitmap reg_set_bitmap;

/* For each block, a bitmap of registers set in the block.
   This is used by expr_killed_p and compute_transp.
   It is computed during hash table computation and not by compute_sets
   as it includes registers added since the last pass (or between cprop and
   gcse) and it's currently not easy to realloc sbitmap vectors.  */
static sbitmap *reg_set_in_block;

/* For each block, non-zero if memory is set in that block.
   This is computed during hash table computation and is used by
   expr_killed_p and compute_transp.
   ??? Handling of memory is very simple, we don't make any attempt
   to optimize things (later).
   ??? This can be computed by compute_sets since the information
   doesn't change.  */
static char *mem_set_in_block;

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

/* A bitmap of all ones for implementing the algorithm for available
   expressions and reaching definitions.  */
/* ??? Available expression bitmaps have a different size than reaching
   definition bitmaps.  This should be the larger of the two, however, it
   is not currently used for reaching definitions.  */
static sbitmap u_bitmap;

/* Each block has a bitmap of each type.
   The length of each blocks bitmap is:

       max_cuid  - for reaching definitions
       n_exprs - for available expressions

   Thus we view the bitmaps as 2 dimensional arrays.  i.e.
   rd_kill[block_num][cuid_num]
   ae_kill[block_num][expr_num]
*/

/* For reaching defs */
static sbitmap *rd_kill, *rd_gen, *reaching_defs, *rd_out;

/* for available exprs */
static sbitmap *ae_kill, *ae_gen, *ae_in, *ae_out;

/* Objects of this type are passed around by the null-pointer check
   removal routines.  */
struct null_pointer_info {
  /* The basic block being processed.  */
  int current_block;
  /* The first register to be handled in this pass.  */
  int min_reg;
  /* One greater than the last register to be handled in this pass.  */
  int max_reg;
  sbitmap *nonnull_local;
  sbitmap *nonnull_killed;
};

static void compute_can_copy	  PROTO ((void));

static char *gmalloc		  PROTO ((unsigned int));
static char *grealloc		 PROTO ((char *, unsigned int));
static char *gcse_alloc	       PROTO ((unsigned long));
static void alloc_gcse_mem	    PROTO ((rtx));
static void free_gcse_mem	     PROTO ((void));
static void alloc_reg_set_mem	 PROTO ((int));
static void free_reg_set_mem	  PROTO ((void));
static int get_bitmap_width           PROTO ((int, int, int));
static void record_one_set	    PROTO ((int, rtx));
static void record_set_info	   PROTO ((rtx, rtx, void *));
static void compute_sets	      PROTO ((rtx));

static void hash_scan_insn	    PROTO ((rtx, int, int));
static void hash_scan_set	     PROTO ((rtx, rtx, int));
static void hash_scan_clobber	 PROTO ((rtx, rtx));
static void hash_scan_call	    PROTO ((rtx, rtx));
static int want_to_gcse_p	     PROTO ((rtx));
static int oprs_unchanged_p	   PROTO ((rtx, rtx, int));
static int oprs_anticipatable_p       PROTO ((rtx, rtx));
static int oprs_available_p	   PROTO ((rtx, rtx));
static void insert_expr_in_table      PROTO ((rtx, enum machine_mode,
					      rtx, int, int));
static void insert_set_in_table       PROTO ((rtx, rtx));
static unsigned int hash_expr	 PROTO ((rtx, enum machine_mode,
					 int *, int));
static unsigned int hash_expr_1       PROTO ((rtx, enum machine_mode, int *));
static unsigned int hash_set	  PROTO ((int, int));
static int expr_equiv_p	       PROTO ((rtx, rtx));
static void record_last_reg_set_info  PROTO ((rtx, int));
static void record_last_mem_set_info  PROTO ((rtx));
static void record_last_set_info      PROTO ((rtx, rtx, void *));
static void compute_hash_table	PROTO ((int));
static void alloc_set_hash_table      PROTO ((int));
static void free_set_hash_table       PROTO ((void));
static void compute_set_hash_table    PROTO ((void));
static void alloc_expr_hash_table     PROTO ((int));
static void free_expr_hash_table      PROTO ((void));
static void compute_expr_hash_table   PROTO ((void));
static void dump_hash_table	   PROTO ((FILE *, const char *, struct expr **,
					   int, int));
static struct expr *lookup_expr       PROTO ((rtx));
static struct expr *lookup_set	PROTO ((int, rtx));
static struct expr *next_set	  PROTO ((int, struct expr *));
static void reset_opr_set_tables      PROTO ((void));
static int oprs_not_set_p	     PROTO ((rtx, rtx));
static void mark_call		 PROTO ((rtx));
static void mark_set		  PROTO ((rtx, rtx));
static void mark_clobber	      PROTO ((rtx, rtx));
static void mark_oprs_set	     PROTO ((rtx));

static void alloc_cprop_mem	   PROTO ((int, int));
static void free_cprop_mem	    PROTO ((void));
static void compute_transp	    PROTO ((rtx, int, sbitmap *, int));
static void compute_transpout	    PROTO ((void));
static void compute_local_properties  PROTO ((sbitmap *, sbitmap *,
					      sbitmap *, int));
static void compute_cprop_data	PROTO ((void));
static void find_used_regs	    PROTO ((rtx));
static int try_replace_reg	    PROTO ((rtx, rtx, rtx));
static struct expr *find_avail_set    PROTO ((int, rtx));
static int cprop_jump			PROTO((rtx, rtx, struct reg_use *, rtx));
#ifdef HAVE_cc0
static int cprop_cc0_jump		PROTO((rtx, struct reg_use *, rtx));
#endif
static int cprop_insn		 PROTO ((rtx, int));
static int cprop		      PROTO ((int));
static int one_cprop_pass	     PROTO ((int, int));

static void alloc_pre_mem	     PROTO ((int, int));
static void free_pre_mem	      PROTO ((void));
static void compute_pre_data	  PROTO ((void));
static int pre_expr_reaches_here_p    PROTO ((int, struct expr *, int));
static void insert_insn_end_bb	PROTO ((struct expr *, int, int));
static void pre_insert_copy_insn      PROTO ((struct expr *, rtx));
static void pre_insert_copies	 PROTO ((void));
static int pre_delete		 PROTO ((void));
static int pre_gcse		   PROTO ((void));
static int one_pre_gcse_pass	  PROTO ((int));

static void add_label_notes	      PROTO ((rtx, rtx));

static void alloc_code_hoist_mem	PROTO ((int, int));
static void free_code_hoist_mem		PROTO ((void));
static void compute_code_hoist_vbeinout	PROTO ((void));
static void compute_code_hoist_data	PROTO ((void));
static int hoist_expr_reaches_here_p	PROTO ((int, int, int, char *));
static void hoist_code			PROTO ((void));
static int one_code_hoisting_pass	PROTO ((void));

static void alloc_rd_mem	      PROTO ((int, int));
static void free_rd_mem	       PROTO ((void));
static void handle_rd_kill_set	PROTO ((rtx, int, int));
static void compute_kill_rd	   PROTO ((void));
static void compute_rd		PROTO ((void));
static void alloc_avail_expr_mem      PROTO ((int, int));
static void free_avail_expr_mem       PROTO ((void));
static void compute_ae_gen	    PROTO ((void));
static int expr_killed_p	      PROTO ((rtx, int));
static void compute_ae_kill	   PROTO ((sbitmap *, sbitmap *));
static int expr_reaches_here_p	PROTO ((struct occr *, struct expr *,
					      int, int));
static rtx computing_insn	     PROTO ((struct expr *, rtx));
static int def_reaches_here_p	 PROTO ((rtx, rtx));
static int can_disregard_other_sets   PROTO ((struct reg_set **, rtx, int));
static int handle_avail_expr	  PROTO ((rtx, struct expr *));
static int classic_gcse	       PROTO ((void));
static int one_classic_gcse_pass      PROTO ((int));
static void invalidate_nonnull_info	PROTO ((rtx, rtx, void *));
static void delete_null_pointer_checks_1 PROTO ((int *, sbitmap *, sbitmap *,
						 struct null_pointer_info *));
static rtx process_insert_insn	PROTO ((struct expr *));
static int pre_edge_insert	PROTO ((struct edge_list *, struct expr **));
static int expr_reaches_here_p_work	PROTO ((struct occr *, struct expr *, int, int, char *));
static int pre_expr_reaches_here_p_work	PROTO ((int, struct expr *,
						int, char *));

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
  find_basic_blocks (f, max_gcse_regno, file, 1);

  if (file)
    dump_flow_info (file);

  /* Return if there's nothing to do.  */
  if (n_basic_blocks <= 1)
    {
      /* Free storage allocated by find_basic_blocks.  */
      free_basic_block_vars (0);
      return 0;
    }

  /* Trying to perform global optimizations on flow graphs which have
     a high connectivity will take a long time and is unlikely to be
     particularly useful.

     In normal circumstances a cfg should have about twice has many edges
     as blocks.  But we do not want to punish small functions which have
     a couple switch statements.  So we require a relatively large number
     of basic blocks and the ratio of edges to blocks to be high.  */
  if (n_basic_blocks > 1000 && n_edges / n_basic_blocks >= 20)
    {
      /* Free storage allocated by find_basic_blocks.  */
      free_basic_block_vars (0);
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

  /* Record where pseudo-registers are set.
     This data is kept accurate during each pass.
     ??? We could also record hard-reg information here
     [since it's unchanging], however it is currently done during
     hash table computation.

     It may be tempting to compute MEM set information here too, but MEM
     sets will be subject to code motion one day and thus we need to compute
     information about memory sets when we build the hash tables.  */

  alloc_reg_set_mem (max_gcse_regno);
  compute_sets (f);

  pass = 0;
  initial_bytes_used = bytes_used;
  max_pass_bytes = 0;
  gcse_obstack_bottom = gcse_alloc (1);
  changed = 1;
  while (changed && pass < MAX_PASSES)
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

  /* Free our obstack.  */
  obstack_free (&gcse_obstack, NULL_PTR);
  /* Free reg_set_table.  */
  free_reg_set_mem ();
  /* Free storage allocated by find_basic_blocks.  */
  free_basic_block_vars (0);
  return run_jump_opt_after_gcse;
}

/* Misc. utilities.  */

/* Compute which modes support reg/reg copy operations.  */

static void
compute_can_copy ()
{
  int i;
#ifndef AVOID_CCMODE_COPIES
  rtx reg,insn;
#endif
  char *free_point = (char *) oballoc (1);

  bzero (can_copy_p, NUM_MACHINE_MODES);

  start_sequence ();
  for (i = 0; i < NUM_MACHINE_MODES; i++)
    {
      switch (GET_MODE_CLASS (i))
	{
	case MODE_CC :
#ifdef AVOID_CCMODE_COPIES
	  can_copy_p[i] = 0;
#else
	  reg = gen_rtx_REG ((enum machine_mode) i, LAST_VIRTUAL_REGISTER + 1);
	  insn = emit_insn (gen_rtx_SET (VOIDmode, reg, reg));
	  if (recog (PATTERN (insn), insn, NULL_PTR) >= 0)
	    can_copy_p[i] = 1;
#endif
	  break;
	default :
	  can_copy_p[i] = 1;
	  break;
	}
    }
  end_sequence ();

  /* Free the objects we just allocated.  */
  obfree (free_point);
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

/* Cover function to obstack_alloc.
   We don't need to record the bytes allocated here since
   obstack_chunk_alloc is set to gmalloc.  */

static char *
gcse_alloc (size)
     unsigned long size;
{
  return (char *) obstack_alloc (&gcse_obstack, size);
}

/* Allocate memory for the cuid mapping array,
   and reg/memory set tracking tables.

   This is called at the start of each pass.  */

static void
alloc_gcse_mem (f)
     rtx f;
{
  int i,n;
  rtx insn;

  /* Find the largest UID and create a mapping from UIDs to CUIDs.
     CUIDs are like UIDs except they increase monotonically, have no gaps,
     and only apply to real insns.  */

  max_uid = get_max_uid ();
  n = (max_uid + 1) * sizeof (int);
  uid_cuid = (int *) gmalloc (n);
  bzero ((char *) uid_cuid, n);
  for (insn = f, i = 0; insn; insn = NEXT_INSN (insn))
    {
      if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
	INSN_CUID (insn) = i++;
      else
	INSN_CUID (insn) = i;
    }

  /* Create a table mapping cuids to insns.  */

  max_cuid = i;
  n = (max_cuid + 1) * sizeof (rtx);
  cuid_insn = (rtx *) gmalloc (n);
  bzero ((char *) cuid_insn, n);
  for (insn = f, i = 0; insn; insn = NEXT_INSN (insn))
    {
      if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
	{
	  CUID_INSN (i) = insn;
	  i++;
	}
    }

  /* Allocate vars to track sets of regs.  */

  reg_set_bitmap = (sbitmap) sbitmap_alloc (max_gcse_regno);

  /* Allocate vars to track sets of regs, memory per block.  */

  reg_set_in_block = (sbitmap *) sbitmap_vector_alloc (n_basic_blocks,
						       max_gcse_regno);
  mem_set_in_block = (char *) gmalloc (n_basic_blocks);
}

/* Free memory allocated by alloc_gcse_mem.  */

static void
free_gcse_mem ()
{
  free (uid_cuid);
  free (cuid_insn);

  free (reg_set_bitmap);

  free (reg_set_in_block);
  free (mem_set_in_block);
}

/* Many of the global optimization algorithms work by solving dataflow
   equations for various expressions.  Initially, some local value is
   computed for each expression in each block.  Then, the values
   across the various blocks are combined (by following flow graph
   edges) to arrive at global values.  Conceptually, each set of
   equations is independent.  We may therefore solve all the equations
   in parallel, solve them one at a time, or pick any intermediate
   approach.  

   When you're going to need N two-dimensional bitmaps, each X (say,
   the number of blocks) by Y (say, the number of expressions), call
   this function.  It's not important what X and Y represent; only
   that Y correspond to the things that can be done in parallel.  This
   function will return an appropriate chunking factor C; you should
   solve C sets of equations in parallel.  By going through this
   function, we can easily trade space against time; by solving fewer
   equations in parallel we use less space.  */

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
   Local properties are those that are defined by the block, irrespective
   of other blocks.

   An expression is transparent in a block if its operands are not modified
   in the block.

   An expression is computed (locally available) in a block if it is computed
   at least once and expression would contain the same value if the
   computation was moved to the end of the block.

   An expression is locally anticipatable in a block if it is computed at
   least once and expression would contain the same value if the computation
   was moved to the beginning of the block.

   We call this routine for cprop, pre and code hoisting.  They all
   compute basically the same information and thus can easily share
   this code.

   TRANSP, COMP, and ANTLOC are destination sbitmaps for recording
   local properties.  If NULL, then it is not necessary to compute
   or record that particular property.

   SETP controls which hash table to look at.  If zero, this routine
   looks at the expr hash table; if nonzero this routine looks at
   the set hash table.  Additionally, TRANSP is computed as ~TRANSP,
   since this is really cprop's ABSALTERED.  */
 
static void
compute_local_properties (transp, comp, antloc, setp)
     sbitmap *transp;
     sbitmap *comp;
     sbitmap *antloc;
     int setp;
{
  int i, hash_table_size;
  struct expr **hash_table;
  
  /* Initialize any bitmaps that were passed in.  */
  if (transp)
    {
      if (setp)
	sbitmap_vector_zero (transp, n_basic_blocks);
      else
	sbitmap_vector_ones (transp, n_basic_blocks);
    }
  if (comp)
    sbitmap_vector_zero (comp, n_basic_blocks);
  if (antloc)
    sbitmap_vector_zero (antloc, n_basic_blocks);

  /* We use the same code for cprop, pre and hoisting.  For cprop
     we care about the set hash table, for pre and hoisting we
     care about the expr hash table.  */
  hash_table_size = setp ? set_hash_table_size : expr_hash_table_size;
  hash_table = setp ? set_hash_table : expr_hash_table;

  for (i = 0; i < hash_table_size; i++)
    {
      struct expr *expr;

      for (expr = hash_table[i]; expr != NULL; expr = expr->next_same_hash)
	{
	  struct occr *occr;
	  int indx = expr->bitmap_index;

	  /* The expression is transparent in this block if it is not killed.
	     We start by assuming all are transparent [none are killed], and
	     then reset the bits for those that are.  */

	  if (transp)
	    compute_transp (expr->expr, indx, transp, setp);

	  /* The occurrences recorded in antic_occr are exactly those that
	     we want to set to non-zero in ANTLOC.  */

	  if (antloc)
	    {
	      for (occr = expr->antic_occr; occr != NULL; occr = occr->next)
		{
		  int bb = BLOCK_NUM (occr->insn);
		  SET_BIT (antloc[bb], indx);

		  /* While we're scanning the table, this is a good place to
		     initialize this.  */
		  occr->deleted_p = 0;
		}
	    }

	  /* The occurrences recorded in avail_occr are exactly those that
	     we want to set to non-zero in COMP.  */
	  if (comp)
	    {
	
	      for (occr = expr->avail_occr; occr != NULL; occr = occr->next)
		{
		  int bb = BLOCK_NUM (occr->insn);
		  SET_BIT (comp[bb], indx);

		  /* While we're scanning the table, this is a good place to
		     initialize this.  */
		  occr->copied_p = 0;
		}
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
  int n;

  reg_set_table_size = n_regs + REG_SET_TABLE_SLOP;
  n = reg_set_table_size * sizeof (struct reg_set *);
  reg_set_table = (struct reg_set **) gmalloc (n);
  bzero ((char *) reg_set_table, n);

  gcc_obstack_init (&reg_set_obstack);
}

static void
free_reg_set_mem ()
{
  free (reg_set_table);
  obstack_free (&reg_set_obstack, NULL_PTR);
}

/* Record REGNO in the reg_set table.  */

static void
record_one_set (regno, insn)
     int regno;
     rtx insn;
{
  /* allocate a new reg_set element and link it onto the list */
  struct reg_set *new_reg_info, *reg_info_ptr1, *reg_info_ptr2;

  /* If the table isn't big enough, enlarge it.  */
  if (regno >= reg_set_table_size)
    {
      int new_size = regno + REG_SET_TABLE_SLOP;
      reg_set_table = (struct reg_set **)
	grealloc ((char *) reg_set_table,
		  new_size * sizeof (struct reg_set *));
      bzero ((char *) (reg_set_table + reg_set_table_size),
	     (new_size - reg_set_table_size) * sizeof (struct reg_set *));
      reg_set_table_size = new_size;
    }

  new_reg_info = (struct reg_set *) obstack_alloc (&reg_set_obstack,
						   sizeof (struct reg_set));
  bytes_used += sizeof (struct reg_set);
  new_reg_info->insn = insn;
  new_reg_info->next = NULL;
  if (reg_set_table[regno] == NULL)
    reg_set_table[regno] = new_reg_info;
  else
    {
      reg_info_ptr1 = reg_info_ptr2 = reg_set_table[regno];
      /* ??? One could keep a "last" pointer to speed this up.  */
      while (reg_info_ptr1 != NULL)
	{
	  reg_info_ptr2 = reg_info_ptr1;
	  reg_info_ptr1 = reg_info_ptr1->next;
	}
      reg_info_ptr2->next = new_reg_info;
    }
}

/* Called from compute_sets via note_stores to handle one
   SET or CLOBBER in an insn.  The DATA is really the instruction
   in which the SET is occurring.  */

static void
record_set_info (dest, setter, data)
     rtx dest, setter ATTRIBUTE_UNUSED;
     void *data;
{
  rtx record_set_insn = (rtx) data;

  if (GET_CODE (dest) == SUBREG)
    dest = SUBREG_REG (dest);

  if (GET_CODE (dest) == REG)
    {
      if (REGNO (dest) >= FIRST_PSEUDO_REGISTER)
	record_one_set (REGNO (dest), record_set_insn);
    }
}

/* Scan the function and record each set of each pseudo-register.

   This is called once, at the start of the gcse pass.
   See the comments for `reg_set_table' for further docs.  */

static void
compute_sets (f)
     rtx f;
{
  rtx insn = f;

  while (insn)
    {
      if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
	note_stores (PATTERN (insn), record_set_info, insn);
      insn = NEXT_INSN (insn);
    }
}

/* Hash table support.  */

#define NEVER_SET -1

/* For each register, the cuid of the first/last insn in the block to set it,
   or -1 if not set.  */
static int *reg_first_set;
static int *reg_last_set;

/* While computing "first/last set" info, this is the CUID of first/last insn
   to set memory or -1 if not set.  `mem_last_set' is also used when
   performing GCSE to record whether memory has been set since the beginning
   of the block.
   Note that handling of memory is very simple, we don't make any attempt
   to optimize things (later).  */
static int mem_first_set;
static int mem_last_set;

/* Perform a quick check whether X, the source of a set, is something
   we want to consider for GCSE.  */

static int
want_to_gcse_p (x)
     rtx x;
{
  enum rtx_code code = GET_CODE (x);

  switch (code)
    {
    case REG:
    case SUBREG:
    case CONST_INT:
    case CONST_DOUBLE:
    case CALL:
      return 0;

    default:
      break;
    }

  return 1;
}

/* Return non-zero if the operands of expression X are unchanged from the
   start of INSN's basic block up to but not including INSN (if AVAIL_P == 0),
   or from INSN to the end of INSN's basic block (if AVAIL_P != 0).  */

static int
oprs_unchanged_p (x, insn, avail_p)
     rtx x, insn;
     int avail_p;
{
  int i;
  enum rtx_code code;
  const char *fmt;

  /* repeat is used to turn tail-recursion into iteration.  */
 repeat:

  if (x == 0)
    return 1;

  code = GET_CODE (x);
  switch (code)
    {
    case REG:
      if (avail_p)
	return (reg_last_set[REGNO (x)] == NEVER_SET
		|| reg_last_set[REGNO (x)] < INSN_CUID (insn));
      else
	return (reg_first_set[REGNO (x)] == NEVER_SET
		|| reg_first_set[REGNO (x)] >= INSN_CUID (insn));

    case MEM:
      if (avail_p)
	{
	  if (mem_last_set != NEVER_SET
	      && mem_last_set >= INSN_CUID (insn))
	    return 0;
	}
      else
	{
	  if (mem_first_set != NEVER_SET
	      && mem_first_set < INSN_CUID (insn))
	    return 0;
	}
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
	  if (! oprs_unchanged_p (tem, insn, avail_p))
	    return 0;
	}
      else if (fmt[i] == 'E')
	{
	  int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    {
	      if (! oprs_unchanged_p (XVECEXP (x, i, j), insn, avail_p))
		return 0;
	    }
	}
    }

  return 1;
}

/* Return non-zero if the operands of expression X are unchanged from
   the start of INSN's basic block up to but not including INSN.  */

static int
oprs_anticipatable_p (x, insn)
     rtx x, insn;
{
  return oprs_unchanged_p (x, insn, 0);
}

/* Return non-zero if the operands of expression X are unchanged from
   INSN to the end of INSN's basic block.  */

static int
oprs_available_p (x, insn)
     rtx x, insn;
{
  return oprs_unchanged_p (x, insn, 1);
}

/* Hash expression X.
   MODE is only used if X is a CONST_INT.
   A boolean indicating if a volatile operand is found or if the expression
   contains something we don't want to insert in the table is stored in
   DO_NOT_RECORD_P.

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

  /* repeat is used to turn tail-recursion into iteration.  */
 repeat:

  if (x == 0)
    return hash;

  code = GET_CODE (x);
  switch (code)
    {
    case REG:
      {
	register int regno = REGNO (x);
	hash += ((unsigned) REG << 7) + regno;
	return hash;
      }

    case CONST_INT:
      {
	unsigned HOST_WIDE_INT tem = INTVAL (x);
	hash += ((unsigned) CONST_INT << 7) + (unsigned) mode + tem;
	return hash;
      }

    case CONST_DOUBLE:
      /* This is like the general case, except that it only counts
	 the integers representing the constant.  */
      hash += (unsigned) code + (unsigned) GET_MODE (x);
      if (GET_MODE (x) != VOIDmode)
	for (i = 2; i < GET_RTX_LENGTH (CONST_DOUBLE); i++)
	  {
	    unsigned tem = XWINT (x, i);
	    hash += tem;
	  }
      else
	hash += ((unsigned) CONST_DOUBLE_LOW (x)
		 + (unsigned) CONST_DOUBLE_HIGH (x));
      return hash;

      /* Assume there is only one rtx object for any given label.  */
    case LABEL_REF:
      /* We don't hash on the address of the CODE_LABEL to avoid bootstrap
	 differences and differences between each stage's debugging dumps.  */
      hash += ((unsigned) LABEL_REF << 7) + CODE_LABEL_NUMBER (XEXP (x, 0));
      return hash;

    case SYMBOL_REF:
      {
	/* Don't hash on the symbol's address to avoid bootstrap differences.
	   Different hash values may cause expressions to be recorded in
	   different orders and thus different registers to be used in the
	   final assembler.  This also avoids differences in the dump files
	   between various stages.  */
	unsigned int h = 0;
	unsigned char *p = (unsigned char *) XSTR (x, 0);
	while (*p)
	  h += (h << 7) + *p++; /* ??? revisit */
	hash += ((unsigned) SYMBOL_REF << 7) + h;
	return hash;
      }

    case MEM:
      if (MEM_VOLATILE_P (x))
	{
	  *do_not_record_p = 1;
	  return 0;
	}
      hash += (unsigned) MEM;
      hash += MEM_ALIAS_SET (x);
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

    default:
      break;
    }

  i = GET_RTX_LENGTH (code) - 1;
  hash += (unsigned) code + (unsigned) GET_MODE (x);
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
	  hash += hash_expr_1 (tem, 0, do_not_record_p);
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
	{
	  register unsigned char *p = (unsigned char *) XSTR (x, i);
	  if (p)
	    while (*p)
	      hash += *p++;
	}
      else if (fmt[i] == 'i')
	{
	  register unsigned tem = XINT (x, i);
	  hash += tem;
	}
      else
	abort ();
    }

  return hash;
}

/* Hash a set of register REGNO.

   Sets are hashed on the register that is set.
   This simplifies the PRE copy propagation code.

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

/* Return non-zero if exp1 is equivalent to exp2.
   ??? Borrowed from cse.c.  Might want to remerge with cse.c.  Later.  */

static int
expr_equiv_p (x, y)
     rtx x, y;
{
  register int i, j;
  register enum rtx_code code;
  register const char *fmt;

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

/* Insert expression X in INSN in the hash table.
   If it is already present, record it as the last occurrence in INSN's
   basic block.

   MODE is the mode of the value X is being stored into.
   It is only used if X is a CONST_INT.

   ANTIC_P is non-zero if X is an anticipatable expression.
   AVAIL_P is non-zero if X is an available expression.  */

static void
insert_expr_in_table (x, mode, insn, antic_p, avail_p)
     rtx x;
     enum machine_mode mode;
     rtx insn;
     int antic_p, avail_p;
{
  int found, do_not_record_p;
  unsigned int hash;
  struct expr *cur_expr, *last_expr = NULL;
  struct occr *antic_occr, *avail_occr;
  struct occr *last_occr = NULL;

  hash = hash_expr (x, mode, &do_not_record_p, expr_hash_table_size);

  /* Do not insert expression in table if it contains volatile operands,
     or if hash_expr determines the expression is something we don't want
     to or can't handle.  */
  if (do_not_record_p)
    return;

  cur_expr = expr_hash_table[hash];
  found = 0;

  while (cur_expr && ! (found = expr_equiv_p (cur_expr->expr, x)))
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
      if (expr_hash_table[hash] == NULL)
	{
	  /* This is the first pattern that hashed to this index.  */
	  expr_hash_table[hash] = cur_expr;
	}
      else
	{
	  /* Add EXPR to end of this hash chain.  */
	  last_expr->next_same_hash = cur_expr;
	}
      /* Set the fields of the expr element.  */ 
      cur_expr->expr = x;
      cur_expr->bitmap_index = n_exprs++;
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
	{
	  /* Found another instance of the expression in the same basic block.
	     Prefer the currently recorded one.  We want the first one in the
	     block and the block is scanned from start to end.  */
	  ; /* nothing to do */
	}
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
	{
	  /* Found another instance of the expression in the same basic block.
	     Prefer this occurrence to the currently recorded one.  We want
	     the last one in the block and the block is scanned from start
	     to end.  */
	  avail_occr->insn = insn;
	}
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
insert_set_in_table (x, insn)
     rtx x;
     rtx insn;
{
  int found;
  unsigned int hash;
  struct expr *cur_expr, *last_expr = NULL;
  struct occr *cur_occr, *last_occr = NULL;

  if (GET_CODE (x) != SET
      || GET_CODE (SET_DEST (x)) != REG)
    abort ();

  hash = hash_set (REGNO (SET_DEST (x)), set_hash_table_size);

  cur_expr = set_hash_table[hash];
  found = 0;

  while (cur_expr && ! (found = expr_equiv_p (cur_expr->expr, x)))
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
      if (set_hash_table[hash] == NULL)
	{
	  /* This is the first pattern that hashed to this index.  */
	  set_hash_table[hash] = cur_expr;
	}
      else
	{
	  /* Add EXPR to end of this hash chain.  */
	  last_expr->next_same_hash = cur_expr;
	}
      /* Set the fields of the expr element.
	 We must copy X because it can be modified when copy propagation is
	 performed on its operands.  */
      /* ??? Should this go in a different obstack?  */
      cur_expr->expr = copy_rtx (x);
      cur_expr->bitmap_index = n_sets++;
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
    {
      /* Found another instance of the expression in the same basic block.
	 Prefer this occurrence to the currently recorded one.  We want
	 the last one in the block and the block is scanned from start
	 to end.  */
      cur_occr->insn = insn;
    }
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

/* Scan pattern PAT of INSN and add an entry to the hash table.
   If SET_P is non-zero, this is for the assignment hash table,
   otherwise it is for the expression hash table.  */

static void
hash_scan_set (pat, insn, set_p)
     rtx pat, insn;
     int set_p;
{
  rtx src = SET_SRC (pat);
  rtx dest = SET_DEST (pat);

  if (GET_CODE (src) == CALL)
    hash_scan_call (src, insn);

  if (GET_CODE (dest) == REG)
    {
      int regno = REGNO (dest);
      rtx tmp;

      /* Only record sets of pseudo-regs in the hash table.  */
      if (! set_p
	  && regno >= FIRST_PSEUDO_REGISTER
	  /* Don't GCSE something if we can't do a reg/reg copy.  */
	  && can_copy_p [GET_MODE (dest)]
	  /* Is SET_SRC something we want to gcse?  */
	  && want_to_gcse_p (src))
	{
	  /* An expression is not anticipatable if its operands are
	     modified before this insn.  */
	  int antic_p = oprs_anticipatable_p (src, insn);
	  /* An expression is not available if its operands are
	     subsequently modified, including this insn.  */
	  int avail_p = oprs_available_p (src, insn);
	  insert_expr_in_table (src, GET_MODE (dest), insn, antic_p, avail_p);
	}
      /* Record sets for constant/copy propagation.  */
      else if (set_p
	       && regno >= FIRST_PSEUDO_REGISTER
	       && ((GET_CODE (src) == REG
		    && REGNO (src) >= FIRST_PSEUDO_REGISTER
		    && can_copy_p [GET_MODE (dest)])
		   || GET_CODE (src) == CONST_INT
		   || GET_CODE (src) == SYMBOL_REF
		   || GET_CODE (src) == CONST_DOUBLE)
	       /* A copy is not available if its src or dest is subsequently
		  modified.  Here we want to search from INSN+1 on, but
		  oprs_available_p searches from INSN on.  */
	       && (insn == BLOCK_END (BLOCK_NUM (insn))
		   || ((tmp = next_nonnote_insn (insn)) != NULL_RTX
		       && oprs_available_p (pat, tmp))))
	insert_set_in_table (pat, insn);
    }
}

static void
hash_scan_clobber (x, insn)
     rtx x ATTRIBUTE_UNUSED, insn ATTRIBUTE_UNUSED;
{
  /* Currently nothing to do.  */
}

static void
hash_scan_call (x, insn)
     rtx x ATTRIBUTE_UNUSED, insn ATTRIBUTE_UNUSED;
{
  /* Currently nothing to do.  */
}

/* Process INSN and add hash table entries as appropriate.

   Only available expressions that set a single pseudo-reg are recorded.

   Single sets in a PARALLEL could be handled, but it's an extra complication
   that isn't dealt with right now.  The trick is handling the CLOBBERs that
   are also in the PARALLEL.  Later.

   If SET_P is non-zero, this is for the assignment hash table,
   otherwise it is for the expression hash table.
   If IN_LIBCALL_BLOCK nonzero, we are in a libcall block, and should
   not record any expressions.  */

static void
hash_scan_insn (insn, set_p, in_libcall_block)
     rtx insn;
     int set_p;
     int in_libcall_block;
{
  rtx pat = PATTERN (insn);

  /* Pick out the sets of INSN and for other forms of instructions record
     what's been modified.  */

  if (GET_CODE (pat) == SET && ! in_libcall_block)
    {
      /* Ignore obvious no-ops.  */
      if (SET_SRC (pat) != SET_DEST (pat))
	hash_scan_set (pat, insn, set_p);
    }
  else if (GET_CODE (pat) == PARALLEL)
    {
      int i;

      for (i = 0; i < XVECLEN (pat, 0); i++)
	{
	  rtx x = XVECEXP (pat, 0, i);

	  if (GET_CODE (x) == SET)
	    {
	      if (GET_CODE (SET_SRC (x)) == CALL)
		hash_scan_call (SET_SRC (x), insn);
	    }
	  else if (GET_CODE (x) == CLOBBER)
	    hash_scan_clobber (x, insn);
	  else if (GET_CODE (x) == CALL)
	    hash_scan_call (x, insn);
	}
    }
  else if (GET_CODE (pat) == CLOBBER)
    hash_scan_clobber (pat, insn);
  else if (GET_CODE (pat) == CALL)
    hash_scan_call (pat, insn);
}

static void
dump_hash_table (file, name, table, table_size, total_size)
     FILE *file;
     const char *name;
     struct expr **table;
     int table_size, total_size;
{
  int i;
  /* Flattened out table, so it's printed in proper order.  */
  struct expr **flat_table;
  unsigned int *hash_val;

  flat_table 
    = (struct expr **) xcalloc (total_size, sizeof (struct expr *));
  hash_val = (unsigned int *) xmalloc (total_size * sizeof (unsigned int));

  for (i = 0; i < table_size; i++)
    {
      struct expr *expr;

      for (expr = table[i]; expr != NULL; expr = expr->next_same_hash)
	{
	  flat_table[expr->bitmap_index] = expr;
	  hash_val[expr->bitmap_index] = i;
	}
    }

  fprintf (file, "%s hash table (%d buckets, %d entries)\n",
	   name, table_size, total_size);

  for (i = 0; i < total_size; i++)
    {
      struct expr *expr = flat_table[i];

      fprintf (file, "Index %d (hash value %d)\n  ",
	       expr->bitmap_index, hash_val[i]);
      print_rtl (file, expr->expr);
      fprintf (file, "\n");
    }

  fprintf (file, "\n");

  /* Clean up.  */
  free (flat_table);
  free (hash_val);
}

/* Record register first/last/block set information for REGNO in INSN.
   reg_first_set records the first place in the block where the register
   is set and is used to compute "anticipatability".
   reg_last_set records the last place in the block where the register
   is set and is used to compute "availability".
   reg_set_in_block records whether the register is set in the block
   and is used to compute "transparency".  */

static void
record_last_reg_set_info (insn, regno)
     rtx insn;
     int regno;
{
  if (reg_first_set[regno] == NEVER_SET)
    reg_first_set[regno] = INSN_CUID (insn);
  reg_last_set[regno] = INSN_CUID (insn);
  SET_BIT (reg_set_in_block[BLOCK_NUM (insn)], regno);
}

/* Record memory first/last/block set information for INSN.  */

static void
record_last_mem_set_info (insn)
     rtx insn;
{
  if (mem_first_set == NEVER_SET)
    mem_first_set = INSN_CUID (insn);
  mem_last_set = INSN_CUID (insn);
  mem_set_in_block[BLOCK_NUM (insn)] = 1;
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
   SET_P is non-zero for computing the assignment hash table.  */

static void
compute_hash_table (set_p)
     int set_p;
{
  int bb;

  /* While we compute the hash table we also compute a bit array of which
     registers are set in which blocks.
     We also compute which blocks set memory, in the absence of aliasing
     support [which is TODO].
     ??? This isn't needed during const/copy propagation, but it's cheap to
     compute.  Later.  */
  sbitmap_vector_zero (reg_set_in_block, n_basic_blocks);
  bzero ((char *) mem_set_in_block, n_basic_blocks);

  /* Some working arrays used to track first and last set in each block.  */
  /* ??? One could use alloca here, but at some size a threshold is crossed
     beyond which one should use malloc.  Are we at that threshold here?  */
  reg_first_set = (int *) gmalloc (max_gcse_regno * sizeof (int));
  reg_last_set = (int *) gmalloc (max_gcse_regno * sizeof (int));

  for (bb = 0; bb < n_basic_blocks; bb++)
    {
      rtx insn;
      int regno;
      int in_libcall_block;
      int i;

      /* First pass over the instructions records information used to
	 determine when registers and memory are first and last set.
	 ??? The mem_set_in_block and hard-reg reg_set_in_block computation
	 could be moved to compute_sets since they currently don't change.  */

      for (i = 0; i < max_gcse_regno; i++)
	reg_first_set[i] = reg_last_set[i] = NEVER_SET;
      mem_first_set = NEVER_SET;
      mem_last_set = NEVER_SET;

      for (insn = BLOCK_HEAD (bb);
	   insn && insn != NEXT_INSN (BLOCK_END (bb));
	   insn = NEXT_INSN (insn))
	{
#ifdef NON_SAVING_SETJMP 
	  if (NON_SAVING_SETJMP && GET_CODE (insn) == NOTE
	      && NOTE_LINE_NUMBER (insn) == NOTE_INSN_SETJMP)
	    {
	      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
		record_last_reg_set_info (insn, regno);
	      continue;
	    }
#endif

	  if (GET_RTX_CLASS (GET_CODE (insn)) != 'i')
	    continue;

	  if (GET_CODE (insn) == CALL_INSN)
	    {
	      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
		if ((call_used_regs[regno]
		     && regno != STACK_POINTER_REGNUM
#if HARD_FRAME_POINTER_REGNUM != FRAME_POINTER_REGNUM
		     && regno != HARD_FRAME_POINTER_REGNUM
#endif
#if ARG_POINTER_REGNUM != FRAME_POINTER_REGNUM
		     && ! (regno == ARG_POINTER_REGNUM && fixed_regs[regno])
#endif
#if defined (PIC_OFFSET_TABLE_REGNUM) && !defined (PIC_OFFSET_TABLE_REG_CALL_CLOBBERED)
		     && ! (regno == PIC_OFFSET_TABLE_REGNUM && flag_pic)
#endif

		     && regno != FRAME_POINTER_REGNUM)
		    || global_regs[regno])
		  record_last_reg_set_info (insn, regno);
	      if (! CONST_CALL_P (insn))
		record_last_mem_set_info (insn);
	    }

	  note_stores (PATTERN (insn), record_last_set_info, insn);
	}

      /* The next pass builds the hash table.  */

      for (insn = BLOCK_HEAD (bb), in_libcall_block = 0;
	   insn && insn != NEXT_INSN (BLOCK_END (bb));
	   insn = NEXT_INSN (insn))
	{
	  if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
	    {
	      if (find_reg_note (insn, REG_LIBCALL, NULL_RTX))
		in_libcall_block = 1;
	      else if (find_reg_note (insn, REG_RETVAL, NULL_RTX))
		in_libcall_block = 0;
	      hash_scan_insn (insn, set_p, in_libcall_block);
	    }
	}
    }

  free (reg_first_set);
  free (reg_last_set);
  /* Catch bugs early.  */
  reg_first_set = reg_last_set = 0;
}

/* Allocate space for the set hash table.
   N_INSNS is the number of instructions in the function.
   It is used to determine the number of buckets to use.  */

static void
alloc_set_hash_table (n_insns)
     int n_insns;
{
  int n;

  set_hash_table_size = n_insns / 4;
  if (set_hash_table_size < 11)
    set_hash_table_size = 11;
  /* Attempt to maintain efficient use of hash table.
     Making it an odd number is simplest for now.
     ??? Later take some measurements.  */
  set_hash_table_size |= 1;
  n = set_hash_table_size * sizeof (struct expr *);
  set_hash_table = (struct expr **) gmalloc (n);
}

/* Free things allocated by alloc_set_hash_table.  */

static void
free_set_hash_table ()
{
  free (set_hash_table);
}

/* Compute the hash table for doing copy/const propagation.  */

static void
compute_set_hash_table ()
{
  /* Initialize count of number of entries in hash table.  */
  n_sets = 0;
  bzero ((char *) set_hash_table, set_hash_table_size * sizeof (struct expr *));

  compute_hash_table (1);
}

/* Allocate space for the expression hash table.
   N_INSNS is the number of instructions in the function.
   It is used to determine the number of buckets to use.  */

static void
alloc_expr_hash_table (n_insns)
     int n_insns;
{
  int n;

  expr_hash_table_size = n_insns / 2;
  /* Make sure the amount is usable.  */
  if (expr_hash_table_size < 11)
    expr_hash_table_size = 11;
  /* Attempt to maintain efficient use of hash table.
     Making it an odd number is simplest for now.
     ??? Later take some measurements.  */
  expr_hash_table_size |= 1;
  n = expr_hash_table_size * sizeof (struct expr *);
  expr_hash_table = (struct expr **) gmalloc (n);
}

/* Free things allocated by alloc_expr_hash_table.  */

static void
free_expr_hash_table ()
{
  free (expr_hash_table);
}

/* Compute the hash table for doing GCSE.  */

static void
compute_expr_hash_table ()
{
  /* Initialize count of number of entries in hash table.  */
  n_exprs = 0;
  bzero ((char *) expr_hash_table, expr_hash_table_size * sizeof (struct expr *));

  compute_hash_table (0);
}

/* Expression tracking support.  */

/* Lookup pattern PAT in the expression table.
   The result is a pointer to the table entry, or NULL if not found.  */

static struct expr *
lookup_expr (pat)
     rtx pat;
{
  int do_not_record_p;
  unsigned int hash = hash_expr (pat, GET_MODE (pat), &do_not_record_p,
				 expr_hash_table_size);
  struct expr *expr;

  if (do_not_record_p)
    return NULL;

  expr = expr_hash_table[hash];

  while (expr && ! expr_equiv_p (expr->expr, pat))
    expr = expr->next_same_hash;

  return expr;
}

/* Lookup REGNO in the set table.
   If PAT is non-NULL look for the entry that matches it, otherwise return
   the first entry for REGNO.
   The result is a pointer to the table entry, or NULL if not found.  */

static struct expr *
lookup_set (regno, pat)
     int regno;
     rtx pat;
{
  unsigned int hash = hash_set (regno, set_hash_table_size);
  struct expr *expr;

  expr = set_hash_table[hash];

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
     int regno;
     struct expr *expr;
{
  do
    expr = expr->next_same_hash;
  while (expr && REGNO (SET_DEST (expr->expr)) != regno);
  return expr;
}

/* Reset tables used to keep track of what's still available [since the
   start of the block].  */

static void
reset_opr_set_tables ()
{
  /* Maintain a bitmap of which regs have been set since beginning of
     the block.  */
  sbitmap_zero (reg_set_bitmap);
  /* Also keep a record of the last instruction to modify memory.
     For now this is very trivial, we only record whether any memory
     location has been modified.  */
  mem_last_set = 0;
}

/* Return non-zero if the operands of X are not set before INSN in
   INSN's basic block.  */

static int
oprs_not_set_p (x, insn)
     rtx x, insn;
{
  int i;
  enum rtx_code code;
  const char *fmt;

  /* repeat is used to turn tail-recursion into iteration.  */
repeat:

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
    case SYMBOL_REF:
    case LABEL_REF:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
      return 1;

    case MEM:
      if (mem_last_set != 0)
	return 0;
      x = XEXP (x, 0);
      goto repeat;

    case REG:
      return ! TEST_BIT (reg_set_bitmap, REGNO (x));

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  int not_set_p;
	  /* If we are about to do the last recursive call
	     needed at this level, change it into iteration.
	     This function is called enough to be worth it.  */
	  if (i == 0)
	    {
	      x = XEXP (x, 0);
	      goto repeat;
	    }
	  not_set_p = oprs_not_set_p (XEXP (x, i), insn);
	  if (! not_set_p)
	    return 0;
	}
      else if (fmt[i] == 'E')
	{
	  int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    {
	      int not_set_p = oprs_not_set_p (XVECEXP (x, i, j), insn);
	      if (! not_set_p)
		return 0;
	    }
	}
    }

  return 1;
}

/* Mark things set by a CALL.  */

static void
mark_call (insn)
     rtx insn;
{
  mem_last_set = INSN_CUID (insn);
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
    SET_BIT (reg_set_bitmap, REGNO (dest));
  else if (GET_CODE (dest) == MEM)
    mem_last_set = INSN_CUID (insn);

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
    SET_BIT (reg_set_bitmap, REGNO (clob));
  else
    mem_last_set = INSN_CUID (insn);
}

/* Record things set by INSN.
   This data is used by oprs_not_set_p.  */

static void
mark_oprs_set (insn)
     rtx insn;
{
  rtx pat = PATTERN (insn);

  if (GET_CODE (pat) == SET)
    mark_set (pat, insn);
  else if (GET_CODE (pat) == PARALLEL)
    {
      int i;

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
  sbitmap_vector_zero (rd_kill, n_basic_blocks);

  rd_gen = (sbitmap *) sbitmap_vector_alloc (n_blocks, n_insns);
  sbitmap_vector_zero (rd_gen, n_basic_blocks);

  reaching_defs = (sbitmap *) sbitmap_vector_alloc (n_blocks, n_insns);
  sbitmap_vector_zero (reaching_defs, n_basic_blocks);

  rd_out = (sbitmap *) sbitmap_vector_alloc (n_blocks, n_insns);
  sbitmap_vector_zero (rd_out, n_basic_blocks);
}

/* Free reaching def variables.  */

static void
free_rd_mem ()
{
  free (rd_kill);
  free (rd_gen);
  free (reaching_defs);
  free (rd_out);
}

/* Add INSN to the kills of BB.
   REGNO, set in BB, is killed by INSN.  */

static void
handle_rd_kill_set (insn, regno, bb)
     rtx insn;
     int regno, bb;
{
  struct reg_set *this_reg = reg_set_table[regno];

  while (this_reg)
    {
      if (BLOCK_NUM (this_reg->insn) != BLOCK_NUM (insn))
	SET_BIT (rd_kill[bb], INSN_CUID (this_reg->insn));
      this_reg = this_reg->next;
    }
}

/* Compute the set of kill's for reaching definitions.  */

static void
compute_kill_rd ()
{
  int bb,cuid;

  /* For each block
       For each set bit in `gen' of the block (i.e each insn which
	   generates a definition in the block)
	 Call the reg set by the insn corresponding to that bit regx
	 Look at the linked list starting at reg_set_table[regx]
	 For each setting of regx in the linked list, which is not in
	     this block
	   Set the bit in `kill' corresponding to that insn
    */

  for (bb = 0; bb < n_basic_blocks; bb++)
    {
      for (cuid = 0; cuid < max_cuid; cuid++)
	{
	  if (TEST_BIT (rd_gen[bb], cuid))
	    {
	      rtx insn = CUID_INSN (cuid);
	      rtx pat = PATTERN (insn);

	      if (GET_CODE (insn) == CALL_INSN)
		{
		  int regno;

		  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
		    {
		      if ((call_used_regs[regno]
			   && regno != STACK_POINTER_REGNUM
#if HARD_FRAME_POINTER_REGNUM != FRAME_POINTER_REGNUM
			   && regno != HARD_FRAME_POINTER_REGNUM
#endif
#if ARG_POINTER_REGNUM != FRAME_POINTER_REGNUM
			   && ! (regno == ARG_POINTER_REGNUM
				 && fixed_regs[regno])
#endif
#if defined (PIC_OFFSET_TABLE_REGNUM) && !defined (PIC_OFFSET_TABLE_REG_CALL_CLOBBERED)
			   && ! (regno == PIC_OFFSET_TABLE_REGNUM && flag_pic)
#endif
			   && regno != FRAME_POINTER_REGNUM)
			  || global_regs[regno])
			handle_rd_kill_set (insn, regno, bb);
		    }
		}

	      if (GET_CODE (pat) == PARALLEL)
		{
		  int i;

		  /* We work backwards because ... */
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
	      else if (GET_CODE (pat) == SET)
		{
		  if (GET_CODE (SET_DEST (pat)) == REG)
		    {
		      /* Each setting of this register outside of this block
			 must be marked in the set of kills in this block.  */
		      handle_rd_kill_set (insn, REGNO (SET_DEST (pat)), bb);
		    }
		}
	      /* FIXME: CLOBBER? */
	    }
	}
    }
}

/* Compute the reaching definitions as in 
   Compilers Principles, Techniques, and Tools. Aho, Sethi, Ullman,
   Chapter 10.  It is the same algorithm as used for computing available
   expressions but applied to the gens and kills of reaching definitions.  */

static void
compute_rd ()
{
  int bb, changed, passes;

  for (bb = 0; bb < n_basic_blocks; bb++)
    sbitmap_copy (rd_out[bb] /*dst*/, rd_gen[bb] /*src*/);

  passes = 0;
  changed = 1;
  while (changed)
    {
      changed = 0;
      for (bb = 0; bb < n_basic_blocks; bb++)
	{
	  sbitmap_union_of_preds (reaching_defs[bb], rd_out, bb);
	  changed |= sbitmap_union_of_diff (rd_out[bb], rd_gen[bb],
					    reaching_defs[bb], rd_kill[bb]);
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
  sbitmap_vector_zero (ae_kill, n_basic_blocks);

  ae_gen = (sbitmap *) sbitmap_vector_alloc (n_blocks, n_exprs);
  sbitmap_vector_zero (ae_gen, n_basic_blocks);

  ae_in = (sbitmap *) sbitmap_vector_alloc (n_blocks, n_exprs);
  sbitmap_vector_zero (ae_in, n_basic_blocks);

  ae_out = (sbitmap *) sbitmap_vector_alloc (n_blocks, n_exprs);
  sbitmap_vector_zero (ae_out, n_basic_blocks);

  u_bitmap = (sbitmap) sbitmap_alloc (n_exprs);
  sbitmap_ones (u_bitmap);
}

static void
free_avail_expr_mem ()
{
  free (ae_kill);
  free (ae_gen);
  free (ae_in);
  free (ae_out);
  free (u_bitmap);
}

/* Compute the set of available expressions generated in each basic block.  */

static void
compute_ae_gen ()
{
  int i;

  /* For each recorded occurrence of each expression, set ae_gen[bb][expr].
     This is all we have to do because an expression is not recorded if it
     is not available, and the only expressions we want to work with are the
     ones that are recorded.  */

  for (i = 0; i < expr_hash_table_size; i++)
    {
      struct expr *expr = expr_hash_table[i];
      while (expr != NULL)
	{
	  struct occr *occr = expr->avail_occr;
	  while (occr != NULL)
	    {
	      SET_BIT (ae_gen[BLOCK_NUM (occr->insn)], expr->bitmap_index);
	      occr = occr->next;
	    }
	  expr = expr->next_same_hash;
	}
    }
}

/* Return non-zero if expression X is killed in BB.  */

static int
expr_killed_p (x, bb)
     rtx x;
     int bb;
{
  int i;
  enum rtx_code code;
  const char *fmt;

  /* repeat is used to turn tail-recursion into iteration.  */
 repeat:

  if (x == 0)
    return 1;

  code = GET_CODE (x);
  switch (code)
    {
    case REG:
      return TEST_BIT (reg_set_in_block[bb], REGNO (x));

    case MEM:
      if (mem_set_in_block[bb])
	return 1;
      x = XEXP (x, 0);
      goto repeat;

    case PC:
    case CC0: /*FIXME*/
    case CONST:
    case CONST_INT:
    case CONST_DOUBLE:
    case SYMBOL_REF:
    case LABEL_REF:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
      return 0;

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
	  if (expr_killed_p (tem, bb))
	    return 1;
	}
      else if (fmt[i] == 'E')
	{
	  int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    {
	      if (expr_killed_p (XVECEXP (x, i, j), bb))
		return 1;
	    }
	}
    }

  return 0;
}

/* Compute the set of available expressions killed in each basic block.  */

static void
compute_ae_kill (ae_gen, ae_kill)
     sbitmap *ae_gen, *ae_kill;
{
  int bb,i;

  for (bb = 0; bb < n_basic_blocks; bb++)
    {
      for (i = 0; i < expr_hash_table_size; i++)
	{
	  struct expr *expr = expr_hash_table[i];

	  for ( ; expr != NULL; expr = expr->next_same_hash)
	    {
	      /* Skip EXPR if generated in this block.  */
	      if (TEST_BIT (ae_gen[bb], expr->bitmap_index))
		continue;

	      if (expr_killed_p (expr->expr, bb))
		SET_BIT (ae_kill[bb], expr->bitmap_index);
	    }
	}
    }
}

/* Actually perform the Classic GCSE optimizations.  */

/* Return non-zero if occurrence OCCR of expression EXPR reaches block BB.

   CHECK_SELF_LOOP is non-zero if we should consider a block reaching itself
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
     int bb;
     int check_self_loop;
     char *visited;
{
  edge pred;

  for (pred = BASIC_BLOCK(bb)->pred; pred != NULL; pred = pred->pred_next)
    {
      int pred_bb = pred->src->index;

      if (visited[pred_bb])
	{
	  /* This predecessor has already been visited.
	     Nothing to do.  */
	  ;
	}
      else if (pred_bb == bb)
	{
	  /* BB loops on itself.  */
	  if (check_self_loop
	      && TEST_BIT (ae_gen[pred_bb], expr->bitmap_index)
	      && BLOCK_NUM (occr->insn) == pred_bb)
	    return 1;
	  visited[pred_bb] = 1;
	}
      /* Ignore this predecessor if it kills the expression.  */
      else if (TEST_BIT (ae_kill[pred_bb], expr->bitmap_index))
	visited[pred_bb] = 1;
      /* Does this predecessor generate this expression?  */
      else if (TEST_BIT (ae_gen[pred_bb], expr->bitmap_index))
	{
	  /* Is this the occurrence we're looking for?
	     Note that there's only one generating occurrence per block
	     so we just need to check the block number.  */
	  if (BLOCK_NUM (occr->insn) == pred_bb)
	    return 1;
	  visited[pred_bb] = 1;
	}
      /* Neither gen nor kill.  */
      else
	{
	  visited[pred_bb] = 1;
	  if (expr_reaches_here_p_work (occr, expr, pred_bb, check_self_loop, 
	      visited))
	    return 1;
	}
    }

  /* All paths have been checked.  */
  return 0;
}

/* This wrapper for expr_reaches_here_p_work() is to ensure that any
   memory allocated for that function is returned. */

static int
expr_reaches_here_p (occr, expr, bb, check_self_loop)
     struct occr *occr;
     struct expr *expr;
     int bb;
     int check_self_loop;
{
  int rval;
  char * visited = (char *) xcalloc (n_basic_blocks, 1);

  rval = expr_reaches_here_p_work(occr, expr, bb, check_self_loop, visited);
  
  free (visited);

  return (rval);
}

/* Return the instruction that computes EXPR that reaches INSN's basic block.
   If there is more than one such instruction, return NULL.

   Called only by handle_avail_expr.  */

static rtx
computing_insn (expr, insn)
     struct expr *expr;
     rtx insn;
{
  int bb = BLOCK_NUM (insn);

  if (expr->avail_occr->next == NULL)
    {    
      if (BLOCK_NUM (expr->avail_occr->insn) == bb)
	{
	  /* The available expression is actually itself
	     (i.e. a loop in the flow graph) so do nothing.  */
	  return NULL;
	}
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
	  if (BLOCK_NUM (occr->insn) == bb)
	    {
	      /* The expression is generated in this block.
		 The only time we care about this is when the expression
		 is generated later in the block [and thus there's a loop].
		 We let the normal cse pass handle the other cases.  */
	      if (INSN_CUID (insn) < INSN_CUID (occr->insn))
		{
		  if (expr_reaches_here_p (occr, expr, bb, 1))
		    {
		      can_reach++;
		      if (can_reach > 1)
			return NULL;
		      insn_computes_expr = occr->insn;
		    }
		}
	    }
	  else /* Computation of the pattern outside this block.  */
	    {
	      if (expr_reaches_here_p (occr, expr, bb, 0))
		{
		  can_reach++;
		  if (can_reach > 1)
		    return NULL;
		  insn_computes_expr = occr->insn;
		}
	    }
	}

      if (insn_computes_expr == NULL)
	abort ();
      return insn_computes_expr;
    }
}

/* Return non-zero if the definition in DEF_INSN can reach INSN.
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
	  if (GET_CODE (PATTERN (def_insn)) == CLOBBER)
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

/* Return non-zero if *ADDR_THIS_REG can only have one value at INSN.
   The value returned is the number of definitions that reach INSN.
   Returning a value of zero means that [maybe] more than one definition
   reaches INSN and the caller can't perform whatever optimization it is
   trying.  i.e. it is always safe to return zero.  */

static int
can_disregard_other_sets (addr_this_reg, insn, for_combine)
     struct reg_set **addr_this_reg;
     rtx insn;
     int for_combine;
{
  int number_of_reaching_defs = 0;
  struct reg_set *this_reg = *addr_this_reg;

  while (this_reg)
    {
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
	    {
	      /* A setting of the reg to a different value reaches INSN.  */
	      return 0;
	    }
	  if (number_of_reaching_defs > 1)
	    {
	      /* If in this setting the value the register is being
		 set to is equal to the previous value the register 
		 was set to and this setting reaches the insn we are
		 trying to do the substitution on then we are ok.  */

	      if (GET_CODE (PATTERN (this_reg->insn)) == CLOBBER)
		return 0;
	      if (! rtx_equal_p (SET_SRC (PATTERN (this_reg->insn)),
				 SET_SRC (PATTERN (insn))))
		return 0;
	    }
	  *addr_this_reg = this_reg; 
	}

      /* prev_this_reg = this_reg; */
      this_reg = this_reg->next;
    }

  return number_of_reaching_defs;
}

/* Expression computed by insn is available and the substitution is legal,
   so try to perform the substitution.

   The result is non-zero if any changes were made.  */

static int
handle_avail_expr (insn, expr)
     rtx insn;
     struct expr *expr;
{
  rtx pat, insn_computes_expr;
  rtx to;
  struct reg_set *this_reg;
  int found_setting, use_src;
  int changed = 0;

  /* We only handle the case where one computation of the expression
     reaches this instruction.  */
  insn_computes_expr = computing_insn (expr, insn);
  if (insn_computes_expr == NULL)
    return 0;

  found_setting = 0;
  use_src = 0;

  /* At this point we know only one computation of EXPR outside of this
     block reaches this insn.  Now try to find a register that the
     expression is computed into.  */

  if (GET_CODE (SET_SRC (PATTERN (insn_computes_expr))) == REG)
    {
      /* This is the case when the available expression that reaches
	 here has already been handled as an available expression.  */
      int regnum_for_replacing = REGNO (SET_SRC (PATTERN (insn_computes_expr)));
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
      int regnum_for_replacing = REGNO (SET_DEST (PATTERN (insn_computes_expr)));
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
	to = SET_SRC (PATTERN (insn_computes_expr));
      else
	to = SET_DEST (PATTERN (insn_computes_expr));
      changed = validate_change (insn, &SET_SRC (pat), to, 0);

      /* We should be able to ignore the return code from validate_change but
	 to play it safe we check.  */
      if (changed)
	{
	  gcse_subst_count++;
	  if (gcse_file != NULL)
	    {
	      fprintf (gcse_file, "GCSE: Replacing the source in insn %d with reg %d %s insn %d\n",
		       INSN_UID (insn), REGNO (to),
		       use_src ? "from" : "set in",
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

      to = gen_reg_rtx (GET_MODE (SET_DEST (PATTERN (insn_computes_expr))));

      /* Generate the new insn.  */
      /* ??? If the change fails, we return 0, even though we created
	 an insn.  I think this is ok.  */
      new_insn
	= emit_insn_after (gen_rtx_SET (VOIDmode, to,
					SET_DEST (PATTERN (insn_computes_expr))),
				  insn_computes_expr);
      /* Keep block number table up to date.  */
      set_block_num (new_insn, BLOCK_NUM (insn_computes_expr));
      /* Keep register set table up to date.  */
      record_one_set (REGNO (to), new_insn);

      gcse_create_count++;
      if (gcse_file != NULL)
	{
	  fprintf (gcse_file, "GCSE: Creating insn %d to copy value of reg %d, computed in insn %d,\n",
		   INSN_UID (NEXT_INSN (insn_computes_expr)),
		   REGNO (SET_SRC (PATTERN (NEXT_INSN (insn_computes_expr)))),
		   INSN_UID (insn_computes_expr));
	  fprintf (gcse_file, "      into newly allocated reg %d\n", REGNO (to));
	}

      pat = PATTERN (insn);

      /* Do register replacement for INSN.  */
      changed = validate_change (insn, &SET_SRC (pat),
				 SET_DEST (PATTERN (NEXT_INSN (insn_computes_expr))),
				 0);

      /* We should be able to ignore the return code from validate_change but
	 to play it safe we check.  */
      if (changed)
	{
	  gcse_subst_count++;
	  if (gcse_file != NULL)
	    {
	      fprintf (gcse_file, "GCSE: Replacing the source in insn %d with reg %d set in insn %d\n",
		       INSN_UID (insn),
		       REGNO (SET_DEST (PATTERN (NEXT_INSN (insn_computes_expr)))),
		       INSN_UID (insn_computes_expr)); 
	    }

	}
    }

  return changed;
}

/* Perform classic GCSE.
   This is called by one_classic_gcse_pass after all the dataflow analysis
   has been done.

   The result is non-zero if a change was made.  */

static int
classic_gcse ()
{
  int bb, changed;
  rtx insn;

  /* Note we start at block 1.  */

  changed = 0;
  for (bb = 1; bb < n_basic_blocks; bb++)
    {
      /* Reset tables used to keep track of what's still valid [since the
	 start of the block].  */
      reset_opr_set_tables ();

      for (insn = BLOCK_HEAD (bb);
	   insn != NULL && insn != NEXT_INSN (BLOCK_END (bb));
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
		  && ((expr = lookup_expr (src)) != NULL)
		  /* Is the expression available [at the start of the
		     block]?  */
		  && TEST_BIT (ae_in[bb], expr->bitmap_index)
		  /* Are the operands unchanged since the start of the
		     block?  */
		  && oprs_not_set_p (src, insn))
		changed |= handle_avail_expr (insn, expr);
	    }

	  /* Keep track of everything modified by this insn.  */
	  /* ??? Need to be careful w.r.t. mods done to INSN.  */
	  if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
	    mark_oprs_set (insn);
	}
    }

  return changed;
}

/* Top level routine to perform one classic GCSE pass.

   Return non-zero if a change was made.  */

static int
one_classic_gcse_pass (pass)
     int pass;
{
  int changed = 0;

  gcse_subst_count = 0;
  gcse_create_count = 0;

  alloc_expr_hash_table (max_cuid);
  alloc_rd_mem (n_basic_blocks, max_cuid);
  compute_expr_hash_table ();
  if (gcse_file)
    dump_hash_table (gcse_file, "Expression", expr_hash_table,
		     expr_hash_table_size, n_exprs);
  if (n_exprs > 0)
    {
      compute_kill_rd ();
      compute_rd ();
      alloc_avail_expr_mem (n_basic_blocks, n_exprs);
      compute_ae_gen ();
      compute_ae_kill (ae_gen, ae_kill);
      compute_available (ae_gen, ae_kill, ae_out, ae_in);
      changed = classic_gcse ();
      free_avail_expr_mem ();
    }
  free_rd_mem ();
  free_expr_hash_table ();

  if (gcse_file)
    {
      fprintf (gcse_file, "\n");
      fprintf (gcse_file, "GCSE of %s, pass %d: %d bytes needed, %d substs, %d insns created\n",
	       current_function_name, pass,
	       bytes_used, gcse_subst_count, gcse_create_count);
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

/* Allocate vars used for copy/const propagation.
   N_BLOCKS is the number of basic blocks.
   N_SETS is the number of sets.  */

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
  free (cprop_pavloc);
  free (cprop_absaltered);
  free (cprop_avin);
  free (cprop_avout);
}

/* For each block, compute whether X is transparent.
   X is either an expression or an assignment [though we don't care which,
   for this context an assignment is treated as an expression].
   For each block where an element of X is modified, set (SET_P == 1) or reset
   (SET_P == 0) the INDX bit in BMAP.  */

static void
compute_transp (x, indx, bmap, set_p)
     rtx x;
     int indx;
     sbitmap *bmap;
     int set_p;
{
  int bb,i;
  enum rtx_code code;
  const char *fmt;

  /* repeat is used to turn tail-recursion into iteration.  */
 repeat:

  if (x == 0)
    return;

  code = GET_CODE (x);
  switch (code)
    {
    case REG:
      {
	reg_set *r;
	int regno = REGNO (x);

	if (set_p)
	  {
	    if (regno < FIRST_PSEUDO_REGISTER)
	      {
		for (bb = 0; bb < n_basic_blocks; bb++)
		  if (TEST_BIT (reg_set_in_block[bb], regno))
		    SET_BIT (bmap[bb], indx);
	      }
	    else
	      {
		for (r = reg_set_table[regno]; r != NULL; r = r->next)
		  {
		    bb = BLOCK_NUM (r->insn);
		    SET_BIT (bmap[bb], indx);
		  }
	      }
	  }
	else
	  {
	    if (regno < FIRST_PSEUDO_REGISTER)
	      {
		for (bb = 0; bb < n_basic_blocks; bb++)
		  if (TEST_BIT (reg_set_in_block[bb], regno))
		    RESET_BIT (bmap[bb], indx);
	      }
	    else
	      {
		for (r = reg_set_table[regno]; r != NULL; r = r->next)
		  {
		    bb = BLOCK_NUM (r->insn);
		    RESET_BIT (bmap[bb], indx);
		  }
	      }
	  }
	return;
      }

    case MEM:
      if (set_p)
	{
	  for (bb = 0; bb < n_basic_blocks; bb++)
	    if (mem_set_in_block[bb])
	      SET_BIT (bmap[bb], indx);
	}
      else
	{
	  for (bb = 0; bb < n_basic_blocks; bb++)
	    if (mem_set_in_block[bb])
	      RESET_BIT (bmap[bb], indx);
	}
      x = XEXP (x, 0);
      goto repeat;

    case PC:
    case CC0: /*FIXME*/
    case CONST:
    case CONST_INT:
    case CONST_DOUBLE:
    case SYMBOL_REF:
    case LABEL_REF:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
      return;

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
	  compute_transp (tem, indx, bmap, set_p);
	}
      else if (fmt[i] == 'E')
	{
	  int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    compute_transp (XVECEXP (x, i, j), indx, bmap, set_p);
	}
    }
}

/* Top level routine to do the dataflow analysis needed by copy/const
   propagation.  */

static void
compute_cprop_data ()
{
  compute_local_properties (cprop_absaltered, cprop_pavloc, NULL, 1);
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

/* Set up a list of register numbers used in INSN.
   The found uses are stored in `reg_use_table'.
   `reg_use_count' is initialized to zero before entry, and
   contains the number of uses in the table upon exit.

   ??? If a register appears multiple times we will record it multiple
   times.  This doesn't hurt anything but it will slow things down.  */

static void
find_used_regs (x)
     rtx x;
{
  int i;
  enum rtx_code code;
  const char *fmt;

  /* repeat is used to turn tail-recursion into iteration.  */
 repeat:

  if (x == 0)
    return;

  code = GET_CODE (x);
  switch (code)
    {
    case REG:
      if (reg_use_count == MAX_USES)
	return;
      reg_use_table[reg_use_count].reg_rtx = x;
      reg_use_count++;
      return;

    case MEM:
      x = XEXP (x, 0);
      goto repeat;

    case PC:
    case CC0:
    case CONST:
    case CONST_INT:
    case CONST_DOUBLE:
    case SYMBOL_REF:
    case LABEL_REF:
    case CLOBBER:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
    case ASM_INPUT: /*FIXME*/
      return;

    case SET:
      if (GET_CODE (SET_DEST (x)) == MEM)
	find_used_regs (SET_DEST (x));
      x = SET_SRC (x);
      goto repeat;

    default:
      break;
    }

  /* Recursively scan the operands of this expression.  */

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
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
	  find_used_regs (XEXP (x, i));
	}
      else if (fmt[i] == 'E')
	{
	  int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    find_used_regs (XVECEXP (x, i, j));
	}
    }
}

/* Try to replace all non-SET_DEST occurrences of FROM in INSN with TO.
   Returns non-zero is successful.  */

static int
try_replace_reg (from, to, insn)
     rtx from, to, insn;
{
  rtx note;
  rtx src;
  int success;
  rtx set;

  note = find_reg_note (insn, REG_EQUAL, NULL_RTX);

  if (!note)
    note = find_reg_note (insn, REG_EQUIV, NULL_RTX);

  /* If this fails we could try to simplify the result of the
     replacement and attempt to recognize the simplified insn.

     But we need a general simplify_rtx that doesn't have pass
     specific state variables.  I'm not aware of one at the moment.  */


  success = validate_replace_src (from, to, insn);
  set = single_set (insn);

  /* We've failed to do replacement. Try to add REG_EQUAL note to not loose
     information.  */
  if (!success && !note)
    {
      if (!set)
	return 0;
      note = REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_EQUAL,
						   copy_rtx (SET_SRC (set)),
						   REG_NOTES (insn));
    }

  /* Always do the replacement in REQ_EQUAL and REG_EQUIV notes.  Also
     try to simplify them.  */
  if (note)
    {
      rtx simplified;
      src = XEXP (note, 0);
      replace_rtx (src, from, to);

      /* Try to simplify resulting note. */
      simplified = simplify_rtx (src);
      if (simplified)
	{
	  src = simplified;
	  XEXP (note, 0) = src;
	}

      /* REG_EQUAL may get simplified into register.
         We don't allow that. Remove that note. This code ought
         not to hapen, because previous code ought to syntetize
         reg-reg move, but be on the safe side.  */
      else if (REG_P (src))
	remove_note (insn, note);
    }
  return success;
}
/* Find a set of REGNO that is available on entry to INSN's block.
   Returns NULL if not found.  */

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
      struct expr *set = lookup_set (regno, NULL_RTX);

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
   JUMP_INSNS.  INSN must be a conditional jump; COPY is a copy of it
   that we can use for substitutions.
   REG_USED is the use we will try to replace, SRC is the constant we
   will try to substitute for it.
   Returns nonzero if a change was made.  */
static int
cprop_jump (insn, copy, reg_used, src)
     rtx insn, copy;
     struct reg_use *reg_used;
     rtx src;
{
  rtx set = PATTERN (copy);
  rtx temp;

  /* Replace the register with the appropriate constant.  */
  replace_rtx (SET_SRC (set), reg_used->reg_rtx, src);

  temp = simplify_ternary_operation (GET_CODE (SET_SRC (set)),
				     GET_MODE (SET_SRC (set)),
				     GET_MODE (XEXP (SET_SRC (set), 0)),
				     XEXP (SET_SRC (set), 0),
				     XEXP (SET_SRC (set), 1),
				     XEXP (SET_SRC (set), 2));

  /* If no simplification can be made, then try the next
     register.  */
  if (temp == 0)
    return 0;
 
  SET_SRC (set) = temp;

  /* That may have changed the structure of TEMP, so
     force it to be rerecognized if it has not turned
     into a nop or unconditional jump.  */
		
  INSN_CODE (copy) = -1;
  if ((SET_DEST (set) == pc_rtx
       && (SET_SRC (set) == pc_rtx
	   || GET_CODE (SET_SRC (set)) == LABEL_REF))
      || recog (PATTERN (copy), copy, NULL) >= 0)
    {
      /* This has either become an unconditional jump
	 or a nop-jump.  We'd like to delete nop jumps
	 here, but doing so confuses gcse.  So we just
	 make the replacement and let later passes
	 sort things out.  */
      PATTERN (insn) = set;
      INSN_CODE (insn) = -1;

      /* One less use of the label this insn used to jump to
	 if we turned this into a NOP jump.  */
      if (SET_SRC (set) == pc_rtx && JUMP_LABEL (insn) != 0)
	--LABEL_NUSES (JUMP_LABEL (insn));

      /* If this has turned into an unconditional jump,
	 then put a barrier after it so that the unreachable
	 code will be deleted.  */
      if (GET_CODE (SET_SRC (set)) == LABEL_REF)
	emit_barrier_after (insn);

      run_jump_opt_after_gcse = 1;

      const_prop_count++;
      if (gcse_file != NULL)
	{
	  int regno = REGNO (reg_used->reg_rtx);
	  fprintf (gcse_file, "CONST-PROP: Replacing reg %d in insn %d with constant ",
		   regno, INSN_UID (insn));
	  print_rtl (gcse_file, src);
	  fprintf (gcse_file, "\n");
	}
      return 1;
    }
  return 0;
}

#ifdef HAVE_cc0
/* Subroutine of cprop_insn that tries to propagate constants into
   JUMP_INSNS for machines that have CC0.  INSN is a single set that
   stores into CC0; the insn following it is a conditional jump.
   REG_USED is the use we will try to replace, SRC is the constant we
   will try to substitute for it.
   Returns nonzero if a change was made.  */
static int
cprop_cc0_jump (insn, reg_used, src)
     rtx insn;
     struct reg_use *reg_used;
     rtx src;
{
  rtx jump = NEXT_INSN (insn);
  rtx copy = copy_rtx (jump);
  rtx set = PATTERN (copy);

  /* We need to copy the source of the cc0 setter, as cprop_jump is going to
     substitute into it.  */
  replace_rtx (SET_SRC (set), cc0_rtx, copy_rtx (SET_SRC (PATTERN (insn))));
  if (! cprop_jump (jump, copy, reg_used, src))
    return 0;

  /* If we succeeded, delete the cc0 setter.  */
  PUT_CODE (insn, NOTE);
  NOTE_LINE_NUMBER (insn) = NOTE_INSN_DELETED;
  NOTE_SOURCE_FILE (insn) = 0;
  return 1;
 }
#endif
 
/* Perform constant and copy propagation on INSN.
   The result is non-zero if a change was made.  */

static int
cprop_insn (insn, alter_jumps)
     rtx insn;
     int alter_jumps;
{
  struct reg_use *reg_used;
  int changed = 0;
  rtx note;

  /* Only propagate into SETs.  Note that a conditional jump is a
     SET with pc_rtx as the destination.  */
  if ((GET_CODE (insn) != INSN
       && GET_CODE (insn) != JUMP_INSN)
      || GET_CODE (PATTERN (insn)) != SET)
    return 0;

  reg_use_count = 0;
  find_used_regs (PATTERN (insn));
  
  note = find_reg_note (insn, REG_EQUIV, NULL_RTX);
  if (!note)
    note = find_reg_note (insn, REG_EQUAL, NULL_RTX);

  /* We may win even when propagating constants into notes. */
  if (note)
    find_used_regs (XEXP (note, 0));

  reg_used = &reg_use_table[0];
  for ( ; reg_use_count > 0; reg_used++, reg_use_count--)
    {
      rtx pat, src;
      struct expr *set;
      int regno = REGNO (reg_used->reg_rtx);

      /* Ignore registers created by GCSE.
	 We do this because ... */
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
      if (GET_CODE (src) == CONST_INT || GET_CODE (src) == CONST_DOUBLE
	  || GET_CODE (src) == SYMBOL_REF)
	{
	  /* Handle normal insns first.  */
	  if (GET_CODE (insn) == INSN
	      && try_replace_reg (reg_used->reg_rtx, src, insn))
	    {
	      changed = 1;
	      const_prop_count++;
	      if (gcse_file != NULL)
		{
		  fprintf (gcse_file, "CONST-PROP: Replacing reg %d in insn %d with constant ",
			   regno, INSN_UID (insn));
		  print_rtl (gcse_file, src);
		  fprintf (gcse_file, "\n");
		}

	      /* The original insn setting reg_used may or may not now be
		 deletable.  We leave the deletion to flow.  */
	    }

	  /* Try to propagate a CONST_INT into a conditional jump.
	     We're pretty specific about what we will handle in this
	     code, we can extend this as necessary over time.

	     Right now the insn in question must look like
	     (set (pc) (if_then_else ...))  */
	  else if (alter_jumps
		   && GET_CODE (insn) == JUMP_INSN
		   && condjump_p (insn)
		   && ! simplejump_p (insn))
	    changed |= cprop_jump (insn, copy_rtx (insn), reg_used, src);
#ifdef HAVE_cc0
	  /* Similar code for machines that use a pair of CC0 setter and
	     conditional jump insn.  */
	  else if (alter_jumps
		   && GET_CODE (PATTERN (insn)) == SET
		   && SET_DEST (PATTERN (insn)) == cc0_rtx
		   && GET_CODE (NEXT_INSN (insn)) == JUMP_INSN
		   && condjump_p (NEXT_INSN (insn))
		   && ! simplejump_p (NEXT_INSN (insn)))
	    changed |= cprop_cc0_jump (insn, reg_used, src);
#endif
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
		  fprintf (gcse_file, "COPY-PROP: Replacing reg %d in insn %d with reg %d\n",
			   regno, INSN_UID (insn), REGNO (src));
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

/* Forward propagate copies.
   This includes copies and constants.
   Return non-zero if a change was made.  */

static int
cprop (alter_jumps)
     int alter_jumps;
{
  int bb, changed;
  rtx insn;

  /* Note we start at block 1.  */

  changed = 0;
  for (bb = 1; bb < n_basic_blocks; bb++)
    {
      /* Reset tables used to keep track of what's still valid [since the
	 start of the block].  */
      reset_opr_set_tables ();

      for (insn = BLOCK_HEAD (bb);
	   insn != NULL && insn != NEXT_INSN (BLOCK_END (bb));
	   insn = NEXT_INSN (insn))
	{
	  if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
	    {
	      changed |= cprop_insn (insn, alter_jumps);

	      /* Keep track of everything modified by this insn.  */
	      /* ??? Need to be careful w.r.t. mods done to INSN.  Don't
	         call mark_oprs_set if we turned the insn into a NOTE.  */
	      if (GET_CODE (insn) != NOTE)
		mark_oprs_set (insn);
	    }
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

  alloc_set_hash_table (max_cuid);
  compute_set_hash_table ();
  if (gcse_file)
    dump_hash_table (gcse_file, "SET", set_hash_table, set_hash_table_size,
		     n_sets);
  if (n_sets > 0)
    {
      alloc_cprop_mem (n_basic_blocks, n_sets);
      compute_cprop_data ();
      changed = cprop (alter_jumps);
      free_cprop_mem ();
    }
  free_set_hash_table ();

  if (gcse_file)
    {
      fprintf (gcse_file, "CPROP of %s, pass %d: %d bytes needed, %d const props, %d copy props\n",
	       current_function_name, pass,
	       bytes_used, const_prop_count, copy_prop_count);
      fprintf (gcse_file, "\n");
    }

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

static sbitmap *temp_bitmap;

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
  temp_bitmap = sbitmap_vector_alloc (n_blocks, n_exprs);

  pre_optimal = NULL;
  pre_redundant = NULL;
  pre_insert_map = NULL;
  pre_delete_map = NULL;
  ae_in = NULL;
  ae_out = NULL;
  u_bitmap = NULL;
  transpout = sbitmap_vector_alloc (n_blocks, n_exprs);
  ae_kill = sbitmap_vector_alloc (n_blocks, n_exprs);
  /* pre_insert and pre_delete are allocated later.  */
}

/* Free vars used for PRE analysis.  */

static void
free_pre_mem ()
{
  free (transp);
  free (comp);
  free (antloc);
  free (temp_bitmap);

  if (pre_optimal)
    free (pre_optimal);
  if (pre_redundant)
    free (pre_redundant);
  if (pre_insert_map)
    free (pre_insert_map);
  if (pre_delete_map)
    free (pre_delete_map);
  if (transpout)
    free (transpout);

  if (ae_in)
    free (ae_in);
  if (ae_out)
    free (ae_out);
  if (ae_kill)
    free (ae_kill);
  if (u_bitmap)
    free (u_bitmap);

  transp = comp = antloc = NULL;
  pre_optimal = pre_redundant = pre_insert_map = pre_delete_map = NULL;
  transpout = ae_in = ae_out = ae_kill = NULL;
  u_bitmap = NULL;

}

/* Top level routine to do the dataflow analysis needed by PRE.  */

static void
compute_pre_data ()
{
  compute_local_properties (transp, comp, antloc, 0);
  compute_transpout ();
  sbitmap_vector_zero (ae_kill, n_basic_blocks);
  compute_ae_kill (comp, ae_kill);
  edge_list = pre_edge_lcm (gcse_file, n_exprs, transp, comp, antloc,
			    ae_kill, &pre_insert_map, &pre_delete_map);
}


/* PRE utilities */

/* Return non-zero if an occurrence of expression EXPR in OCCR_BB would reach
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
     int occr_bb;
     struct expr *expr;
     int bb;
     char *visited;
{
  edge pred;

  for (pred = BASIC_BLOCK (bb)->pred; pred != NULL; pred = pred->pred_next)
    {
      int pred_bb = pred->src->index;

      if (pred->src == ENTRY_BLOCK_PTR
	  /* Has predecessor has already been visited?  */
	  || visited[pred_bb])
	{
	  /* Nothing to do.  */
	}
      /* Does this predecessor generate this expression?  */
      else if (TEST_BIT (comp[pred_bb], expr->bitmap_index))
	{
	  /* Is this the occurrence we're looking for?
	     Note that there's only one generating occurrence per block
	     so we just need to check the block number.  */
	  if (occr_bb == pred_bb)
	    return 1;
	  visited[pred_bb] = 1;
	}
      /* Ignore this predecessor if it kills the expression.  */
      else if (! TEST_BIT (transp[pred_bb], expr->bitmap_index))
	visited[pred_bb] = 1;
      /* Neither gen nor kill.  */
      else
	{
	  visited[pred_bb] = 1;
	  if (pre_expr_reaches_here_p_work (occr_bb, expr, pred_bb, visited))
	    return 1;
	}
    }

  /* All paths have been checked.  */
  return 0;
}

/* The wrapper for pre_expr_reaches_here_work that ensures that any
   memory allocated for that function is returned. */

static int
pre_expr_reaches_here_p (occr_bb, expr, bb)
     int occr_bb;
     struct expr *expr;
     int bb;
{
  int rval;
  char * visited = (char *) xcalloc (n_basic_blocks, 1);

  rval = pre_expr_reaches_here_p_work(occr_bb, expr, bb, visited);

  free (visited);

  return (rval);
}


/* Given an expr, generate RTL which we can insert at the end of a BB,
   or on an edge.  Set the block number of any insns generated to 
   the value of BB.  */

static rtx
process_insert_insn (expr)
     struct expr *expr;
{
  rtx reg = expr->reaching_reg;
  rtx pat, copied_expr;
  rtx first_new_insn;

  start_sequence ();
  copied_expr = copy_rtx (expr->expr);
  emit_move_insn (reg, copied_expr);
  first_new_insn = get_insns ();
  pat = gen_sequence ();
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
     int bb;
     int pre;
{
  rtx insn = BLOCK_END (bb);
  rtx new_insn;
  rtx reg = expr->reaching_reg;
  int regno = REGNO (reg);
  rtx pat;

  pat = process_insert_insn (expr);

  /* If the last insn is a jump, insert EXPR in front [taking care to
     handle cc0, etc. properly].  */

  if (GET_CODE (insn) == JUMP_INSN)
    {
#ifdef HAVE_cc0
      rtx note;
#endif

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
	      && GET_RTX_CLASS (GET_CODE (maybe_cc0_setter)) == 'i'
	      && sets_cc0_p (PATTERN (maybe_cc0_setter)))
	    insn = maybe_cc0_setter;
	}
#endif
      /* FIXME: What if something in cc0/jump uses value set in new insn?  */
      new_insn = emit_insn_before (pat, insn);
      if (BLOCK_HEAD (bb) == insn)
	BLOCK_HEAD (bb) = new_insn;
    }
  /* Likewise if the last insn is a call, as will happen in the presence
     of exception handling.  */
  else if (GET_CODE (insn) == CALL_INSN)
    {
      HARD_REG_SET parm_regs;
      int nparm_regs;
      rtx p;

      /* Keeping in mind SMALL_REGISTER_CLASSES and parameters in registers,
	 we search backward and place the instructions before the first
	 parameter is loaded.  Do this for everyone for consistency and a
	 presumtion that we'll get better code elsewhere as well.  */

      /* It should always be the case that we can put these instructions
	 anywhere in the basic block with performing PRE optimizations.
	 Check this.  */
      if (pre
	  && !TEST_BIT (antloc[bb], expr->bitmap_index)
          && !TEST_BIT (transp[bb], expr->bitmap_index))
	abort ();

      /* Since different machines initialize their parameter registers
	 in different orders, assume nothing.  Collect the set of all
	 parameter registers.  */
      CLEAR_HARD_REG_SET (parm_regs);
      nparm_regs = 0;
      for (p = CALL_INSN_FUNCTION_USAGE (insn); p ; p = XEXP (p, 1))
	if (GET_CODE (XEXP (p, 0)) == USE
	    && GET_CODE (XEXP (XEXP (p, 0), 0)) == REG)
	  {
	    int regno = REGNO (XEXP (XEXP (p, 0), 0));
	    if (regno >= FIRST_PSEUDO_REGISTER)
	      abort ();
	    SET_HARD_REG_BIT (parm_regs, regno);
	    nparm_regs++;
	  }

      /* Search backward for the first set of a register in this set.  */
      while (nparm_regs && BLOCK_HEAD (bb) != insn)
	{
	  insn = PREV_INSN (insn);
	  p = single_set (insn);
	  if (p && GET_CODE (SET_DEST (p)) == REG
	      && REGNO (SET_DEST (p)) < FIRST_PSEUDO_REGISTER
	      && TEST_HARD_REG_BIT (parm_regs, REGNO (SET_DEST (p))))
	    {
	      CLEAR_HARD_REG_BIT (parm_regs, REGNO (SET_DEST (p)));
	      nparm_regs--;
	    }
	}
      
      /* If we found all the parameter loads, then we want to insert
	 before the first parameter load.

	 If we did not find all the parameter loads, then we might have
	 stopped on the head of the block, which could be a CODE_LABEL.
	 If we inserted before the CODE_LABEL, then we would be putting
	 the insn in the wrong basic block.  In that case, put the insn
	 after the CODE_LABEL.

	 ?!? Do we need to account for NOTE_INSN_BASIC_BLOCK here?  */
      if (GET_CODE (insn) != CODE_LABEL)
	{
	  new_insn = emit_insn_before (pat, insn);
	  if (BLOCK_HEAD (bb) == insn)
	    BLOCK_HEAD (bb) = new_insn;
	}
      else
	{
	  new_insn = emit_insn_after (pat, insn);
	}
    }
  else
    {
      new_insn = emit_insn_after (pat, insn);
      BLOCK_END (bb) = new_insn;
    }

  /* Keep block number table up to date.
     Note, PAT could be a multiple insn sequence, we have to make
     sure that each insn in the sequence is handled.  */
  if (GET_CODE (pat) == SEQUENCE)
    {
      int i;

      for (i = 0; i < XVECLEN (pat, 0); i++)
	{
	  rtx insn = XVECEXP (pat, 0, i);
	  set_block_num (insn, bb);
	  if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
	    add_label_notes (PATTERN (insn), new_insn);
	  note_stores (PATTERN (insn), record_set_info, insn);
	}
    }
  else
    {
      add_label_notes (SET_SRC (pat), new_insn);
      set_block_num (new_insn, bb);
      /* Keep register set table up to date.  */
      record_one_set (regno, new_insn);
    }

  gcse_create_count++;

  if (gcse_file)
    {
      fprintf (gcse_file, "PRE/HOIST: end of bb %d, insn %d, copying expression %d to reg %d\n",
	       bb, INSN_UID (new_insn), expr->bitmap_index, regno);
    }
}

/* Insert partially redundant expressions on edges in the CFG to make
   the expressions fully redundant.  */

static int
pre_edge_insert (edge_list, index_map)
     struct edge_list *edge_list;
     struct expr **index_map;
{
  int e, i, num_edges, set_size, did_insert = 0;
  sbitmap *inserted;

  /* Where PRE_INSERT_MAP is nonzero, we add the expression on that edge
     if it reaches any of the deleted expressions.  */

  set_size = pre_insert_map[0]->size;
  num_edges = NUM_EDGES (edge_list);
  inserted = sbitmap_vector_alloc (num_edges, n_exprs);
  sbitmap_vector_zero (inserted, num_edges);

  for (e = 0; e < num_edges; e++)
    {
      int indx;
      basic_block pred = INDEX_EDGE_PRED_BB (edge_list, e);
      int bb = pred->index;

      for (i = indx = 0; i < set_size; i++, indx += SBITMAP_ELT_BITS)
	{
	  SBITMAP_ELT_TYPE insert = pre_insert_map[e]->elms[i];
	  int j;

	  for (j = indx; insert && j < n_exprs; j++, insert >>= 1)
	    {
	      if ((insert & 1) != 0 && index_map[j]->reaching_reg != NULL_RTX)
		{
		  struct expr *expr = index_map[j];
		  struct occr *occr;

		  /* Now look at each deleted occurence of this expression.  */
		  for (occr = expr->antic_occr; occr != NULL; occr = occr->next)
		    {
		      if (! occr->deleted_p)
			continue;

		      /* Insert this expression on this edge if if it would
			 reach the deleted occurence in BB.  */
		      if (!TEST_BIT (inserted[e], j))
			{
			  rtx insn;
			  edge eg = INDEX_EDGE (edge_list, e);
			  /* We can't insert anything on an abnormal 
			     and critical edge, so we insert the
			     insn at the end of the previous block. There
			     are several alternatives detailed in 
			     Morgans book P277 (sec 10.5) for handling 
			     this situation.  This one is easiest for now.  */

			  if ((eg->flags & EDGE_ABNORMAL) == EDGE_ABNORMAL)
			    {
			      insert_insn_end_bb (index_map[j], bb, 0);
			    }
			  else
			    {
			      insn = process_insert_insn (index_map[j]);
			      insert_insn_on_edge (insn, eg);
			    }
			  if (gcse_file)
			    {
			      fprintf (gcse_file,
				       "PRE/HOIST: edge (%d,%d), copy expression %d\n",
				        bb,
					INDEX_EDGE_SUCC_BB (edge_list, e)->index, expr->bitmap_index);
			    }
			  SET_BIT (inserted[e], j);
			  did_insert = 1;
			  gcse_create_count++;
			}
		    }
		}
	    }
	}
    }

  /* Clean up.  */
  free (inserted);

  return did_insert;
}

/* Copy the result of INSN to REG.
   INDX is the expression number.  */

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
  int bb = BLOCK_NUM (insn);

  if (!set)
    abort ();
  new_insn = emit_insn_after (gen_rtx_SET (VOIDmode, reg, SET_DEST (set)),
			      insn);
  /* Keep block number table up to date.  */
  set_block_num (new_insn, bb);
  /* Keep register set table up to date.  */
  record_one_set (regno, new_insn);
  if (insn == BLOCK_END (bb))
    BLOCK_END (bb) = new_insn;

  gcse_create_count++;

  if (gcse_file)
    fprintf (gcse_file,
	     "PRE: bb %d, insn %d, copy expression %d in insn %d to reg %d\n",
	      BLOCK_NUM (insn), INSN_UID (new_insn), indx,
	      INSN_UID (insn), regno);
}

/* Copy available expressions that reach the redundant expression
   to `reaching_reg'.  */

static void
pre_insert_copies ()
{
  int i;

  /* For each available expression in the table, copy the result to
     `reaching_reg' if the expression reaches a deleted one.

     ??? The current algorithm is rather brute force.
     Need to do some profiling.  */

  for (i = 0; i < expr_hash_table_size; i++)
    {
      struct expr *expr;

      for (expr = expr_hash_table[i]; expr != NULL; expr = expr->next_same_hash)
	{
	  struct occr *occr;

	  /* If the basic block isn't reachable, PPOUT will be TRUE.
	     However, we don't want to insert a copy here because the
	     expression may not really be redundant.  So only insert
	     an insn if the expression was deleted.
	     This test also avoids further processing if the expression
	     wasn't deleted anywhere.  */
	  if (expr->reaching_reg == NULL)
	    continue;

	  for (occr = expr->antic_occr; occr != NULL; occr = occr->next)
	    {
	      struct occr *avail;

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
		  if (! pre_expr_reaches_here_p (BLOCK_NUM (avail->insn), expr,
						 BLOCK_NUM (occr->insn)))
		    continue;

		  /* Copy the result of avail to reaching_reg.  */
		  pre_insert_copy_insn (expr, insn);
		  avail->copied_p = 1;
		}
	    }
	}
    }
}

/* Delete redundant computations.
   Deletion is done by changing the insn to copy the `reaching_reg' of
   the expression into the result of the SET.  It is left to later passes
   (cprop, cse2, flow, combine, regmove) to propagate the copy or eliminate it.

   Returns non-zero if a change is made.  */

static int
pre_delete ()
{
  int i, bb, changed;

  /* Compute the expressions which are redundant and need to be replaced by
     copies from the reaching reg to the target reg.  */
  for (bb = 0; bb < n_basic_blocks; bb++)
    sbitmap_copy (temp_bitmap[bb], pre_delete_map[bb]);

  changed = 0;
  for (i = 0; i < expr_hash_table_size; i++)
    {
      struct expr *expr;

      for (expr = expr_hash_table[i]; expr != NULL; expr = expr->next_same_hash)
	{
	  struct occr *occr;
	  int indx = expr->bitmap_index;

	  /* We only need to search antic_occr since we require
	     ANTLOC != 0.  */

	  for (occr = expr->antic_occr; occr != NULL; occr = occr->next)
	    {
	      rtx insn = occr->insn;
	      rtx set;
	      int bb = BLOCK_NUM (insn);

	      if (TEST_BIT (temp_bitmap[bb], indx))
		{
		  set = single_set (insn);
		  if (! set)
		    abort ();

		  /* Create a pseudo-reg to store the result of reaching
		     expressions into.  Get the mode for the new pseudo
		     from the mode of the original destination pseudo.  */
		  if (expr->reaching_reg == NULL)
		    expr->reaching_reg
		      = gen_reg_rtx (GET_MODE (SET_DEST (set)));

		  /* In theory this should never fail since we're creating
		     a reg->reg copy.

		     However, on the x86 some of the movXX patterns actually
		     contain clobbers of scratch regs.  This may cause the
		     insn created by validate_change to not match any pattern
		     and thus cause validate_change to fail.   */
		  if (validate_change (insn, &SET_SRC (set),
				       expr->reaching_reg, 0))
		    {
		      occr->deleted_p = 1;
		      SET_BIT (pre_redundant_insns, INSN_CUID (insn));
		      changed = 1;
		      gcse_subst_count++;
		    }

		  if (gcse_file)
		    {
		      fprintf (gcse_file,
			       "PRE: redundant insn %d (expression %d) in bb %d, reaching reg is %d\n",
			       INSN_UID (insn), indx, bb, REGNO (expr->reaching_reg));
		    }
		}
	    }
	}
    }

  return changed;
}

/* Perform GCSE optimizations using PRE.
   This is called by one_pre_gcse_pass after all the dataflow analysis
   has been done.

   This is based on the original Morel-Renvoise paper Fred Chow's thesis,
   and lazy code motion from Knoop, Ruthing and Steffen as described in
   Advanced Compiler Design and Implementation.

   ??? A new pseudo reg is created to hold the reaching expression.
   The nice thing about the classical approach is that it would try to
   use an existing reg.  If the register can't be adequately optimized
   [i.e. we introduce reload problems], one could add a pass here to
   propagate the new register through the block.

   ??? We don't handle single sets in PARALLELs because we're [currently]
   not able to copy the rest of the parallel when we insert copies to create
   full redundancies from partial redundancies.  However, there's no reason
   why we can't handle PARALLELs in the cases where there are no partial
   redundancies.  */

static int
pre_gcse ()
{
  int i, did_insert;
  int changed;
  struct expr **index_map;

  /* Compute a mapping from expression number (`bitmap_index') to
     hash table entry.  */

  index_map = (struct expr **) xcalloc (n_exprs, sizeof (struct expr *));
  for (i = 0; i < expr_hash_table_size; i++)
    {
      struct expr *expr;

      for (expr = expr_hash_table[i]; expr != NULL; expr = expr->next_same_hash)
	index_map[expr->bitmap_index] = expr;
    }

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
  free (pre_redundant_insns);

  return changed;
}

/* Top level routine to perform one PRE GCSE pass.

   Return non-zero if a change was made.  */

static int
one_pre_gcse_pass (pass)
     int pass;
{
  int changed = 0;

  gcse_subst_count = 0;
  gcse_create_count = 0;

  alloc_expr_hash_table (max_cuid);
  add_noreturn_fake_exit_edges ();
  compute_expr_hash_table ();
  if (gcse_file)
    dump_hash_table (gcse_file, "Expression", expr_hash_table,
		     expr_hash_table_size, n_exprs);
  if (n_exprs > 0)
    {
      alloc_pre_mem (n_basic_blocks, n_exprs);
      compute_pre_data ();
      changed |= pre_gcse ();
      free_edge_list (edge_list);
      free_pre_mem ();
    }
  remove_fake_edges ();
  free_expr_hash_table ();

  if (gcse_file)
    {
      fprintf (gcse_file, "\n");
      fprintf (gcse_file, "PRE GCSE of %s, pass %d: %d bytes needed, %d substs, %d insns created\n",
	       current_function_name, pass,
	       bytes_used, gcse_subst_count, gcse_create_count);
    }

  return changed;
}

/* If X contains any LABEL_REF's, add REG_LABEL notes for them to INSN.
   We have to add REG_LABEL notes, because the following loop optimization
   pass requires them.  */

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
      REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_LABEL, XEXP (x, 0),
					    REG_NOTES (insn));
      return;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
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
  int bb;

  sbitmap_vector_ones (transpout, n_basic_blocks);

  for (bb = 0; bb < n_basic_blocks; ++bb)
    {
      int i;

      /* Note that flow inserted a nop a the end of basic blocks that
	 end in call instructions for reasons other than abnormal
	 control flow.  */
      if (GET_CODE (BLOCK_END (bb)) != CALL_INSN)
	continue;

      for (i = 0; i < expr_hash_table_size; i++)
	{
	  struct expr *expr;
	  for (expr = expr_hash_table[i]; expr ; expr = expr->next_same_hash)
	    if (GET_CODE (expr->expr) == MEM)
	      {
		rtx addr = XEXP (expr->expr, 0);

		if (GET_CODE (addr) == SYMBOL_REF
		    && CONSTANT_POOL_ADDRESS_P (addr))
		  continue;
		
		/* ??? Optimally, we would use interprocedural alias
		   analysis to determine if this mem is actually killed
		   by this call.  */
		RESET_BIT (transpout[bb], expr->bitmap_index);
	      }
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
  int offset, regno;
  struct null_pointer_info* npi = (struct null_pointer_info *) data;

  offset = 0;
  while (GET_CODE (x) == SUBREG)
    x = SUBREG_REG (x);

  /* Ignore anything that is not a register or is a hard register.  */
  if (GET_CODE (x) != REG
      || REGNO (x) < npi->min_reg
      || REGNO (x) >= npi->max_reg)
    return;

  regno = REGNO (x) - npi->min_reg;

  RESET_BIT (npi->nonnull_local[npi->current_block], regno);
  SET_BIT (npi->nonnull_killed[npi->current_block], regno);
}

/* Do null-pointer check elimination for the registers indicated in
   NPI.  NONNULL_AVIN and NONNULL_AVOUT are pre-allocated sbitmaps;
   they are not our responsibility to free.  */

static void
delete_null_pointer_checks_1 (block_reg, nonnull_avin, nonnull_avout, npi)
     int *block_reg;
     sbitmap *nonnull_avin;
     sbitmap *nonnull_avout;
     struct null_pointer_info *npi;
{
  int bb;
  int current_block;
  sbitmap *nonnull_local = npi->nonnull_local;
  sbitmap *nonnull_killed = npi->nonnull_killed;
  
  /* Compute local properties, nonnull and killed.  A register will have
     the nonnull property if at the end of the current block its value is
     known to be nonnull.  The killed property indicates that somewhere in
     the block any information we had about the register is killed.

     Note that a register can have both properties in a single block.  That
     indicates that it's killed, then later in the block a new value is
     computed.  */
  sbitmap_vector_zero (nonnull_local, n_basic_blocks);
  sbitmap_vector_zero (nonnull_killed, n_basic_blocks);
  for (current_block = 0; current_block < n_basic_blocks; current_block++)
    {
      rtx insn, stop_insn;

      /* Set the current block for invalidate_nonnull_info.  */
      npi->current_block = current_block;

      /* Scan each insn in the basic block looking for memory references and
	 register sets.  */
      stop_insn = NEXT_INSN (BLOCK_END (current_block));
      for (insn = BLOCK_HEAD (current_block);
	   insn != stop_insn;
	   insn = NEXT_INSN (insn))
	{
	  rtx set;
	  rtx reg;

	  /* Ignore anything that is not a normal insn.  */
	  if (GET_RTX_CLASS (GET_CODE (insn)) != 'i')
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

	  /* See if we've got a useable memory load.  We handle it first
	     in case it uses its address register as a dest (which kills
	     the nonnull property).  */
	  if (GET_CODE (SET_SRC (set)) == MEM
	      && GET_CODE ((reg = XEXP (SET_SRC (set), 0))) == REG
	      && REGNO (reg) >= npi->min_reg
	      && REGNO (reg) < npi->max_reg)
	    SET_BIT (nonnull_local[current_block],
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
	    SET_BIT (nonnull_local[current_block],
		     REGNO (reg) - npi->min_reg);
	}
    }

  /* Now compute global properties based on the local properties.   This
     is a classic global availablity algorithm.  */
  compute_available (nonnull_local, nonnull_killed,
		     nonnull_avout, nonnull_avin);

  /* Now look at each bb and see if it ends with a compare of a value
     against zero.  */
  for (bb = 0; bb < n_basic_blocks; bb++)
    {
      rtx last_insn = BLOCK_END (bb);
      rtx condition, earliest;
      int compare_and_branch;

      /* Since MIN_REG is always at least FIRST_PSEUDO_REGISTER, and
	 since BLOCK_REG[BB] is zero if this block did not end with a
	 comparison against zero, this condition works.  */
      if (block_reg[bb] < npi->min_reg
	  || block_reg[bb] >= npi->max_reg)
	continue;

      /* LAST_INSN is a conditional jump.  Get its condition.  */
      condition = get_condition (last_insn, &earliest);

      /* If we can't determine the condition then skip.  */
      if (! condition)
	continue;

      /* Is the register known to have a nonzero value?  */
      if (!TEST_BIT (nonnull_avout[bb], block_reg[bb] - npi->min_reg))
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

	  new_jump = emit_jump_insn_before (gen_jump (JUMP_LABEL (last_insn)),
					    last_insn);
	  JUMP_LABEL (new_jump) = JUMP_LABEL (last_insn);
	  LABEL_NUSES (JUMP_LABEL (new_jump))++;
	  emit_barrier_after (new_jump);
	}
      delete_insn (last_insn);
      if (compare_and_branch == 2)
	delete_insn (earliest);

      /* Don't check this block again.  (Note that BLOCK_END is
	 invalid here; we deleted the last instruction in the 
	 block.)  */
      block_reg[bb] = 0;
    }
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

void
delete_null_pointer_checks (f)
     rtx f;
{
  sbitmap *nonnull_avin, *nonnull_avout;
  int *block_reg;
  int bb;
  int reg;
  int regs_per_pass;
  int max_reg;
  struct null_pointer_info npi;

  /* First break the program into basic blocks.  */
  find_basic_blocks (f, max_reg_num (), NULL, 1);

  /* If we have only a single block, then there's nothing to do.  */
  if (n_basic_blocks <= 1)
    {
      /* Free storage allocated by find_basic_blocks.  */
      free_basic_block_vars (0);
      return;
    }

  /* Trying to perform global optimizations on flow graphs which have
     a high connectivity will take a long time and is unlikely to be
     particularly useful.

     In normal circumstances a cfg should have about twice has many edges
     as blocks.  But we do not want to punish small functions which have
     a couple switch statements.  So we require a relatively large number
     of basic blocks and the ratio of edges to blocks to be high.  */
  if (n_basic_blocks > 1000 && n_edges / n_basic_blocks >= 20)
    {
      /* Free storage allocated by find_basic_blocks.  */
      free_basic_block_vars (0);
      return;
    }

  /* We need four bitmaps, each with a bit for each register in each
     basic block.  */
  max_reg = max_reg_num ();
  regs_per_pass = get_bitmap_width (4, n_basic_blocks, max_reg);

  /* Allocate bitmaps to hold local and global properties.  */
  npi.nonnull_local = sbitmap_vector_alloc (n_basic_blocks, regs_per_pass);
  npi.nonnull_killed = sbitmap_vector_alloc (n_basic_blocks, regs_per_pass);
  nonnull_avin = sbitmap_vector_alloc (n_basic_blocks, regs_per_pass);
  nonnull_avout = sbitmap_vector_alloc (n_basic_blocks, regs_per_pass);

  /* Go through the basic blocks, seeing whether or not each block
     ends with a conditional branch whose condition is a comparison
     against zero.  Record the register compared in BLOCK_REG.  */
  block_reg = (int *) xcalloc (n_basic_blocks, sizeof (int));
  for (bb = 0; bb < n_basic_blocks; bb++)
    {
      rtx last_insn = BLOCK_END (bb);
      rtx condition, earliest, reg;

      /* We only want conditional branches.  */
      if (GET_CODE (last_insn) != JUMP_INSN
	  || !condjump_p (last_insn)
	  || simplejump_p (last_insn))
	continue;

      /* LAST_INSN is a conditional jump.  Get its condition.  */
      condition = get_condition (last_insn, &earliest);

      /* If we were unable to get the condition, or it is not a equality
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

      block_reg[bb] = REGNO (reg);
    }

  /* Go through the algorithm for each block of registers.  */
  for (reg = FIRST_PSEUDO_REGISTER; reg < max_reg; reg += regs_per_pass)
    {
      npi.min_reg = reg;
      npi.max_reg = MIN (reg + regs_per_pass, max_reg);
      delete_null_pointer_checks_1 (block_reg, nonnull_avin,
				    nonnull_avout, &npi);
    }

  /* Free storage allocated by find_basic_blocks.  */
  free_basic_block_vars (0);

  /* Free the table of registers compared at the end of every block.  */
  free (block_reg);

  /* Free bitmaps.  */
  free (npi.nonnull_local);
  free (npi.nonnull_killed);
  free (nonnull_avin);
  free (nonnull_avout);
}

/* Code Hoisting variables and subroutines.  */

/* Very busy expressions.  */
static sbitmap *hoist_vbein;
static sbitmap *hoist_vbeout;

/* Hoistable expressions.  */
static sbitmap *hoist_exprs;

/* Dominator bitmaps.  */
static sbitmap *dominators;

/* ??? We could compute post dominators and run this algorithm in
   reverse to to perform tail merging, doing so would probably be
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

  dominators = sbitmap_vector_alloc (n_blocks, n_blocks);
}

/* Free vars used for code hoisting analysis.  */

static void
free_code_hoist_mem ()
{
  free (antloc);
  free (transp);
  free (comp);

  free (hoist_vbein);
  free (hoist_vbeout);
  free (hoist_exprs);
  free (transpout);

  free (dominators);
}

/* Compute the very busy expressions at entry/exit from each block.

   An expression is very busy if all paths from a given point
   compute the expression.  */

static void
compute_code_hoist_vbeinout ()
{
  int bb, changed, passes;

  sbitmap_vector_zero (hoist_vbeout, n_basic_blocks);
  sbitmap_vector_zero (hoist_vbein, n_basic_blocks);

  passes = 0;
  changed = 1;
  while (changed)
    {
      changed = 0;
      /* We scan the blocks in the reverse order to speed up
	 the convergence.  */
      for (bb = n_basic_blocks - 1; bb >= 0; bb--)
	{
	  changed |= sbitmap_a_or_b_and_c (hoist_vbein[bb], antloc[bb],
					   hoist_vbeout[bb], transp[bb]);
	  if (bb != n_basic_blocks - 1)
	    sbitmap_intersection_of_succs (hoist_vbeout[bb], hoist_vbein, bb);
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
  compute_local_properties (transp, comp, antloc, 0);
  compute_transpout ();
  compute_code_hoist_vbeinout ();
  compute_flow_dominators (dominators, NULL);
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
     int expr_bb;
     int expr_index;
     int bb;
     char *visited;
{
  edge pred;
  int visited_allocated_locally = 0;
  

  if (visited == NULL)
    {
       visited_allocated_locally = 1;
       visited = xcalloc (n_basic_blocks, 1);
    }

  visited[expr_bb] = 1;
  for (pred = BASIC_BLOCK (bb)->pred; pred != NULL; pred = pred->pred_next)
    {
      int pred_bb = pred->src->index;

      if (pred->src == ENTRY_BLOCK_PTR)
	break;
      else if (visited[pred_bb])
	continue;
      /* Does this predecessor generate this expression?  */
      else if (TEST_BIT (comp[pred_bb], expr_index))
	break;
      else if (! TEST_BIT (transp[pred_bb], expr_index))
	break;
      /* Not killed.  */
      else
	{
	  visited[pred_bb] = 1;
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
  int bb, dominated, i;
  struct expr **index_map;

  sbitmap_vector_zero (hoist_exprs, n_basic_blocks);

  /* Compute a mapping from expression number (`bitmap_index') to
     hash table entry.  */

  index_map = (struct expr **) xcalloc (n_exprs, sizeof (struct expr *));
  for (i = 0; i < expr_hash_table_size; i++)
    {
      struct expr *expr;

      for (expr = expr_hash_table[i]; expr != NULL; expr = expr->next_same_hash)
	index_map[expr->bitmap_index] = expr;
    }

  /* Walk over each basic block looking for potentially hoistable
     expressions, nothing gets hoisted from the entry block.  */
  for (bb = 0; bb < n_basic_blocks; bb++)
    {
      int found = 0;
      int insn_inserted_p;

      /* Examine each expression that is very busy at the exit of this
	 block.  These are the potentially hoistable expressions.  */
      for (i = 0; i < hoist_vbeout[bb]->n_bits; i++)
	{
	  int hoistable = 0;
	  if (TEST_BIT (hoist_vbeout[bb], i)
	      && TEST_BIT (transpout[bb], i))
	    {
	      /* We've found a potentially hoistable expression, now
		 we look at every block BB dominates to see if it
		 computes the expression.  */
	      for (dominated = 0; dominated < n_basic_blocks; dominated++)
		{
		  /* Ignore self dominance.  */
		  if (bb == dominated
		      || ! TEST_BIT (dominators[dominated], bb))
		    continue;

		  /* We've found a dominated block, now see if it computes
		     the busy expression and whether or not moving that
		     expression to the "beginning" of that block is safe.  */
		  if (!TEST_BIT (antloc[dominated], i))
		    continue;

		  /* Note if the expression would reach the dominated block
		     unimpared if it was placed at the end of BB. 

		     Keep track of how many times this expression is hoistable
		     from a dominated block into BB.  */
		  if (hoist_expr_reaches_here_p (bb, i, dominated, NULL))
		    hoistable++;
		}

	      /* If we found more than one hoistable occurence of this
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
		  SET_BIT (hoist_exprs[bb], i);
		  found = 1;
		}
	    }
	}
		
      /* If we found nothing to hoist, then quit now.  */
      if (! found)
	continue;

      /* Loop over all the hoistable expressions.  */
      for (i = 0; i < hoist_exprs[bb]->n_bits; i++)
	{
	  /* We want to insert the expression into BB only once, so
	     note when we've inserted it.  */
	  insn_inserted_p = 0;

	  /* These tests should be the same as the tests above.  */
	  if (TEST_BIT (hoist_vbeout[bb], i))
	    {
	      /* We've found a potentially hoistable expression, now
		 we look at every block BB dominates to see if it
		 computes the expression.  */
	      for (dominated = 0; dominated < n_basic_blocks; dominated++)
		{
		  /* Ignore self dominance.  */
		  if (bb == dominated
		      || ! TEST_BIT (dominators[dominated], bb))
		    continue;

		  /* We've found a dominated block, now see if it computes
		     the busy expression and whether or not moving that
		     expression to the "beginning" of that block is safe.  */
		  if (!TEST_BIT (antloc[dominated], i))
		    continue;

		  /* The expression is computed in the dominated block and
		     it would be safe to compute it at the start of the
		     dominated block.  Now we have to determine if the
		     expresion would reach the dominated block if it was
		     placed at the end of BB.  */
		  if (hoist_expr_reaches_here_p (bb, i, dominated, NULL))
		    {
		      struct expr *expr = index_map[i];
		      struct occr *occr = expr->antic_occr;
		      rtx insn;
		      rtx set;

		  
		      /* Find the right occurence of this expression.  */
		      while (BLOCK_NUM (occr->insn) != dominated && occr)
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

		      /* In theory this should never fail since we're creating
			 a reg->reg copy.

			 However, on the x86 some of the movXX patterns actually
			 contain clobbers of scratch regs.  This may cause the
			 insn created by validate_change to not match any
			 pattern and thus cause validate_change to fail.   */
		      if (validate_change (insn, &SET_SRC (set),
					   expr->reaching_reg, 0))
			{
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
	}
    }
    free (index_map);
}

/* Top level routine to perform one code hoisting (aka unification) pass

   Return non-zero if a change was made.  */

static int
one_code_hoisting_pass ()
{
  int changed = 0;

  alloc_expr_hash_table (max_cuid);
  compute_expr_hash_table ();
  if (gcse_file)
    dump_hash_table (gcse_file, "Code Hosting Expressions", expr_hash_table,
		     expr_hash_table_size, n_exprs);
  if (n_exprs > 0)
    {
      alloc_code_hoist_mem (n_basic_blocks, n_exprs);
      compute_code_hoist_data ();
      hoist_code ();
      free_code_hoist_mem ();
    }
  free_expr_hash_table ();

  return changed;
}
