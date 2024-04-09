/* Calculate branch probabilities, and basic block execution counts.
   Copyright (C) 1990-2024 Free Software Foundation, Inc.
   Contributed by James E. Wilson, UC Berkeley/Cygnus Support;
   based on some ideas from Dain Samples of UC Berkeley.
   Further mangling by Bob Manson, Cygnus Support.
   Converted to use trees by Dale Johannesen, Apple Computer.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* Generate basic block profile instrumentation and auxiliary files.
   Tree-based version.  See profile.cc for overview.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "memmodel.h"
#include "backend.h"
#include "target.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "tree-pass.h"
#include "ssa.h"
#include "cgraph.h"
#include "coverage.h"
#include "diagnostic-core.h"
#include "fold-const.h"
#include "varasm.h"
#include "tree-nested.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "tree-cfg.h"
#include "tree-into-ssa.h"
#include "value-prof.h"
#include "profile.h"
#include "tree-cfgcleanup.h"
#include "stringpool.h"
#include "attribs.h"
#include "tree-pretty-print.h"
#include "langhooks.h"
#include "stor-layout.h"
#include "xregex.h"
#include "alloc-pool.h"
#include "symbol-summary.h"
#include "symtab-thunks.h"
#include "cfganal.h"

static GTY(()) tree gcov_type_node;
static GTY(()) tree tree_interval_profiler_fn;
static GTY(()) tree tree_pow2_profiler_fn;
static GTY(()) tree tree_topn_values_profiler_fn;
static GTY(()) tree tree_indirect_call_profiler_fn;
static GTY(()) tree tree_average_profiler_fn;
static GTY(()) tree tree_ior_profiler_fn;
static GTY(()) tree tree_time_profiler_counter;


static GTY(()) tree ic_tuple_var;
static GTY(()) tree ic_tuple_counters_field;
static GTY(()) tree ic_tuple_callee_field;

/* Types of counter update methods.

   By default, the counter updates are done for a single threaded system
   (COUNTER_UPDATE_SINGLE_THREAD).

   If the user selected atomic profile counter updates
   (-fprofile-update=atomic), then the counter updates will be done atomically
   on a best-effort basis.  One of three methods to do the counter updates is
   selected according to the target capabilities.

   Ideally, the counter updates are done through atomic operations in hardware
   (COUNTER_UPDATE_ATOMIC_BUILTIN).

   If the target supports only 32-bit atomic increments and gcov_type_node is a
   64-bit integer type, then for the profile edge counters the increment is
   performed through two separate 32-bit atomic increments
   (COUNTER_UPDATE_ATOMIC_SPLIT or COUNTER_UPDATE_ATOMIC_PARTIAL).  If the
   target supports libatomic (targetm.have_libatomic), then other counter
   updates are carried out by libatomic calls (COUNTER_UPDATE_ATOMIC_SPLIT).
   If the target does not support libatomic, then the other counter updates are
   not done atomically (COUNTER_UPDATE_ATOMIC_PARTIAL) and a warning is
   issued.

   If the target does not support atomic operations in hardware, however,  it
   supports libatomic, then all updates are carried out by libatomic calls
   (COUNTER_UPDATE_ATOMIC_BUILTIN).  */
enum counter_update_method {
  COUNTER_UPDATE_SINGLE_THREAD,
  COUNTER_UPDATE_ATOMIC_BUILTIN,
  COUNTER_UPDATE_ATOMIC_SPLIT,
  COUNTER_UPDATE_ATOMIC_PARTIAL
};

static counter_update_method counter_update = COUNTER_UPDATE_SINGLE_THREAD;

/* These functions support measuring modified conditition/decision coverage
   (MC/DC).  MC/DC requires all of the below during testing:

   - Each entry and exit point is invoked
   - Each decision takes every possible outcome
   - Each condition in a decision takes every possible outcome
   - Each condition in a decision is shown to independently affect the outcome
     of the decision

   Independence of a condition is shown by recording it being evaluated to a
   value (true/false) and not being made irrelevant ("masked") by a later term.
   This feature adds some instrumentation code, a few bitwise operators, that
   records the branches taken in conditions and applies a filter for the
   masking effect.  Masking is essentially short-circuiting in reverse: a
   condition does not contribute to the outcome if it would short circuit the
   (sub) expression if it was evaluated right-to-left, (_ && false) and (_ ||
   true).

   The program is essentially rewritten this way:

   - if (a || b) { fn () }
   + if (a) { _t |= 0x1; goto _then; }
   + else   { _f |= 0x1;
   +	if (b) { _t |= 0x2; _mask |= 0x1; goto _then; }
   +	else   { _f |= 0x2; goto _else; }
   + _then:
   + _gcov_t |= (_t & _mask);
   + _gcov_f |= (_f & _mask);
   + fn (); goto _end;
   + _else:
   + _gcov_t |= (_t & _mask);
   + _gcov_f |= (_f & _mask);
   + fn ();
   + _end:

   It is assumed the front end will provide discrimnators so that conditional
   basic blocks (basic block with a conditional jump and outgoing true/false
   edges) that belong to the same Boolean expression have the same
   discriminator.  Masking is determined by analyzing these expressions as a
   reduced order binary decision diagram.  */
namespace
{
/* Some context and reused instances between function calls.  Large embedded
   buffers are used to up-front request enough memory for most programs and
   merge them into a single allocation at the cost of using more memory in the
   average case.  Some numbers from linux v5.13 which is assumed to be a
   reasonably diverse code base: 75% of the functions in linux have less than
   16 nodes in the CFG and approx 2.5% have more than 64 nodes.  The functions
   that go beyond a few dozen nodes tend to be very large (>100) and so 64
   seems like a good balance.

   This is really just a performance balance of the cost of allocation and
   wasted memory.  */
struct conds_ctx
{
    /* This is both a reusable shared allocation which is also used to return
       single expressions, which means it for most code should only hold a
       couple of elements.  */
    auto_vec<basic_block, 64> blocks;

    /* Index for the topological order indexed by basic_block->index to an
       ordering so that expression (a || b && c) => top_index[a] < top_index[b]
       < top_index[c].  */
    auto_vec<int, 256> top_index;

    /* Pre-allocate bitmaps and vectors for per-function book keeping.  This is
       pure instance reuse and the bitmaps carry no data between function
       calls.  */
    auto_vec<basic_block, 64> B1;
    auto_vec<basic_block, 64> B2;
    auto_sbitmap G1;
    auto_sbitmap G2;
    auto_sbitmap G3;

    explicit conds_ctx (unsigned size) noexcept (true) : G1 (size), G2 (size),
    G3 (size)
    {
    }
};

/* Only instrument terms with fewer than number of bits in a (wide) gcov
   integer, which is probably 64.  The algorithm itself does not impose this
   limitation, but it makes for a simpler implementation.

   * Allocating the output data structure (coverage_counter_alloc ()) can
     assume pairs of gcov_type_unsigned and not use a separate length field.
   * A pair gcov_type_unsigned can be used as accumulators.
   * Updating accumulators is can use the bitwise operations |=, &= and not
     custom operators that work for arbitrary-sized bit-sets.

   Most real-world code should be unaffected by this, but it is possible
   (especially for generated code) to exceed this limit.  */
#define CONDITIONS_MAX_TERMS (TYPE_PRECISION (gcov_type_node))
#define EDGE_CONDITION (EDGE_TRUE_VALUE | EDGE_FALSE_VALUE)

/* Compare two basic blocks by their order in the expression i.e. for (a || b)
   then topological_cmp (a, b, ...) < 0.  The result is undefined if LHS, RHS
   belong to different expressions.  The TOP_INDEX argument should be the
   top_index vector from ctx.  */
int
topological_cmp (const void *lhs, const void *rhs, void *top_index)
{
    const_basic_block l = *(const basic_block*) lhs;
    const_basic_block r = *(const basic_block*) rhs;
    const vec<int>* im = (const vec<int>*) top_index;
    return (*im)[l->index] - (*im)[r->index];
}

/* Find the index of NEEDLE in BLOCKS; return -1 if not found.  This has two
   uses, sometimes for the index and sometimes for set member checks.  Sets are
   typically very small (number of conditions, >8 is uncommon) so linear search
   should be very fast.  */
int
index_of (const basic_block needle, array_slice<basic_block> blocks)
{
    for (size_t i = 0; i < blocks.size (); i++)
	if (blocks[i] == needle)
	    return int (i);
    return -1;
}

/* Special cases of the single_*_p and single_*_edge functions in basic-block.h
   that don't consider exception handling or other complex edges.  This helps
   create a view of the CFG with only normal edges - if a basic block has both
   an outgoing fallthrough and exceptional edge, it should be considered a
   single-successor.  */
bool
single_p (const vec<edge, va_gc> *edges)
{
    int n = EDGE_COUNT (edges);
    if (n == 0)
	return false;

    for (edge e : edges)
	if (e->flags & EDGE_COMPLEX)
	    n -= 1;

    return n == 1;
}

/* Get the single, non-complex edge.  Behavior is undefined edges have more
   than 1 non-complex edges.  */
edge
single_edge (const vec<edge, va_gc> *edges)
{
    gcc_checking_assert (single_p (edges));
    for (edge e : edges)
    {
	if (e->flags & EDGE_COMPLEX)
	    continue;
	return e;
    }
    return NULL;
}

/* Sometimes, for example with function calls, goto labels, and C++
   destructors, the CFG gets extra nodes that are essentially single-entry
   single-exit in the middle of boolean expressions.  For example:

   x || can_throw (y)

         A
        /|
       / |
      B  |
      |  |
      C  |
     / \ |
    /   \|
   F     T

   Without the extra node inserted by the function + exception it becomes a
   proper 2-term graph, not 2 single-term graphs.

       A
      /|
     C |
    / \|
   F   T

   This function finds the source edge of these paths.  This is often the
   identity function.  */
edge
contract_edge_up (edge e)
{
    while (true)
    {
	basic_block src = e->src;
	if (!single_p (src->preds))
	    return e;
	if (!single_p (src->succs))
	    return e;
	e = single_edge (src->preds);
    }
}

/* A simple struct for storing/returning outcome block pairs.  Either both
   blocks are set or both are NULL.  */
struct outcomes
{
    basic_block t = NULL;
    basic_block f = NULL;

    operator bool () const noexcept (true)
    {
	return t && f;
    }
};

/* Get the true/false successors of a basic block.  If b is not a conditional
   block both edges are NULL.  */
outcomes
conditional_succs (const basic_block b)
{
    outcomes c;
    for (edge e : b->succs)
    {
	if (e->flags & EDGE_TRUE_VALUE)
	    c.t = e->dest;
	if (e->flags & EDGE_FALSE_VALUE)
	    c.f = e->dest;
    }

    gcc_assert ((c.t && c.f) || (!c.t && !c.f));
    return c;
}

/* Get the index or offset of a conditional flag, 0 for true and 1 for false.
   These indices carry no semantics but must be consistent as they are used to
   index into data structures in code generation and gcov.  */
unsigned
condition_index (unsigned flag)
{
    return (flag & EDGE_CONDITION) == EDGE_TRUE_VALUE ? 0 : 1;
}

/* Returns the condition identifier for the basic block if set, otherwise 0.
   This is only meaningful in GIMPLE and is used for condition coverage.

   There may be conditions created that did not get an uid, such as those
   implicitly created by destructors.  We could include them in the condition
   coverage for completeness (i.e. condition coverage implies (implicit) branch
   coverage), but they have no natural buckets and should all be single-term.
   For now these are ignored and given uid = 0, and branch coverage is left to
   -fprofile-arcs.

   Under optimization, COND_EXPRs may be folded, replaced with switches,
   min-max, etc., which leaves ghost identifiers in basic blocks that do not
   end with a conditional jump.  They are not really meaningful for condition
   coverage anymore, but since coverage is unreliable under optimization anyway
   this is not a big problem.

   The cond_uids map in FN cannot be expected to exist.  It will only be
   created if it is needed, and a function may have gconds even though there
   are none in source.  This can be seen in PR gcov-profile/114601, when
   -finstrument-functions-once is used and the function has no conditions.  */
unsigned
condition_uid (struct function *fn, basic_block b)
{
    gimple *stmt = gsi_stmt (gsi_last_bb (b));
    if (!safe_is_a <gcond*> (stmt) || !fn->cond_uids)
	return 0;

    unsigned *v = fn->cond_uids->get (as_a <gcond*> (stmt));
    return v ? *v : 0;
}

/* Compute the masking table.

   Masking and short circuiting are deeply connected - masking occurs when
   control flow reaches a state that is also reachable with short circuiting.
   In fact, masking corresponds to short circuiting for the reversed
   expression.  This means we can find the limits, the last term in preceeding
   subexpressions, by following the edges that short circuit to the same
   outcome.  The algorithm treats the CFG as a reduced order binary decision
   diagram (see Randall E. Bryant's Graph Based Algorithms for Boolean
   Function Manipulation (1987)).

   In the simplest case a || b:

   a
   |\
   | b
   |/ \
   T   F

   T has multiple incoming edges and is the outcome of a short circuit,
   with top = a, bot = b.  The top node (a) is masked when the edge (b, T) is
   taken.

   The names "top" and "bot" refer to a pair of nodes with a shared
   successor.  The top is always the node corresponding to the left-most
   operand of the two, and it holds that top < bot in a topological ordering.

   Now consider (a && b) || (c && d) and its masking table:

   a
   |\
   b \
   |\|
   | c
   | |\
   | d \
   |/ \|
   T   F

   a[0] = {}
   a[1] = {}
   b[0] = {a}
   b[1] = {}
   c[0] = {}
   c[1] = {}
   d[0] = {c}
   d[1] = {a,b}

   Note that 0 and 1 are indices and not boolean values - a[0] is the index in
   the masking vector when a takes the true edge.

   b[0] and d[0] are identical to the a || b example, and d[1] is the bot in
   the triangle [d, b] -> T.  b is the top node in the [d, b] relationship and
   last term in (a && b).  To find the other terms masked we use the fact that
   all paths in an expression go through either of the outcomes, found by
   collecting all non-complex edges that go out of the expression (the
   neighborhood).  In some cases the outgoing edge go through intermediate (or
   bypass) nodes, and we collect these paths too (see contract_edge_up).

   We find the terms by marking the outcomes (in this case c, T) and walk the
   predecessors starting at top (in this case b) and masking nodes when both
   successors are marked.

   The masking table is represented as two bitfields per term in the expression
   with the index corresponding to the term in the Boolean expression.
   a || b && c becomes the term vector [a b c] and the masking table [a[0]
   a[1] b[0] ...].  The kth bit of a masking vector is set if the kth term
   is masked by taking the edge.

   The out masks are in uint64_t (the practical maximum for gcov_type_node for
   any target) as it has to be big enough to store the target size gcov types
   independent of the host.  */
void
masking_vectors (conds_ctx& ctx, array_slice<basic_block> blocks,
		 array_slice<sbitmap> maps, array_slice<uint64_t> masks)
{
    gcc_assert (blocks.is_valid ());
    gcc_assert (!blocks.empty ());
    gcc_assert (maps.is_valid ());
    gcc_assert (masks.is_valid ());
    gcc_assert (sizeof (masks[0]) * BITS_PER_UNIT >= CONDITIONS_MAX_TERMS);

    if (bitmap_count_bits (maps[0]) == 1)
	return;

    sbitmap marks = ctx.G1;
    const sbitmap core = maps[0];
    const sbitmap allg = maps[1];
    vec<basic_block>& queue = ctx.B1;
    vec<basic_block>& body = ctx.B2;
    const vec<int>& top_index = ctx.top_index;

    /* Set up for the iteration - include the outcome nodes in the traversal.
       The algorithm compares pairs of nodes and is not really sensitive to
       traversal order, but need to maintain topological order because the
       index of masking nodes maps to the index in the accumulators.  We must
       also check the incoming-to-outcome pairs.  These edges may in turn be
       split (this happens with labels on top of then/else blocks) so we must
       follow any single-in single-out path.  The non-condition blocks do not
       have to be in order as they are non-condition blocks and will not be
       considered for the set-bit index.  */
    body.truncate (0);
    body.reserve (blocks.size () + 2);
    for (const basic_block b : blocks)
	if (bitmap_bit_p (core, b->index))
	    body.quick_push (b);

    for (basic_block b : blocks)
    {
	if (!bitmap_bit_p (core, b->index))
	    continue;

	for (edge e : b->succs)
	{
	    if (e->flags & EDGE_COMPLEX)
		continue;
	    if (bitmap_bit_p (allg, e->dest->index))
		continue;
	    body.safe_push (e->dest);

	    /* There may be multiple nodes between the condition edge and the
	       actual outcome, and we need to know when these paths join to
	       determine if there is short circuit/masking.  This is
	       effectively creating a virtual edge from the condition node to
	       the real outcome.  */
	    while (!(e->flags & EDGE_DFS_BACK) && single_p (e->dest->succs))
	    {
		e = single_edge (e->dest->succs);
		body.safe_push (e->dest);
	    }
	}
    }

    /* Find the masking.  The leftmost element cannot mask anything, so
       start at 1.  */
    for (size_t i = 1; i != body.length (); i++)
    {
	const basic_block b = body[i];
	for (edge e1 : b->preds)
	for (edge e2 : b->preds)
	{
	    if (e1 == e2)
		continue;
	    if ((e1->flags | e2->flags) & EDGE_COMPLEX)
		continue;

	    edge etop = contract_edge_up (e1);
	    edge ebot = contract_edge_up (e2);
	    gcc_assert (etop != ebot);

	    const basic_block top = etop->src;
	    const basic_block bot = ebot->src;
	    const unsigned cond = etop->flags & ebot->flags & EDGE_CONDITION;
	    if (!cond)
		continue;
	    if (top_index[top->index] > top_index[bot->index])
		continue;
	    if (!bitmap_bit_p (core, top->index))
		continue;
	    if (!bitmap_bit_p (core, bot->index))
		continue;

	    outcomes out = conditional_succs (top);
	    gcc_assert (out);
	    bitmap_clear (marks);
	    bitmap_set_bit (marks, out.t->index);
	    bitmap_set_bit (marks, out.f->index);
	    queue.truncate (0);
	    queue.safe_push (top);

	    // The edge bot -> outcome triggers the masking
	    const int m = 2*index_of (bot, body) + condition_index (cond);
	    gcc_assert (m >= 0);
	    while (!queue.is_empty ())
	    {
		basic_block q = queue.pop ();
		/* q may have been processed & completed by being added to the
		   queue multiple times, so check that there is still work to
		   do before continuing.  */
		if (bitmap_bit_p (marks, q->index))
		    continue;

		outcomes succs = conditional_succs (q);
		if (!bitmap_bit_p (marks, succs.t->index))
		    continue;
		if (!bitmap_bit_p (marks, succs.f->index))
		    continue;

		const int index = index_of (q, body);
		gcc_assert (index != -1);
		masks[m] |= uint64_t (1) << index;
		bitmap_set_bit (marks, q->index);

		for (edge e : q->preds)
		{
		    e = contract_edge_up (e);
		    if (e->flags & EDGE_DFS_BACK)
			continue;
		    if (bitmap_bit_p (marks, e->src->index))
			continue;
		    if (!bitmap_bit_p (core, e->src->index))
			continue;
		    queue.safe_push (e->src);
		}
	    }
	}
    }
}

/* Emit LHS = RHS on edges.  This is just a short hand that automates the
   building of the assign and immediately puts it on the edge, which becomes
   noisy.  */
tree
emit_assign (edge e, tree lhs, tree rhs)
{
    gassign *w = gimple_build_assign (lhs, rhs);
    gsi_insert_on_edge (e, w);
    return lhs;
}

/* Emit lhs = RHS on edges.  The lhs is created.  */
tree
emit_assign (edge e, tree rhs)
{
    return emit_assign (e, make_ssa_name (gcov_type_node), rhs);
}

/* Emit LHS = OP1 <OP> OP2 on edges.  */
tree
emit_bitwise_op (edge e, tree op1, tree_code op, tree op2 = NULL_TREE)
{
    tree lhs = make_ssa_name (gcov_type_node);
    gassign *w = gimple_build_assign (lhs, op, op1, op2);
    gsi_insert_on_edge (e, w);
    return lhs;
}

/* Visitor for make_top_index.  */
void
make_top_index_visit (basic_block b, vec<basic_block>& L, vec<int>& marks)
{
    if (marks[b->index])
	return;

    /* Follow the false edge first, if it exists, so that true paths are given
       the lower index in the ordering.  Any iteration order
       would yield a valid and useful topological ordering, but making sure the
       true branch has the lower index first makes reporting work better for
       expressions with ternaries.  Walk the false branch first because the
       array will be reversed to finalize the topological order.

       With the wrong ordering (a ? b : c) && d could become [a c b d], but the
       (expected) order is really [a b c d].  */

    const unsigned false_fwd = EDGE_DFS_BACK | EDGE_FALSE_VALUE;
    for (edge e : b->succs)
	if ((e->flags & false_fwd) == EDGE_FALSE_VALUE)
	    make_top_index_visit (e->dest, L, marks);

    for (edge e : b->succs)
	if (!(e->flags & false_fwd))
	    make_top_index_visit (e->dest, L, marks);

    marks[b->index] = 1;
    L.quick_push (b);
}

/* Find a topological sorting of the blocks in a function so that left operands
   are before right operands including subexpressions.  Sorting on block index
   does not guarantee this property and the syntactical order of terms is very
   important to the condition coverage.  The sorting algorithm is from Cormen
   et al (2001) but with back-edges ignored and thus there is no need for
   temporary marks (for cycle detection).  The L argument is a buffer/working
   memory, and the output will be written to TOP_INDEX.

   For the expression (a || (b && c) || d) the blocks should be [a b c d].  */
void
make_top_index (array_slice<basic_block> blocks, vec<basic_block>& L,
		vec<int>& top_index)
{
    L.truncate (0);
    L.reserve (blocks.size ());

    /* Use of the output map as a temporary for tracking visited status.  */
    top_index.truncate (0);
    top_index.safe_grow_cleared (blocks.size ());
    for (const basic_block b : blocks)
	make_top_index_visit (b, L, top_index);

    /* Insert canaries - if there are unreachable nodes (for example infinite
       loops) then the unreachable nodes should never be needed for comparison,
       and L.length () < max_index.  An index mapping should also never be
       recorded twice.  */
    for (unsigned i = 0; i != top_index.length (); i++)
	top_index[i] = -1;

    gcc_assert (blocks.size () == L.length ());
    L.reverse ();
    const unsigned nblocks = L.length ();
    for (unsigned i = 0; i != nblocks; i++)
    {
	gcc_assert (L[i]->index != -1);
	top_index[L[i]->index] = int (i);
    }
}

/* Find all nodes including non-conditions in a Boolean expression.  We need to
   know the paths through the expression so that the masking and
   instrumentation phases can limit searches and know what subgraphs must be
   threaded through, but not counted, such as the (b || c) in
   a && fn (b || c) && d.

   It is essentially the intersection of downwards paths from the expression
   nodes EXPR to the post-dominator and upwards from the post-dominator.
   Finding the dominator is slightly more involved than picking the first/last,
   particularly under optimization, because both incoming and outgoing paths
   may have multiple entries/exits.

   It is assumed GRAPH is an array_slice of the basic blocks of this function
   sorted by the basic block index.  */
vec<basic_block>&
paths_between (conds_ctx &ctx, array_slice<basic_block> graph,
	       const vec<basic_block>& expr)
{
    if (expr.length () == 1)
    {
	ctx.blocks.truncate (0);
	ctx.blocks.safe_push (expr[0]);
	return ctx.blocks;
    }

    basic_block dom;
    sbitmap up = ctx.G1;
    sbitmap down = ctx.G2;
    sbitmap paths = ctx.G3;
    vec<basic_block>& queue = ctx.B1;

    queue.truncate (0);
    bitmap_clear (down);
    dom = get_immediate_dominator (CDI_POST_DOMINATORS, expr[0]);
    for (basic_block b : expr)
	if (dom != b)
	    dom = nearest_common_dominator (CDI_POST_DOMINATORS, dom, b);
    queue.safe_splice (expr);
    while (!queue.is_empty ())
    {
	basic_block b = queue.pop ();
	if (!bitmap_set_bit (down, b->index))
	    continue;
	if (b == dom)
	    continue;
	for (edge e : b->succs)
	    if (!(e->flags & (EDGE_COMPLEX | EDGE_DFS_BACK)))
		queue.safe_push (e->dest);
    }

    queue.truncate (0);
    bitmap_clear (up);
    dom = expr[0];
    for (basic_block b : expr)
	if (dom != b)
	    dom = nearest_common_dominator (CDI_DOMINATORS, dom, b);
    queue.safe_splice (expr);
    while (!queue.is_empty ())
    {
	basic_block b = queue.pop ();
	if (!bitmap_set_bit (up, b->index))
	    continue;
	if (b == dom)
	    continue;
	for (edge e : b->preds)
	    if (!(e->flags & (EDGE_COMPLEX | EDGE_DFS_BACK)))
		queue.safe_push (e->src);
    }

    bitmap_and (paths, up, down);
    vec<basic_block>& blocks = ctx.blocks;
    blocks.truncate (0);
    blocks.reserve (graph.size ());
    sbitmap_iterator itr;
    unsigned index;
    EXECUTE_IF_SET_IN_BITMAP (paths, 0, index, itr)
	blocks.quick_push (graph[index]);
    return blocks;
}

}

/* Context object for the condition coverage.  This stores conds_ctx (the
   buffers reused when analyzing the cfg) and the output arrays.  This is
   designed to be heap allocated and aggressively preallocates large buffers to
   avoid having to reallocate for most programs.  */
struct condcov
{
    explicit condcov (unsigned nblocks) noexcept (true) : ctx (nblocks),
    m_maps (sbitmap_vector_alloc (2 * nblocks, nblocks))
    {
	bitmap_vector_clear (m_maps, 2 * nblocks);
    }
    auto_vec<size_t, 128> m_index;
    auto_vec<basic_block, 256> m_blocks;
    auto_vec<uint64_t, 512> m_masks;
    conds_ctx ctx;
    sbitmap *m_maps;
};

/* Get the length, that is the number of Boolean expression found.  cov_length
   is the one-past index for cov_{blocks,masks,maps}.  */
size_t
cov_length (const struct condcov* cov)
{
    if (cov->m_index.is_empty ())
	return 0;
    return cov->m_index.length () - 1;
}

/* The subgraph, exluding intermediates, for the nth Boolean expression.  */
array_slice<basic_block>
cov_blocks (struct condcov* cov, size_t n)
{
    if (n >= cov->m_index.length ())
	return array_slice<basic_block>::invalid ();

    basic_block *begin = cov->m_blocks.begin () + cov->m_index[n];
    basic_block *end = cov->m_blocks.begin () + cov->m_index[n + 1];
    return array_slice<basic_block> (begin, end - begin);
}

/* The masks for the nth Boolean expression.  */
array_slice<uint64_t>
cov_masks (struct condcov* cov, size_t n)
{
    if (n >= cov->m_index.length ())
	return array_slice<uint64_t>::invalid ();

    uint64_t *begin = cov->m_masks.begin () + 2*cov->m_index[n];
    uint64_t *end = cov->m_masks.begin () + 2*cov->m_index[n + 1];
    return array_slice<uint64_t> (begin, end - begin);
}

/* The maps for the nth Boolean expression.  */
array_slice<sbitmap>
cov_maps (struct condcov* cov, size_t n)
{
    if (n >= cov->m_index.length ())
	return array_slice<sbitmap>::invalid ();

    sbitmap *begin = cov->m_maps + 2*n;
    sbitmap *end = begin + 2;
    return array_slice<sbitmap> (begin, end - begin);
}

/* Deleter for condcov.  */
void
cov_free (struct condcov* cov)
{
    sbitmap_vector_free (cov->m_maps);
    delete cov;
}

/* Condition coverage (MC/DC)

   Whalen, Heimdahl, De Silva in "Efficient Test Coverage Measurement for
   MC/DC" describe an algorithm for modified condition/decision coverage based
   on AST analysis.  This algorithm does analyzes the control flow graph
   (interpreted as a binary decision diagram) to determine the masking vectors.
   The individual phases are described in more detail closer to the
   implementation.

   The coverage only considers the positions, not the symbols, in a
   conditional, e.g. !A || (!B && A) is a 3-term conditional even though A
   appears twice.  Subexpressions have no effect on term ordering:
   (a && (b || (c && d)) || e) comes out as [a b c d e].  Functions whose
   arguments are Boolean expressions are treated as separate expressions, that
   is, a && fn (b || c) && d is treated as [a _fn d] and [b c], not [a b c d].

   The output for gcov is a vector of pairs of unsigned integers, interpreted
   as bit-sets, where the bit index corresponds to the index of the condition
   in the expression.

   The returned condcov should be released by the caller with cov_free.  */
struct condcov*
find_conditions (struct function *fn)
{
    mark_dfs_back_edges (fn);
    const bool have_dom = dom_info_available_p (fn, CDI_DOMINATORS);
    const bool have_post_dom = dom_info_available_p (fn, CDI_POST_DOMINATORS);
    if (!have_dom)
	calculate_dominance_info (CDI_DOMINATORS);
    if (!have_post_dom)
	calculate_dominance_info (CDI_POST_DOMINATORS);

    const unsigned nblocks = n_basic_blocks_for_fn (fn);
    basic_block *fnblocksp = basic_block_info_for_fn (fn)->address ();
    condcov *cov = new condcov (nblocks);
    conds_ctx& ctx = cov->ctx;
    array_slice<basic_block> fnblocks (fnblocksp, nblocks);
    make_top_index (fnblocks, ctx.B1, ctx.top_index);

    /* Bin the Boolean expressions so that exprs[id] -> [x1, x2, ...].  */
    hash_map<int_hash<unsigned, 0>, vec<basic_block>> exprs;
    for (basic_block b : fnblocks)
    {
	const unsigned uid = condition_uid (fn, b);
	if (uid == 0)
	    continue;
	exprs.get_or_insert (uid).safe_push (b);
    }

    /* Visit all reachable nodes and collect conditions.  Topological order is
       important so the first node of a boolean expression is visited first
       (it will mark subsequent terms).  */
    cov->m_index.safe_push (0);
    for (auto expr : exprs)
    {
	vec<basic_block>& conds = expr.second;
	if (conds.length () > CONDITIONS_MAX_TERMS)
	{
	    location_t loc = gimple_location (gsi_stmt (gsi_last_bb (conds[0])));
	    warning_at (loc, OPT_Wcoverage_too_many_conditions,
			"Too many conditions (found %u); giving up coverage",
			conds.length ());
	    continue;
	}
	conds.sort (topological_cmp, &ctx.top_index);
	vec<basic_block>& subgraph = paths_between (ctx, fnblocks, conds);
	subgraph.sort (topological_cmp, &ctx.top_index);
	const unsigned index = cov->m_index.length () - 1;
	sbitmap condm = cov->m_maps[0 + 2*index];
	sbitmap subgm = cov->m_maps[1 + 2*index];
	for (basic_block b : conds)
	    bitmap_set_bit (condm, b->index);
	for (basic_block b : subgraph)
	    bitmap_set_bit (subgm, b->index);
	cov->m_blocks.safe_splice (subgraph);
	cov->m_index.safe_push (cov->m_blocks.length ());
    }

    if (!have_dom)
	free_dominance_info (fn, CDI_DOMINATORS);
    if (!have_post_dom)
	free_dominance_info (fn, CDI_POST_DOMINATORS);

    cov->m_masks.safe_grow_cleared (2 * cov->m_index.last ());
    const size_t length = cov_length (cov);
    for (size_t i = 0; i != length; i++)
	masking_vectors (ctx, cov_blocks (cov, i), cov_maps (cov, i),
			 cov_masks (cov, i));

    return cov;
}

namespace
{

/* Stores the incoming edge and previous counters (in SSA form) on that edge
   for the node e->deston that edge for the node e->dest.  The counters record
   the seen-true (0), seen-false (1), and current-mask (2).  They are stored in
   an array rather than proper members for access-by-index as the code paths
   tend to be identical for the different counters.  */
struct counters
{
    edge e;
    tree counter[3];
    tree& operator [] (size_t i) { return counter[i]; }
};

/* Find the counters for the incoming edge e, or NULL if the edge has not been
   recorded (could be for complex incoming edges).  */
counters*
find_counters (vec<counters>& candidates, edge e)
{
    for (counters& candidate : candidates)
	if (candidate.e == e)
	    return &candidate;
    return NULL;
}

/* Resolve the SSA for a specific counter KIND.  If it is not modified by any
   incoming edges, simply forward it, otherwise create a phi node of all the
   candidate counters and return it.  */
tree
resolve_counter (vec<counters>& cands, size_t kind)
{
    gcc_assert (!cands.is_empty ());
    gcc_assert (kind < 3);

    counters& fst = cands[0];

    if (!fst.e || fst.e->dest->preds->length () == 1)
    {
	gcc_assert (cands.length () == 1);
	return fst[kind];
    }

    tree zero0 = build_int_cst (gcov_type_node, 0);
    tree ssa = make_ssa_name (gcov_type_node);
    gphi *phi = create_phi_node (ssa, fst.e->dest);
    for (edge e : fst.e->dest->preds)
    {
	counters *prev = find_counters (cands, e);
	if (prev)
	    add_phi_arg (phi, (*prev)[kind], e, UNKNOWN_LOCATION);
	else
	{
	    tree zero = make_ssa_name (gcov_type_node);
	    gimple_stmt_iterator gsi = gsi_after_labels (e->src);
	    gassign *set = gimple_build_assign (zero, zero0);
	    gsi_insert_before (&gsi, set, GSI_NEW_STMT);
	    add_phi_arg (phi, zero, e, UNKNOWN_LOCATION);
	}
    }
    return ssa;
}

/* Resolve all the counters for a node.  Note that the edge is undefined, as
   the counters are intended to form the base to push to the successors, and
   because the is only meaningful for nodes with a single predecessor.  */
counters
resolve_counters (vec<counters>& cands)
{
    counters next;
    next[0] = resolve_counter (cands, 0);
    next[1] = resolve_counter (cands, 1);
    next[2] = resolve_counter (cands, 2);
    return next;
}

}

/* Add instrumentation to a decision subgraph.  EXPR should be the
   (topologically sorted) block of nodes returned by cov_blocks, MAPS the
   bitmaps returned by cov_maps, and MASKS the block of bitsets returned by
   cov_masks.  CONDNO should be the index of this condition in the function,
   i.e. the same argument given to cov_{masks,graphs}.  EXPR may contain nodes
   in-between the conditions, e.g.  when an operand contains a function call,
   or there is a setjmp and the cfg is filled with complex edges.

   Every node is annotated with three counters; the true, false, and mask
   value.  First, walk the graph and determine what if there are multiple
   possible values for either accumulator depending on the path taken, in which
   case a phi node is created and registered as the accumulator.  Then, those
   values are pushed as accumulators to the immediate successors.  For some
   very particular programs there may be multiple paths into the expression
   (e.g. when prior terms are determined by a surrounding conditional) in which
   case the default zero-counter is pushed, otherwise all predecessors will
   have been considered before the successor because of topologically ordered
   traversal.  Finally, expr is traversed again to look for edges to the
   outcomes, that is, edges with a destination outside of expr, and the local
   accumulators are flushed to the global gcov counters on these edges.  In
   some cases there are edge splits that cause 3+ edges to the two outcome
   nodes.

   If a complex edge is taken (e.g. on a longjmp) the accumulators are
   attempted poisoned so that there would be no change to the global counters,
   but this has proven unreliable in the presence of undefined behavior, see
   the setjmp003 test.

   It is important that the flushes happen on the basic condition outgoing
   edge, otherwise flushes could be lost to exception handling or other
   abnormal control flow.  */
size_t
instrument_decisions (array_slice<basic_block> expr, size_t condno,
		      array_slice<sbitmap> maps, array_slice<uint64_t> masks)
{
    tree zero = build_int_cst (gcov_type_node, 0);
    tree poison = build_int_cst (gcov_type_node, ~0ULL);
    const sbitmap core = maps[0];
    const sbitmap allg = maps[1];

    hash_map<basic_block, vec<counters>> table;
    counters zerocounter;
    zerocounter.e = NULL;
    zerocounter[0] = zero;
    zerocounter[1] = zero;
    zerocounter[2] = zero;

    unsigned xi = 0;
    bool increment = false;
    tree rhs = build_int_cst (gcov_type_node, 1ULL << xi);
    for (basic_block current : expr)
    {
	vec<counters>& candidates = table.get_or_insert (current);
	if (candidates.is_empty ())
	    candidates.safe_push (zerocounter);
	counters prev = resolve_counters (candidates);

	if (increment)
	{
	    xi += 1;
	    gcc_checking_assert (xi < sizeof (uint64_t) * BITS_PER_UNIT);
	    rhs = build_int_cst (gcov_type_node, 1ULL << xi);
	    increment = false;
	}

	for (edge e : current->succs)
	{
	    counters next = prev;
	    next.e = e;

	    if (bitmap_bit_p (core, e->src->index) && (e->flags & EDGE_CONDITION))
	    {
		const int k = condition_index (e->flags);
		next[k] = emit_bitwise_op (e, prev[k], BIT_IOR_EXPR, rhs);
		if (masks[2*xi + k])
		{
		    tree m = build_int_cst (gcov_type_node, masks[2*xi + k]);
		    next[2] = emit_bitwise_op (e, prev[2], BIT_IOR_EXPR, m);
		}
		increment = true;
	    }
	    else if (e->flags & EDGE_COMPLEX)
	    {
		/* A complex edge has been taken - wipe the accumulators and
		   poison the mask so that this path does not contribute to
		   coverage.  */
		next[0] = poison;
		next[1] = poison;
		next[2] = poison;
	    }
	    table.get_or_insert (e->dest).safe_push (next);
	}
    }

    /* Since this is also the return value, the number of conditions, make sure
       to include the increment of the last basic block.  */
    if (increment)
	xi += 1;

    gcc_assert (xi == bitmap_count_bits (core));

    const tree relaxed = build_int_cst (integer_type_node, MEMMODEL_RELAXED);
    const bool atomic = flag_profile_update == PROFILE_UPDATE_ATOMIC;
    const tree atomic_ior = builtin_decl_explicit
	(TYPE_PRECISION (gcov_type_node) > 32
	 ? BUILT_IN_ATOMIC_FETCH_OR_8
	 : BUILT_IN_ATOMIC_FETCH_OR_4);

    /* Flush to the gcov accumulators.  */
    for (const basic_block b : expr)
    {
	if (!bitmap_bit_p (core, b->index))
	    continue;

	for (edge e : b->succs)
	{
	    /* Flush the accumulators on leaving the Boolean function.  The
	       destination may be inside the function only when it returns to
	       the loop header, such as do { ... } while (x);  */
	    if (bitmap_bit_p (allg, e->dest->index)) {
		if (!(e->flags & EDGE_DFS_BACK))
		    continue;
		if (e->dest != expr[0])
		    continue;
	    }

	    vec<counters> *cands = table.get (e->dest);
	    gcc_assert (cands);
	    counters *prevp = find_counters (*cands, e);
	    gcc_assert (prevp);
	    counters prev = *prevp;

	    /* _true &= ~mask, _false &= ~mask  */
	    counters next;
	    next[2] = emit_bitwise_op (e, prev[2], BIT_NOT_EXPR);
	    next[0] = emit_bitwise_op (e, prev[0], BIT_AND_EXPR, next[2]);
	    next[1] = emit_bitwise_op (e, prev[1], BIT_AND_EXPR, next[2]);

	    /* _global_true |= _true, _global_false |= _false  */
	    for (size_t k = 0; k != 2; ++k)
	    {
		tree ref = tree_coverage_counter_ref (GCOV_COUNTER_CONDS,
						      2*condno + k);
		if (atomic)
		{
		    ref = unshare_expr (ref);
		    gcall *flush = gimple_build_call (atomic_ior, 3,
						      build_addr (ref),
						      next[k], relaxed);
		    gsi_insert_on_edge (e, flush);
		}
		else
		{
		    tree get = emit_assign (e, ref);
		    tree put = emit_bitwise_op (e, next[k], BIT_IOR_EXPR, get);
		    emit_assign (e, unshare_expr (ref), put);
		}
	    }
	}
    }

    return xi;
}

#undef CONDITIONS_MAX_TERMS
#undef EDGE_CONDITION

/* Do initialization work for the edge profiler.  */

/* Add code:
   __thread gcov*	__gcov_indirect_call.counters; // pointer to actual counter
   __thread void*	__gcov_indirect_call.callee; // actual callee address
   __thread int __gcov_function_counter; // time profiler function counter
*/
static void
init_ic_make_global_vars (void)
{
  tree gcov_type_ptr;

  gcov_type_ptr = build_pointer_type (get_gcov_type ());

  tree tuple_type = lang_hooks.types.make_type (RECORD_TYPE);

  /* callee */
  ic_tuple_callee_field = build_decl (BUILTINS_LOCATION, FIELD_DECL, NULL_TREE,
				      ptr_type_node);

  /* counters */
  ic_tuple_counters_field = build_decl (BUILTINS_LOCATION, FIELD_DECL,
					NULL_TREE, gcov_type_ptr);
  DECL_CHAIN (ic_tuple_counters_field) = ic_tuple_callee_field;

  finish_builtin_struct (tuple_type, "indirect_call_tuple",
			 ic_tuple_counters_field, NULL_TREE);

  ic_tuple_var
    = build_decl (UNKNOWN_LOCATION, VAR_DECL,
		  get_identifier ("__gcov_indirect_call"), tuple_type);
  TREE_PUBLIC (ic_tuple_var) = 1;
  DECL_ARTIFICIAL (ic_tuple_var) = 1;
  DECL_INITIAL (ic_tuple_var) = NULL;
  DECL_EXTERNAL (ic_tuple_var) = 1;
  if (targetm.have_tls)
    set_decl_tls_model (ic_tuple_var, decl_default_tls_model (ic_tuple_var));
}

/* Create the type and function decls for the interface with gcov.  */

void
gimple_init_gcov_profiler (void)
{
  tree interval_profiler_fn_type;
  tree pow2_profiler_fn_type;
  tree topn_values_profiler_fn_type;
  tree gcov_type_ptr;
  tree ic_profiler_fn_type;
  tree average_profiler_fn_type;
  const char *fn_name;

  if (!gcov_type_node)
    {
      const char *fn_suffix
	= flag_profile_update == PROFILE_UPDATE_ATOMIC ? "_atomic" : "";

      gcov_type_node = get_gcov_type ();
      gcov_type_ptr = build_pointer_type (gcov_type_node);

      /* void (*) (gcov_type *, gcov_type, int, unsigned)  */
      interval_profiler_fn_type
	      = build_function_type_list (void_type_node,
					  gcov_type_ptr, gcov_type_node,
					  integer_type_node,
					  unsigned_type_node, NULL_TREE);
      fn_name = concat ("__gcov_interval_profiler", fn_suffix, NULL);
      tree_interval_profiler_fn = build_fn_decl (fn_name,
						 interval_profiler_fn_type);
      free (CONST_CAST (char *, fn_name));
      TREE_NOTHROW (tree_interval_profiler_fn) = 1;
      DECL_ATTRIBUTES (tree_interval_profiler_fn)
	= tree_cons (get_identifier ("leaf"), NULL,
		     DECL_ATTRIBUTES (tree_interval_profiler_fn));

      /* void (*) (gcov_type *, gcov_type)  */
      pow2_profiler_fn_type
	      = build_function_type_list (void_type_node,
					  gcov_type_ptr, gcov_type_node,
					  NULL_TREE);
      fn_name = concat ("__gcov_pow2_profiler", fn_suffix, NULL);
      tree_pow2_profiler_fn = build_fn_decl (fn_name, pow2_profiler_fn_type);
      free (CONST_CAST (char *, fn_name));
      TREE_NOTHROW (tree_pow2_profiler_fn) = 1;
      DECL_ATTRIBUTES (tree_pow2_profiler_fn)
	= tree_cons (get_identifier ("leaf"), NULL,
		     DECL_ATTRIBUTES (tree_pow2_profiler_fn));

      /* void (*) (gcov_type *, gcov_type)  */
      topn_values_profiler_fn_type
	      = build_function_type_list (void_type_node,
					  gcov_type_ptr, gcov_type_node,
					  NULL_TREE);
      fn_name = concat ("__gcov_topn_values_profiler", fn_suffix, NULL);
      tree_topn_values_profiler_fn
	= build_fn_decl (fn_name, topn_values_profiler_fn_type);
      free (CONST_CAST (char *, fn_name));

      TREE_NOTHROW (tree_topn_values_profiler_fn) = 1;
      DECL_ATTRIBUTES (tree_topn_values_profiler_fn)
	= tree_cons (get_identifier ("leaf"), NULL,
		     DECL_ATTRIBUTES (tree_topn_values_profiler_fn));

      init_ic_make_global_vars ();

      /* void (*) (gcov_type, void *)  */
      ic_profiler_fn_type
	       = build_function_type_list (void_type_node,
					  gcov_type_node,
					  ptr_type_node,
					  NULL_TREE);
      fn_name = concat ("__gcov_indirect_call_profiler_v4", fn_suffix, NULL);
      tree_indirect_call_profiler_fn
	= build_fn_decl (fn_name, ic_profiler_fn_type);
      free (CONST_CAST (char *, fn_name));

      TREE_NOTHROW (tree_indirect_call_profiler_fn) = 1;
      DECL_ATTRIBUTES (tree_indirect_call_profiler_fn)
	= tree_cons (get_identifier ("leaf"), NULL,
		     DECL_ATTRIBUTES (tree_indirect_call_profiler_fn));

      tree_time_profiler_counter
	= build_decl (UNKNOWN_LOCATION, VAR_DECL,
		      get_identifier ("__gcov_time_profiler_counter"),
		      get_gcov_type ());
      TREE_PUBLIC (tree_time_profiler_counter) = 1;
      DECL_EXTERNAL (tree_time_profiler_counter) = 1;
      TREE_STATIC (tree_time_profiler_counter) = 1;
      DECL_ARTIFICIAL (tree_time_profiler_counter) = 1;
      DECL_INITIAL (tree_time_profiler_counter) = NULL;

      /* void (*) (gcov_type *, gcov_type)  */
      average_profiler_fn_type
	      = build_function_type_list (void_type_node,
					  gcov_type_ptr, gcov_type_node, NULL_TREE);
      fn_name = concat ("__gcov_average_profiler", fn_suffix, NULL);
      tree_average_profiler_fn = build_fn_decl (fn_name,
						average_profiler_fn_type);
      free (CONST_CAST (char *, fn_name));
      TREE_NOTHROW (tree_average_profiler_fn) = 1;
      DECL_ATTRIBUTES (tree_average_profiler_fn)
	= tree_cons (get_identifier ("leaf"), NULL,
		     DECL_ATTRIBUTES (tree_average_profiler_fn));
      fn_name = concat ("__gcov_ior_profiler", fn_suffix, NULL);
      tree_ior_profiler_fn = build_fn_decl (fn_name, average_profiler_fn_type);
      free (CONST_CAST (char *, fn_name));
      TREE_NOTHROW (tree_ior_profiler_fn) = 1;
      DECL_ATTRIBUTES (tree_ior_profiler_fn)
	= tree_cons (get_identifier ("leaf"), NULL,
		     DECL_ATTRIBUTES (tree_ior_profiler_fn));

      /* LTO streamer needs assembler names.  Because we create these decls
         late, we need to initialize them by hand.  */
      DECL_ASSEMBLER_NAME (tree_interval_profiler_fn);
      DECL_ASSEMBLER_NAME (tree_pow2_profiler_fn);
      DECL_ASSEMBLER_NAME (tree_topn_values_profiler_fn);
      DECL_ASSEMBLER_NAME (tree_indirect_call_profiler_fn);
      DECL_ASSEMBLER_NAME (tree_average_profiler_fn);
      DECL_ASSEMBLER_NAME (tree_ior_profiler_fn);
    }
}

/* If RESULT is not null, then output instructions as GIMPLE trees to assign
   the updated counter from CALL of FUNC to RESULT.  Insert the CALL and the
   optional assignment instructions to GSI.  Use NAME for temporary values.  */

static inline void
gen_assign_counter_update (gimple_stmt_iterator *gsi, gcall *call, tree func,
			   tree result, const char *name)
{
  if (result)
    {
      tree result_type = TREE_TYPE (TREE_TYPE (func));
      tree tmp1 = make_temp_ssa_name (result_type, NULL, name);
      gimple_set_lhs (call, tmp1);
      gsi_insert_after (gsi, call, GSI_NEW_STMT);
      tree tmp2 = make_temp_ssa_name (TREE_TYPE (result), NULL, name);
      gassign *assign = gimple_build_assign (tmp2, NOP_EXPR, tmp1);
      gsi_insert_after (gsi, assign, GSI_NEW_STMT);
      assign = gimple_build_assign (result, tmp2);
      gsi_insert_after (gsi, assign, GSI_NEW_STMT);
    }
  else
    gsi_insert_after (gsi, call, GSI_NEW_STMT);
}

/* Output instructions as GIMPLE trees to increment the COUNTER.  If RESULT is
   not null, then assign the updated counter value to RESULT.  Insert the
   instructions to GSI.  Use NAME for temporary values.  */

static inline void
gen_counter_update (gimple_stmt_iterator *gsi, tree counter, tree result,
		    const char *name)
{
  tree type = gcov_type_node;
  tree addr = build_fold_addr_expr (counter);
  tree one = build_int_cst (type, 1);
  tree relaxed = build_int_cst (integer_type_node, MEMMODEL_RELAXED);

  if (counter_update == COUNTER_UPDATE_ATOMIC_BUILTIN
      || (result && counter_update == COUNTER_UPDATE_ATOMIC_SPLIT))
    {
      /* __atomic_fetch_add (&counter, 1, MEMMODEL_RELAXED); */
      tree f = builtin_decl_explicit (TYPE_PRECISION (type) > 32
				      ? BUILT_IN_ATOMIC_ADD_FETCH_8
				      : BUILT_IN_ATOMIC_ADD_FETCH_4);
      gcall *call = gimple_build_call (f, 3, addr, one, relaxed);
      gen_assign_counter_update (gsi, call, f, result, name);
    }
  else if (!result && (counter_update == COUNTER_UPDATE_ATOMIC_SPLIT
		       || counter_update == COUNTER_UPDATE_ATOMIC_PARTIAL))
    {
      /* low = __atomic_add_fetch_4 (addr, 1, MEMMODEL_RELAXED);
	 high_inc = low == 0 ? 1 : 0;
	 __atomic_add_fetch_4 (addr_high, high_inc, MEMMODEL_RELAXED); */
      tree zero32 = build_zero_cst (uint32_type_node);
      tree one32 = build_one_cst (uint32_type_node);
      tree addr_high = make_temp_ssa_name (TREE_TYPE (addr), NULL, name);
      tree four = build_int_cst (size_type_node, 4);
      gassign *assign1 = gimple_build_assign (addr_high, POINTER_PLUS_EXPR,
					      addr, four);
      gsi_insert_after (gsi, assign1, GSI_NEW_STMT);
      if (WORDS_BIG_ENDIAN)
	std::swap (addr, addr_high);
      tree f = builtin_decl_explicit (BUILT_IN_ATOMIC_ADD_FETCH_4);
      gcall *call1 = gimple_build_call (f, 3, addr, one, relaxed);
      tree low = make_temp_ssa_name (uint32_type_node, NULL, name);
      gimple_call_set_lhs (call1, low);
      gsi_insert_after (gsi, call1, GSI_NEW_STMT);
      tree is_zero = make_temp_ssa_name (boolean_type_node, NULL, name);
      gassign *assign2 = gimple_build_assign (is_zero, EQ_EXPR, low,
					      zero32);
      gsi_insert_after (gsi, assign2, GSI_NEW_STMT);
      tree high_inc = make_temp_ssa_name (uint32_type_node, NULL, name);
      gassign *assign3 = gimple_build_assign (high_inc, COND_EXPR,
					      is_zero, one32, zero32);
      gsi_insert_after (gsi, assign3, GSI_NEW_STMT);
      gcall *call2 = gimple_build_call (f, 3, addr_high, high_inc,
					relaxed);
      gsi_insert_after (gsi, call2, GSI_NEW_STMT);
    }
  else
    {
      tree tmp1 = make_temp_ssa_name (type, NULL, name);
      gassign *assign1 = gimple_build_assign (tmp1, counter);
      gsi_insert_after (gsi, assign1, GSI_NEW_STMT);
      tree tmp2 = make_temp_ssa_name (type, NULL, name);
      gassign *assign2 = gimple_build_assign (tmp2, PLUS_EXPR, tmp1, one);
      gsi_insert_after (gsi, assign2, GSI_NEW_STMT);
      gassign *assign3 = gimple_build_assign (unshare_expr (counter), tmp2);
      gsi_insert_after (gsi, assign3, GSI_NEW_STMT);
      if (result)
	{
	  gassign *assign4 = gimple_build_assign (result, tmp2);
	  gsi_insert_after (gsi, assign4, GSI_NEW_STMT);
	}
    }
}

/* Output instructions as GIMPLE trees to increment the edge
   execution count, and insert them on E.  */

void
gimple_gen_edge_profiler (int edgeno, edge e)
{
  gimple_stmt_iterator gsi = gsi_last (PENDING_STMT (e));
  tree counter = tree_coverage_counter_ref (GCOV_COUNTER_ARCS, edgeno);
  gen_counter_update (&gsi, counter, NULL_TREE, "PROF_edge_counter");
}

/* Emits code to get VALUE to instrument at GSI, and returns the
   variable containing the value.  */

static tree
prepare_instrumented_value (gimple_stmt_iterator *gsi, histogram_value value)
{
  tree val = value->hvalue.value;
  if (POINTER_TYPE_P (TREE_TYPE (val)))
    val = fold_convert (build_nonstandard_integer_type
			  (TYPE_PRECISION (TREE_TYPE (val)), 1), val);
  return force_gimple_operand_gsi (gsi, fold_convert (gcov_type_node, val),
				   true, NULL_TREE, true, GSI_SAME_STMT);
}

/* Output instructions as GIMPLE trees to increment the interval histogram
   counter.  VALUE is the expression whose value is profiled.  TAG is the
   tag of the section for counters, BASE is offset of the counter position.  */

void
gimple_gen_interval_profiler (histogram_value value, unsigned tag)
{
  gimple *stmt = value->hvalue.stmt;
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  tree ref = tree_coverage_counter_ref (tag, 0), ref_ptr;
  gcall *call;
  tree val;
  tree start = build_int_cst_type (integer_type_node,
				   value->hdata.intvl.int_start);
  tree steps = build_int_cst_type (unsigned_type_node,
				   value->hdata.intvl.steps);

  ref_ptr = force_gimple_operand_gsi (&gsi,
				      build_addr (ref),
				      true, NULL_TREE, true, GSI_SAME_STMT);
  val = prepare_instrumented_value (&gsi, value);
  call = gimple_build_call (tree_interval_profiler_fn, 4,
			    ref_ptr, val, start, steps);
  gsi_insert_before (&gsi, call, GSI_NEW_STMT);
}

/* Output instructions as GIMPLE trees to increment the power of two histogram
   counter.  VALUE is the expression whose value is profiled.  TAG is the tag
   of the section for counters.  */

void
gimple_gen_pow2_profiler (histogram_value value, unsigned tag)
{
  gimple *stmt = value->hvalue.stmt;
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  tree ref_ptr = tree_coverage_counter_addr (tag, 0);
  gcall *call;
  tree val;

  ref_ptr = force_gimple_operand_gsi (&gsi, ref_ptr,
				      true, NULL_TREE, true, GSI_SAME_STMT);
  val = prepare_instrumented_value (&gsi, value);
  call = gimple_build_call (tree_pow2_profiler_fn, 2, ref_ptr, val);
  gsi_insert_before (&gsi, call, GSI_NEW_STMT);
}

/* Output instructions as GIMPLE trees for code to find the most N common
   values.  VALUE is the expression whose value is profiled.  TAG is the tag
   of the section for counters.  */

void
gimple_gen_topn_values_profiler (histogram_value value, unsigned tag)
{
  gimple *stmt = value->hvalue.stmt;
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  tree ref_ptr = tree_coverage_counter_addr (tag, 0);
  gcall *call;
  tree val;

  ref_ptr = force_gimple_operand_gsi (&gsi, ref_ptr,
				      true, NULL_TREE, true, GSI_SAME_STMT);
  val = prepare_instrumented_value (&gsi, value);
  call = gimple_build_call (tree_topn_values_profiler_fn, 2, ref_ptr, val);
  gsi_insert_before (&gsi, call, GSI_NEW_STMT);
}


/* Output instructions as GIMPLE trees for code to find the most
   common called function in indirect call.
   VALUE is the call expression whose indirect callee is profiled.
   TAG is the tag of the section for counters.  */

void
gimple_gen_ic_profiler (histogram_value value, unsigned tag)
{
  tree tmp1;
  gassign *stmt1, *stmt2, *stmt3;
  gimple *stmt = value->hvalue.stmt;
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  tree ref_ptr = tree_coverage_counter_addr (tag, 0);

  ref_ptr = force_gimple_operand_gsi (&gsi, ref_ptr,
				      true, NULL_TREE, true, GSI_SAME_STMT);

  /* Insert code:

    stmt1: __gcov_indirect_call.counters = get_relevant_counter_ptr ();
    stmt2: tmp1 = (void *) (indirect call argument value)
    stmt3: __gcov_indirect_call.callee = tmp1;

    Example:
      f_1 = foo;
      __gcov_indirect_call.counters = &__gcov4.main[0];
      PROF_fn_9 = f_1;
      __gcov_indirect_call.callee = PROF_fn_9;
      _4 = f_1 ();
   */

  tree gcov_type_ptr = build_pointer_type (get_gcov_type ());

  tree counter_ref = build3 (COMPONENT_REF, gcov_type_ptr,
			     ic_tuple_var, ic_tuple_counters_field, NULL_TREE);

  stmt1 = gimple_build_assign (counter_ref, ref_ptr);
  tmp1 = make_temp_ssa_name (ptr_type_node, NULL, "PROF_fn");
  stmt2 = gimple_build_assign (tmp1, unshare_expr (value->hvalue.value));
  tree callee_ref = build3 (COMPONENT_REF, ptr_type_node,
			     ic_tuple_var, ic_tuple_callee_field, NULL_TREE);
  stmt3 = gimple_build_assign (callee_ref, tmp1);

  gsi_insert_before (&gsi, stmt1, GSI_SAME_STMT);
  gsi_insert_before (&gsi, stmt2, GSI_SAME_STMT);
  gsi_insert_before (&gsi, stmt3, GSI_SAME_STMT);
}


/* Output instructions as GIMPLE trees for code to find the most
   common called function in indirect call. Insert instructions at the
   beginning of every possible called function.
  */

void
gimple_gen_ic_func_profiler (void)
{
  struct cgraph_node * c_node = cgraph_node::get (current_function_decl);
  gcall *stmt1;
  tree tree_uid, cur_func, void0;

  /* Disable indirect call profiling for an IFUNC resolver and its
     callees since it requires TLS which hasn't been set up yet when
     the dynamic linker is resolving IFUNC symbols.  See
     https://gcc.gnu.org/bugzilla/show_bug.cgi?id=114115
   */
  if (c_node->only_called_directly_p ()
      || c_node->called_by_ifunc_resolver)
    return;

  gimple_init_gcov_profiler ();

  basic_block entry = ENTRY_BLOCK_PTR_FOR_FN (cfun);
  basic_block cond_bb = split_edge (single_succ_edge (entry));
  basic_block update_bb = split_edge (single_succ_edge (cond_bb));

  /* We need to do an extra split in order to not create an input
     for a possible PHI node.  */
  split_edge (single_succ_edge (update_bb));

  edge true_edge = single_succ_edge (cond_bb);
  true_edge->flags = EDGE_TRUE_VALUE;

  profile_probability probability;
  if (DECL_VIRTUAL_P (current_function_decl))
    probability = profile_probability::very_likely ();
  else
    probability = profile_probability::unlikely ();

  true_edge->probability = probability;
  edge e = make_edge (cond_bb, single_succ_edge (update_bb)->dest,
		      EDGE_FALSE_VALUE);
  e->probability = true_edge->probability.invert ();

  /* Insert code:

     if (__gcov_indirect_call.callee != NULL)
       __gcov_indirect_call_profiler_v3 (profile_id, &current_function_decl);

     The function __gcov_indirect_call_profiler_v3 is responsible for
     resetting __gcov_indirect_call.callee to NULL.  */

  gimple_stmt_iterator gsi = gsi_start_bb (cond_bb);
  void0 = build_int_cst (ptr_type_node, 0);

  tree callee_ref = build3 (COMPONENT_REF, ptr_type_node,
			    ic_tuple_var, ic_tuple_callee_field, NULL_TREE);

  tree ref = force_gimple_operand_gsi (&gsi, callee_ref, true, NULL_TREE,
				       true, GSI_SAME_STMT);

  gcond *cond = gimple_build_cond (NE_EXPR, ref,
				   void0, NULL, NULL);
  gsi_insert_before (&gsi, cond, GSI_NEW_STMT);

  gsi = gsi_after_labels (update_bb);

  cur_func = force_gimple_operand_gsi (&gsi,
				       build_addr (current_function_decl),
				       true, NULL_TREE,
				       true, GSI_SAME_STMT);
  tree_uid = build_int_cst
	      (gcov_type_node,
	       cgraph_node::get (current_function_decl)->profile_id);
  stmt1 = gimple_build_call (tree_indirect_call_profiler_fn, 2,
			     tree_uid, cur_func);
  gsi_insert_before (&gsi, stmt1, GSI_SAME_STMT);
}

/* Output instructions as GIMPLE tree at the beginning for each function.
   TAG is the tag of the section for counters, BASE is offset of the
   counter position and GSI is the iterator we place the counter.  */

void
gimple_gen_time_profiler (unsigned tag)
{
  tree type = get_gcov_type ();
  basic_block entry = ENTRY_BLOCK_PTR_FOR_FN (cfun);
  basic_block cond_bb = split_edge (single_succ_edge (entry));
  basic_block update_bb = split_edge (single_succ_edge (cond_bb));

  /* We need to do an extra split in order to not create an input
     for a possible PHI node.  */
  split_edge (single_succ_edge (update_bb));

  edge true_edge = single_succ_edge (cond_bb);
  true_edge->flags = EDGE_TRUE_VALUE;
  true_edge->probability = profile_probability::unlikely ();
  edge e
    = make_edge (cond_bb, single_succ_edge (update_bb)->dest, EDGE_FALSE_VALUE);
  e->probability = true_edge->probability.invert ();

  gimple_stmt_iterator gsi = gsi_start_bb (cond_bb);
  tree original_ref = tree_coverage_counter_ref (tag, 0);
  tree ref = force_gimple_operand_gsi (&gsi, original_ref, true, NULL_TREE,
				       true, GSI_SAME_STMT);

  /* Emit: if (counters[0] != 0).  */
  gcond *cond = gimple_build_cond (EQ_EXPR, ref, build_int_cst (type, 0),
				   NULL, NULL);
  gsi_insert_before (&gsi, cond, GSI_NEW_STMT);

  /* Emit: counters[0] = ++__gcov_time_profiler_counter.  */
  gsi = gsi_start_bb (update_bb);
  gen_counter_update (&gsi, tree_time_profiler_counter, original_ref,
		      "PROF_time_profile");
}

/* Output instructions as GIMPLE trees to increment the average histogram
   counter.  VALUE is the expression whose value is profiled.  TAG is the
   tag of the section for counters, BASE is offset of the counter position.  */

void
gimple_gen_average_profiler (histogram_value value, unsigned tag)
{
  gimple *stmt = value->hvalue.stmt;
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  tree ref_ptr = tree_coverage_counter_addr (tag, 0);
  gcall *call;
  tree val;

  ref_ptr = force_gimple_operand_gsi (&gsi, ref_ptr,
				      true, NULL_TREE,
				      true, GSI_SAME_STMT);
  val = prepare_instrumented_value (&gsi, value);
  call = gimple_build_call (tree_average_profiler_fn, 2, ref_ptr, val);
  gsi_insert_before (&gsi, call, GSI_NEW_STMT);
}

/* Output instructions as GIMPLE trees to increment the ior histogram
   counter.  VALUE is the expression whose value is profiled.  TAG is the
   tag of the section for counters, BASE is offset of the counter position.  */

void
gimple_gen_ior_profiler (histogram_value value, unsigned tag)
{
  gimple *stmt = value->hvalue.stmt;
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  tree ref_ptr = tree_coverage_counter_addr (tag, 0);
  gcall *call;
  tree val;

  ref_ptr = force_gimple_operand_gsi (&gsi, ref_ptr,
				      true, NULL_TREE, true, GSI_SAME_STMT);
  val = prepare_instrumented_value (&gsi, value);
  call = gimple_build_call (tree_ior_profiler_fn, 2, ref_ptr, val);
  gsi_insert_before (&gsi, call, GSI_NEW_STMT);
}

static vec<regex_t> profile_filter_files;
static vec<regex_t> profile_exclude_files;

/* Parse list of provided REGEX (separated with semi-collon) and
   create expressions (of type regex_t) and save them into V vector.
   If there is a regular expression parsing error, error message is
   printed for FLAG_NAME.  */

static void
parse_profile_filter (const char *regex, vec<regex_t> *v,
		      const char *flag_name)
{
  v->create (4);
  if (regex != NULL)
    {
      char *str = xstrdup (regex);
      for (char *p = strtok (str, ";"); p != NULL; p = strtok (NULL, ";"))
	{
	  regex_t r;
	  if (regcomp (&r, p, REG_EXTENDED | REG_NOSUB) != 0)
	    {
	      error ("invalid regular expression %qs in %qs",
		     p, flag_name);
	      return;
	    }

	  v->safe_push (r);
	}
    }
}

/* Parse values of -fprofile-filter-files and -fprofile-exclude-files
   options.  */

static void
parse_profile_file_filtering ()
{
  parse_profile_filter (flag_profile_filter_files, &profile_filter_files,
			"-fprofile-filter-files");
  parse_profile_filter (flag_profile_exclude_files, &profile_exclude_files,
			"-fprofile-exclude-files");
}

/* Parse vectors of regular expressions.  */

static void
release_profile_file_filtering ()
{
  profile_filter_files.release ();
  profile_exclude_files.release ();
}

/* Return true when FILENAME should be instrumented based on
   -fprofile-filter-files and -fprofile-exclude-files options.  */

static bool
include_source_file_for_profile (const char *filename)
{
  /* First check whether file is included in flag_profile_exclude_files.  */
  for (unsigned i = 0; i < profile_exclude_files.length (); i++)
    if (regexec (&profile_exclude_files[i],
		 filename, 0, NULL, 0) == REG_NOERROR)
      return false;

  /* For non-empty flag_profile_filter_files include only files matching a
     regex in the flag.  */
  if (profile_filter_files.is_empty ())
    return true;

  for (unsigned i = 0; i < profile_filter_files.length (); i++)
    if (regexec (&profile_filter_files[i], filename, 0, NULL, 0) == REG_NOERROR)
      return true;

  return false;
}

#ifndef HAVE_sync_compare_and_swapsi
#define HAVE_sync_compare_and_swapsi 0
#endif
#ifndef HAVE_atomic_compare_and_swapsi
#define HAVE_atomic_compare_and_swapsi 0
#endif

#ifndef HAVE_sync_compare_and_swapdi
#define HAVE_sync_compare_and_swapdi 0
#endif
#ifndef HAVE_atomic_compare_and_swapdi
#define HAVE_atomic_compare_and_swapdi 0
#endif

/* Profile all functions in the callgraph.  */

static unsigned int
tree_profiling (void)
{
  struct cgraph_node *node;

  /* Verify whether we can utilize atomic update operations.  */
  bool can_support_atomic = targetm.have_libatomic;
  unsigned HOST_WIDE_INT gcov_type_size
    = tree_to_uhwi (TYPE_SIZE_UNIT (get_gcov_type ()));
  bool have_atomic_4
    = HAVE_sync_compare_and_swapsi || HAVE_atomic_compare_and_swapsi;
  bool have_atomic_8
    = HAVE_sync_compare_and_swapdi || HAVE_atomic_compare_and_swapdi;
  bool needs_split = gcov_type_size == 8 && !have_atomic_8 && have_atomic_4;
  if (!can_support_atomic)
    {
      if (gcov_type_size == 4)
	can_support_atomic = have_atomic_4;
      else if (gcov_type_size == 8)
	can_support_atomic = have_atomic_8;
    }

  if (flag_profile_update != PROFILE_UPDATE_SINGLE && needs_split)
    counter_update = COUNTER_UPDATE_ATOMIC_PARTIAL;

  if (flag_profile_update == PROFILE_UPDATE_ATOMIC
      && !can_support_atomic)
    {
      warning (0, "target does not support atomic profile update, "
	       "single mode is selected");
      flag_profile_update = PROFILE_UPDATE_SINGLE;
    }
  else if (flag_profile_update == PROFILE_UPDATE_PREFER_ATOMIC)
    flag_profile_update
      = can_support_atomic ? PROFILE_UPDATE_ATOMIC : PROFILE_UPDATE_SINGLE;

  if (flag_profile_update == PROFILE_UPDATE_ATOMIC)
    {
      if (needs_split)
	counter_update = COUNTER_UPDATE_ATOMIC_SPLIT;
      else
	counter_update = COUNTER_UPDATE_ATOMIC_BUILTIN;
    }

  /* This is a small-ipa pass that gets called only once, from
     cgraphunit.cc:ipa_passes().  */
  gcc_assert (symtab->state == IPA_SSA);

  init_node_map (true);
  parse_profile_file_filtering ();

  FOR_EACH_DEFINED_FUNCTION (node)
    {
      bool thunk = false;
      if (!gimple_has_body_p (node->decl) && !node->thunk)
	continue;

      /* Don't profile functions produced for builtin stuff.  */
      if (DECL_SOURCE_LOCATION (node->decl) == BUILTINS_LOCATION)
	continue;

      if (lookup_attribute ("no_profile_instrument_function",
			    DECL_ATTRIBUTES (node->decl)))
	continue;
      /* Do not instrument extern inline functions when testing coverage.
	 While this is not perfectly consistent (early inlined extern inlines
	 will get acocunted), testsuite expects that.  */
      if (DECL_EXTERNAL (node->decl)
	  && flag_test_coverage)
	continue;

      const char *file = LOCATION_FILE (DECL_SOURCE_LOCATION (node->decl));
      if (!include_source_file_for_profile (file))
	continue;

      if (node->thunk)
	{
	  /* We cannot expand variadic thunks to Gimple.  */
	  if (stdarg_p (TREE_TYPE (node->decl)))
	    continue;
	  thunk = true;
	  /* When generate profile, expand thunk to gimple so it can be
	     instrumented same way as other functions.  */
	  if (profile_arc_flag || condition_coverage_flag)
	    expand_thunk (node, false, true);
	  /* Read cgraph profile but keep function as thunk at profile-use
	     time.  */
	  else
	    {
	      read_thunk_profile (node);
	      continue;
	    }
	}

      push_cfun (DECL_STRUCT_FUNCTION (node->decl));

      if (dump_file)
	dump_function_header (dump_file, cfun->decl, dump_flags);

      /* Local pure-const may imply need to fixup the cfg.  */
      if (gimple_has_body_p (node->decl)
	  && (execute_fixup_cfg () & TODO_cleanup_cfg))
	cleanup_tree_cfg ();

      branch_prob (thunk);

      if (! flag_branch_probabilities
	  && flag_profile_values)
	gimple_gen_ic_func_profiler ();

      if (flag_branch_probabilities
	  && !thunk
	  && flag_profile_values
	  && flag_value_profile_transformations
	  && profile_status_for_fn (cfun) == PROFILE_READ)
	gimple_value_profile_transformations ();

      /* The above could hose dominator info.  Currently there is
	 none coming in, this is a safety valve.  It should be
	 easy to adjust it, if and when there is some.  */
      free_dominance_info (CDI_DOMINATORS);
      free_dominance_info (CDI_POST_DOMINATORS);
      pop_cfun ();
    }

  release_profile_file_filtering ();

  /* Drop pure/const flags from instrumented functions.  */
  if (profile_arc_flag || condition_coverage_flag || flag_test_coverage)
    FOR_EACH_DEFINED_FUNCTION (node)
      {
	if (!gimple_has_body_p (node->decl)
	    || !(!node->clone_of
		 || node->decl != node->clone_of->decl))
	  continue;

	/* Don't profile functions produced for builtin stuff.  */
	if (DECL_SOURCE_LOCATION (node->decl) == BUILTINS_LOCATION)
	  continue;

	node->set_const_flag (false, false);
	node->set_pure_flag (false, false);
      }

  /* Update call statements and rebuild the cgraph.  */
  FOR_EACH_DEFINED_FUNCTION (node)
    {
      basic_block bb;

      if (!gimple_has_body_p (node->decl)
	  || !(!node->clone_of
	  || node->decl != node->clone_of->decl))
	continue;

      /* Don't profile functions produced for builtin stuff.  */
      if (DECL_SOURCE_LOCATION (node->decl) == BUILTINS_LOCATION)
	continue;

      push_cfun (DECL_STRUCT_FUNCTION (node->decl));

      if (profile_arc_flag || condition_coverage_flag || flag_test_coverage)
	FOR_EACH_BB_FN (bb, cfun)
	  {
	    gimple_stmt_iterator gsi;
	    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	      {
		gcall *call = dyn_cast <gcall *> (gsi_stmt (gsi));
		if (!call || gimple_call_internal_p (call))
		  continue;

		/* We do not clear pure/const on decls without body.  */
		tree fndecl = gimple_call_fndecl (call);
		cgraph_node *callee;
		if (fndecl
		    && (callee = cgraph_node::get (fndecl))
		    && callee->get_availability (node) == AVAIL_NOT_AVAILABLE)
		  continue;

		/* Drop the const attribute from the call type (the pure
		   attribute is not available on types).  */
		tree fntype = gimple_call_fntype (call);
		if (fntype && TYPE_READONLY (fntype))
		  {
		    int quals = TYPE_QUALS (fntype) & ~TYPE_QUAL_CONST;
		    fntype = build_qualified_type (fntype, quals);
		    gimple_call_set_fntype (call, fntype);
		  }

		/* Update virtual operands of calls to no longer const/pure
		   functions.  */
		update_stmt (call);
	      }
	  }

      /* re-merge split blocks.  */
      cleanup_tree_cfg ();
      update_ssa (TODO_update_ssa);

      cgraph_edge::rebuild_edges ();

      pop_cfun ();
    }

  handle_missing_profiles ();

  del_node_map ();
  return 0;
}

namespace {

const pass_data pass_data_ipa_tree_profile =
{
  SIMPLE_IPA_PASS, /* type */
  "profile", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_IPA_PROFILE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_dump_symtab, /* todo_flags_finish */
};

class pass_ipa_tree_profile : public simple_ipa_opt_pass
{
public:
  pass_ipa_tree_profile (gcc::context *ctxt)
    : simple_ipa_opt_pass (pass_data_ipa_tree_profile, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override;
  unsigned int execute (function *) final override { return tree_profiling (); }

}; // class pass_ipa_tree_profile

bool
pass_ipa_tree_profile::gate (function *)
{
  /* When profile instrumentation, use or test coverage shall be performed.
     But for AutoFDO, this there is no instrumentation, thus this pass is
     disabled.  */
  return (!in_lto_p && !flag_auto_profile
	  && (flag_branch_probabilities || flag_test_coverage
	      || profile_arc_flag || condition_coverage_flag));
}

} // anon namespace

simple_ipa_opt_pass *
make_pass_ipa_tree_profile (gcc::context *ctxt)
{
  return new pass_ipa_tree_profile (ctxt);
}

#include "gt-tree-profile.h"
