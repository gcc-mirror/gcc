..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Loop representation, Loop analysis

.. _loop-representation:

Loop representation
*******************

This chapter describes the representation of loops in GCC, and functions
that can be used to build, modify and analyze this representation.  Most
of the interfaces and data structures are declared in :samp:`cfgloop.h`.
Loop structures are analyzed and this information disposed or updated
at the discretion of individual passes.  Still most of the generic
CFG manipulation routines are aware of loop structures and try to
keep them up-to-date.  By this means an increasing part of the
compilation pipeline is setup to maintain loop structure across
passes to allow attaching meta information to individual loops
for consumption by later passes.

In general, a natural loop has one entry block (header) and possibly
several back edges (latches) leading to the header from the inside of
the loop.  Loops with several latches may appear if several loops share
a single header, or if there is a branching in the middle of the loop.
The representation of loops in GCC however allows only loops with a
single latch.  During loop analysis, headers of such loops are split and
forwarder blocks are created in order to disambiguate their structures.
Heuristic based on profile information and structure of the induction
variables in the loops is used to determine whether the latches
correspond to sub-loops or to control flow in a single loop.  This means
that the analysis sometimes changes the CFG, and if you run it in the
middle of an optimization pass, you must be able to deal with the new
blocks.  You may avoid CFG changes by passing
``LOOPS_MAY_HAVE_MULTIPLE_LATCHES`` flag to the loop discovery,
note however that most other loop manipulation functions will not work
correctly for loops with multiple latch edges (the functions that only
query membership of blocks to loops and subloop relationships, or
enumerate and test loop exits, can be expected to work).

Body of the loop is the set of blocks that are dominated by its header,
and reachable from its latch against the direction of edges in CFG.  The
loops are organized in a containment hierarchy (tree) such that all the
loops immediately contained inside loop L are the children of L in the
tree.  This tree is represented by the ``struct loops`` structure.
The root of this tree is a fake loop that contains all blocks in the
function.  Each of the loops is represented in a ``struct loop``
structure.  Each loop is assigned an index (``num`` field of the
``struct loop`` structure), and the pointer to the loop is stored in
the corresponding field of the ``larray`` vector in the loops
structure.  The indices do not have to be continuous, there may be
empty (``NULL``) entries in the ``larray`` created by deleting
loops.  Also, there is no guarantee on the relative order of a loop
and its subloops in the numbering.  The index of a loop never changes.

The entries of the ``larray`` field should not be accessed directly.
The function ``get_loop`` returns the loop description for a loop with
the given index.  ``number_of_loops`` function returns number of loops
in the function.  To traverse all loops, use a range-based for loop with
class ``loops_list`` instance. The ``flags`` argument passed to the
constructor function of class ``loops_list`` is used to determine the
direction of traversal and the set of loops visited.  Each loop is
guaranteed to be visited exactly once, regardless of the changes to the
loop tree, and the loops may be removed during the traversal.  The newly
created loops are never traversed, if they need to be visited, this must
be done separately after their creation.

Each basic block contains the reference to the innermost loop it belongs
to (``loop_father``).  For this reason, it is only possible to have
one ``struct loops`` structure initialized at the same time for each
CFG.  The global variable ``current_loops`` contains the
``struct loops`` structure.  Many of the loop manipulation functions
assume that dominance information is up-to-date.

The loops are analyzed through ``loop_optimizer_init`` function.  The
argument of this function is a set of flags represented in an integer
bitmask.  These flags specify what other properties of the loop
structures should be calculated/enforced and preserved later:

* ``LOOPS_MAY_HAVE_MULTIPLE_LATCHES`` : If this flag is set, no
  changes to CFG will be performed in the loop analysis, in particular,
  loops with multiple latch edges will not be disambiguated.  If a loop
  has multiple latches, its latch block is set to NULL.  Most of
  the loop manipulation functions will not work for loops in this shape.
  No other flags that require CFG changes can be passed to
  loop_optimizer_init.

* ``LOOPS_HAVE_PREHEADERS`` : Forwarder blocks are created in such
  a way that each loop has only one entry edge, and additionally, the
  source block of this entry edge has only one successor.  This creates a
  natural place where the code can be moved out of the loop, and ensures
  that the entry edge of the loop leads from its immediate super-loop.

* ``LOOPS_HAVE_SIMPLE_LATCHES`` : Forwarder blocks are created to
  force the latch block of each loop to have only one successor.  This
  ensures that the latch of the loop does not belong to any of its
  sub-loops, and makes manipulation with the loops significantly easier.
  Most of the loop manipulation functions assume that the loops are in
  this shape.  Note that with this flag, the 'normal' loop without any
  control flow inside and with one exit consists of two basic blocks.

* ``LOOPS_HAVE_MARKED_IRREDUCIBLE_REGIONS`` : Basic blocks and
  edges in the strongly connected components that are not natural loops
  (have more than one entry block) are marked with
  ``BB_IRREDUCIBLE_LOOP`` and ``EDGE_IRREDUCIBLE_LOOP`` flags.  The
  flag is not set for blocks and edges that belong to natural loops that
  are in such an irreducible region (but it is set for the entry and exit
  edges of such a loop, if they lead to/from this region).

* ``LOOPS_HAVE_RECORDED_EXITS`` : The lists of exits are recorded
  and updated for each loop.  This makes some functions (e.g.,
  ``get_loop_exit_edges``) more efficient.  Some functions (e.g.,
  ``single_exit``) can be used only if the lists of exits are
  recorded.

These properties may also be computed/enforced later, using functions
``create_preheaders``, ``force_single_succ_latches``,
``mark_irreducible_loops`` and ``record_loop_exits``.
The properties can be queried using ``loops_state_satisfies_p``.

The memory occupied by the loops structures should be freed with
``loop_optimizer_finalize`` function.  When loop structures are
setup to be preserved across passes this function reduces the
information to be kept up-to-date to a minimum (only
``LOOPS_MAY_HAVE_MULTIPLE_LATCHES`` set).

The CFG manipulation functions in general do not update loop structures.
Specialized versions that additionally do so are provided for the most
common tasks.  On GIMPLE, ``cleanup_tree_cfg_loop`` function can be
used to cleanup CFG while updating the loops structures if
``current_loops`` is set.

At the moment loop structure is preserved from the start of GIMPLE
loop optimizations until the end of RTL loop optimizations.  During
this time a loop can be tracked by its ``struct loop`` and number.
