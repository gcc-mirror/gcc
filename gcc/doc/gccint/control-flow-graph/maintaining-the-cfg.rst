..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: cfghooks.h

.. _maintaining-the-cfg:

Maintaining the CFG
*******************

An important task of each compiler pass is to keep both the control
flow graph and all profile information up-to-date.  Reconstruction of
the control flow graph after each pass is not an option, since it may be
very expensive and lost profile information cannot be reconstructed at
all.

GCC has two major intermediate representations, and both use the
``basic_block`` and ``edge`` data types to represent control
flow.  Both representations share as much of the CFG maintenance code
as possible.  For each representation, a set of :dfn:`hooks` is defined
so that each representation can provide its own implementation of CFG
manipulation routines when necessary.  These hooks are defined in
:samp:`cfghooks.h`.  There are hooks for almost all common CFG
manipulations, including block splitting and merging, edge redirection
and creating and deleting basic blocks.  These hooks should provide
everything you need to maintain and manipulate the CFG in both the RTL
and ``GIMPLE`` representation.

At the moment, the basic block boundaries are maintained transparently
when modifying instructions, so there rarely is a need to move them
manually (such as in case someone wants to output instruction outside
basic block explicitly).

.. index:: BLOCK_FOR_INSN, gimple_bb

In the RTL representation, each instruction has a
``BLOCK_FOR_INSN`` value that represents pointer to the basic block
that contains the instruction.  In the ``GIMPLE`` representation, the
function ``gimple_bb`` returns a pointer to the basic block
containing the queried statement.

.. index:: GIMPLE statement iterators

When changes need to be applied to a function in its ``GIMPLE``
representation, :dfn:`GIMPLE statement iterators` should be used.  These
iterators provide an integrated abstraction of the flow graph and the
instruction stream.  Block statement iterators are constructed using
the ``gimple_stmt_iterator`` data structure and several modifiers are
available, including the following:

``gsi_start``
  This function initializes a ``gimple_stmt_iterator`` that points to
  the first non-empty statement in a basic block.

``gsi_last``
  This function initializes a ``gimple_stmt_iterator`` that points to
  the last statement in a basic block.

``gsi_end_p``
  This predicate is ``true`` if a ``gimple_stmt_iterator``
  represents the end of a basic block.

``gsi_next``
  This function takes a ``gimple_stmt_iterator`` and makes it point to
  its successor.

``gsi_prev``
  This function takes a ``gimple_stmt_iterator`` and makes it point to
  its predecessor.

``gsi_insert_after``
  This function inserts a statement after the ``gimple_stmt_iterator``
  passed in.  The final parameter determines whether the statement
  iterator is updated to point to the newly inserted statement, or left
  pointing to the original statement.

``gsi_insert_before``
  This function inserts a statement before the ``gimple_stmt_iterator``
  passed in.  The final parameter determines whether the statement
  iterator is updated to point to the newly inserted statement, or left
  pointing to the original  statement.

``gsi_remove``
  This function removes the ``gimple_stmt_iterator`` passed in and
  rechains the remaining statements in a basic block, if any.

.. index:: BB_HEAD, BB_END

In the RTL representation, the macros ``BB_HEAD`` and ``BB_END``
may be used to get the head and end ``rtx`` of a basic block.  No
abstract iterators are defined for traversing the insn chain, but you
can just use ``NEXT_INSN`` and ``PREV_INSN`` instead.  See :ref:`insns`.

.. index:: purge_dead_edges

Usually a code manipulating pass simplifies the instruction stream and
the flow of control, possibly eliminating some edges.  This may for
example happen when a conditional jump is replaced with an
unconditional jump.  Updating of edges
is not transparent and each optimization pass is required to do so
manually.  However only few cases occur in practice.  The pass may
call ``purge_dead_edges`` on a given basic block to remove
superfluous edges, if any.

.. index:: redirect_edge_and_branch, redirect_jump

Another common scenario is redirection of branch instructions, but
this is best modeled as redirection of edges in the control flow graph
and thus use of ``redirect_edge_and_branch`` is preferred over more
low level functions, such as ``redirect_jump`` that operate on RTL
chain only.  The CFG hooks defined in :samp:`cfghooks.h` should provide
the complete API required for manipulating and maintaining the CFG.

.. index:: split_block

It is also possible that a pass has to insert control flow instruction
into the middle of a basic block, thus creating an entry point in the
middle of the basic block, which is impossible by definition: The
block must be split to make sure it only has one entry point, i.e. the
head of the basic block.  The CFG hook ``split_block`` may be used
when an instruction in the middle of a basic block has to become the
target of a jump or branch instruction.

.. index:: insert_insn_on_edge, commit_edge_insertions, gsi_insert_on_edge, gsi_commit_edge_inserts, edge splitting

For a global optimizer, a common operation is to split edges in the
flow graph and insert instructions on them.  In the RTL
representation, this can be easily done using the
``insert_insn_on_edge`` function that emits an instruction
'on the edge', caching it for a later ``commit_edge_insertions``
call that will take care of moving the inserted instructions off the
edge into the instruction stream contained in a basic block.  This
includes the creation of new basic blocks where needed.  In the
``GIMPLE`` representation, the equivalent functions are
``gsi_insert_on_edge`` which inserts a block statement
iterator on an edge, and ``gsi_commit_edge_inserts`` which flushes
the instruction to actual instruction stream.

.. index:: verify_flow_info, CFG verification

While debugging the optimization pass, the ``verify_flow_info``
function may be useful to find bugs in the control flow graph updating
code.