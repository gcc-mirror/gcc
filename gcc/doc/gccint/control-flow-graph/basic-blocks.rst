..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: basic block, basic_block

.. _basic-blocks:

Basic Blocks
************

A basic block is a straight-line sequence of code with only one entry
point and only one exit.  In GCC, basic blocks are represented using
the ``basic_block`` data type.

.. index:: ENTRY_BLOCK_PTR, EXIT_BLOCK_PTR

Special basic blocks represent possible entry and exit points of a
function.  These blocks are called ``ENTRY_BLOCK_PTR`` and
``EXIT_BLOCK_PTR``.  These blocks do not contain any code.

.. index:: BASIC_BLOCK

The ``BASIC_BLOCK`` array contains all basic blocks in an
unspecified order.  Each ``basic_block`` structure has a field
that holds a unique integer identifier ``index`` that is the
index of the block in the ``BASIC_BLOCK`` array.
The total number of basic blocks in the function is
``n_basic_blocks``.  Both the basic block indices and
the total number of basic blocks may vary during the compilation
process, as passes reorder, create, duplicate, and destroy basic
blocks.  The index for any block should never be greater than
``last_basic_block``.  The indices 0 and 1 are special codes
reserved for ``ENTRY_BLOCK`` and ``EXIT_BLOCK``, the
indices of ``ENTRY_BLOCK_PTR`` and ``EXIT_BLOCK_PTR``.

.. index:: next_bb, prev_bb, FOR_EACH_BB, FOR_ALL_BB

Two pointer members of the ``basic_block`` structure are the
pointers ``next_bb`` and ``prev_bb``.  These are used to keep
doubly linked chain of basic blocks in the same order as the
underlying instruction stream.  The chain of basic blocks is updated
transparently by the provided API for manipulating the CFG.  The macro
``FOR_EACH_BB`` can be used to visit all the basic blocks in
lexicographical order, except ``ENTRY_BLOCK`` and ``EXIT_BLOCK``.
The macro ``FOR_ALL_BB`` also visits all basic blocks in
lexicographical order, including ``ENTRY_BLOCK`` and ``EXIT_BLOCK``.

.. index:: post_order_compute, inverted_post_order_compute, walk_dominator_tree

The functions ``post_order_compute`` and ``inverted_post_order_compute``
can be used to compute topological orders of the CFG.  The orders are
stored as vectors of basic block indices.  The ``BASIC_BLOCK`` array
can be used to iterate each basic block by index.
Dominator traversals are also possible using
``walk_dominator_tree``.  Given two basic blocks A and B, block A
dominates block B if A is *always* executed before B.

Each ``basic_block`` also contains pointers to the first
instruction (the :dfn:`head`) and the last instruction (the :dfn:`tail`)
or :dfn:`end` of the instruction stream contained in a basic block.  In
fact, since the ``basic_block`` data type is used to represent
blocks in both major intermediate representations of GCC (``GIMPLE``
and RTL), there are pointers to the head and end of a basic block for
both representations, stored in intermediate representation specific
data in the ``il`` field of ``struct basic_block_def``.

.. index:: CODE_LABEL, NOTE_INSN_BASIC_BLOCK

For RTL, these pointers are ``BB_HEAD`` and ``BB_END``.

.. index:: insn notes, notes, NOTE_INSN_BASIC_BLOCK

In the RTL representation of a function, the instruction stream
contains not only the 'real' instructions, but also :dfn:`notes`
or :dfn:`insn notes` (to distinguish them from :dfn:`reg notes`).
Any function that moves or duplicates the basic blocks needs
to take care of updating of these notes.  Many of these notes expect
that the instruction stream consists of linear regions, so updating
can sometimes be tedious.  All types of insn notes are defined
in :samp:`insn-notes.def`.

In the RTL function representation, the instructions contained in a
basic block always follow a ``NOTE_INSN_BASIC_BLOCK``, but zero
or more ``CODE_LABEL`` nodes can precede the block note.
A basic block ends with a control flow instruction or with the last
instruction before the next ``CODE_LABEL`` or
``NOTE_INSN_BASIC_BLOCK``.
By definition, a ``CODE_LABEL`` cannot appear in the middle of
the instruction stream of a basic block.

.. index:: can_fallthru, table jump

In addition to notes, the jump table vectors are also represented as
'pseudo-instructions' inside the insn stream.  These vectors never
appear in the basic block and should always be placed just after the
table jump instructions referencing them.  After removing the
table-jump it is often difficult to eliminate the code computing the
address and referencing the vector, so cleaning up these vectors is
postponed until after liveness analysis.   Thus the jump table vectors
may appear in the insn stream unreferenced and without any purpose.
Before any edge is made :dfn:`fall-thru`, the existence of such
construct in the way needs to be checked by calling
``can_fallthru`` function.

.. index:: GIMPLE statement iterators

For the ``GIMPLE`` representation, the PHI nodes and statements
contained in a basic block are in a ``gimple_seq`` pointed to by
the basic block intermediate language specific pointers.
Abstract containers and iterators are used to access the PHI nodes
and statements in a basic blocks.  These iterators are called
:dfn:`GIMPLE statement iterators` (GSIs).  Grep for ``^gsi``
in the various :samp:`gimple-*` and :samp:`tree-*` files.
There is a ``gimple_stmt_iterator`` type for iterating over
all kinds of statement, and a ``gphi_iterator`` subclass for
iterating over PHI nodes.
The following snippet will pretty-print all PHI nodes the statements
of the current function in the GIMPLE representation.

.. code-block:: c++

  basic_block bb;

  FOR_EACH_BB (bb)
    {
     gphi_iterator pi;
     gimple_stmt_iterator si;

     for (pi = gsi_start_phis (bb); !gsi_end_p (pi); gsi_next (&pi))
       {
         gphi *phi = pi.phi ();
         print_gimple_stmt (dump_file, phi, 0, TDF_SLIM);
       }
     for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
       {
         gimple stmt = gsi_stmt (si);
         print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
       }
    }