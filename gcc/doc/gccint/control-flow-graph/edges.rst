..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: edge in the flow graph, edge

.. _edges:

Edges
*****

Edges represent possible control flow transfers from the end of some
basic block A to the head of another basic block B.  We say that A is
a predecessor of B, and B is a successor of A.  Edges are represented
in GCC with the ``edge`` data type.  Each ``edge`` acts as a
link between two basic blocks: The ``src`` member of an edge
points to the predecessor basic block of the ``dest`` basic block.
The members ``preds`` and ``succs`` of the ``basic_block`` data
type point to type-safe vectors of edges to the predecessors and
successors of the block.

.. index:: edge iterators

When walking the edges in an edge vector, :dfn:`edge iterators` should
be used.  Edge iterators are constructed using the
``edge_iterator`` data structure and several methods are available
to operate on them:

``ei_start``
  This function initializes an ``edge_iterator`` that points to the
  first edge in a vector of edges.

``ei_last``
  This function initializes an ``edge_iterator`` that points to the
  last edge in a vector of edges.

``ei_end_p``
  This predicate is ``true`` if an ``edge_iterator`` represents
  the last edge in an edge vector.

``ei_one_before_end_p``
  This predicate is ``true`` if an ``edge_iterator`` represents
  the second last edge in an edge vector.

``ei_next``
  This function takes a pointer to an ``edge_iterator`` and makes it
  point to the next edge in the sequence.

``ei_prev``
  This function takes a pointer to an ``edge_iterator`` and makes it
  point to the previous edge in the sequence.

``ei_edge``
  This function returns the ``edge`` currently pointed to by an
  ``edge_iterator``.

``ei_safe_edge``
  This function returns the ``edge`` currently pointed to by an
  ``edge_iterator``, but returns ``NULL`` if the iterator is
  pointing at the end of the sequence.  This function has been provided
  for existing code makes the assumption that a ``NULL`` edge
  indicates the end of the sequence.

  The convenience macro ``FOR_EACH_EDGE`` can be used to visit all of
  the edges in a sequence of predecessor or successor edges.  It must
  not be used when an element might be removed during the traversal,
  otherwise elements will be missed.  Here is an example of how to use
  the macro:

.. code-block:: c++

  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->succs)
    {
       if (e->flags & EDGE_FALLTHRU)
         break;
    }

.. index:: fall-thru

There are various reasons why control flow may transfer from one block
to another.  One possibility is that some instruction, for example a
``CODE_LABEL``, in a linearized instruction stream just always
starts a new basic block.  In this case a :dfn:`fall-thru` edge links
the basic block to the first following basic block.  But there are
several other reasons why edges may be created.  The ``flags``
field of the ``edge`` data type is used to store information
about the type of edge we are dealing with.  Each edge is of one of
the following types:

**jump**

  No type flags are set for edges corresponding to jump instructions.
  These edges are used for unconditional or conditional jumps and in
  RTL also for table jumps.  They are the easiest to manipulate as they
  may be freely redirected when the flow graph is not in SSA form.

**fall-thru**

  .. index:: EDGE_FALLTHRU, force_nonfallthru

  Fall-thru edges are present in case where the basic block may continue
  execution to the following one without branching.  These edges have
  the ``EDGE_FALLTHRU`` flag set.  Unlike other types of edges, these
  edges must come into the basic block immediately following in the
  instruction stream.  The function ``force_nonfallthru`` is
  available to insert an unconditional jump in the case that redirection
  is needed.  Note that this may require creation of a new basic block.

**exception handling**

  .. index:: exception handling, EDGE_ABNORMAL, EDGE_EH

  Exception handling edges represent possible control transfers from a
  trapping instruction to an exception handler.  The definition of
  'trapping' varies.  In C++, only function calls can throw, but for
  Ada exceptions like division by zero or segmentation fault are
  defined and thus each instruction possibly throwing this kind of
  exception needs to be handled as control flow instruction.  Exception
  edges have the ``EDGE_ABNORMAL`` and ``EDGE_EH`` flags set.

  .. index:: purge_dead_edges

  When updating the instruction stream it is easy to change possibly
  trapping instruction to non-trapping, by simply removing the exception
  edge.  The opposite conversion is difficult, but should not happen
  anyway.  The edges can be eliminated via ``purge_dead_edges`` call.

  .. index:: REG_EH_REGION, EDGE_ABNORMAL_CALL

  In the RTL representation, the destination of an exception edge is
  specified by ``REG_EH_REGION`` note attached to the insn.
  In case of a trapping call the ``EDGE_ABNORMAL_CALL`` flag is set
  too.  In the ``GIMPLE`` representation, this extra flag is not set.

  .. index:: may_trap_p, tree_could_trap_p

  In the RTL representation, the predicate ``may_trap_p`` may be used
  to check whether instruction still may trap or not.  For the tree
  representation, the ``tree_could_trap_p`` predicate is available,
  but this predicate only checks for possible memory traps, as in
  dereferencing an invalid pointer location.

**sibling calls**

  .. index:: sibling call, EDGE_ABNORMAL, EDGE_SIBCALL

  Sibling calls or tail calls terminate the function in a non-standard
  way and thus an edge to the exit must be present.
  ``EDGE_SIBCALL`` and ``EDGE_ABNORMAL`` are set in such case.
  These edges only exist in the RTL representation.

**computed jumps**

  .. index:: computed jump, EDGE_ABNORMAL

  Computed jumps contain edges to all labels in the function referenced
  from the code.  All those edges have ``EDGE_ABNORMAL`` flag set.
  The edges used to represent computed jumps often cause compile time
  performance problems, since functions consisting of many taken labels
  and many computed jumps may have *very* dense flow graphs, so
  these edges need to be handled with special care.  During the earlier
  stages of the compilation process, GCC tries to avoid such dense flow
  graphs by factoring computed jumps.  For example, given the following
  series of jumps,

  .. code-block:: c++

      goto *x;
      [ ... ]

      goto *x;
      [ ... ]

      goto *x;
      [ ... ]

  factoring the computed jumps results in the following code sequence
  which has a much simpler flow graph:

  .. code-block:: c++

      goto y;
      [ ... ]

      goto y;
      [ ... ]

      goto y;
      [ ... ]

    y:
      goto *x;

  .. index:: pass_duplicate_computed_gotos

  However, the classic problem with this transformation is that it has a
  runtime cost in there resulting code: An extra jump.  Therefore, the
  computed jumps are un-factored in the later passes of the compiler
  (in the pass called ``pass_duplicate_computed_gotos``).
  Be aware of that when you work on passes in that area.  There have
  been numerous examples already where the compile time for code with
  unfactored computed jumps caused some serious headaches.

**nonlocal goto handlers**

  .. index:: nonlocal goto handler, EDGE_ABNORMAL, EDGE_ABNORMAL_CALL

  GCC allows nested functions to return into caller using a ``goto``
  to a label passed to as an argument to the callee.  The labels passed
  to nested functions contain special code to cleanup after function
  call.  Such sections of code are referred to as 'nonlocal goto
  receivers'.  If a function contains such nonlocal goto receivers, an
  edge from the call to the label is created with the
  ``EDGE_ABNORMAL`` and ``EDGE_ABNORMAL_CALL`` flags set.

**function entry points**

  .. index:: function entry point, alternate function entry point, LABEL_ALTERNATE_NAME

  By definition, execution of function starts at basic block 0, so there
  is always an edge from the ``ENTRY_BLOCK_PTR`` to basic block 0.
  There is no ``GIMPLE`` representation for alternate entry points at
  this moment.  In RTL, alternate entry points are specified by
  ``CODE_LABEL`` with ``LABEL_ALTERNATE_NAME`` defined.  This
  feature is currently used for multiple entry point prologues and is
  limited to post-reload passes only.  This can be used by back-ends to
  emit alternate prologues for functions called from different contexts.
  In future full support for multiple entry functions defined by Fortran
  90 needs to be implemented.

**function exits**

  In the pre-reload representation a function terminates after the last
  instruction in the insn chain and no explicit return instructions are
  used.  This corresponds to the fall-thru edge into exit block.  After
  reload, optimal RTL epilogues are used that use explicit (conditional)
  return instructions that are represented by edges with no flags set.
