..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Sequence iterators

.. _sequence-iterators:

Sequence iterators
******************

Sequence iterators are convenience constructs for iterating
through statements in a sequence.  Given a sequence ``SEQ``, here is
a typical use of gimple sequence iterators:

.. code-block:: c++

  gimple_stmt_iterator gsi;

  for (gsi = gsi_start (seq); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple g = gsi_stmt (gsi);
      /* Do something with gimple statement G.  */
    }

Backward iterations are possible:

.. code-block:: c++

          for (gsi = gsi_last (seq); !gsi_end_p (gsi); gsi_prev (&gsi))

Forward and backward iterations on basic blocks are possible with
``gsi_start_bb`` and ``gsi_last_bb``.

In the documentation below we sometimes refer to enum
``gsi_iterator_update``.  The valid options for this enumeration are:

* ``GSI_NEW_STMT``
  Only valid when a single statement is added.  Move the iterator to it.

* ``GSI_SAME_STMT``
  Leave the iterator at the same statement.

* ``GSI_CONTINUE_LINKING``
  Move iterator to whatever position is suitable for linking other
  statements in the same direction.

Below is a list of the functions used to manipulate and use
statement iterators.

.. function:: gimple_stmt_iterator gsi_start (gimple_seq seq)

  Return a new iterator pointing to the sequence ``SEQ`` 's first
  statement.  If ``SEQ`` is empty, the iterator's basic block is ``NULL``.
  Use ``gsi_start_bb`` instead when the iterator needs to always have
  the correct basic block set.

.. function:: gimple_stmt_iterator gsi_start_bb (basic_block bb)

  Return a new iterator pointing to the first statement in basic
  block ``BB``.

.. function:: gimple_stmt_iterator gsi_last (gimple_seq seq)

  Return a new iterator initially pointing to the last statement of
  sequence ``SEQ``.  If ``SEQ`` is empty, the iterator's basic block is
  ``NULL``.  Use ``gsi_last_bb`` instead when the iterator needs to always
  have the correct basic block set.

.. function:: gimple_stmt_iterator gsi_last_bb (basic_block bb)

  Return a new iterator pointing to the last statement in basic
  block ``BB``.

.. function:: bool gsi_end_p (gimple_stmt_iterator i)

  Return ``TRUE`` if at the end of ``I``.

.. function:: bool gsi_one_before_end_p (gimple_stmt_iterator i)

  Return ``TRUE`` if we're one statement before the end of ``I``.

.. function:: void gsi_next (gimple_stmt_iterator *i)

  Advance the iterator to the next gimple statement.

.. function:: void gsi_prev (gimple_stmt_iterator *i)

  Advance the iterator to the previous gimple statement.

.. function:: gimple gsi_stmt (gimple_stmt_iterator i)

  Return the current stmt.

.. function:: gimple_stmt_iterator gsi_after_labels (basic_block bb)

  Return a block statement iterator that points to the first
  non-label statement in block ``BB``.

.. function:: gimple * gsi_stmt_ptr (gimple_stmt_iterator *i)

  Return a pointer to the current stmt.

.. function:: basic_block gsi_bb (gimple_stmt_iterator i)

  Return the basic block associated with this iterator.

.. function:: gimple_seq gsi_seq (gimple_stmt_iterator i)

  Return the sequence associated with this iterator.

.. function:: void gsi_remove (gimple_stmt_iterator *i, bool remove_eh_info)

  Remove the current stmt from the sequence.  The iterator is
  updated to point to the next statement.  When ``REMOVE_EH_INFO`` is
  true we remove the statement pointed to by iterator ``I`` from the ``EH``
  tables.  Otherwise we do not modify the ``EH`` tables.  Generally,
  ``REMOVE_EH_INFO`` should be true when the statement is going to be
  removed from the ``IL`` and not reinserted elsewhere.

.. function:: void gsi_link_seq_before (gimple_stmt_iterator *i, gimple_seq seq, enum gsi_iterator_update mode)

  Links the sequence of statements ``SEQ`` before the statement pointed
  by iterator ``I``.  ``MODE`` indicates what to do with the iterator
  after insertion (see ``enum gsi_iterator_update`` above).

.. function:: void gsi_link_before (gimple_stmt_iterator *i, gimple g, enum gsi_iterator_update mode)

  Links statement ``G`` before the statement pointed-to by iterator ``I``.
  Updates iterator ``I`` according to ``MODE``.

.. function:: void gsi_link_seq_after (gimple_stmt_iterator *i, gimple_seq seq, enum gsi_iterator_update mode)

  Links sequence ``SEQ`` after the statement pointed-to by iterator ``I``.
  ``MODE`` is as in ``gsi_insert_after``.

.. function:: void gsi_link_after (gimple_stmt_iterator *i, gimple g, enum gsi_iterator_update mode)

  Links statement ``G`` after the statement pointed-to by iterator ``I``.
  ``MODE`` is as in ``gsi_insert_after``.

.. function:: gimple_seq gsi_split_seq_after (gimple_stmt_iterator i)

  Move all statements in the sequence after ``I`` to a new sequence.
  Return this new sequence.

.. function:: gimple_seq gsi_split_seq_before (gimple_stmt_iterator *i)

  Move all statements in the sequence before ``I`` to a new sequence.
  Return this new sequence.

.. function:: void gsi_replace (gimple_stmt_iterator *i, gimple stmt, bool update_eh_info)

  Replace the statement pointed-to by ``I`` to ``STMT``.  If ``UPDATE_EH_INFO``
  is true, the exception handling information of the original
  statement is moved to the new statement.

.. function:: void gsi_insert_before (gimple_stmt_iterator *i, gimple stmt, enum gsi_iterator_update mode)

  Insert statement ``STMT`` before the statement pointed-to by iterator
  ``I``, update ``STMT`` 's basic block and scan it for new operands.  ``MODE``
  specifies how to update iterator ``I`` after insertion (see enum
  ``gsi_iterator_update``).

.. function:: void gsi_insert_seq_before (gimple_stmt_iterator *i, gimple_seq seq, enum gsi_iterator_update mode)

  Like ``gsi_insert_before``, but for all the statements in ``SEQ``.

.. function:: void gsi_insert_after (gimple_stmt_iterator *i, gimple stmt, enum gsi_iterator_update mode)

  Insert statement ``STMT`` after the statement pointed-to by iterator
  ``I``, update ``STMT`` 's basic block and scan it for new operands.  ``MODE``
  specifies how to update iterator ``I`` after insertion (see enum
  ``gsi_iterator_update``).

.. function:: void gsi_insert_seq_after (gimple_stmt_iterator *i, gimple_seq seq, enum gsi_iterator_update mode)

  Like ``gsi_insert_after``, but for all the statements in ``SEQ``.

.. function:: gimple_stmt_iterator gsi_for_stmt (gimple stmt)

  Finds iterator for ``STMT``.

.. function:: void gsi_move_after (gimple_stmt_iterator *from, gimple_stmt_iterator *to)

  Move the statement at ``FROM`` so it comes right after the statement
  at ``TO``.

.. function:: void gsi_move_before (gimple_stmt_iterator *from, gimple_stmt_iterator *to)

  Move the statement at ``FROM`` so it comes right before the statement
  at ``TO``.

.. function:: void gsi_move_to_bb_end (gimple_stmt_iterator *from, basic_block bb)

  Move the statement at ``FROM`` to the end of basic block ``BB``.

.. function:: void gsi_insert_on_edge (edge e, gimple stmt)

  Add ``STMT`` to the pending list of edge ``E``.  No actual insertion is
  made until a call to ``gsi_commit_edge_inserts`` () is made.

.. function:: void gsi_insert_seq_on_edge (edge e, gimple_seq seq)

  Add the sequence of statements in ``SEQ`` to the pending list of edge
  ``E``.  No actual insertion is made until a call to
  ``gsi_commit_edge_inserts`` () is made.

.. function:: basic_block gsi_insert_on_edge_immediate (edge e, gimple stmt)

  Similar to ``gsi_insert_on_edge`` + ``gsi_commit_edge_inserts``.  If a new
  block has to be created, it is returned.

.. function:: void gsi_commit_one_edge_insert (edge e, basic_block *new_bb)

  Commit insertions pending at edge ``E``.  If a new block is created,
  set ``NEW_BB`` to this block, otherwise set it to ``NULL``.

.. function:: void gsi_commit_edge_inserts (void)

  This routine will commit all pending edge insertions, creating
  any new basic blocks which are necessary.