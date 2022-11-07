..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GIMPLE sequences

.. _gimple-sequences:

GIMPLE sequences
****************

GIMPLE sequences are the tuple equivalent of ``STATEMENT_LIST`` 's
used in ``GENERIC``.  They are used to chain statements together, and
when used in conjunction with sequence iterators, provide a
framework for iterating through statements.

GIMPLE sequences are of type struct ``gimple_sequence``, but are more
commonly passed by reference to functions dealing with sequences.
The type for a sequence pointer is ``gimple_seq`` which is the same
as struct ``gimple_sequence`` \*.  When declaring a local sequence,
you can define a local variable of type struct ``gimple_sequence``.
When declaring a sequence allocated on the garbage collected
heap, use the function ``gimple_seq_alloc`` documented below.

There are convenience functions for iterating through sequences
in the section entitled Sequence Iterators.

Below is a list of functions to manipulate and query sequences.

.. function:: void gimple_seq_add_stmt (gimple_seq *seq, gimple g)

  Link a gimple statement to the end of the sequence \* ``SEQ`` if ``G`` is
  not ``NULL``.  If \* ``SEQ`` is ``NULL``, allocate a sequence before linking.

.. function:: void gimple_seq_add_seq (gimple_seq *dest, gimple_seq src)

  Append sequence ``SRC`` to the end of sequence \* ``DEST`` if ``SRC`` is not
  ``NULL``.  If \* ``DEST`` is ``NULL``, allocate a new sequence before
  appending.

.. function:: gimple_seq gimple_seq_deep_copy (gimple_seq src)

  Perform a deep copy of sequence ``SRC`` and return the result.

.. function:: gimple_seq gimple_seq_reverse (gimple_seq seq)

  Reverse the order of the statements in the sequence ``SEQ``.  Return
  ``SEQ``.

.. function:: gimple gimple_seq_first (gimple_seq s)

  Return the first statement in sequence ``S``.

.. function:: gimple gimple_seq_last (gimple_seq s)

  Return the last statement in sequence ``S``.

.. function:: void gimple_seq_set_last (gimple_seq s, gimple last)

  Set the last statement in sequence ``S`` to the statement in ``LAST``.

.. function:: void gimple_seq_set_first (gimple_seq s, gimple first)

  Set the first statement in sequence ``S`` to the statement in ``FIRST``.

.. function:: void gimple_seq_init (gimple_seq s)

  Initialize sequence ``S`` to an empty sequence.

.. function:: gimple_seq gimple_seq_alloc (void)

  Allocate a new sequence in the garbage collected store and return
  it.

.. function:: void gimple_seq_copy (gimple_seq dest, gimple_seq src)

  Copy the sequence ``SRC`` into the sequence ``DEST``.

.. function:: bool gimple_seq_empty_p (gimple_seq s)

  Return true if the sequence ``S`` is empty.

.. function:: gimple_seq bb_seq (basic_block bb)

  Returns the sequence of statements in ``BB``.

.. function:: void set_bb_seq (basic_block bb, gimple_seq seq)

  Sets the sequence of statements in ``BB`` to ``SEQ``.

.. function:: bool gimple_seq_singleton_p (gimple_seq seq)

  Determine whether ``SEQ`` contains exactly one statement.