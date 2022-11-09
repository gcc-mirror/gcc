..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GIMPLE_OMP_FOR

GIMPLE_OMP_FOR
^^^^^^^^^^^^^^

.. function:: gomp_for *gimple_build_omp_for (gimple_seq body, tree clauses, tree index, tree initial, tree final, tree incr, gimple_seq pre_body, enum tree_code omp_for_cond)

  Build a ``GIMPLE_OMP_FOR`` statement. ``BODY`` is sequence of statements
  inside the for loop.  ``CLAUSES``, are any of the loop
  construct's clauses.  ``PRE_BODY`` is the
  sequence of statements that are loop invariant.  ``INDEX`` is the
  index variable.  ``INITIAL`` is the initial value of ``INDEX``.  ``FINAL`` is
  final value of ``INDEX``.  OMP_FOR_COND is the predicate used to
  compare ``INDEX`` and ``FINAL``.  ``INCR`` is the increment expression.

.. function:: tree gimple_omp_for_clauses (gimple g)

  Return the clauses associated with ``OMP_FOR`` ``G``.

.. function:: tree * gimple_omp_for_clauses_ptr (gimple g)

  Return a pointer to the ``OMP_FOR`` ``G``.

.. function:: void gimple_omp_for_set_clauses (gimple g, tree clauses)

  Set ``CLAUSES`` to be the list of clauses associated with ``OMP_FOR`` ``G``.

.. function:: tree gimple_omp_for_index (gimple g)

  Return the index variable for ``OMP_FOR`` ``G``.

.. function:: tree * gimple_omp_for_index_ptr (gimple g)

  Return a pointer to the index variable for ``OMP_FOR`` ``G``.

.. function:: void gimple_omp_for_set_index (gimple g, tree index)

  Set ``INDEX`` to be the index variable for ``OMP_FOR`` ``G``.

.. function:: tree gimple_omp_for_initial (gimple g)

  Return the initial value for ``OMP_FOR`` ``G``.

.. function:: tree * gimple_omp_for_initial_ptr (gimple g)

  Return a pointer to the initial value for ``OMP_FOR`` ``G``.

.. function:: void gimple_omp_for_set_initial (gimple g, tree initial)

  Set ``INITIAL`` to be the initial value for ``OMP_FOR`` ``G``.

.. function:: tree gimple_omp_for_final (gimple g)

  Return the final value for ``OMP_FOR`` ``G``.

.. function:: tree * gimple_omp_for_final_ptr (gimple g)

  turn a pointer to the final value for ``OMP_FOR`` ``G``.

.. function:: void gimple_omp_for_set_final (gimple g, tree final)

  Set ``FINAL`` to be the final value for ``OMP_FOR`` ``G``.

.. function:: tree gimple_omp_for_incr (gimple g)

  Return the increment value for ``OMP_FOR`` ``G``.

.. function:: tree * gimple_omp_for_incr_ptr (gimple g)

  Return a pointer to the increment value for ``OMP_FOR`` ``G``.

.. function:: void gimple_omp_for_set_incr (gimple g, tree incr)

  Set ``INCR`` to be the increment value for ``OMP_FOR`` ``G``.

.. function:: gimple_seq gimple_omp_for_pre_body (gimple g)

  Return the sequence of statements to execute before the ``OMP_FOR``
  statement ``G`` starts.

.. function:: void gimple_omp_for_set_pre_body (gimple g, gimple_seq pre_body)

  Set ``PRE_BODY`` to be the sequence of statements to execute before
  the ``OMP_FOR`` statement ``G`` starts.

.. function:: void gimple_omp_for_set_cond (gimple g, enum tree_code cond)

  Set ``COND`` to be the condition code for ``OMP_FOR`` ``G``.

.. function:: enum tree_code gimple_omp_for_cond (gimple g)

  Return the condition code associated with ``OMP_FOR`` ``G``.
