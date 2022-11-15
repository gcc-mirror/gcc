..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GIMPLE_OMP_PARALLEL

GIMPLE_OMP_PARALLEL
^^^^^^^^^^^^^^^^^^^

.. function:: gomp_parallel *gimple_build_omp_parallel (gimple_seq body, tree clauses, tree child_fn, tree data_arg)

  Build a ``GIMPLE_OMP_PARALLEL`` statement.

  ``BODY`` is sequence of statements which are executed in parallel.
  ``CLAUSES``, are the ``OMP`` parallel construct's clauses.  ``CHILD_FN`` is
  the function created for the parallel threads to execute.
  ``DATA_ARG`` are the shared data argument(s).

.. function:: bool gimple_omp_parallel_combined_p (gimple g)

  Return true if ``OMP`` parallel statement ``G`` has the
  ``GF_OMP_PARALLEL_COMBINED`` flag set.

.. function:: void gimple_omp_parallel_set_combined_p (gimple g)

  Set the ``GF_OMP_PARALLEL_COMBINED`` field in ``OMP`` parallel statement
  ``G``.

.. function:: gimple_seq gimple_omp_body (gimple g)

  Return the body for the ``OMP`` statement ``G``.

.. function:: void gimple_omp_set_body (gimple g, gimple_seq body)

  Set ``BODY`` to be the body for the ``OMP`` statement ``G``.

.. function:: tree gimple_omp_parallel_clauses (gimple g)

  Return the clauses associated with ``OMP_PARALLEL`` ``G``.

.. function:: tree * gimple_omp_parallel_clauses_ptr ( gomp_parallel *g)

  Return a pointer to the clauses associated with ``OMP_PARALLEL`` ``G``.

.. function:: void gimple_omp_parallel_set_clauses ( gomp_parallel *g, tree clauses)

  Set ``CLAUSES`` to be the list of clauses associated with
  ``OMP_PARALLEL`` ``G``.

.. function:: tree gimple_omp_parallel_child_fn ( const gomp_parallel *g)

  Return the child function used to hold the body of ``OMP_PARALLEL``
  ``G``.

.. function:: tree * gimple_omp_parallel_child_fn_ptr ( gomp_parallel *g)

  Return a pointer to the child function used to hold the body of
  ``OMP_PARALLEL`` ``G``.

.. function:: void gimple_omp_parallel_set_child_fn ( gomp_parallel *g, tree child_fn)

  Set ``CHILD_FN`` to be the child function for ``OMP_PARALLEL`` ``G``.

.. function:: tree gimple_omp_parallel_data_arg ( const gomp_parallel *g)

  Return the artificial argument used to send variables and values
  from the parent to the children threads in ``OMP_PARALLEL`` ``G``.

.. function:: tree * gimple_omp_parallel_data_arg_ptr ( gomp_parallel *g)

  Return a pointer to the data argument for ``OMP_PARALLEL`` ``G``.

.. function:: void gimple_omp_parallel_set_data_arg ( gomp_parallel *g, tree data_arg)

  Set ``DATA_ARG`` to be the data argument for ``OMP_PARALLEL`` ``G``.
