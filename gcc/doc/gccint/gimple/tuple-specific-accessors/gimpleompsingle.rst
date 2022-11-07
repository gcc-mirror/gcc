..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GIMPLE_OMP_SINGLE

GIMPLE_OMP_SINGLE
^^^^^^^^^^^^^^^^^

.. function:: gomp_single *gimple_build_omp_single ( gimple_seq body, tree clauses)

  Build a ``GIMPLE_OMP_SINGLE`` statement. ``BODY`` is the sequence of
  statements that will be executed once.  ``CLAUSES`` are any of the
  ``OMP`` single construct's clauses: private, firstprivate,
  copyprivate, nowait.

.. function:: tree gimple_omp_single_clauses (gimple g)

  Return the clauses associated with ``OMP_SINGLE`` ``G``.

.. function:: tree * gimple_omp_single_clauses_ptr (gimple g)

  Return a pointer to the clauses associated with ``OMP_SINGLE`` ``G``.

.. function:: void gimple_omp_single_set_clauses ( gomp_single *g, tree clauses)

  Set ``CLAUSES`` to be the clauses associated with ``OMP_SINGLE`` ``G``.