..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GIMPLE_OMP_CRITICAL

GIMPLE_OMP_CRITICAL
^^^^^^^^^^^^^^^^^^^

.. function:: gomp_critical *gimple_build_omp_critical ( gimple_seq body, tree name)

  Build a ``GIMPLE_OMP_CRITICAL`` statement. ``BODY`` is the sequence of
  statements for which only one thread can execute.  ``NAME`` is an
  optional identifier for this critical block.

.. function:: tree gimple_omp_critical_name ( const gomp_critical *g)

  Return the name associated with ``OMP_CRITICAL`` statement ``G``.

.. function:: tree * gimple_omp_critical_name_ptr ( gomp_critical *g)

  Return a pointer to the name associated with ``OMP`` critical
  statement ``G``.

.. function:: void gimple_omp_critical_set_name ( gomp_critical *g, tree name)

  Set ``NAME`` to be the name associated with ``OMP`` critical statement ``G``.
