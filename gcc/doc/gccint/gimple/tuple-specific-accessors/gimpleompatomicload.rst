..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GIMPLE_OMP_ATOMIC_LOAD

GIMPLE_OMP_ATOMIC_LOAD
^^^^^^^^^^^^^^^^^^^^^^

.. function:: gomp_atomic_load *gimple_build_omp_atomic_load ( tree lhs, tree rhs)

  Build a ``GIMPLE_OMP_ATOMIC_LOAD`` statement.  ``LHS`` is the left-hand
  side of the assignment.  ``RHS`` is the right-hand side of the
  assignment.

.. function:: void gimple_omp_atomic_load_set_lhs ( gomp_atomic_load *g, tree lhs)

  Set the ``LHS`` of an atomic load.

.. function:: tree gimple_omp_atomic_load_lhs ( const gomp_atomic_load *g)

  Get the ``LHS`` of an atomic load.

.. function:: void gimple_omp_atomic_load_set_rhs ( gomp_atomic_load *g, tree rhs)

  Set the ``RHS`` of an atomic set.

.. function:: tree gimple_omp_atomic_load_rhs ( const gomp_atomic_load *g)

  Get the ``RHS`` of an atomic set.
