..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GIMPLE_OMP_ATOMIC_STORE

GIMPLE_OMP_ATOMIC_STORE
^^^^^^^^^^^^^^^^^^^^^^^

.. function:: gomp_atomic_store *gimple_build_omp_atomic_store ( tree val)

  Build a ``GIMPLE_OMP_ATOMIC_STORE`` statement. ``VAL`` is the value to be
  stored.

.. function:: void gimple_omp_atomic_store_set_val ( gomp_atomic_store *g, tree val)

  Set the value being stored in an atomic store.

.. function:: tree gimple_omp_atomic_store_val ( const gomp_atomic_store *g)

  Return the value being stored in an atomic store.
