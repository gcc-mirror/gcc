..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GIMPLE_OMP_CONTINUE

GIMPLE_OMP_CONTINUE
^^^^^^^^^^^^^^^^^^^

.. function:: gomp_continue *gimple_build_omp_continue ( tree control_def, tree control_use)

  Build a ``GIMPLE_OMP_CONTINUE`` statement.  ``CONTROL_DEF`` is the
  definition of the control variable.  ``CONTROL_USE`` is the use of
  the control variable.

.. function:: tree gimple_omp_continue_control_def ( const gomp_continue *s)

  Return the definition of the control variable on a
  ``GIMPLE_OMP_CONTINUE`` in ``S``.

.. function:: tree gimple_omp_continue_control_def_ptr ( gomp_continue *s)

  Same as above, but return the pointer.

.. function:: tree gimple_omp_continue_set_control_def ( gomp_continue *s)

  Set the control variable definition for a ``GIMPLE_OMP_CONTINUE``
  statement in ``S``.

.. function:: tree gimple_omp_continue_control_use ( const gomp_continue *s)

  Return the use of the control variable on a ``GIMPLE_OMP_CONTINUE``
  in ``S``.

.. function:: tree gimple_omp_continue_control_use_ptr ( gomp_continue *s)

  Same as above, but return the pointer.

.. function:: tree gimple_omp_continue_set_control_use ( gomp_continue *s)

  Set the control variable use for a ``GIMPLE_OMP_CONTINUE`` statement
  in ``S``.
