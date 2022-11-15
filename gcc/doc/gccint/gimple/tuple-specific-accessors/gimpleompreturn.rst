..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GIMPLE_OMP_RETURN

GIMPLE_OMP_RETURN
^^^^^^^^^^^^^^^^^

.. function:: gimple gimple_build_omp_return (bool wait_p)

  Build a ``GIMPLE_OMP_RETURN`` statement. ``WAIT_P`` is true if this is a
  non-waiting return.

.. function:: void gimple_omp_return_set_nowait (gimple s)

  Set the nowait flag on ``GIMPLE_OMP_RETURN`` statement ``S``.

.. function:: bool gimple_omp_return_nowait_p (gimple g)

  Return true if ``OMP`` return statement ``G`` has the
  ``GF_OMP_RETURN_NOWAIT`` flag set.
