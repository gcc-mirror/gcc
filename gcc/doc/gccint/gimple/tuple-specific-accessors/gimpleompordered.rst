..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GIMPLE_OMP_ORDERED

GIMPLE_OMP_ORDERED
^^^^^^^^^^^^^^^^^^

.. function:: gimple gimple_build_omp_ordered (gimple_seq body)

  Build a ``GIMPLE_OMP_ORDERED`` statement.

  ``BODY`` is the sequence of statements inside a loop that will
  executed in sequence.
