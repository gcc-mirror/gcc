..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GIMPLE_OMP_MASTER

GIMPLE_OMP_MASTER
^^^^^^^^^^^^^^^^^

.. function:: gimple gimple_build_omp_master (gimple_seq body)

  Build a ``GIMPLE_OMP_MASTER`` statement. ``BODY`` is the sequence of
  statements to be executed by just the master.