..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GIMPLE_OMP_SECTION

GIMPLE_OMP_SECTION
^^^^^^^^^^^^^^^^^^

.. function:: gimple gimple_build_omp_section (gimple_seq body)

  Build a ``GIMPLE_OMP_SECTION`` statement for a sections statement.

  ``BODY`` is the sequence of statements in the section.

.. function:: bool gimple_omp_section_last_p (gimple g)

  Return true if ``OMP`` section statement ``G`` has the
  ``GF_OMP_SECTION_LAST`` flag set.

.. function:: void gimple_omp_section_set_last (gimple g)

  Set the ``GF_OMP_SECTION_LAST`` flag on ``G``.
