..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: GIMPLE_OMP_SECTIONS

GIMPLE_OMP_SECTIONS
^^^^^^^^^^^^^^^^^^^

.. function:: gomp_sections *gimple_build_omp_sections ( gimple_seq body, tree clauses)

  Build a ``GIMPLE_OMP_SECTIONS`` statement. ``BODY`` is a sequence of
  section statements.  ``CLAUSES`` are any of the ``OMP`` sections
  construct's clauses: private, firstprivate, lastprivate,
  reduction, and nowait.

.. function:: gimple gimple_build_omp_sections_switch (void)

  Build a ``GIMPLE_OMP_SECTIONS_SWITCH`` statement.

.. function:: tree gimple_omp_sections_control (gimple g)

  Return the control variable associated with the
  ``GIMPLE_OMP_SECTIONS`` in ``G``.

.. function:: tree * gimple_omp_sections_control_ptr (gimple g)

  Return a pointer to the clauses associated with the
  ``GIMPLE_OMP_SECTIONS`` in ``G``.

.. function:: void gimple_omp_sections_set_control (gimple g, tree control)

  Set ``CONTROL`` to be the set of clauses associated with the
  ``GIMPLE_OMP_SECTIONS`` in ``G``.

.. function:: tree gimple_omp_sections_clauses (gimple g)

  Return the clauses associated with ``OMP_SECTIONS`` ``G``.

.. function:: tree * gimple_omp_sections_clauses_ptr (gimple g)

  Return a pointer to the clauses associated with ``OMP_SECTIONS`` ``G``.

.. function:: void gimple_omp_sections_set_clauses (gimple g, tree clauses)

  Set ``CLAUSES`` to be the set of clauses associated with ``OMP_SECTIONS``
  ``G``.