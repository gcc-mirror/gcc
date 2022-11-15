..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: anchored addresses, -fsection-anchors

.. _anchored-addresses:

Anchored Addresses
******************

GCC usually addresses every static object as a separate entity.
For example, if we have:

.. code-block:: c++

  static int a, b, c;
  int foo (void) { return a + b + c; }

the code for ``foo`` will usually calculate three separate symbolic
addresses: those of ``a``, ``b`` and ``c``.  On some targets,
it would be better to calculate just one symbolic address and access
the three variables relative to it.  The equivalent pseudocode would
be something like:

.. code-block:: c++

  int foo (void)
  {
    register int *xr = &x;
    return xr[&a - &x] + xr[&b - &x] + xr[&c - &x];
  }

(which isn't valid C).  We refer to shared addresses like ``x`` as
'section anchors'.  Their use is controlled by :option:`-fsection-anchors`.

The hooks below describe the target properties that GCC needs to know
in order to make effective use of section anchors.  It won't use
section anchors at all unless either ``TARGET_MIN_ANCHOR_OFFSET``
or ``TARGET_MAX_ANCHOR_OFFSET`` is set to a nonzero value.

.. include:: tm.rst.in
  :start-after: [TARGET_MIN_ANCHOR_OFFSET]
  :end-before: [TARGET_MIN_ANCHOR_OFFSET]


.. include:: tm.rst.in
  :start-after: [TARGET_MAX_ANCHOR_OFFSET]
  :end-before: [TARGET_MAX_ANCHOR_OFFSET]


.. include:: tm.rst.in
  :start-after: [TARGET_ASM_OUTPUT_ANCHOR]
  :end-before: [TARGET_ASM_OUTPUT_ANCHOR]


.. include:: tm.rst.in
  :start-after: [TARGET_USE_ANCHORS_FOR_SYMBOL_P]
  :end-before: [TARGET_USE_ANCHORS_FOR_SYMBOL_P]
