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

.. c:var:: HOST_WIDE_INT TARGET_MIN_ANCHOR_OFFSET

  .. hook-start:TARGET_MIN_ANCHOR_OFFSET

  The minimum offset that should be applied to a section anchor.
  On most targets, it should be the smallest offset that can be
  applied to a base register while still giving a legitimate address
  for every mode.  The default value is 0.

.. hook-end

.. c:var:: HOST_WIDE_INT TARGET_MAX_ANCHOR_OFFSET

  .. hook-start:TARGET_MAX_ANCHOR_OFFSET

  Like ``TARGET_MIN_ANCHOR_OFFSET``, but the maximum (inclusive)
  offset that should be applied to section anchors.  The default
  value is 0.

.. hook-end

.. function:: void TARGET_ASM_OUTPUT_ANCHOR (rtx x)

  .. hook-start:TARGET_ASM_OUTPUT_ANCHOR

  Write the assembly code to define section anchor :samp:`{x}`, which is a
  ``SYMBOL_REF`` for which :samp:`SYMBOL_REF_ANCHOR_P ({x})` is true.
  The hook is called with the assembly output position set to the beginning
  of ``SYMBOL_REF_BLOCK (x)``.

  If ``ASM_OUTPUT_DEF`` is available, the hook's default definition uses
  it to define the symbol as :samp:`. + SYMBOL_REF_BLOCK_OFFSET ({x})`.
  If ``ASM_OUTPUT_DEF`` is not available, the hook's default definition
  is ``NULL``, which disables the use of section anchors altogether.

.. hook-end

.. function:: bool TARGET_USE_ANCHORS_FOR_SYMBOL_P (const_rtx x)

  .. hook-start:TARGET_USE_ANCHORS_FOR_SYMBOL_P

  Return true if GCC should attempt to use anchors to access ``SYMBOL_REF``
  :samp:`{x}`.  You can assume :samp:`SYMBOL_REF_HAS_BLOCK_INFO_P ({x})` and
  :samp:`!SYMBOL_REF_ANCHOR_P ({x})`.

  The default version is correct for most targets, but you might need to
  intercept this hook to handle things like target-specific attributes
  or target-specific sections.

.. hook-end