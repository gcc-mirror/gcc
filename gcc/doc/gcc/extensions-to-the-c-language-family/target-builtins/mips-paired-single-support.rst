..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _mips-paired-single-support:

MIPS Paired-Single Support
^^^^^^^^^^^^^^^^^^^^^^^^^^

The MIPS64 architecture includes a number of instructions that
operate on pairs of single-precision floating-point values.
Each pair is packed into a 64-bit floating-point register,
with one element being designated the 'upper half' and
the other being designated the 'lower half'.

GCC supports paired-single operations using both the generic
vector extensions (see :ref:`vector-extensions`) and a collection of
MIPS-specific built-in functions.  Both kinds of support are
enabled by the :option:`-mpaired-single` command-line option.

The vector type associated with paired-single values is usually
called ``v2sf``.  It can be defined in C as follows:

.. code-block:: c++

  typedef float v2sf __attribute__ ((vector_size (8)));

``v2sf`` values are initialized in the same way as aggregates.
For example:

.. code-block:: c++

  v2sf a = {1.5, 9.1};
  v2sf b;
  float e, f;
  b = (v2sf) {e, f};

.. note::
  The CPU's endianness determines which value is stored in
  the upper half of a register and which value is stored in the lower half.
  On little-endian targets, the first value is the lower one and the second
  value is the upper one.  The opposite order applies to big-endian targets.
  For example, the code above sets the lower half of ``a`` to
  ``1.5`` on little-endian targets and ``9.1`` on big-endian targets.
