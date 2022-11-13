..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: bit-fields

.. _bit-fields:

Bit-Fields
**********

Special expression codes exist to represent bit-field instructions.

.. index:: sign_extract, BITS_BIG_ENDIAN, effect on sign_extract

:samp:`(sign_extract:{m} {loc} {size} {pos})`
  This represents a reference to a sign-extended bit-field contained or
  starting in :samp:`{loc}` (a memory or register reference).  The bit-field
  is :samp:`{size}` bits wide and starts at bit :samp:`{pos}`.  The compilation
  option ``BITS_BIG_ENDIAN`` says which end of the memory unit
  :samp:`{pos}` counts from.

  If :samp:`{loc}` is in memory, its mode must be a single-byte integer mode.
  If :samp:`{loc}` is in a register, the mode to use is specified by the
  operand of the ``insv`` or ``extv`` pattern
  (see :ref:`standard-names`) and is usually a full-word integer mode,
  which is the default if none is specified.

  The mode of :samp:`{pos}` is machine-specific and is also specified
  in the ``insv`` or ``extv`` pattern.

  The mode :samp:`{m}` is the same as the mode that would be used for
  :samp:`{loc}` if it were a register.

  A ``sign_extract`` cannot appear as an lvalue, or part thereof,
  in RTL.

  .. index:: zero_extract

:samp:`(zero_extract:{m} {loc} {size} {pos})`
  Like ``sign_extract`` but refers to an unsigned or zero-extended
  bit-field.  The same sequence of bits are extracted, but they
  are filled to an entire word with zeros instead of by sign-extension.

  Unlike ``sign_extract``, this type of expressions can be lvalues
  in RTL; they may appear on the left side of an assignment, indicating
  insertion of a value into the specified bit-field.