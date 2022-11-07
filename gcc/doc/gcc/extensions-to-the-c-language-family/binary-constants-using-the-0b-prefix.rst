..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Binary constants using the 0b prefix

.. _binary-constants:

Binary Constants using the 0b Prefix
************************************

Integer constants can be written as binary constants, consisting of a
sequence of :samp:`0` and :samp:`1` digits, prefixed by :samp:`0b` or
:samp:`0B`.  This is particularly useful in environments that operate a
lot on the bit level (like microcontrollers).

The following statements are identical:

.. code-block:: c++

  i =       42;
  i =     0x2a;
  i =      052;
  i = 0b101010;

The type of these constants follows the same rules as for octal or
hexadecimal integer constants, so suffixes like :samp:`L` or :samp:`UL`
can be applied.