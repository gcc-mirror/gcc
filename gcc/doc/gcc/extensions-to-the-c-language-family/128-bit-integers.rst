..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: __int128 data types

.. _int128:

128-bit Integers
****************

As an extension the integer scalar type ``__int128`` is supported for
targets which have an integer mode wide enough to hold 128 bits.
Simply write ``__int128`` for a signed 128-bit integer, or
``unsigned __int128`` for an unsigned 128-bit integer.  There is no
support in GCC for expressing an integer constant of type ``__int128``
for targets with ``long long`` integer less than 128 bits wide.