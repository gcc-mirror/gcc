..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _structures-unions-enumerations-and-bit-fields-implementation:

Structures, Unions, Enumerations, and Bit-Fields
************************************************

* A member of a union object is accessed using a member of a
  different type (C90 6.3.2.3).

  The relevant bytes of the representation of the object are treated as
  an object of the type used for the access.  See :ref:`type-punning`.  This
  may be a trap representation.

* Whether a 'plain' ``int`` bit-field is treated as a
  ``signed int`` bit-field or as an ``unsigned int`` bit-field
  (C90 6.5.2, C90 6.5.2.1, C99 and C11 6.7.2, C99 and C11 6.7.2.1).

  .. index:: funsigned-bitfields

  By default it is treated as ``signed int`` but this may be changed
  by the :option:`-funsigned-bitfields` option.

* Allowable bit-field types other than ``_Bool``, ``signed int``,
  and ``unsigned int`` (C99 and C11 6.7.2.1).

  Other integer types, such as ``long int``, and enumerated types are
  permitted even in strictly conforming mode.

* Whether atomic types are permitted for bit-fields (C11 6.7.2.1).

  Atomic types are not permitted for bit-fields.

* Whether a bit-field can straddle a storage-unit boundary (C90
  6.5.2.1, C99 and C11 6.7.2.1).

  Determined by ABI.

* The order of allocation of bit-fields within a unit (C90
  6.5.2.1, C99 and C11 6.7.2.1).

  Determined by ABI.

* The alignment of non-bit-field members of structures (C90
  6.5.2.1, C99 and C11 6.7.2.1).

  Determined by ABI.

* The integer type compatible with each enumerated type (C90
  6.5.2.2, C99 and C11 6.7.2.2).

  .. index:: fshort-enums

  Normally, the type is ``unsigned int`` if there are no negative
  values in the enumeration, otherwise ``int``.  If
  :option:`-fshort-enums` is specified, then if there are negative values
  it is the first of ``signed char``, ``short`` and ``int``
  that can represent all the values, otherwise it is the first of
  ``unsigned char``, ``unsigned short`` and ``unsigned int``
  that can represent all the values.

  .. On a few unusual targets with 64-bit int, this doesn't agree with

  .. the code and one of the types accessed via mode attributes (which

  .. are not currently considered extended integer types) may be used.

  .. If these types are made extended integer types, it would still be

  .. the case that -fshort-enums stops the implementation from

  .. conforming to C90 on those targets.

  On some targets, :option:`-fshort-enums` is the default; this is
  determined by the ABI.
