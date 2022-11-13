..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: memory model

.. _memory-model:

Memory model
************

The memory model used by the middle-end models that of the C/C++
languages.  The middle-end has the notion of an effective type
of a memory region which is used for type-based alias analysis.

The following is a refinement of ISO C99 6.5/6, clarifying the block copy case
to follow common sense and extending the concept of a dynamic effective
type to objects with a declared type as required for C++.

::

  The effective type of an object for an access to its stored value is
  the declared type of the object or the effective type determined by
  a previous store to it.  If a value is stored into an object through
  an lvalue having a type that is not a character type, then the
  type of the lvalue becomes the effective type of the object for that
  access and for subsequent accesses that do not modify the stored value.
  If a value is copied into an object using memcpy or memmove,
  or is copied as an array of character type, then the effective type
  of the modified object for that access and for subsequent accesses that
  do not modify the value is undetermined.  For all other accesses to an
  object, the effective type of the object is simply the type of the
  lvalue used for the access.