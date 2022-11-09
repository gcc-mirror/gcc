..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _arrays-and-pointers-implementation:

Arrays and Pointers
*******************

* The result of converting a pointer to an integer or
  vice versa (C90 6.3.4, C99 and C11 6.3.2.3).

  A cast from pointer to integer discards most-significant bits if the
  pointer representation is larger than the integer type,
  sign-extends ([#f1]_) if the pointer representation is smaller than the integer type, otherwise
  the bits are unchanged.

  .. ??? We've always claimed that pointers were unsigned entities.

  .. Shouldn't we therefore be doing zero-extension?  If so, the bug

  .. is in convert_to_integer, where we call type_for_size and request

  .. a signed integral type.  On the other hand, it might be most useful

  .. for the target if we extend according to POINTERS_EXTEND_UNSIGNED.

  A cast from integer to pointer discards most-significant bits if the
  pointer representation is smaller than the integer type, extends according
  to the signedness of the integer type if the pointer representation
  is larger than the integer type, otherwise the bits are unchanged.

  When casting from pointer to integer and back again, the resulting
  pointer must reference the same object as the original pointer, otherwise
  the behavior is undefined.  That is, one may not use integer arithmetic to
  avoid the undefined behavior of pointer arithmetic as proscribed in
  C99 and C11 6.5.6/8.

* The size of the result of subtracting two pointers to elements
  of the same array (C90 6.3.6, C99 and C11 6.5.6).

  The value is as specified in the standard and the type is determined
  by the ABI.

.. [#f1] Future versions of GCC may zero-extend, or use a target-defined ``ptr_extend`` pattern.  Do not rely on sign extension.
