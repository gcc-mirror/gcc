..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _qualifiers-implementation:

Qualifiers
**********

* What constitutes an access to an object that has volatile-qualified
  type (C90 6.5.3, C99 and C11 6.7.3).

  Such an object is normally accessed by pointers and used for accessing
  hardware.  In most expressions, it is intuitively obvious what is a read
  and what is a write.  For example

  .. code-block:: c++

    volatile int *dst = somevalue;
    volatile int *src = someothervalue;
    *dst = *src;

  will cause a read of the volatile object pointed to by :samp:`{src}` and store the
  value into the volatile object pointed to by :samp:`{dst}`.  There is no
  guarantee that these reads and writes are atomic, especially for objects
  larger than ``int``.

  However, if the volatile storage is not being modified, and the value of
  the volatile storage is not used, then the situation is less obvious.
  For example

  .. code-block:: c++

    volatile int *src = somevalue;
    *src;

  According to the C standard, such an expression is an rvalue whose type
  is the unqualified version of its original type, i.e. ``int``.  Whether
  GCC interprets this as a read of the volatile object being pointed to or
  only as a request to evaluate the expression for its side effects depends
  on this type.

  If it is a scalar type, or on most targets an aggregate type whose only
  member object is of a scalar type, or a union type whose member objects
  are of scalar types, the expression is interpreted by GCC as a read of
  the volatile object; in the other cases, the expression is only evaluated
  for its side effects.

  When an object of an aggregate type, with the same size and alignment as a
  scalar type ``S``, is the subject of a volatile access by an assignment
  expression or an atomic function, the access to it is performed as if the
  object's declared type were ``volatile S``.