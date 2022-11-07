..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: SAME_TYPE_AS

.. _same_type_as:

SAME_TYPE_AS ---  Query dynamic types for equality
**************************************************

.. function:: SAME_TYPE_AS(A, B)

  Query dynamic types for equality.

  :param A:
    Shall be an object of extensible declared type or
    unlimited polymorphic.

  :param B:
    Shall be an object of extensible declared type or
    unlimited polymorphic.

  :return:
    The return value is a scalar of type default logical. It is true if and
    only if the dynamic type of A is the same as the dynamic type of B.

  Standard:
    Fortran 2003 and later

  Class:
    Inquiry function

  Syntax:
    .. code-block:: fortran

      RESULT = SAME_TYPE_AS(A, B)

  See also:
    :ref:`EXTENDS_TYPE_OF`