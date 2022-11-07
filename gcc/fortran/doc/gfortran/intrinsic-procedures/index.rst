..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _index-intrinsic:

.. index:: INDEX

.. index:: substring position

.. index:: string, find substring

INDEX --- Position of a substring within a string
*************************************************

.. function:: INDEX(STRING, SUBSTRING , BACK , KIND)

  Returns the position of the start of the first occurrence of string
  :samp:`{SUBSTRING}` as a substring in :samp:`{STRING}`, counting from one.  If
  :samp:`{SUBSTRING}` is not present in :samp:`{STRING}`, zero is returned.  If
  the :samp:`{BACK}` argument is present and true, the return value is the
  start of the last occurrence rather than the first.

  :param STRING:
    Shall be a scalar ``CHARACTER``, with
    ``INTENT(IN)``

  :param SUBSTRING:
    Shall be a scalar ``CHARACTER``, with
    ``INTENT(IN)``

  :param BACK:
    (Optional) Shall be a scalar ``LOGICAL``, with
    ``INTENT(IN)``

  :param KIND:
    (Optional) An ``INTEGER`` initialization
    expression indicating the kind parameter of the result.

  :return:
    The return value is of type ``INTEGER`` and of kind :samp:`{KIND}`. If
    :samp:`{KIND}` is absent, the return value is of default integer kind.

  Standard:
    Fortran 77 and later, with :samp:`{KIND}` argument Fortran 2003 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = INDEX(STRING, SUBSTRING [, BACK [, KIND]])

  Specific names:
    .. list-table::
       :header-rows: 1

       * - Name
         - Argument
         - Return type
         - Standard

       * - ``INDEX(STRING,SUBSTRING)``
         - ``CHARACTER``
         - ``INTEGER(4)``
         - Fortran 77 and later

  See also:
    :ref:`SCAN`,
    :ref:`VERIFY`