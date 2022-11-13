..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _is_iostat_eor:

IS_IOSTAT_EOR --- Test for end-of-record value
**********************************************

.. index:: IS_IOSTAT_EOR, IOSTAT, end of record

.. function:: IS_IOSTAT_EOR(I)

  ``IS_IOSTAT_EOR`` tests whether an variable has the value of the I/O
  status 'end of record'. The function is equivalent to comparing the
  variable with the ``IOSTAT_EOR`` parameter of the intrinsic module
  ``ISO_FORTRAN_ENV``.

  :param I:
    Shall be of the type ``INTEGER``.

  :return:
    Returns a ``LOGICAL`` of the default kind, which ``.TRUE.`` if
    :samp:`{I}` has the value which indicates an end of file condition for
    ``IOSTAT=`` specifiers, and is ``.FALSE.`` otherwise.

  Standard:
    Fortran 2003 and later

  Class:
    Elemental function

  Syntax:
    .. code-block:: fortran

      RESULT = IS_IOSTAT_EOR(I)

  Example:
    .. code-block:: fortran

      PROGRAM iostat
        IMPLICIT NONE
        INTEGER :: stat, i(50)
        OPEN(88, FILE='test.dat', FORM='UNFORMATTED')
        READ(88, IOSTAT=stat) i
        IF(IS_IOSTAT_EOR(stat)) STOP 'END OF RECORD'
      END PROGRAM