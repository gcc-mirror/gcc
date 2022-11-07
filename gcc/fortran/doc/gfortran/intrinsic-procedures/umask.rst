..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: UMASK, file system, file creation mask

.. _umask:

UMASK --- Set the file creation mask
************************************

.. function:: UMASK(MASK)

  Sets the file creation mask to :samp:`{MASK}`. If called as a function, it
  returns the old value. If called as a subroutine and argument :samp:`{OLD}`
  if it is supplied, it is set to the old value. See ``umask(2)``.

  :param MASK:
    Shall be a scalar of type ``INTEGER``.

  :param OLD:
    (Optional) Shall be a scalar of type
    ``INTEGER``.

  Standard:
    GNU extension

  Class:
    Subroutine, function

  Syntax:
    .. code-block:: fortran

      CALL UMASK(MASK [, OLD])
      OLD = UMASK(MASK)