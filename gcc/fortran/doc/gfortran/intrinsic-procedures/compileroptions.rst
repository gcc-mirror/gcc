..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _compiler_options:

COMPILER_OPTIONS --- Options passed to the compiler
***************************************************

.. index:: COMPILER_OPTIONS, flags inquiry function, options inquiry function, compiler flags inquiry function

.. function:: COMPILER_OPTIONS()

  ``COMPILER_OPTIONS`` returns a string with the options used for
  compiling.

  :return:
    The return value is a default-kind string with system-dependent length.
    It contains the compiler flags used to compile the file, which called
    the ``COMPILER_OPTIONS`` intrinsic.

  Standard:
    Fortran 2008

  Class:
    Inquiry function of the module ``ISO_FORTRAN_ENV``

  Syntax:
    .. code-block:: fortran

      STR = COMPILER_OPTIONS()

  Arguments:
    None

  Example:
    .. code-block:: fortran

         use iso_fortran_env
         print '(4a)', 'This file was compiled by ', &
                       compiler_version(), ' using the options ', &
                       compiler_options()
         end

  See also:
    :ref:`COMPILER_VERSION`,
    :ref:`ISO_FORTRAN_ENV`
