..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _compiler_version:

COMPILER_VERSION --- Compiler version string
********************************************

.. index:: COMPILER_VERSION, compiler, name and version, version of the compiler

.. function:: COMPILER_VERSION()

  ``COMPILER_VERSION`` returns a string with the name and the
  version of the compiler.

  :return:
    The return value is a default-kind string with system-dependent length.
    It contains the name of the compiler and its version number.

  Standard:
    Fortran 2008

  Class:
    Inquiry function of the module ``ISO_FORTRAN_ENV``

  Syntax:
    .. code-block:: fortran

      STR = COMPILER_VERSION()

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
    :ref:`COMPILER_OPTIONS`,
    :ref:`ISO_FORTRAN_ENV`
