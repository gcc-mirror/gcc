..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: pointer arguments in variadic functions, variadic functions, pointer arguments

.. _variadic-pointer-args:

Pointer Arguments in Variadic Functions
***************************************

Standard C requires that pointer types used with ``va_arg`` in
functions with variable argument lists either must be compatible with
that of the actual argument, or that one type must be a pointer to
``void`` and the other a pointer to a character type.  GNU C
implements the POSIX XSI extension that additionally permits the use
of ``va_arg`` with a pointer type to receive arguments of any other
pointer type.

In particular, in GNU C :samp:`va_arg (ap, void *)` can safely be used
to consume an argument of any pointer type.