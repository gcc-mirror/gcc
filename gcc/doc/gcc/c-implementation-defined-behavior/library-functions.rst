..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _library-functions-implementation:

Library Functions
*****************

The behavior of most of these points are dependent on the implementation
of the C library, and are not defined by GCC itself.

* The null pointer constant to which the macro ``NULL`` expands
  (C90 7.1.6, C99 7.17, C11 7.19).

  In ``<stddef.h>``, ``NULL`` expands to ``((void *)0)``.  GCC
  does not provide the other headers which define ``NULL`` and some
  library implementations may use other definitions in those headers.