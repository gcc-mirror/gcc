..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _identifiers-implementation:

Identifiers
***********

* Which additional multibyte characters may appear in identifiers
  and their correspondence to universal character names (C99 and C11 6.4.2).

  See :ref:`cpp:implementation-defined-behavior`.

* The number of significant initial characters in an identifier
  (C90 6.1.2, C90, C99 and C11 5.2.4.1, C99 and C11 6.4.2).

  For internal names, all characters are significant.  For external names,
  the number of significant characters are defined by the linker; for
  almost all targets, all characters are significant.

* Whether case distinctions are significant in an identifier with
  external linkage (C90 6.1.2).

  This is a property of the linker.  C99 and C11 require that case distinctions
  are always significant in identifiers with external linkage and
  systems without this property are not supported by GCC.
