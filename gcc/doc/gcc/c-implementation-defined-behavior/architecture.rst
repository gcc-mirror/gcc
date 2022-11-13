..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _architecture-implementation:

Architecture
************

* The values or expressions assigned to the macros specified in the
  headers ``<float.h>``, ``<limits.h>``, and ``<stdint.h>``
  (C90, C99 and C11 5.2.4.2, C99 7.18.2, C99 7.18.3, C11 7.20.2, C11 7.20.3).

  Determined by ABI.

* The result of attempting to indirectly access an object with
  automatic or thread storage duration from a thread other than the one
  with which it is associated (C11 6.2.4).

  Such accesses are supported, subject to the same requirements for
  synchronization for concurrent accesses as for concurrent accesses to
  any object.

* The number, order, and encoding of bytes in any object
  (when not explicitly specified in this International Standard) (C99
  and C11 6.2.6.1).

  Determined by ABI.

* Whether any extended alignments are supported and the contexts
  in which they are supported (C11 6.2.8).

  Extended alignments up to 2^{28} (bytes) are supported for
  objects of automatic storage duration.  Alignments supported for
  objects of static and thread storage duration are determined by the
  ABI.

* Valid alignment values other than those returned by an _Alignof
  expression for fundamental types, if any (C11 6.2.8).

  Valid alignments are powers of 2 up to and including 2^{28}.

* The value of the result of the ``sizeof`` and ``_Alignof``
  operators (C90 6.3.3.4, C99 and C11 6.5.3.4).

  Determined by ABI.