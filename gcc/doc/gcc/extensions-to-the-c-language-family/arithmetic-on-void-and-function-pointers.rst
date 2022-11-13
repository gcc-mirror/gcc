..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: void pointers, arithmetic, void, size of pointer to, function pointers, arithmetic, function, size of pointer to

.. _pointer-arith:

Arithmetic on void- and Function-Pointers
*****************************************

In GNU C, addition and subtraction operations are supported on pointers to
``void`` and on pointers to functions.  This is done by treating the
size of a ``void`` or of a function as 1.

A consequence of this is that ``sizeof`` is also allowed on ``void``
and on function types, and returns 1.

.. index:: Wpointer-arith

The option :option:`-Wpointer-arith` requests a warning if these extensions
are used.