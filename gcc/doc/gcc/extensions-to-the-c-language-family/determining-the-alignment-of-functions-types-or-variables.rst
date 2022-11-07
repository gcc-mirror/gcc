..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: alignment, type alignment, variable alignment

.. _alignment:

Determining the Alignment of Functions, Types or Variables
**********************************************************

The keyword ``__alignof__`` determines the alignment requirement of
a function, object, or a type, or the minimum alignment usually required
by a type.  Its syntax is just like ``sizeof`` and C11 ``_Alignof``.

For example, if the target machine requires a ``double`` value to be
aligned on an 8-byte boundary, then ``__alignof__ (double)`` is 8.
This is true on many RISC machines.  On more traditional machine
designs, ``__alignof__ (double)`` is 4 or even 2.

Some machines never actually require alignment; they allow references to any
data type even at an odd address.  For these machines, ``__alignof__``
reports the smallest alignment that GCC gives the data type, usually as
mandated by the target ABI.

If the operand of ``__alignof__`` is an lvalue rather than a type,
its value is the required alignment for its type, taking into account
any minimum alignment specified by attribute :var-attr:`aligned`
(see :ref:`common-variable-attributes`).  For example, after this
declaration:

.. code-block:: c++

  struct foo { int x; char y; } foo1;

the value of ``__alignof__ (foo1.y)`` is 1, even though its actual
alignment is probably 2 or 4, the same as ``__alignof__ (int)``.
It is an error to ask for the alignment of an incomplete type other
than ``void``.

If the operand of the ``__alignof__`` expression is a function,
the expression evaluates to the alignment of the function which may
be specified by attribute :fn-attr:`aligned` (see :ref:`common-function-attributes`).