..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: restricted pointers, restricted references, restricted this pointer

.. _restricted-pointers:

Restricting Pointer Aliasing
****************************

As with the C front end, G++ understands the C99 feature of restricted pointers,
specified with the ``__restrict__``, or ``__restrict`` type
qualifier.  Because you cannot compile C++ by specifying the :option:`-std=c99`
language flag, ``restrict`` is not a keyword in C++.

In addition to allowing restricted pointers, you can specify restricted
references, which indicate that the reference is not aliased in the local
context.

.. code-block:: c++

  void fn (int *__restrict__ rptr, int &__restrict__ rref)
  {
    /* ... */
  }

In the body of ``fn``, :samp:`{rptr}` points to an unaliased integer and
:samp:`{rref}` refers to a (different) unaliased integer.

You may also specify whether a member function's :samp:`{this}` pointer is
unaliased by using ``__restrict__`` as a member function qualifier.

.. code-block:: c++

  void T::fn () __restrict__
  {
    /* ... */
  }

Within the body of ``T::fn``, :samp:`{this}` has the effective
definition ``T *__restrict__ const this``.  Notice that the
interpretation of a ``__restrict__`` member function qualifier is
different to that of ``const`` or ``volatile`` qualifier, in that it
is applied to the pointer rather than the object.  This is consistent with
other compilers that implement restricted pointers.

As with all outermost parameter qualifiers, ``__restrict__`` is
ignored in function definition matching.  This means you only need to
specify ``__restrict__`` in a function definition, rather than
in a function prototype as well.