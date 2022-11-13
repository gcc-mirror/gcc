..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: subscripting, arrays, non-lvalue, subscripting and function values

.. _subscripting:

Non-Lvalue Arrays May Have Subscripts
*************************************

In ISO C99, arrays that are not lvalues still decay to pointers, and
may be subscripted, although they may not be modified or used after
the next sequence point and the unary :samp:`&` operator may not be
applied to them.  As an extension, GNU C allows such arrays to be
subscripted in C90 mode, though otherwise they do not decay to
pointers outside C99 mode.  For example,
this is valid in GNU C though not valid in C90:

.. code-block:: c++

  struct foo {int a[4];};

  struct foo f();

  bar (int index)
  {
    return f().a[index];
  }