..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: variable-length arrays, arrays of variable length, VLAs

.. _variable-length:

Arrays of Variable Length
*************************

Variable-length automatic arrays are allowed in ISO C99, and as an
extension GCC accepts them in C90 mode and in C++.  These arrays are
declared like any other automatic arrays, but with a length that is not
a constant expression.  The storage is allocated at the point of
declaration and deallocated when the block scope containing the declaration
exits.  For
example:

.. code-block:: c++

  FILE *
  concat_fopen (char *s1, char *s2, char *mode)
  {
    char str[strlen (s1) + strlen (s2) + 1];
    strcpy (str, s1);
    strcat (str, s2);
    return fopen (str, mode);
  }

.. index:: scope of a variable length array, variable-length array scope, deallocating variable length arrays

Jumping or breaking out of the scope of the array name deallocates the
storage.  Jumping into the scope is not allowed; you get an error
message for it.

.. index:: variable-length array in a structure

As an extension, GCC accepts variable-length arrays as a member of
a structure or a union.  For example:

.. code-block:: c++

  void
  foo (int n)
  {
    struct S { int x[n]; };
  }

.. index:: alloca vs variable-length arrays

You can use the function ``alloca`` to get an effect much like
variable-length arrays.  The function ``alloca`` is available in
many other C implementations (but not in all).  On the other hand,
variable-length arrays are more elegant.

There are other differences between these two methods.  Space allocated
with ``alloca`` exists until the containing *function* returns.
The space for a variable-length array is deallocated as soon as the array
name's scope ends, unless you also use ``alloca`` in this scope.

You can also use variable-length arrays as arguments to functions:

.. code-block:: c++

  struct entry
  tester (int len, char data[len][len])
  {
    /* ... */
  }

The length of an array is computed once when the storage is allocated
and is remembered for the scope of the array in case you access it with
``sizeof``.

If you want to pass the array first and the length afterward, you can
use a forward declaration in the parameter list---another GNU extension.

.. code-block:: c++

  struct entry
  tester (int len; char data[len][len], int len)
  {
    /* ... */
  }

.. index:: parameter forward declaration

The :samp:`int len` before the semicolon is a :dfn:`parameter forward
declaration`, and it serves the purpose of making the name ``len``
known when the declaration of ``data`` is parsed.

You can write any number of such parameter forward declarations in the
parameter list.  They can be separated by commas or semicolons, but the
last one must end with a semicolon, which is followed by the 'real'
parameter declarations.  Each forward declaration must match a 'real'
declaration in parameter name and data type.  ISO C99 does not support
parameter forward declarations.
