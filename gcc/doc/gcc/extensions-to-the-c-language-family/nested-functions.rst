..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: nested functions, downward funargs, thunks

.. _nested-functions:

Nested Functions
****************

A :dfn:`nested function` is a function defined inside another function.
Nested functions are supported as an extension in GNU C, but are not
supported by GNU C++.

The nested function's name is local to the block where it is defined.
For example, here we define a nested function named ``square``, and
call it twice:

.. code-block:: c++

  foo (double a, double b)
  {
    double square (double z) { return z * z; }

    return square (a) + square (b);
  }

The nested function can access all the variables of the containing
function that are visible at the point of its definition.  This is
called :dfn:`lexical scoping`.  For example, here we show a nested
function which uses an inherited variable named ``offset`` :

.. code-block:: c++

  bar (int *array, int offset, int size)
  {
    int access (int *array, int index)
      { return array[index + offset]; }
    int i;
    /* ... */
    for (i = 0; i < size; i++)
      /* ... */ access (array, i) /* ... */
  }

Nested function definitions are permitted within functions in the places
where variable definitions are allowed; that is, in any block, mixed
with the other declarations and statements in the block.

It is possible to call the nested function from outside the scope of its
name by storing its address or passing the address to another function:

.. code-block:: c++

  hack (int *array, int size)
  {
    void store (int index, int value)
      { array[index] = value; }

    intermediate (store, size);
  }

Here, the function ``intermediate`` receives the address of
``store`` as an argument.  If ``intermediate`` calls ``store``,
the arguments given to ``store`` are used to store into ``array``.
But this technique works only so long as the containing function
(``hack``, in this example) does not exit.

If you try to call the nested function through its address after the
containing function exits, all hell breaks loose.  If you try
to call it after a containing scope level exits, and if it refers
to some of the variables that are no longer in scope, you may be lucky,
but it's not wise to take the risk.  If, however, the nested function
does not refer to anything that has gone out of scope, you should be
safe.

GCC implements taking the address of a nested function using a technique
called :dfn:`trampolines`.  This technique was described in
Lexical Closures for C++ (Thomas M. Breuel, USENIX
C++ Conference Proceedings, October 17-21, 1988).

A nested function can jump to a label inherited from a containing
function, provided the label is explicitly declared in the containing
function (see :ref:`local-labels`).  Such a jump returns instantly to the
containing function, exiting the nested function that did the
``goto`` and any intermediate functions as well.  Here is an example:

.. code-block:: c++

  bar (int *array, int offset, int size)
  {
    __label__ failure;
    int access (int *array, int index)
      {
        if (index > size)
          goto failure;
        return array[index + offset];
      }
    int i;
    /* ... */
    for (i = 0; i < size; i++)
      /* ... */ access (array, i) /* ... */
    /* ... */
    return 0;

   /* Control comes here from access
      if it detects an error.  */
   failure:
    return -1;
  }

A nested function always has no linkage.  Declaring one with
``extern`` or ``static`` is erroneous.  If you need to declare the nested function
before its definition, use ``auto`` (which is otherwise meaningless
for function declarations).

.. code-block:: c++

  bar (int *array, int offset, int size)
  {
    __label__ failure;
    auto int access (int *, int);
    /* ... */
    int access (int *array, int index)
      {
        if (index > size)
          goto failure;
        return array[index + offset];
      }
    /* ... */
  }