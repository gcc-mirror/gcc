..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: labels as values, computed gotos, goto with computed label, address of a label

.. _labels-as-values:

Labels as Values
****************

You can get the address of a label defined in the current function
(or a containing function) with the unary operator :samp:`&&`.  The
value has type ``void *``.  This value is a constant and can be used
wherever a constant of that type is valid.  For example:

.. code-block:: c++

  void *ptr;
  /* ... */
  ptr = &&foo;

To use these values, you need to be able to jump to one.  This is done
with the computed goto statement [#f1]_, ``goto *exp;``.  For example,

.. code-block:: c++

  goto *ptr;

Any expression of type ``void *`` is allowed.

One way of using these constants is in initializing a static array that
serves as a jump table:

.. code-block:: c++

  static void *array[] = { &&foo, &&bar, &&hack };

Then you can select a label with indexing, like this:

.. code-block:: c++

  goto *array[i];

Note that this does not check whether the subscript is in bounds---array
indexing in C never does that.

Such an array of label values serves a purpose much like that of the
``switch`` statement.  The ``switch`` statement is cleaner, so
use that rather than an array unless the problem does not fit a
``switch`` statement very well.

Another use of label values is in an interpreter for threaded code.
The labels within the interpreter function can be stored in the
threaded code for super-fast dispatching.

You may not use this mechanism to jump to code in a different function.
If you do that, totally unpredictable things happen.  The best way to
avoid this is to store the label address only in automatic variables and
never pass it as an argument.

An alternate way to write the above example is

.. code-block:: c++

  static const int array[] = { &&foo - &&foo, &&bar - &&foo,
                               &&hack - &&foo };
  goto *(&&foo + array[i]);

This is more friendly to code living in shared libraries, as it reduces
the number of dynamic relocations that are needed, and by consequence,
allows the data to be read-only.
This alternative with label differences is not supported for the AVR target,
please use the first approach for AVR programs.

The ``&&foo`` expressions for the same label might have different
values if the containing function is inlined or cloned.  If a program
relies on them being always the same,
``__attribute__((__noinline__,__noclone__))`` should be used to
prevent inlining and cloning.  If ``&&foo`` is used in a static
variable initializer, inlining and cloning is forbidden.

.. [#f1] The analogous feature in Fortran is called an assigned goto, but that name seems inappropriate in
  C, where one can do more than simply store label addresses in label
  variables.