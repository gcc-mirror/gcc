..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: typeof, sizeof, macros, types of arguments

.. _typeof:

Referring to a Type with typeof
*******************************

Another way to refer to the type of an expression is with ``typeof``.
The syntax of using of this keyword looks like ``sizeof``, but the
construct acts semantically like a type name defined with ``typedef``.

There are two ways of writing the argument to ``typeof`` : with an
expression or with a type.  Here is an example with an expression:

.. code-block:: c++

  typeof (x[0](1))

This assumes that ``x`` is an array of pointers to functions;
the type described is that of the values of the functions.

Here is an example with a typename as the argument:

.. code-block:: c++

  typeof (int *)

Here the type described is that of pointers to ``int``.

If you are writing a header file that must work when included in ISO C
programs, write ``__typeof__`` instead of ``typeof``.
See :ref:`alternate-keywords`.

A ``typeof`` construct can be used anywhere a typedef name can be
used.  For example, you can use it in a declaration, in a cast, or inside
of ``sizeof`` or ``typeof``.

The operand of ``typeof`` is evaluated for its side effects if and
only if it is an expression of variably modified type or the name of
such a type.

``typeof`` is often useful in conjunction with
statement expressions (see :ref:`statement-exprs`).
Here is how the two together can
be used to define a safe 'maximum' macro which operates on any
arithmetic type and evaluates each of its arguments exactly once:

.. code-block:: c++

  #define max(a,b) \
    ({ typeof (a) _a = (a); \
        typeof (b) _b = (b); \
      _a > _b ? _a : _b; })

.. index:: underscores in variables in macros, _ in variables in macros, local variables in macros, variables, local, in macros, macros, local variables in

The reason for using names that start with underscores for the local
variables is to avoid conflicts with variable names that occur within the
expressions that are substituted for ``a`` and ``b``.  Eventually we
hope to design a new form of declaration syntax that allows you to declare
variables whose scopes start only after their initializers; this will be a
more reliable way to prevent such conflicts.

Some more examples of the use of ``typeof`` :

* This declares ``y`` with the type of what ``x`` points to.

  .. code-block:: c++

    typeof (*x) y;

* This declares ``y`` as an array of such values.

  .. code-block:: c++

    typeof (*x) y[4];

* This declares ``y`` as an array of pointers to characters:

  .. code-block:: c++

    typeof (typeof (char *)[4]) y;

  It is equivalent to the following traditional C declaration:

  .. code-block:: c++

    char *y[4];

  To see the meaning of the declaration using ``typeof``, and why it
  might be a useful way to write, rewrite it with these macros:

  .. code-block:: c++

    #define pointer(T)  typeof(T *)
    #define array(T, N) typeof(T [N])

  Now the declaration can be rewritten this way:

  .. code-block:: c++

    array (pointer (char), 4) y;

  Thus, ``array (pointer (char), 4)`` is the type of arrays of 4
  pointers to ``char``.

In GNU C, but not GNU C++, you may also declare the type of a variable
as ``__auto_type``.  In that case, the declaration must declare
only one variable, whose declarator must just be an identifier, the
declaration must be initialized, and the type of the variable is
determined by the initializer; the name of the variable is not in
scope until after the initializer.  (In C++, you should use C++11
``auto`` for this purpose.)  Using ``__auto_type``, the
'maximum' macro above could be written as:

.. code-block:: c++

  #define max(a,b) \
    ({ __auto_type _a = (a); \
        __auto_type _b = (b); \
      _a > _b ? _a : _b; })

Using ``__auto_type`` instead of ``typeof`` has two advantages:

* Each argument to the macro appears only once in the expansion of
  the macro.  This prevents the size of the macro expansion growing
  exponentially when calls to such macros are nested inside arguments of
  such macros.

* If the argument to the macro has variably modified type, it is
  evaluated only once when using ``__auto_type``, but twice if
  ``typeof`` is used.
