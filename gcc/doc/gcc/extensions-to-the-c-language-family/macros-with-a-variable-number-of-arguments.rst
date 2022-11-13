..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: variable number of arguments, macro with variable arguments, rest argument (in macro), variadic macros

.. _variadic-macros:

Macros with a Variable Number of Arguments.
*******************************************

In the ISO C standard of 1999, a macro can be declared to accept a
variable number of arguments much as a function can.  The syntax for
defining the macro is similar to that of a function.  Here is an
example:

.. code-block:: c++

  #define debug(format, ...) fprintf (stderr, format, __VA_ARGS__)

Here :samp:`...` is a :dfn:`variable argument`.  In the invocation of
such a macro, it represents the zero or more tokens until the closing
parenthesis that ends the invocation, including any commas.  This set of
tokens replaces the identifier ``__VA_ARGS__`` in the macro body
wherever it appears.  See the CPP manual for more information.

GCC has long supported variadic macros, and used a different syntax that
allowed you to give a name to the variable arguments just like any other
argument.  Here is an example:

.. code-block:: c++

  #define debug(format, args...) fprintf (stderr, format, args)

This is in all ways equivalent to the ISO C example above, but arguably
more readable and descriptive.

GNU CPP has two further variadic macro extensions, and permits them to
be used with either of the above forms of macro definition.

In standard C, you are not allowed to leave the variable argument out
entirely; but you are allowed to pass an empty argument.  For example,
this invocation is invalid in ISO C, because there is no comma after
the string:

.. code-block:: c++

  debug ("A message")

GNU CPP permits you to completely omit the variable arguments in this
way.  In the above examples, the compiler would complain, though since
the expansion of the macro still has the extra comma after the format
string.

To help solve this problem, CPP behaves specially for variable arguments
used with the token paste operator, :samp:`##`.  If instead you write

.. code-block:: c++

  #define debug(format, ...) fprintf (stderr, format, ## __VA_ARGS__)

and if the variable arguments are omitted or empty, the :samp:`##`
operator causes the preprocessor to remove the comma before it.  If you
do provide some variable arguments in your macro invocation, GNU CPP
does not complain about the paste operation and instead places the
variable arguments after the comma.  Just like any other pasted macro
argument, these arguments are not macro expanded.