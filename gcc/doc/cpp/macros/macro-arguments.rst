..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: arguments, macros with arguments, arguments in macro definitions

.. _macro-arguments:

Macro Arguments
***************

Function-like macros can take :dfn:`arguments`, just like true functions.
To define a macro that uses arguments, you insert :dfn:`parameters`
between the pair of parentheses in the macro definition that make the
macro function-like.  The parameters must be valid C identifiers,
separated by commas and optionally whitespace.

To invoke a macro that takes arguments, you write the name of the macro
followed by a list of :dfn:`actual arguments` in parentheses, separated
by commas.  The invocation of the macro need not be restricted to a
single logical line---it can cross as many lines in the source file as
you wish.  The number of arguments you give must match the number of
parameters in the macro definition.  When the macro is expanded, each
use of a parameter in its body is replaced by the tokens of the
corresponding argument.  (You need not use all of the parameters in the
macro body.)

As an example, here is a macro that computes the minimum of two numeric
values, as it is defined in many C programs, and some uses.

.. code-block::

  #define min(X, Y)  ((X) < (Y) ? (X) : (Y))
    x = min(a, b);          →  x = ((a) < (b) ? (a) : (b));
    y = min(1, 2);          →  y = ((1) < (2) ? (1) : (2));
    z = min(a + 28, *p);    →  z = ((a + 28) < (*p) ? (a + 28) : (*p));

(In this small example you can already see several of the dangers of
macro arguments.  See :ref:`macro-pitfalls`, for detailed explanations.)

Leading and trailing whitespace in each argument is dropped, and all
whitespace between the tokens of an argument is reduced to a single
space.  Parentheses within each argument must balance; a comma within
such parentheses does not end the argument.  However, there is no
requirement for square brackets or braces to balance, and they do not
prevent a comma from separating arguments.  Thus,

.. code-block:: c++

  macro (array[x = y, x + 1])

passes two arguments to ``macro`` : ``array[x = y`` and ``x +
1]``.  If you want to supply ``array[x = y, x + 1]`` as an argument,
you can write it as ``array[(x = y, x + 1)]``, which is equivalent C
code.

All arguments to a macro are completely macro-expanded before they are
substituted into the macro body.  After substitution, the complete text
is scanned again for macros to expand, including the arguments.  This rule
may seem strange, but it is carefully designed so you need not worry
about whether any function call is actually a macro invocation.  You can
run into trouble if you try to be too clever, though.  See :ref:`argument-prescan`, for detailed discussion.

For example, ``min (min (a, b), c)`` is first expanded to

.. code-block:: c++

    min (((a) < (b) ? (a) : (b)), (c))

and then to

.. code-block:: c++

  ((((a) < (b) ? (a) : (b))) < (c)
   ? (((a) < (b) ? (a) : (b)))
   : (c))

(Line breaks shown here for clarity would not actually be generated.)

.. index:: empty macro arguments

You can leave macro arguments empty; this is not an error to the
preprocessor (but many macros will then expand to invalid code).
You cannot leave out arguments entirely; if a macro takes two arguments,
there must be exactly one comma at the top level of its argument list.
Here are some silly examples using ``min`` :

.. code-block::

  min(, b)        → ((   ) < (b) ? (   ) : (b))
  min(a, )        → ((a  ) < ( ) ? (a  ) : ( ))
  min(,)          → ((   ) < ( ) ? (   ) : ( ))
  min((,),)       → (((,)) < ( ) ? ((,)) : ( ))

  min()      error macro "min" requires 2 arguments, but only 1 given
  min(,,)    error macro "min" passed 3 arguments, but takes just 2

Whitespace is not a preprocessing token, so if a macro ``foo`` takes
one argument, ``foo ()`` and ``foo ( )`` both supply it an
empty argument.  Previous GNU preprocessor implementations and
documentation were incorrect on this point, insisting that a
function-like macro that takes a single argument be passed a space if an
empty argument was required.

Macro parameters appearing inside string literals are not replaced by
their corresponding actual arguments.

.. code-block::

  #define foo(x) x, "x"
  foo(bar)        → bar, "x"
