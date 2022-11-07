..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _traditional-macros:

Traditional macros
******************

The major difference between traditional and ISO macros is that the
former expand to text rather than to a token sequence.  CPP removes
all leading and trailing horizontal whitespace from a macro's
replacement text before storing it, but preserves the form of internal
whitespace.

One consequence is that it is legitimate for the replacement text to
contain an unmatched quote (see :ref:`traditional-lexical-analysis`).  An
unclosed string or character constant continues into the text
following the macro call.  Similarly, the text at the end of a macro's
expansion can run together with the text after the macro invocation to
produce a single token.

Normally comments are removed from the replacement text after the
macro is expanded, but if the :option:`-CC` option is passed on the
command-line comments are preserved.  (In fact, the current
implementation removes comments even before saving the macro
replacement text, but it careful to do it in such a way that the
observed effect is identical even in the function-like macro case.)

The ISO stringizing operator :samp:`#` and token paste operator
:samp:`##` have no special meaning.  As explained later, an effect
similar to these operators can be obtained in a different way.  Macro
names that are embedded in quotes, either from the main file or after
macro replacement, do not expand.

CPP replaces an unquoted object-like macro name with its replacement
text, and then rescans it for further macros to replace.  Unlike
standard macro expansion, traditional macro expansion has no provision
to prevent recursion.  If an object-like macro appears unquoted in its
replacement text, it will be replaced again during the rescan pass,
and so on *ad infinitum*.  GCC detects when it is expanding
recursive macros, emits an error message, and continues after the
offending macro invocation.

.. code-block::

  #define PLUS +
  #define INC(x) PLUS+x
  INC(foo);
       → ++foo;

Function-like macros are similar in form but quite different in
behavior to their ISO counterparts.  Their arguments are contained
within parentheses, are comma-separated, and can cross physical lines.
Commas within nested parentheses are not treated as argument
separators.  Similarly, a quote in an argument cannot be left
unclosed; a following comma or parenthesis that comes before the
closing quote is treated like any other character.  There is no
facility for handling variadic macros.

This implementation removes all comments from macro arguments, unless
the :option:`-C` option is given.  The form of all other horizontal
whitespace in arguments is preserved, including leading and trailing
whitespace.  In particular

.. code-block:: c++

  f( )

is treated as an invocation of the macro :samp:`f` with a single
argument consisting of a single space.  If you want to invoke a
function-like macro that takes no arguments, you must not leave any
whitespace between the parentheses.

If a macro argument crosses a new line, the new line is replaced with
a space when forming the argument.  If the previous line contained an
unterminated quote, the following line inherits the quoted state.

Traditional preprocessors replace parameters in the replacement text
with their arguments regardless of whether the parameters are within
quotes or not.  This provides a way to stringize arguments.  For
example

.. code-block::

  #define str(x) "x"
  str(/* A comment */some text )
       → "some text "

Note that the comment is removed, but that the trailing space is
preserved.  Here is an example of using a comment to effect token
pasting.

.. code-block::

  #define suffix(x) foo_/**/x
  suffix(bar)
       → foo_bar