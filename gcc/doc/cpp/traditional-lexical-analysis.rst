..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _traditional-lexical-analysis:

Traditional lexical analysis
****************************

The traditional preprocessor does not decompose its input into tokens
the same way a standards-conforming preprocessor does.  The input is
simply treated as a stream of text with minimal internal form.

This implementation does not treat trigraphs (see :ref:`trigraphs`)
specially since they were an invention of the standards committee.  It
handles arbitrarily-positioned escaped newlines properly and splices
the lines as you would expect; many traditional preprocessors did not
do this.

The form of horizontal whitespace in the input file is preserved in
the output.  In particular, hard tabs remain hard tabs.  This can be
useful if, for example, you are preprocessing a Makefile.

Traditional CPP only recognizes C-style block comments, and treats the
:samp:`/*` sequence as introducing a comment only if it lies outside
quoted text.  Quoted text is introduced by the usual single and double
quotes, and also by an initial :samp:`<` in a ``#include``
directive.

Traditionally, comments are completely removed and are not replaced
with a space.  Since a traditional compiler does its own tokenization
of the output of the preprocessor, this means that comments can
effectively be used as token paste operators.  However, comments
behave like separators for text handled by the preprocessor itself,
since it doesn't re-lex its input.  For example, in

.. code-block:: c++

  #if foo/**/bar

:samp:`foo` and :samp:`bar` are distinct identifiers and expanded
separately if they happen to be macros.  In other words, this
directive is equivalent to

.. code-block:: c++

  #if foo bar

rather than

.. code-block:: c++

  #if foobar

Generally speaking, in traditional mode an opening quote need not have
a matching closing quote.  In particular, a macro may be defined with
replacement text that contains an unmatched quote.  Of course, if you
attempt to compile preprocessed output containing an unmatched quote
you will get a syntax error.

However, all preprocessing directives other than ``#define``
require matching quotes.  For example:

.. code-block:: c++

  #define m This macro's fine and has an unmatched quote
  "/* This is not a comment.  */
  /* This is a comment.  The following #include directive
     is ill-formed.  */
  #include <stdio.h

Just as for the ISO preprocessor, what would be a closing quote can be
escaped with a backslash to prevent the quoted text from closing.