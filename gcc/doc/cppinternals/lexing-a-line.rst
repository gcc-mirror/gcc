..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: token run

.. _lexing-a-line:

Lexing a line
*************

When the preprocessor was changed to return pointers to tokens, one
feature I wanted was some sort of guarantee regarding how long a
returned pointer remains valid.  This is important to the stand-alone
preprocessor, the future direction of the C family front ends, and even
to cpplib itself internally.

Occasionally the preprocessor wants to be able to peek ahead in the
token stream.  For example, after the name of a function-like macro, it
wants to check the next token to see if it is an opening parenthesis.
Another example is that, after reading the first few tokens of a
``#pragma`` directive and not recognizing it as a registered pragma,
it wants to backtrack and allow the user-defined handler for unknown
pragmas to access the full ``#pragma`` token stream.  The stand-alone
preprocessor wants to be able to test the current token with the
previous one to see if a space needs to be inserted to preserve their
separate tokenization upon re-lexing (paste avoidance), so it needs to
be sure the pointer to the previous token is still valid.  The
recursive-descent C++ parser wants to be able to perform tentative
parsing arbitrarily far ahead in the token stream, and then to be able
to jump back to a prior position in that stream if necessary.

The rule I chose, which is fairly natural, is to arrange that the
preprocessor lex all tokens on a line consecutively into a token buffer,
which I call a :dfn:`token run`, and when meeting an unescaped new line
(newlines within comments do not count either), to start lexing back at
the beginning of the run.  Note that we do *not* lex a line of
tokens at once; if we did that ``parse_identifier`` would not have
state flags available to warn about invalid identifiers (see :ref:`Invalid identifiers <invalid-identifiers>`).

In other words, accessing tokens that appeared earlier in the current
line is valid, but since each logical line overwrites the tokens of the
previous line, tokens from prior lines are unavailable.  In particular,
since a directive only occupies a single logical line, this means that
the directive handlers like the ``#pragma`` handler can jump around
in the directive's tokens if necessary.

Two issues remain: what about tokens that arise from macro expansions,
and what happens when we have a long line that overflows the token run?

Since we promise clients that we preserve the validity of pointers that
we have already returned for tokens that appeared earlier in the line,
we cannot reallocate the run.  Instead, on overflow it is expanded by
chaining a new token run on to the end of the existing one.

The tokens forming a macro's replacement list are collected by the
``#define`` handler, and placed in storage that is only freed by
``cpp_destroy``.  So if a macro is expanded in the line of tokens,
the pointers to the tokens of its expansion that are returned will always
remain valid.  However, macros are a little trickier than that, since
they give rise to three sources of fresh tokens.  They are the built-in
macros like ``__LINE__``, and the :samp:`#` and :samp:`##` operators
for stringizing and token pasting.  I handled this by allocating
space for these tokens from the lexer's token run chain.  This means
they automatically receive the same lifetime guarantees as lexed tokens,
and we don't need to concern ourselves with freeing them.

Lexing into a line of tokens solves some of the token memory management
issues, but not all.  The opening parenthesis after a function-like
macro name might lie on a different line, and the front ends definitely
want the ability to look ahead past the end of the current line.  So
cpplib only moves back to the start of the token run at the end of a
line if the variable ``keep_tokens`` is zero.  Line-buffering is
quite natural for the preprocessor, and as a result the only time cpplib
needs to increment this variable is whilst looking for the opening
parenthesis to, and reading the arguments of, a function-like macro.  In
the near future cpplib will export an interface to increment and
decrement this variable, so that clients can share full control over the
lifetime of token pointers too.

The routine ``_cpp_lex_token`` handles moving to new token runs,
calling ``_cpp_lex_direct`` to lex new tokens, or returning
previously-lexed tokens if we stepped back in the token stream.  It also
checks each token for the ``BOL`` flag, which might indicate a
directive that needs to be handled, or require a start-of-line call-back
to be made.  ``_cpp_lex_token`` also handles skipping over tokens in
failed conditional blocks, and invalidates the control macro of the
multiple-include optimization if a token was successfully lexed outside
a directive.  In other words, its callers do not need to concern
themselves with such issues.
