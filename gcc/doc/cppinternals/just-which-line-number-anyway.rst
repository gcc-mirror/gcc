..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

Just which line number anyway?
******************************

There are three reasonable requirements a cpplib client might have for
the line number of a token passed to it:

* The source line it was lexed on.

* The line it is output on.  This can be different to the line it was
  lexed on if, for example, there are intervening escaped newlines or
  C-style comments.  For example:

  .. code-block::

    foo /* A long
    comment */ bar \
    baz
    ⇒
    foo bar baz

* If the token results from a macro expansion, the line of the macro name,
  or possibly the line of the closing parenthesis in the case of
  function-like macro expansion.

The ``cpp_token`` structure contains ``line`` and ``col``
members.  The lexer fills these in with the line and column of the first
character of the token.  Consequently, but maybe unexpectedly, a token
from the replacement list of a macro expansion carries the location of
the token within the ``#define`` directive, because cpplib expands a
macro by returning pointers to the tokens in its replacement list.  The
current implementation of cpplib assigns tokens created from built-in
macros and the :samp:`#` and :samp:`##` operators the location of the most
recently lexed token.  This is a because they are allocated from the
lexer's token runs, and because of the way the diagnostic routines infer
the appropriate location to report.

The diagnostic routines in cpplib display the location of the most
recently *lexed* token, unless they are passed a specific line and
column to report.  For diagnostics regarding tokens that arise from
macro expansions, it might also be helpful for the user to see the
original location in the macro definition that the token came from.
Since that is exactly the information each token carries, such an
enhancement could be made relatively easily in future.

The stand-alone preprocessor faces a similar problem when determining
the correct line to output the token on: the position attached to a
token is fairly useless if the token came from a macro expansion.  All
tokens on a logical line should be output on its first physical line, so
the token's reported location is also wrong if it is part of a physical
line other than the first.

To solve these issues, cpplib provides a callback that is generated
whenever it lexes a preprocessing token that starts a new logical line
other than a directive.  It passes this token (which may be a
``CPP_EOF`` token indicating the end of the translation unit) to the
callback routine, which can then use the line and column of this token
to produce correct output.