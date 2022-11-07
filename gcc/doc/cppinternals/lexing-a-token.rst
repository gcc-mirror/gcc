..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

Lexing a token
**************

Lexing of an individual token is handled by ``_cpp_lex_direct`` and
its subroutines.  In its current form the code is quite complicated,
with read ahead characters and such-like, since it strives to not step
back in the character stream in preparation for handling non-ASCII file
encodings.  The current plan is to convert any such files to UTF-8
before processing them.  This complexity is therefore unnecessary and
will be removed, so I'll not discuss it further here.

The job of ``_cpp_lex_direct`` is simply to lex a token.  It is not
responsible for issues like directive handling, returning lookahead
tokens directly, multiple-include optimization, or conditional block
skipping.  It necessarily has a minor rôle to play in memory
management of lexed lines.  I discuss these issues in a separate section
(see :ref:`lexing-a-line`).

The lexer places the token it lexes into storage pointed to by the
variable ``cur_token``, and then increments it.  This variable is
important for correct diagnostic positioning.  Unless a specific line
and column are passed to the diagnostic routines, they will examine the
``line`` and ``col`` values of the token just before the location
that ``cur_token`` points to, and use that location to report the
diagnostic.

The lexer does not consider whitespace to be a token in its own right.
If whitespace (other than a new line) precedes a token, it sets the
``PREV_WHITE`` bit in the token's flags.  Each token has its
``line`` and ``col`` variables set to the line and column of the
first character of the token.  This line number is the line number in
the translation unit, and can be converted to a source (file, line) pair
using the line map code.

The first token on a logical, i.e. unescaped, line has the flag
``BOL`` set for beginning-of-line.  This flag is intended for
internal use, both to distinguish a :samp:`#` that begins a directive
from one that doesn't, and to generate a call-back to clients that want
to be notified about the start of every non-directive line with tokens
on it.  Clients cannot reliably determine this for themselves: the first
token might be a macro, and the tokens of a macro expansion do not have
the ``BOL`` flag set.  The macro expansion may even be empty, and the
next token on the line certainly won't have the ``BOL`` flag set.

New lines are treated specially; exactly how the lexer handles them is
context-dependent.  The C standard mandates that directives are
terminated by the first unescaped newline character, even if it appears
in the middle of a macro expansion.  Therefore, if the state variable
``in_directive`` is set, the lexer returns a ``CPP_EOF`` token,
which is normally used to indicate end-of-file, to indicate
end-of-directive.  In a directive a ``CPP_EOF`` token never means
end-of-file.  Conveniently, if the caller was ``collect_args``, it
already handles ``CPP_EOF`` as if it were end-of-file, and reports an
error about an unterminated macro argument list.

The C standard also specifies that a new line in the middle of the
arguments to a macro is treated as whitespace.  This white space is
important in case the macro argument is stringized.  The state variable
``parsing_args`` is nonzero when the preprocessor is collecting the
arguments to a macro call.  It is set to 1 when looking for the opening
parenthesis to a function-like macro, and 2 when collecting the actual
arguments up to the closing parenthesis, since these two cases need to
be distinguished sometimes.  One such time is here: the lexer sets the
``PREV_WHITE`` flag of a token if it meets a new line when
``parsing_args`` is set to 2.  It doesn't set it if it meets a new
line when ``parsing_args`` is 1, since then code like

.. code-block:: c++

  #define foo() bar
  foo
  baz

would be output with an erroneous space before :samp:`baz`:

.. code-block:: c++

  foo
   baz

This is a good example of the subtlety of getting token spacing correct
in the preprocessor; there are plenty of tests in the testsuite for
corner cases like this.

The lexer is written to treat each of :samp:`\\r`, :samp:`\\n`, :samp:`\\r\\n`
and :samp:`\\n\\r` as a single new line indicator.  This allows it to
transparently preprocess MS-DOS, Macintosh and Unix files without their
needing to pass through a special filter beforehand.

We also decided to treat a backslash, either ``\`` or the trigraph
``??/``, separated from one of the above newline indicators by
non-comment whitespace only, as intending to escape the newline.  It
tends to be a typing mistake, and cannot reasonably be mistaken for
anything else in any of the C-family grammars.  Since handling it this
way is not strictly conforming to the ISO standard, the library issues a
warning wherever it encounters it.

Handling newlines like this is made simpler by doing it in one place
only.  The function ``handle_newline`` takes care of all newline
characters, and ``skip_escaped_newlines`` takes care of arbitrarily
long sequences of escaped newlines, deferring to ``handle_newline``
to handle the newlines themselves.

The most painful aspect of lexing ISO-standard C and C++ is handling
trigraphs and backlash-escaped newlines.  Trigraphs are processed before
any interpretation of the meaning of a character is made, and unfortunately
there is a trigraph representation for a backslash, so it is possible for
the trigraph ``??/`` to introduce an escaped newline.

Escaped newlines are tedious because theoretically they can occur
anywhere---between the :samp:`+` and :samp:`=` of the :samp:`+=` token,
within the characters of an identifier, and even between the :samp:`*`
and :samp:`/` that terminates a comment.  Moreover, you cannot be sure
there is just one---there might be an arbitrarily long sequence of them.

So, for example, the routine that lexes a number, ``parse_number``,
cannot assume that it can scan forwards until the first non-number
character and be done with it, because this could be the :samp:`\\`
introducing an escaped newline, or the :samp:`?` introducing the trigraph
sequence that represents the :samp:`\\` of an escaped newline.  If it
encounters a :samp:`?` or :samp:`\\`, it calls ``skip_escaped_newlines``
to skip over any potential escaped newlines before checking whether the
number has been finished.

Similarly code in the main body of ``_cpp_lex_direct`` cannot simply
check for a :samp:`=` after a :samp:`+` character to determine whether it
has a :samp:`+=` token; it needs to be prepared for an escaped newline of
some sort.  Such cases use the function ``get_effective_char``, which
returns the first character after any intervening escaped newlines.

The lexer needs to keep track of the correct column position, including
counting tabs as specified by the :option:`-ftabstop=` option.  This
should be done even within C-style comments; they can appear in the
middle of a line, and we want to report diagnostics in the correct
position for text appearing after the end of the comment.

.. _invalid-identifiers:

Some identifiers, such as ``__VA_ARGS__`` and poisoned identifiers,
may be invalid and require a diagnostic.  However, if they appear in a
macro expansion we don't want to complain with each use of the macro.
It is therefore best to catch them during the lexing stage, in
``parse_identifier``.  In both cases, whether a diagnostic is needed
or not is dependent upon the lexer's state.  For example, we don't want
to issue a diagnostic for re-poisoning a poisoned identifier, or for
using ``__VA_ARGS__`` in the expansion of a variable-argument macro.
Therefore ``parse_identifier`` makes use of state flags to determine
whether a diagnostic is appropriate.  Since we change state on a
per-token basis, and don't lex whole lines at a time, this is not a
problem.

Another place where state flags are used to change behavior is whilst
lexing header names.  Normally, a :samp:`<` would be lexed as a single
token.  After a ``#include`` directive, though, it should be lexed as
a single token as far as the nearest :samp:`>` character.  Note that we
don't allow the terminators of header names to be escaped; the first
:samp:`"` or :samp:`>` terminates the header name.

Interpretation of some character sequences depends upon whether we are
lexing C, C++ or Objective-C, and on the revision of the standard in
force.  For example, :samp:`::` is a single token in C++, but in C it is
two separate :samp:`:` tokens and almost certainly a syntax error.  Such
cases are handled by ``_cpp_lex_direct`` based upon command-line
flags stored in the ``cpp_options`` structure.

Once a token has been lexed, it leads an independent existence.  The
spelling of numbers, identifiers and strings is copied to permanent
storage from the original input buffer, so a token remains valid and
correct even if its source buffer is freed with ``_cpp_pop_buffer``.
The storage holding the spellings of such tokens remains until the
client program calls cpp_destroy, probably at the end of the translation
unit.