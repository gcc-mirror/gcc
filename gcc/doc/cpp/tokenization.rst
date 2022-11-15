..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: tokens, preprocessing tokens

.. _tokenization:

Tokenization
************

After the textual transformations are finished, the input file is
converted into a sequence of :dfn:`preprocessing tokens`.  These mostly
correspond to the syntactic tokens used by the C compiler, but there are
a few differences.  White space separates tokens; it is not itself a
token of any kind.  Tokens do not have to be separated by white space,
but it is often necessary to avoid ambiguities.

When faced with a sequence of characters that has more than one possible
tokenization, the preprocessor is greedy.  It always makes each token,
starting from the left, as big as possible before moving on to the next
token.  For instance, ``a+++++b`` is interpreted as
``a ++ ++ + b``, not as ``a ++ + ++ b``, even though the
latter tokenization could be part of a valid C program and the former
could not.

Once the input file is broken into tokens, the token boundaries never
change, except when the :samp:`##` preprocessing operator is used to paste
tokens together.  See :ref:`concatenation`.  For example,

.. code-block::

  #define foo() bar
  foo()baz
       → bar baz
  not
       → barbaz

The compiler does not re-tokenize the preprocessor's output.  Each
preprocessing token becomes one compiler token.

.. index:: identifiers

Preprocessing tokens fall into five broad classes: identifiers,
preprocessing numbers, string literals, punctuators, and other.  An
:dfn:`identifier` is the same as an identifier in C: any sequence of
letters, digits, or underscores, which begins with a letter or
underscore.  Keywords of C have no significance to the preprocessor;
they are ordinary identifiers.  You can define a macro whose name is a
keyword, for instance.  The only identifier which can be considered a
preprocessing keyword is ``defined``.  See :ref:`defined`.

This is mostly true of other languages which use the C preprocessor.
However, a few of the keywords of C++ are significant even in the
preprocessor.  See :ref:`c++-named-operators`.

In the 1999 C standard, identifiers may contain letters which are not
part of the 'basic source character set', at the implementation's
discretion (such as accented Latin letters, Greek letters, or Chinese
ideograms).  This may be done with an extended character set, or the
:samp:`\\u` and :samp:`\\U` escape sequences.

As an extension, GCC treats :samp:`$` as a letter.  This is for
compatibility with some systems, such as VMS, where :samp:`$` is commonly
used in system-defined function and object names.  :samp:`$` is not a
letter in strictly conforming mode, or if you specify the :option:`-$`
option.  See :ref:`invocation`.

.. index:: numbers, preprocessing numbers

A :dfn:`preprocessing number` has a rather bizarre definition.  The
category includes all the normal integer and floating point constants
one expects of C, but also a number of other things one might not
initially recognize as a number.  Formally, preprocessing numbers begin
with an optional period, a required decimal digit, and then continue
with any sequence of letters, digits, underscores, periods, and
exponents.  Exponents are the two-character sequences :samp:`e+`,
:samp:`e-`, :samp:`E+`, :samp:`E-`, :samp:`p+`, :samp:`p-`, :samp:`P+`, and
:samp:`P-`.  (The exponents that begin with :samp:`p` or :samp:`P` are
used for hexadecimal floating-point constants.)

The purpose of this unusual definition is to isolate the preprocessor
from the full complexity of numeric constants.  It does not have to
distinguish between lexically valid and invalid floating-point numbers,
which is complicated.  The definition also permits you to split an
identifier at any position and get exactly two tokens, which can then be
pasted back together with the :samp:`##` operator.

It's possible for preprocessing numbers to cause programs to be
misinterpreted.  For example, ``0xE+12`` is a preprocessing number
which does not translate to any valid numeric constant, therefore a
syntax error.  It does not mean ``0xE + 12``, which is what you
might have intended.

.. index:: string literals, string constants, character constants, header file names

.. the @: prevents makeinfo from turning '' into ".

:dfn:`String literals` are string constants, character constants, and
header file names (the argument of :samp:`#include`) [#f1]_.

String constants and character
constants are straightforward: ``"..."`` or ``'...'``.  In
either case embedded quotes should be escaped with a backslash:
``'\''`` is the character constant for :samp:`'`.  There is no limit on
the length of a character constant, but the value of a character
constant that contains more than one character is
implementation-defined.  See :ref:`implementation-details`.

Header file names either look like string constants, ``"..."``, or are
written with angle brackets instead, ``<...>``.  In either case,
backslash is an ordinary character.  There is no way to escape the
closing quote or angle bracket.  The preprocessor looks for the header
file in different places depending on which form you use.  See :ref:`include-operation`.

No string literal may extend past the end of a line.  You may use continued
lines instead, or string constant concatenation.

.. index:: punctuators, digraphs, alternative tokens

:dfn:`Punctuators` are all the usual bits of punctuation which are
meaningful to C and C++.  All but three of the punctuation characters in
ASCII are C punctuators.  The exceptions are :samp:`@`, :samp:`$`, and
:samp:`\``.  In addition, all the two- and three-character operators are
punctuators.  There are also six :dfn:`digraphs`, which the C++ standard
calls :dfn:`alternative tokens`, which are merely alternate ways to spell
other punctuators.  This is a second attempt to work around missing
punctuation in obsolete systems.  It has no negative side effects,
unlike trigraphs, but does not cover as much ground.  The digraphs and
their corresponding normal punctuators are:

.. code-block::

  Digraph:        <%  %>  <:  :>  %:  %:%:
  Punctuator:      {   }   [   ]   #    ##

.. index:: other tokens

Any other single byte is considered 'other' and passed on to the
preprocessor's output unchanged.  The C compiler will almost certainly
reject source code containing 'other' tokens.  In ASCII, the only
'other' characters are :samp:`@`, :samp:`$`, :samp:`\``, and control
characters other than NUL (all bits zero).  (Note that :samp:`$` is
normally considered a letter.)  All bytes with the high bit set
(numeric range 0x7F--0xFF) that were not succesfully interpreted as
part of an extended character in the input encoding are also 'other'
in the present implementation.

NUL is a special case because of the high probability that its
appearance is accidental, and because it may be invisible to the user
(many terminals do not display NUL at all).  Within comments, NULs are
silently ignored, just as any other character would be.  In running
text, NUL is considered white space.  For example, these two directives
have the same meaning.

.. code-block:: c++

  #define X^@1
  #define X 1

(where :samp:`^@` is ASCII NUL).  Within string or character constants,
NULs are preserved.  In the latter two cases the preprocessor emits a
warning message.

.. [#f1] The C
  standard uses the term :dfn:`string literal` to refer only to what we are
  calling :dfn:`string constants`.
