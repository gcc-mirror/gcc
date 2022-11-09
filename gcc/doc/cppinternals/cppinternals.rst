..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. @smallbook
   @cropmarks
   @finalout

.. index:: interface, header files

.. _conventions:

Conventions
===========

cpplib has two interfaces---one is exposed internally only, and the
other is for both internal and external use.

The convention is that functions and types that are exposed to multiple
files internally are prefixed with :samp:`_cpp_`, and are to be found in
the file :samp:`internal.h`.  Functions and types exposed to external
clients are in :samp:`cpplib.h`, and prefixed with :samp:`cpp_`.  For
historical reasons this is no longer quite true, but we should strive to
stick to it.

We are striving to reduce the information exposed in :samp:`cpplib.h` to the
bare minimum necessary, and then to keep it there.  This makes clear
exactly what external clients are entitled to assume, and allows us to
change internals in the future without worrying whether library clients
are perhaps relying on some kind of undocumented implementation-specific
behavior.

.. index:: lexer, newlines, escaped newlines

.. _lexer:

The Lexer
=========

.. toctree::
  :maxdepth: 2

  overview
  lexing-a-token
  lexing-a-line

.. index:: hash table, identifiers, macros, assertions, named operators

.. _hash-nodes:

Hash Nodes
==========

When cpplib encounters an 'identifier', it generates a hash code for
it and stores it in the hash table.  By 'identifier' we mean tokens
with type ``CPP_NAME`` ; this includes identifiers in the usual C
sense, as well as keywords, directive names, macro names and so on.  For
example, all of ``pragma``, ``int``, ``foo`` and
``__GNUC__`` are identifiers and hashed when lexed.

Each node in the hash table contain various information about the
identifier it represents.  For example, its length and type.  At any one
time, each identifier falls into exactly one of three categories:

* Macros

  These have been declared to be macros, either on the command line or
  with ``#define``.  A few, such as ``__TIME__`` are built-ins
  entered in the hash table during initialization.  The hash node for a
  normal macro points to a structure with more information about the
  macro, such as whether it is function-like, how many arguments it takes,
  and its expansion.  Built-in macros are flagged as special, and instead
  contain an enum indicating which of the various built-in macros it is.

* Assertions

  Assertions are in a separate namespace to macros.  To enforce this, cpp
  actually prepends a ``#`` character before hashing and entering it in
  the hash table.  An assertion's node points to a chain of answers to
  that assertion.

* Void

  Everything else falls into this category---an identifier that is not
  currently a macro, or a macro that has since been undefined with
  ``#undef``.

  When preprocessing C++, this category also includes the named operators,
  such as ``xor``.  In expressions these behave like the operators they
  represent, but in contexts where the spelling of a token matters they
  are spelt differently.  This spelling distinction is relevant when they
  are operands of the stringizing and pasting macro operators ``#`` and
  ``##``.  Named operator hash nodes are flagged, both to catch the
  spelling distinction and to prevent them from being defined as macros.

The same identifiers share the same hash node.  Since each identifier
token, after lexing, contains a pointer to its hash node, this is used
to provide rapid lookup of various information.  For example, when
parsing a ``#define`` statement, CPP flags each argument's identifier
hash node with the index of that argument.  This makes duplicated
argument checking an O(1) operation for each argument.  Similarly, for
each identifier in the macro's expansion, lookup to see if it is an
argument, and which argument it is, is also an O(1) operation.  Further,
each directive name, such as ``endif``, has an associated directive
enum stored in its hash node, so that directive lookup is also O(1).

.. index:: macro expansion

.. _macro-expansion:

Macro Expansion Algorithm
=========================

Macro expansion is a tricky operation, fraught with nasty corner cases
and situations that render what you thought was a nifty way to
optimize the preprocessor's expansion algorithm wrong in quite subtle
ways.

I strongly recommend you have a good grasp of how the C and C++
standards require macros to be expanded before diving into this
section, let alone the code!.  If you don't have a clear mental
picture of how things like nested macro expansion, stringizing and
token pasting are supposed to work, damage to your sanity can quickly
result.

.. toctree::
  :maxdepth: 2

  internal-representation-of-macros
  macro-expansion-overview
  scanning-the-replacement-list-for-macros-to-expand
  looking-for-a-function-like-macros-opening-parenthesis
  marking-tokens-ineligible-for-future-expansion

.. index:: paste avoidance, spacing, token spacing

.. _token-spacing:

Token Spacing
=============

First, consider an issue that only concerns the stand-alone
preprocessor: there needs to be a guarantee that re-reading its preprocessed
output results in an identical token stream.  Without taking special
measures, this might not be the case because of macro substitution.
For example:

.. code-block::

  #define PLUS +
  #define EMPTY
  #define f(x) =x=
  +PLUS -EMPTY- PLUS+ f(=)
          → + + - - + + = = =
  not
          → ++ -- ++ ===

One solution would be to simply insert a space between all adjacent
tokens.  However, we would like to keep space insertion to a minimum,
both for aesthetic reasons and because it causes problems for people who
still try to abuse the preprocessor for things like Fortran source and
Makefiles.

For now, just notice that when tokens are added (or removed, as shown by
the ``EMPTY`` example) from the original lexed token stream, we need
to check for accidental token pasting.  We call this :dfn:`paste
avoidance`.  Token addition and removal can only occur because of macro
expansion, but accidental pasting can occur in many places: both before
and after each macro replacement, each argument replacement, and
additionally each token created by the :samp:`#` and :samp:`##` operators.

Look at how the preprocessor gets whitespace output correct
normally.  The ``cpp_token`` structure contains a flags byte, and one
of those flags is ``PREV_WHITE``.  This is flagged by the lexer, and
indicates that the token was preceded by whitespace of some form other
than a new line.  The stand-alone preprocessor can use this flag to
decide whether to insert a space between tokens in the output.

Now consider the result of the following macro expansion:

.. code-block::

  #define add(x, y, z) x + y +z;
  sum = add (1,2, 3);
          → sum = 1 + 2 +3;

The interesting thing here is that the tokens :samp:`1` and :samp:`2` are
output with a preceding space, and :samp:`3` is output without a
preceding space, but when lexed none of these tokens had that property.
Careful consideration reveals that :samp:`1` gets its preceding
whitespace from the space preceding :samp:`add` in the macro invocation,
*not* replacement list.  :samp:`2` gets its whitespace from the
space preceding the parameter :samp:`y` in the macro replacement list,
and :samp:`3` has no preceding space because parameter :samp:`z` has none
in the replacement list.

Once lexed, tokens are effectively fixed and cannot be altered, since
pointers to them might be held in many places, in particular by
in-progress macro expansions.  So instead of modifying the two tokens
above, the preprocessor inserts a special token, which I call a
:dfn:`padding token`, into the token stream to indicate that spacing of
the subsequent token is special.  The preprocessor inserts padding
tokens in front of every macro expansion and expanded macro argument.
These point to a :dfn:`source token` from which the subsequent real token
should inherit its spacing.  In the above example, the source tokens are
:samp:`add` in the macro invocation, and :samp:`y` and :samp:`z` in the
macro replacement list, respectively.

It is quite easy to get multiple padding tokens in a row, for example if
a macro's first replacement token expands straight into another macro.

.. code-block::

  #define foo bar
  #define bar baz
  [foo]
          → [baz]

Here, two padding tokens are generated with sources the :samp:`foo` token
between the brackets, and the :samp:`bar` token from foo's replacement
list, respectively.  Clearly the first padding token is the one to
use, so the output code should contain a rule that the first
padding token in a sequence is the one that matters.

But what if a macro expansion is left?  Adjusting the above
example slightly:

.. code-block::

  #define foo bar
  #define bar EMPTY baz
  #define EMPTY
  [foo] EMPTY;
          → [ baz] ;

As shown, now there should be a space before :samp:`baz` and the
semicolon in the output.

The rules we decided above fail for :samp:`baz`: we generate three
padding tokens, one per macro invocation, before the token :samp:`baz`.
We would then have it take its spacing from the first of these, which
carries source token :samp:`foo` with no leading space.

It is vital that cpplib get spacing correct in these examples since any
of these macro expansions could be stringized, where spacing matters.

So, this demonstrates that not just entering macro and argument
expansions, but leaving them requires special handling too.  I made
cpplib insert a padding token with a ``NULL`` source token when
leaving macro expansions, as well as after each replaced argument in a
macro's replacement list.  It also inserts appropriate padding tokens on
either side of tokens created by the :samp:`#` and :samp:`##` operators.
I expanded the rule so that, if we see a padding token with a
``NULL`` source token, *and* that source token has no leading
space, then we behave as if we have seen no padding tokens at all.  A
quick check shows this rule will then get the above example correct as
well.

Now a relationship with paste avoidance is apparent: we have to be
careful about paste avoidance in exactly the same locations we have
padding tokens in order to get white space correct.  This makes
implementation of paste avoidance easy: wherever the stand-alone
preprocessor is fixing up spacing because of padding tokens, and it
turns out that no space is needed, it has to take the extra step to
check that a space is not needed after all to avoid an accidental paste.
The function ``cpp_avoid_paste`` advises whether a space is required
between two consecutive tokens.  To avoid excessive spacing, it tries
hard to only require a space if one is likely to be necessary, but for
reasons of efficiency it is slightly conservative and might recommend a
space where one is not strictly needed.

.. index:: line numbers

.. _line-numbering:

Line numbering
==============

.. toctree::
  :maxdepth: 2

  just-which-line-number-anyway
  representation-of-line-numbers
