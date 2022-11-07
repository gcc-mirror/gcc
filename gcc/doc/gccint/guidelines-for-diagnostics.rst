..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: guidelines for diagnostics, diagnostics, guidelines for

.. _guidelines-for-diagnostics:

Guidelines for Diagnostics
**************************

Talk in terms of the user's code
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Diagnostics should be worded in terms of the user's source code, and the
source language, rather than GCC's own implementation details.

.. index:: diagnostics, actionable

Diagnostics are actionable
^^^^^^^^^^^^^^^^^^^^^^^^^^

A good diagnostic is :dfn:`actionable`: it should assist the user in
taking action.

Consider what an end user will want to do when encountering a diagnostic.

Given an error, an end user will think: 'How do I fix this?'

Given a warning, an end user will think:

* 'Is this a real problem?'

* 'Do I care?'

* if they decide it's genuine: 'How do I fix this?'

A good diagnostic provides pertinent information to allow the user to
easily answer the above questions.

The user's attention is important
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A perfect compiler would issue a warning on every aspect of the user's
source code that ought to be fixed, and issue no other warnings.
Naturally, this ideal is impossible to achieve.

.. index:: signal-to-noise ratio (metaphorical usage for diagnostics), diagnostics, false positive, diagnostics, true positive, false positive, true positive

Warnings should have a good :dfn:`signal-to-noise ratio`: we should have few
:dfn:`false positives` (falsely issuing a warning when no warning is
warranted) and few :dfn:`false negatives` (failing to issue a warning when
one *is* justified).

Note that a false positive can mean, in practice, a warning that the
user doesn't agree with.  Ideally a diagnostic should contain enough
information to allow the user to make an informed choice about whether
they should care (and how to fix it), but a balance must be drawn against
overloading the user with irrelevant data.

Sometimes the user didn't write the code
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

GCC is typically used in two different ways:

* Semi-interactive usage: GCC is used as a development tool when the user
  is writing code, as the 'compile' part of the 'edit-compile-debug'
  cycle.  The user is actively hacking on the code themself (perhaps a
  project they wrote, or someone else's), where they just made a change
  to the code and want to see what happens, and to be warned about
  mistakes.

* Batch rebuilds: where the user is recompiling one or more existing
  packages, and GCC is a detail that's being invoked by various build
  scripts.  Examples include a user trying to bring up an operating system
  consisting of hundreds of packages on a new CPU architecture, where the
  packages were written by many different people, or simply rebuilding
  packages after a dependency changed, where the user is hoping
  'nothing breaks', since they are unfamiliar with the code.

Keep both of these styles of usage in mind when implementing diagnostics.

Precision of Wording
^^^^^^^^^^^^^^^^^^^^

Provide the user with details that allow them to identify what the
problem is.  For example, the vaguely-worded message:

.. code-block::

  demo.c:1:1: warning: 'noinline' attribute ignored [-Wattributes]
      1 | int foo __attribute__((noinline));
        | ^~~

doesn't tell the user why the attribute was ignored, or what kind of
entity the compiler thought the attribute was being applied to (the
source location for the diagnostic is also poor;
see :ref:`input_location_example`).
A better message would be:

.. code-block::

  demo.c:1:24: warning: attribute 'noinline' on variable 'foo' was
     ignored [-Wattributes]
      1 | int foo __attribute__((noinline));
        |     ~~~ ~~~~~~~~~~~~~~~^~~~~~~~~
  demo.c:1:24: note: attribute 'noinline' is only applicable to functions

which spells out the missing information (and fixes the location
information, as discussed below).

The above example uses a note to avoid a combinatorial explosion of possible
messages.

Try the diagnostic on real-world code
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

It's worth testing a new warning on many instances of real-world code,
written by different people, and seeing what it complains about, and
what it doesn't complain about.

This may suggest heuristics that silence common false positives.

It may also suggest ways to improve the precision of the message.

Make mismatches clear
^^^^^^^^^^^^^^^^^^^^^

Many diagnostics relate to a mismatch between two different places in the
user's source code.  Examples include:

* a type mismatch, where the type at a usage site does not match the type
  at a declaration

* the argument count at a call site does not match the parameter count
  at the declaration

* something is erroneously duplicated (e.g. an error, due to breaking a
  uniqueness requirement, or a warning, if it's suggestive of a bug)

* an 'opened' syntactic construct (such as an open-parenthesis) is not
  closed

.. todo:: more examples?

In each case, the diagnostic should indicate **both** pertinent
locations (so that the user can easily see the problem and how to fix it).

The standard way to do this is with a note (via ``inform``).  For
example:

.. code-block:: c++

    auto_diagnostic_group d;
    if (warning_at (loc, OPT_Wduplicated_cond,
                    "duplicated %<if%> condition"))
      inform (EXPR_LOCATION (t), "previously used here");

which leads to:

.. code-block::

  demo.c: In function 'test':
  demo.c:5:17: warning: duplicated 'if' condition [-Wduplicated-cond]
      5 |   else if (flag > 3)
        |            ~~~~~^~~
  demo.c:3:12: note: previously used here
      3 |   if (flag > 3)
        |       ~~~~~^~~

The ``inform`` call should be guarded by the return value from the
``warning_at`` call so that the note isn't emitted when the warning
is suppressed.

For cases involving punctuation where the locations might be near
each other, they can be conditionally consolidated via
``gcc_rich_location::add_location_if_nearby`` :

.. code-block:: c++

      auto_diagnostic_group d;
      gcc_rich_location richloc (primary_loc);
      bool added secondary = richloc.add_location_if_nearby (secondary_loc);
      error_at (&richloc, "main message");
      if (!added secondary)
        inform (secondary_loc, "message for secondary");

This will emit either one diagnostic with two locations:

.. code-block:: c++

    demo.c:42:10: error: main message
      (foo)
      ~   ^

or two diagnostics:

.. code-block:: c++

    demo.c:42:4: error: main message
      foo)
         ^
    demo.c:40:2: note: message for secondary
      (
      ^

.. index:: diagnostics, locations, location information, source code, location information, caret

Location Information
^^^^^^^^^^^^^^^^^^^^

GCC's ``location_t`` type can support both ordinary locations,
and locations relating to a macro expansion.

As of GCC 6, ordinary locations changed from supporting just a
point in the user's source code to supporting three points: the
:dfn:`caret` location, plus a start and a finish:

.. code-block:: c++

        a = foo && bar;
            ~~~~^~~~~~
            |   |    |
            |   |    finish
            |   caret
            start

Tokens coming out of libcpp have locations of the form ``caret == start``,
such as for ``foo`` here:

.. code-block:: c++

        a = foo && bar;
            ^~~
            | |
            | finish
            caret == start

Compound expressions should be reported using the location of the
expression as a whole, rather than just of one token within it.

For example, in ``-Wformat``, rather than underlining just the first
token of a bad argument:

.. code-block:: c++

     printf("hello %i %s", (long)0, "world");
                   ~^      ~
                   %li

the whole of the expression should be underlined, so that the user can
easily identify what is being referred to:

.. code-block:: c++

     printf("hello %i %s", (long)0, "world");
                   ~^      ~~~~~~~
                   %li

.. this was r251239

Avoid using the ``input_location`` global, and the diagnostic functions
that implicitly use it---use ``error_at`` and ``warning_at`` rather
than ``error`` and ``warning``, and provide the most appropriate
``location_t`` value available at that phase of the compilation.  It's
possible to supply secondary ``location_t`` values via
``rich_location``.

.. _input_location_example:

For example, in the example of imprecise wording above, generating the
diagnostic using ``warning`` :

.. code-block:: c++

    // BAD: implicitly uses input_location
    warning (OPT_Wattributes, "%qE attribute ignored", name);

leads to:

.. code-block::

  // BAD: uses input_location
  demo.c:1:1: warning: 'noinline' attribute ignored [-Wattributes]
      1 | int foo __attribute__((noinline));
        | ^~~

which thus happened to use the location of the ``int`` token, rather
than that of the attribute.  Using ``warning_at`` with the location of
the attribute, providing the location of the declaration in question
as a secondary location, and adding a note:

.. code-block:: c++

    auto_diagnostic_group d;
    gcc_rich_location richloc (attrib_loc);
    richloc.add_range (decl_loc);
    if (warning_at (OPT_Wattributes, &richloc,
                    "attribute %qE on variable %qE was ignored", name))
      inform (attrib_loc, "attribute %qE is only applicable to functions");

would lead to:

.. code-block::

  // OK: use location of attribute, with a secondary location
  demo.c:1:24: warning: attribute 'noinline' on variable 'foo' was
     ignored [-Wattributes]
      1 | int foo __attribute__((noinline));
        |     ~~~ ~~~~~~~~~~~~~~~^~~~~~~~~
  demo.c:1:24: note: attribute 'noinline' is only applicable to functions

..todo:: labelling of ranges

Coding Conventions
^^^^^^^^^^^^^^^^^^

See the `diagnostics section <https://gcc.gnu.org/codingconventions.html#Diagnostics>`_ of the GCC coding conventions.

In the C++ front end, when comparing two types in a message, use :samp:`%H`
and :samp:`%I` rather than :samp:`%T`, as this allows the diagnostics
subsystem to highlight differences between template-based types.
For example, rather than using :samp:`%qT`:

.. code-block:: c++

    // BAD: a pair of %qT used in C++ front end for type comparison
    error_at (loc, "could not convert %qE from %qT to %qT", expr,
              TREE_TYPE (expr), type);

which could lead to:

.. code-block::

  error: could not convert 'map<int, double>()' from 'map<int,double>'
     to 'map<int,int>'

using :samp:`%H` and :samp:`%I` (via :samp:`%qH` and :samp:`%qI`):

.. code-block:: c++

    // OK: compare types in C++ front end via %qH and %qI
    error_at (loc, "could not convert %qE from %qH to %qI", expr,
              TREE_TYPE (expr), type);

allows the above output to be simplified to:

.. code-block::

  error: could not convert 'map<int, double>()' from 'map<[...],double>'
     to 'map<[...],int>'

where the ``double`` and ``int`` are colorized to highlight them.

.. %H and %I were added in r248698.

Group logically-related diagnostics
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Use ``auto_diagnostic_group`` when issuing multiple related
diagnostics (seen in various examples on this page).  This informs the
diagnostic subsystem that all diagnostics issued within the lifetime
of the ``auto_diagnostic_group`` are related.  For example,
:option:`-fdiagnostics-format=json` will treat the first diagnostic
emitted within the group as a top-level diagnostic, and all subsequent
diagnostics within the group as its children.

Quoting
^^^^^^^

Text should be quoted by either using the :samp:`q` modifier in a directive
such as :samp:`%qE`, or by enclosing the quoted text in a pair of :samp:`%<`
and :samp:`%>` directives, and never by using explicit quote characters.
The directives handle the appropriate quote characters for each language
and apply the correct color or highlighting.

The following elements should be quoted in GCC diagnostics:

* Language keywords.

* Tokens.

* Boolean, numerical, character, and string constants that appear in the
  source code.

* Identifiers, including function, macro, type, and variable names.

Other elements such as numbers that do not refer to numeric constants that
appear in the source code should not be quoted. For example, in the message:

.. code-block:: c++

  argument %d of %qE must be a pointer type

since the argument number does not refer to a numerical constant in the
source code it should not be quoted.

Spelling and Terminology
^^^^^^^^^^^^^^^^^^^^^^^^

See the `Spelling, terminology and markup <https://gcc.gnu.org/codingconventions.html#Spelling>`_
section of the GCC coding conventions.

.. index:: fix-it hints, diagnostics guidelines, fix-it hints

Fix-it hints
^^^^^^^^^^^^

GCC's diagnostic subsystem can emit :dfn:`fix-it hints`: small suggested
edits to the user's source code.

They are printed by default underneath the code in question.  They
can also be viewed via :option:`-fdiagnostics-generate-patch` and
:option:`-fdiagnostics-parseable-fixits`.  With the latter, an IDE
ought to be able to offer to automatically apply the suggested fix.

Fix-it hints contain code fragments, and thus they should not be marked
for translation.

Fix-it hints can be added to a diagnostic by using a ``rich_location``
rather than a ``location_t`` - the fix-it hints are added to the
``rich_location`` using one of the various ``add_fixit`` member
functions of ``rich_location``.  They are documented with
``rich_location`` in :samp:`libcpp/line-map.h`.
It's easiest to use the ``gcc_rich_location`` subclass of
``rich_location`` found in :samp:`gcc-rich-location.h`, as this
implicitly supplies the ``line_table`` variable.

For example:

.. code-block:: c++

     if (const char *suggestion = hint.suggestion ())
       {
         gcc_rich_location richloc (location);
         richloc.add_fixit_replace (suggestion);
         error_at (&richloc,
                   "%qE does not name a type; did you mean %qs?",
                   id, suggestion);
       }

which can lead to:

.. code-block::

  spellcheck-typenames.C:73:1: error: 'singed' does not name a type; did
     you mean 'signed'?
     73 | singed char ch;
        | ^~~~~~
        | signed

Non-trivial edits can be built up by adding multiple fix-it hints to one
``rich_location``.  It's best to express the edits in terms of the
locations of individual tokens.  Various handy functions for adding
fix-it hints for idiomatic C and C++ can be seen in
:samp:`gcc-rich-location.h`.

Fix-it hints should work
~~~~~~~~~~~~~~~~~~~~~~~~

When implementing a fix-it hint, please verify that the suggested edit
leads to fixed, compilable code.  (Unfortunately, this currently must be
done by hand using :option:`-fdiagnostics-generate-patch`.  It would be
good to have an automated way of verifying that fix-it hints actually fix
the code).

For example, a 'gotcha' here is to forget to add a space when adding a
missing reserved word.  Consider a C++ fix-it hint that adds
``typename`` in front of a template declaration.  A naive way to
implement this might be:

.. code-block:: c++

  gcc_rich_location richloc (loc);
  // BAD: insertion is missing a trailing space
  richloc.add_fixit_insert_before ("typename");
  error_at (&richloc, "need %<typename%> before %<%T::%E%> because "
                       "%qT is a dependent scope",
                       parser->scope, id, parser->scope);

When applied to the code, this might lead to:

.. code-block:: c++

  T::type x;

being 'corrected' to:

.. code-block:: c++

  typenameT::type x;

In this case, the correct thing to do is to add a trailing space after
``typename`` :

.. code-block:: c++

  gcc_rich_location richloc (loc);
  // OK: note that here we have a trailing space
  richloc.add_fixit_insert_before ("typename ");
  error_at (&richloc, "need %<typename%> before %<%T::%E%> because "
                       "%qT is a dependent scope",
                       parser->scope, id, parser->scope);

leading to this corrected code:

.. code-block:: c++

  typename T::type x;

Express deletion in terms of deletion, not replacement
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It's best to express deletion suggestions in terms of deletion fix-it
hints, rather than replacement fix-it hints.  For example, consider this:

.. code-block:: c++

      auto_diagnostic_group d;
      gcc_rich_location richloc (location_of (retval));
      tree name = DECL_NAME (arg);
      richloc.add_fixit_replace (IDENTIFIER_POINTER (name));
      warning_at (&richloc, OPT_Wredundant_move,
                  "redundant move in return statement");

which is intended to e.g. replace a ``std::move`` with the underlying
value:

.. code-block:: c++

     return std::move (retval);
            ~~~~~~~~~~^~~~~~~~
            retval

where the change has been expressed as replacement, replacing
with the name of the declaration.
This works for simple cases, but consider this case:

.. code-block:: c++

  #ifdef SOME_CONFIG_FLAG
  # define CONFIGURY_GLOBAL global_a
  #else
  # define CONFIGURY_GLOBAL global_b
  #endif

  int fn ()
  {
    return std::move (CONFIGURY_GLOBAL /* some comment */);
  }

The above implementation erroneously strips out the macro and the
comment in the fix-it hint:

.. code-block:: c++

     return std::move (CONFIGURY_GLOBAL /* some comment */);
            ~~~~~~~~~~^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            global_a

and thus this resulting code:

.. code-block:: c++

     return global_a;

It's better to do deletions in terms of deletions; deleting the
``std::move (`` and the trailing close-paren, leading to
this:

.. code-block:: c++

     return std::move (CONFIGURY_GLOBAL /* some comment */);
            ~~~~~~~~~~^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            CONFIGURY_GLOBAL /* some comment */

and thus this result:

.. code-block:: c++

     return CONFIGURY_GLOBAL /* some comment */;

Unfortunately, the pertinent ``location_t`` values are not always
available.

.. the above was https://gcc.gnu.org/ml/gcc-patches/2018-08/msg01474.html

Multiple suggestions
~~~~~~~~~~~~~~~~~~~~

In the rare cases where you need to suggest more than one mutually
exclusive solution to a problem, this can be done by emitting
multiple notes and calling
``rich_location::fixits_cannot_be_auto_applied`` on each note's
``rich_location``.  If this is called, then the fix-it hints in
the ``rich_location`` will be printed, but will not be added to
generated patches.