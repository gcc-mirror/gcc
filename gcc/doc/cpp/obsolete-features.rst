..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _obsolete-features:

Obsolete Features
*****************

CPP has some features which are present mainly for compatibility with
older programs.  We discourage their use in new code.  In some cases,
we plan to remove the feature in a future version of GCC.

.. index:: assertions

Assertions
^^^^^^^^^^

:dfn:`Assertions` are a deprecated alternative to macros in writing
conditionals to test what sort of computer or system the compiled
program will run on.  Assertions are usually predefined, but you can
define them with preprocessing directives or command-line options.

Assertions were intended to provide a more systematic way to describe
the compiler's target system and we added them for compatibility with
existing compilers.  In practice they are just as unpredictable as the
system-specific predefined macros.  In addition, they are not part of
any standard, and only a few compilers support them.
Therefore, the use of assertions is **less** portable than the use
of system-specific predefined macros.  We recommend you do not use them at
all.

.. index:: predicates

An assertion looks like this:

.. code-block:: c++

  #predicate (answer)

:samp:`{predicate}` must be a single identifier.  :samp:`{answer}` can be any
sequence of tokens; all characters are significant except for leading
and trailing whitespace, and differences in internal whitespace
sequences are ignored.  (This is similar to the rules governing macro
redefinition.)  Thus, ``(x + y)`` is different from ``(x+y)`` but
equivalent to ``( x + y )``.  Parentheses do not nest inside an
answer.

.. index:: testing predicates

To test an assertion, you write it in an :samp:`#if`.  For example, this
conditional succeeds if either ``vax`` or ``ns16000`` has been
asserted as an answer for ``machine``.

.. code-block:: c++

  #if #machine (vax) || #machine (ns16000)

You can test whether *any* answer is asserted for a predicate by
omitting the answer in the conditional:

.. code-block:: c++

  #if #machine

.. index:: #assert

Assertions are made with the :samp:`#assert` directive.  Its sole
argument is the assertion to make, without the leading :samp:`#` that
identifies assertions in conditionals.

.. code-block:: c++

  #assert predicate (answer)

You may make several assertions with the same predicate and different
answers.  Subsequent assertions do not override previous ones for the
same predicate.  All the answers for any given predicate are
simultaneously true.

.. index:: assertions, canceling, #unassert

Assertions can be canceled with the :samp:`#unassert` directive.  It
has the same syntax as :samp:`#assert`.  In that form it cancels only the
answer which was specified on the :samp:`#unassert` line; other answers
for that predicate remain true.  You can cancel an entire predicate by
leaving out the answer:

.. code-block:: c++

  #unassert predicate

In either form, if no such assertion has been made, :samp:`#unassert` has
no effect.

You can also make or cancel assertions using command-line options.
See :ref:`invocation`.
