..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _selectors:

Selecting targets to which a test applies
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Several test directives include :samp:`{selector}` s to limit the targets
for which a test is run or to declare that a test is expected to fail
on particular targets.

A selector is:

* one or more target triplets, possibly including wildcard characters;
  use :samp:`*-*-*` to match any target

* a single effective-target keyword (see :ref:`effective-target-keywords`)

* a list of compiler options that should be included or excluded
  (as described in more detail below)

* a logical expression

Depending on the context, the selector specifies whether a test is
skipped and reported as unsupported or is expected to fail.  A context
that allows either :samp:`target` or :samp:`xfail` also allows
:samp:`{ target {selector1} xfail {selector2} }`
to skip the test for targets that don't match :samp:`{selector1}` and the
test to fail for targets that match :samp:`{selector2}`.

A selector expression appears within curly braces and uses a single
logical operator: one of :samp:`!`, :samp:`&&`, or :samp:`||`.  An
operand is one of the following:

* another selector expression, in curly braces

* an effective-target keyword, such as ``lp64``

* a single target triplet

* a list of target triplets within quotes or curly braces

* one of the following:

  :samp:`{ any-opts {opt1} ... {optn} }`
    Each of :samp:`{opt1}` to :samp:`{optn}` is a space-separated list of option globs.
    The selector expression evaluates to true if, for one of these strings,
    every glob in the string matches an option that was passed to the compiler.
    For example:

    .. code-block:: c++

      { any-opts "-O3 -flto" "-O[2g]" }

    is true if any of the following are true:

    * :option:`-O2` was passed to the compiler

    * :option:`-Og` was passed to the compiler

    * both :option:`-O3` and :option:`-flto` were passed to the compiler

    This kind of selector can only be used within ``dg-final`` directives.
    Use ``dg-skip-if``, ``dg-xfail-if`` or ``dg-xfail-run-if`` to
    skip whole tests based on options, or to mark them as expected to fail
    with certain options.

  :samp:`{ no-opts {opt1} ... {optn} }`
    As for ``any-opts`` above, each of :samp:`{opt1}` to :samp:`{optn}` is a
    space-separated list of option globs.  The selector expression
    evaluates to true if, for all of these strings, there is at least
    one glob that does not match an option that was passed to the compiler.
    It is shorthand for:

    .. code-block:: c++

      { ! { any-opts opt1 ... optn } }

    For example:

    .. code-block:: c++

      { no-opts "-O3 -flto" "-O[2g]" }

    is true if all of the following are true:

    * :option:`-O2` was not passed to the compiler

    * :option:`-Og` was not passed to the compiler

    * at least one of :option:`-O3` or :option:`-flto` was not passed to the compiler

    Like ``any-opts``, this kind of selector can only be used within
    ``dg-final`` directives.

Here are some examples of full target selectors:

.. code-block:: c++

  { target { ! "hppa*-*-* ia64*-*-*" } }
  { target { powerpc*-*-* && lp64 } }
  { xfail { lp64 || vect_no_align } }
  { xfail { aarch64*-*-* && { any-opts "-O2" } } }
