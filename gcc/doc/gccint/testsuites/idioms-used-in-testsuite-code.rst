..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _test-idioms:

Idioms Used in Testsuite Code
*****************************

In general, C testcases have a trailing :samp:`-{n}.c`, starting
with :samp:`-1.c`, in case other testcases with similar names are added
later.  If the test is a test of some well-defined feature, it should
have a name referring to that feature such as
:samp:`{feature}-1.c`.  If it does not test a well-defined feature
but just happens to exercise a bug somewhere in the compiler, and a
bug report has been filed for this bug in the GCC bug database,
:samp:`pr{bug-number}-1.c` is the appropriate form of name.
Otherwise (for miscellaneous bugs not filed in the GCC bug database),
and previously more generally, test cases are named after the date on
which they were added.  This allows people to tell at a glance whether
a test failure is because of a recently found bug that has not yet
been fixed, or whether it may be a regression, but does not give any
other information about the bug or where discussion of it may be
found.  Some other language testsuites follow similar conventions.

In the :samp:`gcc.dg` testsuite, it is often necessary to test that an
error is indeed a hard error and not just a warning---for example,
where it is a constraint violation in the C standard, which must
become an error with :option:`-pedantic-errors`.  The following idiom,
where the first line shown is line :samp:`{line}` of the file and the line
that generates the error, is used for this:

.. code-block:: c++

  /* { dg-bogus "warning" "warning in place of error" } */
  /* { dg-error "regexp" "message" { target *-*-* } line } */

It may be necessary to check that an expression is an integer constant
expression and has a certain value.  To check that ``E`` has
value ``V``, an idiom similar to the following is used:

.. code-block:: c++

  char x[((E) == (V) ? 1 : -1)];

In :samp:`gcc.dg` tests, ``__typeof__`` is sometimes used to make
assertions about the types of expressions.  See, for example,
:samp:`gcc.dg/c99-condexpr-1.c`.  The more subtle uses depend on the
exact rules for the types of conditional expressions in the C
standard; see, for example, :samp:`gcc.dg/c99-intconst-1.c`.

It is useful to be able to test that optimizations are being made
properly.  This cannot be done in all cases, but it can be done where
the optimization will lead to code being optimized away (for example,
where flow analysis or alias analysis should show that certain code
cannot be called) or to functions not being called because they have
been expanded as built-in functions.  Such tests go in
:samp:`gcc.c-torture/execute`.  Where code should be optimized away, a
call to a nonexistent function such as ``link_failure ()`` may be
inserted; a definition

.. code-block:: c++

  #ifndef __OPTIMIZE__
  void
  link_failure (void)
  {
    abort ();
  }
  #endif

will also be needed so that linking still succeeds when the test is
run without optimization.  When all calls to a built-in function
should have been optimized and no calls to the non-built-in version of
the function should remain, that function may be defined as
``static`` to call ``abort ()`` (although redeclaring a function
as static may not work on all targets).

All testcases must be portable.  Target-specific testcases must have
appropriate code to avoid causing failures on unsupported systems;
unfortunately, the mechanisms for this differ by directory.

.. todo:: discuss non-C testsuites here
